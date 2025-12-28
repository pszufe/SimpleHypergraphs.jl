"""
    hg_load(
        io::IO,
        format::HIF_Format;
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V::Union{Type, Symbol} = :auto,
        E::Union{Type, Symbol} = :auto,
        sort_by_id::Bool=false,
        add_original_id_to_meta::Union{Symbol, Nothing}=nothing,
        show_warning::Bool=true
    ) where {U<:Real, H <: AbstractSimpleHypergraph}

Loads a hypergraph from an input stream `io` in `HIF` format where
 `T` is type of weight values stored in the hypergraph's adjacency matrix 
and `D` is the type of the dictionary used to store weights for each hyperedge.

If the hypergraph has vertex metadata or hyperedge metadata, their types can be specified
using the `V` and `E` parameters respectively. 
If `V` or `E` is set to `:auto`, the types will be inferred from the data.

SimpleHypergraphs.jl uses 1-based indexing so node and edge ids are regenerated accordingly.
The original ids of vertices and hyperedges can be preserved by setting `add_original_id_to_meta` 
to a Symbol representing the key under which the original id will be stored in the metadata dictionary.
"""
function hg_load(
    io::IO,
    ::HIF_Format;
    HType::Type{H} = Hypergraph,
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V::Union{Type, Symbol} = :auto,
    E::Union{Type, Symbol} = :auto,
    sort_by_id::Bool=false,
    add_original_id_to_meta::Union{Symbol, Nothing}=nothing,
    show_warning::Bool=true,
) where {U<:Real, H <: AbstractSimpleHypergraph}
    data = JSON.parse(io; dicttype=Dict{Symbol, Any})

    haskey(data, :incidences) || throw(ArgumentError("Missing required attribute 'incidences'"))

    if isempty(data[:incidences])
        if isempty(get(data, :edges, [])) && isempty(get(data, :nodes, []))
            return Hypergraph{
                T, 
                V == :auto ? Nothing : V, 
                E == :auto ? Nothing : E,
                D,
            }(0, 0)
        end
    end

    nodesdf = _build_attr_dataframe(data, :nodes, V, add_original_id_to_meta)
    edgesdf = _build_attr_dataframe(data, :edges, E, add_original_id_to_meta)

    attr_nodes_N = nrow(nodesdf)  
    attr_edges_N = nrow(edgesdf)
    if attr_nodes_N == 0 && isnothing(add_original_id_to_meta)
        # no node attributes found so all attrs set to Nothing
        nodesdf.attrs = Nothing[]
    end
    if attr_edges_N == 0 && isnothing(add_original_id_to_meta)
        # no edge attributes found so all attrs set to Nothing
        edgesdf.attrs = Nothing[]
    end

    _add_nodes_and_edges_from_incidences!(data, nodesdf, edgesdf, add_original_id_to_meta)

    # narrow types for attrs if V or E is :auto
    if V == :auto
        _sanitize_types_items!(nodesdf)
    end
    if E == :auto
        _sanitize_types_items!(edgesdf)
    end

    _add_id_sort_column!(nodesdf)
    _add_id_sort_column!(edgesdf)
    # if all nodes or edges were discovered from incidences, sort by id to have consistent ordering
    if attr_nodes_N == 0
        sort!(nodesdf, :id_sort)
    end
    if attr_edges_N == 0
        sort!(edgesdf, :id_sort)
    end

    if sort_by_id
        sort!(nodesdf, :id_sort)
        sort!(edgesdf, :id_sort)
    end

    if show_warning
        if nrow(nodesdf) > 0 && nodesdf.id != 1:nrow(nodesdf)
            @warn "Nodes in the source file were not sorted or not consistent - their order will change"
        end

        if nrow(edgesdf) > 0 && edgesdf.id != 1:nrow(edgesdf)
            @warn "Edges in the source file were not sorted or not consistent - their order will change"
        end
    end

    hg = HType{
        T, 
        eltype(nodesdf.attrs), 
        eltype(edgesdf.attrs), 
        D,
    }(nrow(nodesdf), nrow(edgesdf), nodesdf.attrs, edgesdf.attrs)

    _add_weights_from_incidences!(data, hg, edgesdf, nodesdf)
    hg
end


"""
    _add_weights_from_incidences!(data::Dict{String, Any}, hg::AbstractSimpleHypergraph, edges::DataFrame, nodes::DataFrame)

THIS FUNCTION IS INTERNAL AND SHOULD NOT BE CALLED DIRECTLY.
Adds weights to the hypergraph `hg` based on the incidences provided in `data`.
The `edges` and `nodes` DataFrames are used to map edge and node identifiers to
their respective indices in the hypergraph.
"""
function _add_weights_from_incidences!(data::Dict{Symbol, Any}, 
            hg::AbstractSimpleHypergraph{Union{Nothing, T}}, 
            edges::DataFrame, nodes::DataFrame) where {T <: Real}
    node_dict = Dict{Union{String, Int}, Int}(id => idx for (id, idx) in zip(nodes.id, 1:nrow(nodes)))
    edge_dict = Dict{Union{String, Int}, Int}(id => idx for (id, idx) in zip(edges.id, 1:nrow(edges))) 
    incidences = data[:incidences]
    for incidence in incidences
        node_idx = node_dict[incidence[:node]]
        edge_idx = edge_dict[incidence[:edge]]
        weight = get(incidence, :weight, one(T))
        hg[node_idx, edge_idx] = T(weight)
    end
end

function _build_attr_dataframe(data::Dict{Symbol, Any}, field::Symbol, V::Union{Type, Symbol},
            add_original_id_to_meta::Union{Symbol, Nothing})
    @assert field ∈ (:nodes, :edges)
    fid = Symbol(string(field)[1:end-1])  # :node or :edge

    target_attr_type = Union{Nothing, Any} 
    if V != :auto
        if isnothing(add_original_id_to_meta)
            target_attr_type = Union{Nothing, V}
        else
            target_attr_type = Union{Nothing, Dict{Symbol, Union{V, Int, String}}}
        end
    end
    items = DataFrame(; 
        id=Union{String, Int}[], 
        attrs= target_attr_type[]
    )
    if !haskey(data, field)
        return items
    end
    seen = Set{Union{Int, String}}()
    for item in data[field]
        id = item[fid]
        if id ∈ seen
            continue
        end
        val = get(item, :attrs, nothing)
        if V == String && val !== nothing && !(isa(val, String))
            val = JSON.json(val)
        end
        if isnothing(add_original_id_to_meta)
            if val !== nothing && V != :auto
                val = convert(V, val)
            end
        else 
            if isnothing(val)
                val = Dict(add_original_id_to_meta => id)
            elseif val isa AbstractDict && ismutable(val)
                val[add_original_id_to_meta] = id
            else 
                val = Dict{Symbol, Union{typeof(val), typeof(id)}}(
                    add_original_id_to_meta => id, :value => val
                ) 
            end
        end
        push!(items, [id, val])
        push!(seen, id)
    end
    items
end

function _add_id_sort_column!(items::DataFrame)
    if any(x -> x isa String, items.id) 
        items.id_sort = string.(items.id)
    else
        items.id_sort = Int.(items.id)
    end
end

"""
    _sanitize_types_items!(items::DataFrame)
Narrow the type of the `attrs` column in the provided `items` DataFrame
THIS FUNCTION IS INTERNAL AND SHOULD NOT BE CALLED DIRECTLY.
"""
function _sanitize_types_items!(items::DataFrame)
    types = unique!(typeof.(items.attrs))
    if length(types) <= 5
        vals = Union{Nothing, types...}[]
        items.attrs = append!(vals, items.attrs)
    end
end

"""
    _add_nodes_and_edges_from_incidences!(data::Dict{String, Any}, nodes::DataFrame, edges::DataFrame)

THIS FUNCTION IS INTERNAL AND SHOULD NOT BE CALLED DIRECTLY.
Adds nodes and edges to the provided `nodes` and `edges` DataFrames
based on the incidences provided in `data`.
"""
function _add_nodes_and_edges_from_incidences!(data::Dict{Symbol, Any}, nodes::DataFrame, edges::DataFrame, add_original_id_to_meta::Union{Symbol, Nothing})
    seen_node_ids = Set{Union{String, Int}}(nodes.id)
    seen_edge_ids = Set{Union{String, Int}}(edges.id)
    for incidence in data[:incidences]
        node = incidence[:node]
        edge = incidence[:edge]
        if node ∉ seen_node_ids
            if isnothing(add_original_id_to_meta)
                push!(nodes, [node, nothing])
            else
                push!(nodes, [node, Dict(add_original_id_to_meta => node)])
            end
            push!(seen_node_ids, node)
        end
        if edge ∉ seen_edge_ids
            if isnothing(add_original_id_to_meta)
                push!(edges, [edge, nothing])
            else
                push!(edges, [edge, Dict(add_original_id_to_meta => edge)])
            end
            push!(seen_edge_ids, edge)
        end
    end
end


"""
    hg_save(io::IO, h::Hypergraph, format::HIF_Format; pretty::Bool=false)

Saves a hypergraph `h` to an output stream `io` in `HIF` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
TODO: complete this part

"""
function hg_save(io::IO, h::Hypergraph{T, V, E, D}, ::HIF_Format; pretty::Bool=false) where {T, V, E, D}
    incidences = Vector{OrderedDict{Symbol, Union{Int, T}}}()
    for i in 1:nhv(h)
        for j in sort!(collect(keys(gethyperedges(h, i))))
            push!(incidences, OrderedDict{Symbol, Union{Int, T}}(:node => i, :edge => j, :weight => T(h[i, j])))
        end
    end
    #decide whether to include metadata for nodes and edges
    #there are two poossible reasons to include metadata:
    #1. there is at least one metadata entry
    #2. there is at least one node or edge with no connections (isolated vertex or empty hyperedge)
    node_meta_included = any(x -> !(isnothing(x)), h.v_meta ) || any(v -> isempty(gethyperedges(h, v)), 1:nhv(h))
    edge_meta_included = any(x -> !(isnothing(x)), h.he_meta) || any(e -> isempty(getvertices(h, e)), 1:nhe(h))
    
    json_node_meta = Vector{OrderedDict{Symbol, Any}}()
    json_edge_meta = Vector{OrderedDict{Symbol, Any}}()
    
    if node_meta_included
        for i in 1:nhv(h)
            node_entry = OrderedDict{Symbol, Union{Int, typeof(h.v_meta[i])}}(:node => i)
            if !(isnothing(h.v_meta[i]))
                node_entry[:attrs] = h.v_meta[i]
            end
            push!(json_node_meta, node_entry)
        end
    end
    if edge_meta_included
        for j in 1:nhe(h)
            edge_entry = OrderedDict{Symbol, Union{Int, typeof(h.he_meta[j])}}(:edge => j)
            if !(isnothing(h.he_meta[j]))
                edge_entry[:attrs] = h.he_meta[j]
            end
            push!(json_edge_meta, edge_entry)
        end
    end
    json_hg = OrderedDict{Symbol, Union{typeof(incidences), typeof(json_node_meta), typeof(json_edge_meta)}}()
    json_hg[:incidences] = incidences
    if length(json_node_meta) > 0
        json_hg[:nodes] = json_node_meta
    end
    if length(json_edge_meta) > 0
        json_hg[:edges] = json_edge_meta
    end
    JSON.json(io, json_hg; pretty)  
end
