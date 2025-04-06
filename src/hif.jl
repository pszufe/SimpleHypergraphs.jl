using JSON3


struct HIF_Format <: Abstract_HG_format end

"""
    hg_load(
        io::IO,
        format::HIF_Format;
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
    ) where {U <: Real}

Loads a hypergraph from a stream `io` from `HIF` format.
More info: https://github.com/pszufe/HIF-standard

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
"""
function hg_load(
    io::IO,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V = Nothing,
    E = Nothing
) where {U<:Real}
    data = JSON3.read(read(io, String), Dict{String, Any})

    if !haskey(data, "incidences")
        throw(ArgumentError("Invalid JSON schema: missing required key 'incidences'"))
    end

    nodes, edges = get_nodes_and_edges(data, V, E)

    h = init_hypergraph(data, nodes, edges, T, D, V, E)

    add_weights_from_incidences!(h, data["incidences"], nodes, edges)

    h
end


function init_hypergraph(
    data::Dict{String, Any},
    nodes::AbstractVector{Union{String, Int}},
    edges::AbstractVector{Union{String, Int}},
    T::Type{U},
    D::Type{<:AbstractDict{Int,U}},
    V = Nothing,
    E = Nothing
) where {U<:Real}
    n = haskey(data, "nodes") ? max(length(nodes), length(data["nodes"])) : length(nodes)
    k = haskey(data, "edges") ? max(length(edges), length(data["edges"])) : length(edges)

    node_metadata = Vector{Union{V, Nothing}}([nothing for _ in 1:n])
    edge_metadata = Vector{Union{E, Nothing}}([nothing for _ in 1:k])

    if haskey(data, "nodes")
        tmp = [node_obj["node"] for node_obj in data["nodes"]]
        s_tmp = Set{V}()

        for (i, node) in pairs(tmp)
            if node in s_tmp
                continue
            end

            node_metadata[i] = node
            push!(s_tmp, node)
        end
    end

    if haskey(data, "edges")
        tmp = [edge_obj["edge"] for edge_obj in data["edges"]]
        s_tmp = Set{E}()

        for (i, edge) in pairs(tmp)
            if edge in s_tmp
                continue
            end

            edge_metadata[i] = edge
            push!(s_tmp, edge)
        end
    end

    return Hypergraph{T,V,E,D}(n, k, node_metadata, edge_metadata)
end


function get_nodes_and_edges(data::Dict{String, Any}, V, E)
    node_set = Set{Union{String, Int}}()
    edge_set = Set{Union{String, Int}}()

    nodes = Vector{Union{String, Int}}()
    edges = Vector{Union{String, Int}}()

    for inc in data["incidences"]
        node = (V == String) ? string(inc["node"]) : inc["node"]
        edge = (E == String) ? string(inc["edge"]) : inc["edge"]

        if node ∉ node_set
            push!(node_set, node)
            push!(nodes, node)
        end

        if edge ∉ edge_set
            push!(edge_set, edge)
            push!(edges, edge)
        end
    end

    sort!(nodes)
    sort!(edges)

    return nodes, edges
end


function add_weights_from_incidences!(
    h::Hypergraph{T,V,E,D}, 
    incidences::AbstractVector,
    nodes::Vector{Union{String, Int}},
    edges::Vector{Union{String, Int}}
    ) where {T, V, E, D}
    node_dict = Dict(val => i for (i, val) in pairs(nodes))
    edge_dict = Dict(val => i for (i, val) in pairs(edges))

    for inc in incidences
        node = (V == String) ? string(inc["node"]) : inc["node"]
        edge = (E == String) ? string(inc["edge"]) : inc["edge"]
        node_idx = node_dict[node]
        he_idx = edge_dict[edge]

        h[node_idx, he_idx] = haskey(inc, "weight") ? inc["weight"] : 1
    end
end


"""
    hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HIF_Format(),
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        ) where {U <: Real}
    )
Loads a hypergraph from a file `fname`.
The default saving format is `json`.
**Arguments**
* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
"""
function hg_load(
    fname::AbstractString,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V = Nothing,
    E = Nothing
) where {U<:Real}
    open(io -> hg_load(io, format; T = T, D = D, V = V, E = E), fname, "r")
end


HIFEntryType = Dict{String, Union{String, Number, Dict{String, Any}}}

"""
    hg_save(io::IO, h::Hypergraph, format::HIF_Format)

Saves a hypergraph `h` to an output stream `io` in `HIF` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
the user has to explicit tell the JSON3 package about it, for instance using:

`JSON3.StructType(::Type{MyType}) = JSON3.Struct()`.

See the (JSON3.jl documentation)[https://github.com/quinnj/JSON3.jl] for more details.

"""
function hg_save(io::IO, h::Hypergraph{T, V, E, D}, format::HIF_Format) where {T, V, E, D}
    _ = format

    json_hg = Dict{Symbol,Any}()

    nodes_meta = prepare_metadata(h.v_meta, handle_node)
    edges_meta = prepare_metadata(h.he_meta, handle_edge)

    incidences = prepare_incidences(h)
    
    json_hg[:incidences] = incidences

    if !isempty(nodes_meta)
        json_hg[:nodes] = nodes_meta
    end

    if !isempty(edges_meta)
        json_hg[:edges] = edges_meta
    end

    JSON3.write(io, json_hg)
end


function prepare_incidences(h::Hypergraph{T, V, E, D}) where {T, V, E, D}
    incidences = Vector{HIFEntryType}()

    for node_idx in eachindex(h.v_meta)
        edges = gethyperedges(h, node_idx)

        node = isnothing(h.v_meta[node_idx]) ? node_idx : h.v_meta[node_idx]

        _node = (V == Dict{String, Any}) ? node["node"] : node

        for (edge, weight) in edges
            _edge = isnothing(h.he_meta[edge]) ? edge : h.he_meta[edge]
            if T == Bool
                push!(incidences, Dict("edge" => _edge, "node" => _node))
            else
                push!(incidences, Dict("edge" => _edge, "node" => _node, "weight" => weight))
            end
        end
    end

    return incidences
end


function prepare_metadata(
    metadata::Vector{Union{T, Nothing}}, 
    handling_func::Function
) where {T}
    result = Vector{HIFEntryType}()

    for item in metadata
        if isnothing(item)
            continue
        end

        handled = handling_func(item)
        push!(result, handled)
    end

    return result
end


function handle_node(node::Union{String, Int})
    return Dict{String, Union{String, Int}}(
        "node" => node
    )
end

function handle_node(node::Dict{String, Any})
    result = HIFEntryType(
        "node" => node["node"]
    )

    add_optional_params!(result, node)

    return result
end


function handle_edge(edge::Union{String, Int})
    return Dict{String, Union{String, Int}}(
        "edge" => edge
    )
end


function handle_edge(edge::Dict{String, Any})
    result = HIFEntryType(
        "edge" => edge["edge"]
    )

    add_optional_params!(result, edge)

    return result
end


function add_optional_params!(result::HIFEntryType, item::Dict{String, Any})
    if haskey(item, "weight")
        result["weight"] = item["weight"]
    end

    if haskey(item, "attrs")
        result["attrs"] = item["attrs"]
    end
end
