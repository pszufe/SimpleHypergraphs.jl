using JSON3
using DataFrames


struct HIF_Format <: Abstract_HG_format end


function hg_load(
    io::IO,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V::Type{Z} = Union{String, Int},
    E::Type{Z} = Union{String, Int},
    sort_by_id::Bool=false,
    show_warning::Bool=true,
) where {U<:Real, Z<:Union{Int, String}}
    data = JSON3.read(read(io, String), Dict{String, Any})

    haskey(data, "incidences") || throw(ArgumentError("Missing required attribute 'incidences'"))

    if isempty(data["incidences"])
        if isempty(get(data, "edges", [])) && isempty(get(data, "nodes", []))
            return Hypergraph{
                T, 
                Union{V, Dict{String, Any}}, 
                Union{E, Dict{String, Any}}, 
                D,
            }(0, 0)
        elseif isempty(data["edges"]) || isempty(data["nodes"])
            throw(ArgumentError("When incidences are empty, both 'nodes' and 'edges' must contain data"))
        end
    end

    edges = build_edges_dataframe(data, E)
    nodes = build_nodes_dataframe(data, V)

    add_nodes_and_edges_from_incidences!(data, edges, nodes, V, E)

    if sort_by_id
        sort!(edges, (:edge))
        sort!(nodes, (:node))
    end

    if show_warning
        if edges.edge != 1:nrow(edges)
            @warn "Edges in the source file were not sorted - their order was changed."
        end

        if nodes.node != 1:nrow(nodes)
            @warn "Nodes in the source file were not sorted - their order was changed"
        end
    end

    v_meta = Vector{Union{V, Dict{String, Any}}}()
    he_meta = Vector{Union{V, Dict{String, Any}}}()

    for row in eachrow(nodes)
        attrs = row.attrs
        if isnothing(attrs)
            attrs = row.node
        end
        push!(v_meta, attrs)
    end

    for row in eachrow(edges)
        attrs = row.attrs
        if isnothing(attrs)
            attrs = row.edge
        end

        push!(he_meta, attrs)
    end

    hg = Hypergraph{
        T, 
        Union{V, Dict{String, Any}}, 
        Union{E, Dict{String, Any}}, 
        D,
    }(nrow(nodes), nrow(edges), v_meta, he_meta)

    add_weights_from_incidences!(data, hg, edges, nodes, V, E)

    hg
end


function hg_load(
    fname::String,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V::Type{Z} = Union{String, Int},
    E::Type{Z} = Union{String, Int},
    sort_by_id::Bool=false,
    show_warning::Bool=true,
) where {U<:Real, Z<:Union{Int, String}}
    open(io -> hg_load(io, format, T=T, D=D, V=V, E=E, sort_by_id=sort_by_id, show_warning=show_warning), fname, "r")
end



function add_weights_from_incidences!(
    data::Dict{String, Any},
    hg::Hypergraph,
    edges::DataFrame,
    nodes::DataFrame,
    V::Type{Z},
    E::Type{Z}
) where {Z<:Union{Int, String}}
    edge_dict = Dict{E, Int}(row.edge => idx for (row, idx) in zip(eachrow(edges), 1:nrow(edges))) 
    node_dict = Dict{V, Int}(row.node => idx for (row, idx) in zip(eachrow(nodes), 1:nrow(nodes)))

    incidences = data["incidences"]

    for inc in incidences
        haskey(edge_dict, inc["edge"]) || continue  # duplicates
        haskey(node_dict, inc["node"]) || continue  # duplicates
        edge_idx = edge_dict[inc["edge"]]
        node_idx = node_dict[inc["node"]]

        weight = (haskey(inc, "weight")) ? inc["weight"] : 1

        hg[node_idx, edge_idx] = weight

        pop!(edge_dict, inc["edge"])
        pop!(node_dict, inc["node"])
    end

    # add remaining items
    for (node, edge) in zip(values(node_dict), values(edge_dict))
        hg[node, edge] = 1
    end

end

function build_edges_dataframe(
    data::Dict{String, Any},
    E::Type{Z}
) where {Z<:Union{Int, String}}
    edges = DataFrame(
        ; 
        edge=E[], 
        attrs=Union{Nothing, Dict{String, Any}}[]
    )

    if !haskey(data, "edges")
        return edges
    end

    seen = Set{Union{Int, String}}()

    for edge in data["edges"]
        if edge["edge"] ∈ seen
            continue
        end
        attrs = (haskey(edge, "attrs")) ? edge["attrs"] : nothing

        push!(edges, [edge["edge"], attrs])
        push!(seen, edge["edge"])
    end

    edges
end

function build_nodes_dataframe(
    data::Dict{String, Any},
    V::Type{Z}
) where {Z<:Union{Int, String}}
    nodes = DataFrame(
        ; 
        node=V[], 
        attrs=Union{Nothing, Dict{String, Any}}[]
    )

    if !haskey(data, "nodes")
        return nodes
    end

    seen = Set{Union{String, Int}}()

    for node in data["nodes"]
        if node["node"] ∈ seen
            continue
        end

        attrs = (haskey(node, "attrs")) ? node["attrs"] : nothing

        push!(nodes, [node["node"], attrs])
        push!(seen, node["node"])
    end

    nodes
end


function add_nodes_and_edges_from_incidences!(
    data::Dict{String, Any},
    edges::DataFrame,
    nodes::DataFrame,
    V::Type{Z},
    E::Type{Z}
) where {Z<:Union{Int, String}}
    edge_ids = Set{E}(edges.edge)
    node_ids = Set{V}(nodes.node)
    for incidence in data["incidences"]
        node = incidence["node"]
        edge = incidence["edge"]

        if node ∉ node_ids
            push!(nodes, [node, nothing])
            push!(node_ids, node)
        end

        if edge ∉ edge_ids
            push!(edges, [edge, nothing])
            push!(edge_ids, edge)
        end

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

    incidences = Vector{Dict{String, Union{String, Int}}}()

    for i in 1:length(h.v_meta)
        for j in 1:length(h.he_meta)

            if isnothing(h[i, j])
                continue
            end

            weight = h[i, j]

            push!(incidences, Dict{String, Union{String, Int}}("edge" => i, "node" => j, "weight" => Int(weight)))
        end
    end

    json_hg = Dict{Symbol, Any}(:incidences => incidences)

    JSON3.write(io, json_hg)
end
