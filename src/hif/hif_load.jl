using JSON3

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
    D::Type{<:AbstractDict{Int,U}} = Dict{Int,T},
) where {U<:Real}
    _ = format

    data = JSON3.read(read(io, String), Dict{String, Any})

    if !haskey(data, "incidences")
        throw(ArgumentError("Invalid JSON schema: missing required key 'incidences'"))
    end

    nodes, edges = get_nodes_and_edges(data)

    h = init_hypergraph(data, length(nodes), length(edges), T, D)

    add_weights_from_incidences!(h, data["incidences"], nodes, edges)

    h
end


function init_hypergraph(
    data::Dict{String, Any},
    n::Int64,
    k::Int64,
    T::Type{U},
    D::Type{<:AbstractDict{Int,U}},
) where {U<:Real}
    node_metadata = Vector{Union{Dict{String, Any}, Nothing}}()
    edge_metadata = Vector{Union{Dict{String, Any}, Nothing}}()

    if haskey(data, "nodes")
        append!(node_metadata, data["nodes"])
    else
        append!(node_metadata, [nothing for _ in 1:n])
    end

    if haskey(data, "edges")
        append!(edge_metadata, data["edges"])
    else
        append!(edge_metadata, [nothing for _ in 1:k])
    end

    return Hypergraph{T,Dict{String, Any},Dict{String, Any},D}(n, k, node_metadata, edge_metadata)
end


function get_nodes_and_edges(data::Dict{String, Any})
    node_set = Set{Union{String, Int}}()
    edge_set = Set{Union{String, Int}}()

    nodes = Vector{Union{String, Int}}()
    edges = Vector{Union{String, Int}}()

    for inc in data["incidences"]
        if inc["node"] ∉ node_set
            push!(node_set, inc["node"])
            push!(nodes, inc["node"])
        end

        if inc["edge"] ∉ edge_set
            push!(edge_set, inc["edge"])
            push!(edges, inc["edge"])
        end
    end

    return nodes, edges
end


function add_weights_from_incidences!(
    h::Hypergraph, 
    incidences::AbstractVector,
    nodes::Vector{Union{String, Int}},
    edges::Vector{Union{String, Int}}
    )
    node_dict = Dict(val => i for (i, val) in pairs(nodes))
    edge_dict = Dict(val => i for (i, val) in pairs(edges))

    for inc in incidences
        node_idx = node_dict[inc["node"]]
        he_idx = edge_dict[inc["edge"]]

        h[node_idx, he_idx] = inc["weight"]
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
    D::Type{<:AbstractDict{Int,U}} = Dict{Int,T},
) where {U<:Real}
    open(io -> hg_load(io, format; T = T, D = D), fname, "r")
end
