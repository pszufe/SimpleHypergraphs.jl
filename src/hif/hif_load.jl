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
