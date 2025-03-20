struct HIF_Format <: Abstract_HG_format end


"""
    hg_save(io::IO, h::Hypergraph, format::HIF_Format)

Saves a hypergraph `h` to an output stream `io` in `HIF` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
the user has to explicit tell the JSON3 package about it, for instance using:

`JSON3.StructType(::Type{MyType}) = JSON3.Struct()`.

See the (JSON3.jl documentation)[https://github.com/quinnj/JSON3.jl] for more details.

"""
function hg_save(io::IO, h::Hypergraph, format::HIF_Format)
    _ = format

    json_hg = Dict{Symbol, Any}()
    incidences = []
    v_meta = h.v_meta
    he_meta = h.he_meta

    if any(isnothing, h.v_meta)
        v_meta = [i for i in 1:length(h.v_meta)]
    end

    if any(isnothing, h.he_meta)
        he_meta = [i for i in 1:length(h.he_meta)]
    end

    node_dict = Dict(i => val for (i, val) in pairs(v_meta))
    edge_dict = Dict(i => val for (i, val) in pairs(he_meta))


    types = collect(typeof(h).parameters)
    V = types[2]
    E = types[3]

    for node_idx in 1:length(v_meta)
        for edge_idx in 1:length(he_meta)
            node = node_dict[node_idx]
            if V == String
                node = string(node)
            end

            edge = edge_dict[edge_idx]
            if E == String
                edge = string(edge)
            end

            weight = h[node_idx, edge_idx]

            if isnothing(weight)
                continue
            end

            push!(incidences, Dict(
                "edge" => edge,
                "node" => node,
                "weight" => weight
            ))
        end
    end

    json_hg[:incidences] = incidences
    
    JSON3.write(io, json_hg)
end


"""
    hg_load(
        io::IO,
        format::HIF_Format;
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V::Union{Type{String}, Type{Int}} = String,
        E::Union{Type{String}, Type{Int}} = String
    ) where {U <: Real}

Loads a hypergraph from a stream `io` from `HIF` format.
More info: https://github.com/pszufe/HIF-standard

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

"""
function hg_load(
    io::IO,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V::Union{Type{String}, Type{Int}} = String,
    E::Union{Type{String}, Type{Int}} = String
    ) where {U <: Real}
    _ = format

    data = JSON3.read(read(io, String))

    nodes = get(data, "nodes", [])
    edges = get(data, "edges", [])

    if length(nodes) == 0 || length(edges) == 0
        node_set = Set{V}()
        edge_set = Set{E}()

        for inc in data.incidences
            if inc.node ∉ node_set
                push!(node_set, inc.node)
                push!(nodes, inc.node)
            end

            if inc.edge ∉ edge_set
                push!(edge_set, inc.edge)
                push!(edges, inc.edge)
            end
        end
    else
        nodes = [node.node for node in nodes]
        edges = [edge.edge for edge in edges]        
    end

    sort!(nodes)
    sort!(edges)

    node_dict = Dict(val => i for (i, val) in pairs(nodes))
    edge_dict = Dict(val => i for (i, val) in pairs(edges))

    n = length(nodes)
    k = length(edges)

    h = Hypergraph{T, V, E, D}(n, k, nodes, edges)

    for inc in data.incidences
        node_idx = node_dict[inc.node]
        he_idx = edge_dict[inc.edge]

        h[node_idx, he_idx] = inc.weight
    end

    h
end


"""
    hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HIF_Format(),
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V::Union{Type{String}, Type{Int}} = String,
        E::Union{Type{String}, Type{Int}} = String
        ) where {U <: Real}
    )

Loads a hypergraph from a file `fname`.
The default saving format is `json`.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

"""
function hg_load(
    fname::AbstractString,
    format::HIF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V::Union{Type{String}, Type{Int}} = String,
    E::Union{Type{String}, Type{Int}} = String
    ) where {U <: Real}
    open(io -> hg_load(io, format; T=T, D=D, V=V, E=E), fname, "r")
end
