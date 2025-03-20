# TODO: maybe more fancy file format and correctness checking should be done

abstract type Abstract_HG_format end
struct HGF_Format <: Abstract_HG_format end
struct JSON_Format <: Abstract_HG_format end
struct HIF_Format <: Abstract_HG_format end


"""
    hg_save(io::IO, h::Hypergraph, format::HGF_Format)

Saves a hypergraph `h` to an output stream `io` in `hgf` format.

"""
function hg_save(io::IO, h::Hypergraph, format::HGF_Format)
    println(io, length(h.v2he), " ", length(h.he2v))
    for he in h.he2v
        skeys = sort(collect(keys(he)))
        println(io, join(["$k=$(he[k])" for k in skeys], ' '))
    end
end


"""
    hg_save(io::IO, h::Hypergraph, format::JSON_Format)

Saves a hypergraph `h` to an output stream `io` in `json` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
the user has to explicit tell the JSON3 package about it, for instance using:

`JSON3.StructType(::Type{MyType}) = JSON3.Struct()`.

See the (JSON3.jl documentation)[https://github.com/quinnj/JSON3.jl] for more details.

The `json` in output contains the following information (keys):

* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation of `h` where rows are vertices and columns are hyperedges
* `v2he` : mapping vertices to hyperedges
* `v_meta` : vertices metadata
* `he_meta` : hyperedges metadata

"""
function hg_save(io::IO, h::Hypergraph, format::JSON_Format)
    json_hg = Dict{Symbol, Any}()

    json_hg[:n] = nhv(h)
    json_hg[:k] = nhe(h)

    json_hg[:m] = JSON3.write(Matrix(h))
    json_hg[:v2he] = JSON3.write(h. v2he)

    json_hg[:v_meta] = JSON3.write(h.v_meta)
    json_hg[:he_meta] = JSON3.write(h.he_meta)

    JSON3.write(io, json_hg)
end


"""
    hg_save(
        fname::AbstractString, h::Hypergraph;
        format::Abstract_HG_format=HGF_Format()
    )

Saves a hypergraph `h` to a file `fname` in the specified `format`.
The default saving format is `hgf`.

"""
hg_save(
    fname::AbstractString, h::Hypergraph;
    format::Abstract_HG_format=HGF_Format()) =
    open(io -> hg_save(io, h, format), fname, "w")


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

    node_dict = Dict(i => val for (i, val) in pairs(h.v_meta))
    edge_dict = Dict(i => val for (i, val) in pairs(h.he_meta))

    incidences = []

    for node_idx in 1:length(h.v_meta)
        for edge_idx in 1:length(h.he_meta)
            node = node_dict[node_idx]
            edge = edge_dict[edge_idx]
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
        format::HGF_Format();
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V=Nothing,
        E=Nothing
    ) where {U <: Real}

Loads a hypergraph from a stream `io` from `hgf` format.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

Skips a single initial comment.

"""
function hg_load(
    io::IO,
    format::HGF_Format;
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V=Nothing,
    E=Nothing
) where {U <: Real}
    line = readline(io)

    if startswith(line, "\"\"\"")
      singleline = true
        while(
            !( (!singleline && endswith(line, "\"\"\"")) ||
            (singleline && endswith(line, "\"\"\"") && length(line)>5)
            ) &&
            !eof(io)
            )
                line = readline(io)
                singleline = false
        end
        if eof(io)
            throw(ArgumentError("malformed input"))
        end
       line = readline(io)
    end

    l = split(line)
    length(l) == 2 || throw(ArgumentError("expected two integers"))
    n, k = parse.(Int, l)
    h = Hypergraph{T, V, E, D}(n, k)

    for i in 1:k
        lastv = 0
        for pos in split(readline(io))
            entry = split(pos, '=')
            length(entry) == 2 || throw(ArgumentError("expected vertex=weight"))
            v = parse(Int, entry[1])
            w = parse(T, entry[2])

            if v > lastv
                lastv = v
            else
                throw(ArgumentError("vertices in hyperedge must be sorted"))
            end
            h[v, i] = w
        end
    end
    # we ignore lines beyond k+1 in the file
    h
end


"""
    hg_load(
        io::IO,
        format::JSON_Format;
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V = Nothing,
        E = Nothing
    ) where {U <: Real}

Loads a hypergraph from a stream `io` from `json` format.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

"""
function hg_load(
        io::IO,
        format::JSON_Format;
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V = Nothing,
        E = Nothing
    ) where {U <: Real}
    json_hg = JSON3.read(readline(io))

    m = reshape(JSON3.read(json_hg.m, Array{Union{T, Nothing}}), json_hg.n, json_hg.k)

    v_meta = Vector{Union{Nothing,V}}(nothing, size(m, 1))
    he_meta = Vector{Union{Nothing,E}}(nothing, size(m, 2))

    if V != Nothing
        v_meta = JSON3.read(json_hg.v_meta, Array{Union{V, Nothing}})
    end

    if E != Nothing
        he_meta = JSON3.read(json_hg.he_meta, Array{Union{E, Nothing}})
    end

    h = Hypergraph{T, V, E, D}(m; v_meta=v_meta, he_meta=he_meta)

    h
end


"""
    hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HGF_Format(),
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V = Nothing,
        E = Nothing) where {U <: Real}
    )

Loads a hypergraph from a file `fname`.
The default saving format is `hgf`.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

"""
hg_load(
    fname::AbstractString;
    format::Abstract_HG_format = HGF_Format(),
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V = Nothing,
    E = Nothing) where {U <: Real} =
    open(io -> hg_load(io, format; T=T, D=D, V=V, E=E), fname, "r")



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
