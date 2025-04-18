# TODO: maybe more fancy file format and correctness checking should be done

abstract type Abstract_HG_format end
struct HGF_Format <: Abstract_HG_format end
struct JSON_Format <: Abstract_HG_format end


"""``
    hg_save(io::IO, h::H, format::HGF_Format) where {H <: AbstractSimpleHypergraph}

Saves an undirected hypergraph `h` to an output stream `io` in `hgf` format.

TODO: what to do about metadata?

"""
function hg_save(io::IO, h::H, format::HGF_Format) where {H <: AbstractSimpleHypergraph}
    println(io, length(h.v2he), " ", length(h.he2v))
    for he in h.he2v
        skeys = sort(collect(keys(he)))
        println(io, join(["$k=$(he[k])" for k in skeys], ' '))
    end
end


"""
    hg_save(io::IO, h::Hypergraph, format::JSON_Format)

Saves an undirected hypergraph `h` to an output stream `io` in `json` format.

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
    json_hg[:v2he] = JSON3.write(h.v2he)

    json_hg[:v_meta] = JSON3.write(h.v_meta)
    json_hg[:he_meta] = JSON3.write(h.he_meta)

    JSON3.write(io, json_hg)
end


"""
    hg_save(
        fname::AbstractString, h::AbstractHypergraph;
        format::Abstract_HG_format=HGF_Format()
    )

Saves a hypergraph `h` to a file `fname` in the specified `format`.
The default saving format is `hgf`.

"""
hg_save(
    fname::AbstractString, h::AbstractHypergraph;
    format::Abstract_HG_format=HGF_Format()
    ) = open(io -> hg_save(io, h, format), fname, "w")


"""
    hg_load(
        io::IO,
        format::HGF_Format;
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    ) where {U <: Real, H <: AbstractSimpleHypergraph}

Loads a hypergraph from a stream `io` from `hgf` format.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`

Skips a single initial comment.

"""
function hg_load(
    io::IO,
    format::HGF_Format;
    HType::Type{H} = Hypergraph,
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
) where {U <: Real, H <: AbstractSimpleHypergraph}
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
    h = HType{T, D}(n, k)

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
        T::Type{H},
        format::JSON_Format;
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int,U},
        V = Nothing,
        E = Nothing
    ) where {H <: AbstractHypergraph, U <: Real}

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
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V = Nothing,
        E = Nothing
    ) where {H <: AbstractSimpleHypergraph, U <: Real}
    json_hg = JSON3.read(readline(io))

    m = reshape(JSON3.read(json_hg.m, Array{Union{T, Nothing}}), json_hg.n, json_hg.k)

    if V != Nothing && E != Nothing && hasvertexmeta(HType) && hashyperedgemeta(HType)
        v_meta = JSON3.read(json_hg.v_meta, Array{Union{V, Nothing}})
        he_meta = JSON3.read(json_hg.he_meta, Array{Union{E, Nothing}})
        h = HType{T, V, E, D}(m; v_meta=v_meta, he_meta=he_meta)
    elseif V != Nothing && hasvertexmeta(HType)
        v_meta = JSON3.read(json_hg.v_meta, Array{Union{V, Nothing}})
        h = HType{T, V, D}(m; v_meta=v_meta)
    elseif E != Nothing && hashyperedgemeta(HType)
        he_meta = JSON3.read(json_hg.he_meta, Array{Union{E, Nothing}})
        h = HType{T, E, D}(m; he_meta=he_meta)
    else
        h = HType{T, D}(m)
    end

    h
end


"""
    hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HGF_Format(),
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V = Nothing,
        E = Nothing
    ) where {U <: Real, H <: AbstractSimpleHypergraph}

Loads a hypergraph from a file `fname`.
The default saving format is `hgf`.

**Arguments**

* `HType`: type of hypergraph to store data in
* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph

"""
function hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HGF_Format(),
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V = Nothing,
        E = Nothing
    ) where {U <: Real, H <: AbstractSimpleHypergraph}

    if format == HGF_Format()
        if HType == Hypergraph
            open(io -> hg_load(io, format; HType=HType, T=T, D=D), fname, "r")
        else
            error("HGF loading only implemented for Hypergraph")
        end
    else
        open(io -> hg_load(io, format; HType=HType, T=T, D=D, V=V, E=E), fname, "r")
    end
end
