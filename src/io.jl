
abstract type Abstract_HG_format end
"""
    HGF_Format
Simple text serialization format.
This format only stores the incidence structure of the hypergraph and ignores any metadata.
"""
struct HGF_Format <: Abstract_HG_format end

"""
    JSON_Format

    Implementation of the `JSON` format for hypergraph input/output.
Note that more advanced features are supported in `HIF_Format`.
"""
struct JSON_Format <: Abstract_HG_format end

"""
    HIF_Format

    Implementation of the `HIF` (Hypergraph Interchange Format) format for hypergraph input/output.
See https://github.com/pszufe/HIF-standard for more details about the format.
See also the paper https://doi.org/10.1017/nws.2025.10018
"""
struct HIF_Format <: Abstract_HG_format end
# note that the HIF format support is implemented in a separate file io_hif.jl

"""
    elemtypes(v::AbstractVector) 
    
Returns the union of the element types of the elements of vector `v`.
"""
elemtypes(v::AbstractVector) = foldl((T, x) -> Union{T, typeof(x)}, v; init=Union{})

"""
    hg_save(io::IO, h::H, format::HGF_Format; pretty::Bool=false) where {H <: AbstractSimpleHypergraph}

Saves an undirected hypergraph `h` to an output stream `io` in `hgf` format.
This format only stores the incidence structure of the hypergraph and ignores any metadata.

TODO: pretty option currently ignored

"""
function hg_save(io::IO, h::H, ::HGF_Format; pretty::Bool=false) where {H <: AbstractSimpleHypergraph}
    println(io, length(h.v2he), " ", length(h.he2v))
    for he in h.he2v
        skeys = sort(collect(keys(he)))
        println(io, join(["$k=$(he[k])" for k in skeys], ' '))
    end
end


"""
    hg_save(io::IO, h::Hypergraph, ::JSON_Format; pretty::Bool=false)

Saves an undirected hypergraph `h` to an output stream `io` in `json` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
...TODO: complete this part


The `json` in output contains the following information (keys):

* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation of `h` where rows are vertices and columns are hyperedges
* `v2he` : mapping vertices to hyperedges
* `v_meta` : vertices metadata
* `he_meta` : hyperedges metadata

"""
function hg_save(io::IO, h::Hypergraph, ::JSON_Format; pretty::Bool=false)
    json_hg = Dict{Symbol, Any}()

    json_hg[:n] = nhv(h)
    json_hg[:k] = nhe(h)

    #vec was addded when upgrading to JSON.jl from JSON3.jl 
    #to ensure proper serialization - JSON3 serialized to vec
    json_hg[:m] = vec(Matrix(h))

    json_hg[:v2he] = h.v2he

    json_hg[:v_meta] = h.v_meta
    json_hg[:he_meta] = h.he_meta
    JSON.json(io, json_hg; pretty)
end


"""
    hg_save(
        fname::AbstractString, h::AbstractHypergraph;
        format::Abstract_HG_format=HGF_Format(), pretty::Bool=false
    )

Saves a hypergraph `h` to a file `fname` in the specified `format`.
The default saving format is `hgf`.
"""
function hg_save(
    fname::AbstractString, h::AbstractHypergraph;
        format::Abstract_HG_format = HGF_Format(), pretty::Bool=false
    )
    open(io -> hg_save(io, h, format; pretty=pretty), fname, "w")
end





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
    ::HGF_Format;
    HType::Type{H} = Hypergraph,
    T::Type{U} = Bool,
    D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
    V = Nothing,
    E = Nothing
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
    h = HType{T, V, E, D}(n, k)

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
        ::JSON_Format;
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        V = Nothing,
        E = Nothing
    ) where {H <: AbstractSimpleHypergraph, U <: Real}
    json_hg = JSON.parse(read(io, String))
    m = reshape(Vector{Union{T, Nothing}}(json_hg.m), json_hg.n, json_hg.k)
    
    V2 = (V == :auto) ? ("v_meta" ∈ keys(json_hg) && length(json_hg.v_meta) > 0 ? elemtypes(json_hg.v_meta) : Nothing) : V
    E2 = (E == :auto) ? ("he_meta" ∈ keys(json_hg) && length(json_hg.he_meta) > 0 ? elemtypes(json_hg.he_meta) : Nothing) : E

    if V2 != Nothing && E2 != Nothing && hasvertexmeta(HType) && hashyperedgemeta(HType)
        v_meta = Vector{Union{V2, Nothing}}(json_hg.v_meta)
        he_meta = Vector{Union{E2, Nothing}}(json_hg.he_meta)
        h = HType{T, V2, E2, D}(m; v_meta, he_meta)
    elseif V2 != Nothing && hasvertexmeta(HType)
        v_meta = Vector{Union{V2, Nothing}}(json_hg.v_meta)
        h = HType{T, V2, D}(m; v_meta=v_meta)
    elseif E2 != Nothing && hashyperedgemeta(HType)
        he_meta = Vector{Union{E2, Nothing}}(json_hg.he_meta)
        h = HType{T, E2, D}(m; he_meta=he_meta)
    else
        h = HType{T, V2, E2, D}(m)
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
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `show_warning` : whether to show warnings during loading
* `sort_by_id` : whether to sort vertices and hyperedges by their original ids (only supported for HIF_Format)
* `add_original_id_to_meta` : if a `Symbol` is provided, the original ids are added to the vertex and hyperedge metadata under that key (only supported for HIF_Format)
"""
function hg_load(
        fname::AbstractString;
        format::Abstract_HG_format = HGF_Format(),
        HType::Type{H} = Hypergraph,
        T::Type{U} = Bool,
        V = :auto,
        E = :auto,
        D::Type{<:AbstractDict{Int, U}} = Dict{Int, T},
        show_warning::Bool=true,
        sort_by_id::Bool=false,
        add_original_id_to_meta::Union{Symbol, Nothing}=nothing
    ) where {U <: Real, H <: AbstractSimpleHypergraph}
    @assert format isa HGF_Format || !sort_by_id  "sort_by_id only supported for HIF_Format"
    if format isa HGF_Format
        if HType == Hypergraph
            open(io -> hg_load(io, format; HType, T, D), fname, "r")
        else
            error("HGF loading only implemented for Hypergraph")
        end
    else
        if format isa HIF_Format
            open(io -> hg_load(io, format; HType, T, D, V, E, show_warning, sort_by_id, add_original_id_to_meta), fname, "r")
        else
            open(io -> hg_load(io, format; HType, T, D, V, E), fname, "r")
        end
    end
end
