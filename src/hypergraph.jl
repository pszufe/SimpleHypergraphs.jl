"""
    Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}

A hypergraph storing information about vertices and hyperedges.

**Constructors**

    Hypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    Hypergraph{T,V}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n)
        ) where {T<:Real, V}
    Hypergraph{T,V,E}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, V, E}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges.

    Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{V}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1))
        ) where {T<:Real, V}
    Hypergraph{V, E}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, V, E}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges.

    Hypergraph(g::LightGraphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of LightGraphs.Graph

**Arguments**

* `T` : type of weight values stored in the hypergraph
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict`, however for several cases using `SortedDict` will only guarantee replicability
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
"""
struct Hypergraph{T<:Real,V,E,D<:AbstractDict{Int,T}} <: AbstractMatrix{Union{T, Nothing}}
      v2he::Vector{D}
      he2v::Vector{D}
      v_meta::Vector{Union{V,Nothing}}
      he_meta::Vector{Union{E,Nothing}}
      Hypergraph{T,V,E,D}(n::Integer, k::Integer,
              v_meta=Vector{Union{V,Nothing}}(nothing, n),
              he_meta=Vector{Union{E,Nothing}}(nothing, k)
              ) where {T<:Real,V,E,D<:AbstractDict{Int,T}} =
          new{T,V,E,D}(
              [D() for i in 1:n],[D() for i in 1:k],
              v_meta, he_meta)
end


Hypergraph{T,V,E}(n::Integer, k::Integer) where {T<:Real, V, E} = Hypergraph{T,V,E,Dict{Int,T}}(n, k)


Hypergraph{T}(n::Integer, k::Integer) where {T<:Real} =  Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(n, k)


function Hypergraph{V, E}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
                        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
                        ) where {V<:Any,E<:Any,T<:Real}
    n, k = size(m)
    h = Hypergraph{T, V, E, Dict{Int,T}}(n, k, v_meta, he_meta)
    h .= m
    h
end


Hypergraph{V}(m::AbstractMatrix{Union{T, Nothing}};
            v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1))) where {T<:Real, V} =
    Hypergraph{V, Nothing}(m; v_meta=v_meta)

Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real} =
    Hypergraph{Nothing, Nothing}(m)

function Hypergraph(g::LightGraphs.Graph)
    h = Hypergraph{Bool}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end

"""
    Base.size(h::Hypergraph)

Returns the size of Hypergraph `h`.
The result is a tuple of the number of vertices and the number of hyperedges

"""
Base.size(h::Hypergraph) = (nhv(h), nhe(h))

"""
    Base.getindex(h::Hypergraph, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair `idx` for a hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::Hypergraph, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

"""
    Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})

Removes a vertex from a given hyperedge for a hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2], nothing)
    pop!(h.he2v[idx[2]], idx[1], nothing)
    h
end

"""
    Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})

Adds a vertex to a hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end

"""
    getvertices(h::Hypergraph, he_id::Int)

Returns vertices from a hypergraph `a` for a given hyperedge `he_id`.

"""
@inline getvertices(h::Hypergraph, he_id::Int) = h.he2v[he_id]

"""
    gethyperedges(h::Hypergraph, v_id::Int)

Returns hyperedges for a given vertex `v_id` in a hypergraph `h`.

"""
@inline gethyperedges(h::Hypergraph, v_id::Int) = h.v2he[v_id]

"""
    add_vertex!(h::Hypergraph{T, V, E}; hyperedges::Dict{Int,T} = Dict{Int,T}(),
                v_meta::Union{V,Nothing} = nothing) where {T <: Real, V, E}

Adds a vertex to a given hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the vertex using the `v_meta` keyword parameter.

"""
function add_vertex!(h::Hypergraph{T, V, E}; hyperedges::AbstractDict{Int,T} = Dict{Int,T}(),
                     v_meta::Union{V, Nothing} = nothing) where {T <: Real, V, E}
    @boundscheck (checkbounds(h,1,k) for k in keys(hyperedges))
    push!(h.v2he,hyperedges)
    ix = length(h.v2he)
    for k in keys(hyperedges)
        h[ix,k]=hyperedges[k]
    end
    push!(h.v_meta, v_meta)
    ix
end

"""
    remove_vertex!(h::Hypergraph, v::Int)

Removes the vertex `v` from a given hypergraph `h`.
Note that running this function will cause reordering of vertices in the hypergraph:
the vertex `v` will replaced by the last vertex
of the hypergraph and the list of vertices will be shrunk.
"""
function remove_vertex!(h::Hypergraph, v::Int)
    n = nhv(h)
    if v < n
        h.v2he[v] = h.v2he[n]
        h.v_meta[v] = h.v_meta[n]
    end

    for hv in h.he2v
        if v < n && haskey(hv, n)
            hv[v] = hv[n]
            delete!(hv, n)
        else
            delete!(hv, v)
        end
    end
    resize!(h.v2he, length(h.v2he) - 1)
    h
end

"""
    add_hyperedge!(h::Hypergraph{T, V, E};
                   vertices::AbstractDict{Int,T} = Dict{Int,T}(),
                   he_meta::Union{E,Nothing}=nothing) where {T <: Real, V, E}

Adds a hyperedge to a given hypergraph `h`. Optionally, existing vertices can be added
to the created hyperedge. The paramater `vertices` represents a dictionary
of vertex identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the hyperedge using the `he_meta` keyword parameter.

"""
function add_hyperedge!(h::Hypergraph{T, V, E}; vertices::AbstractDict{Int,T} = Dict{Int,T}(),
                        he_meta::Union{E,Nothing}=nothing) where {T <: Real, V, E}
    @boundscheck (checkbounds(h,k,1) for k in keys(vertices))
    push!(h.he2v,vertices)
    ix = length(h.he2v)
    for k in keys(vertices)
        h[k,ix]=vertices[k]
    end
    push!(h.he_meta, he_meta)
    ix
end

"""
    set_vertex_meta!(h::Hypergraph{T, V, E}, new_value::Union{V,Nothing}, id::Int)
        where {T <: Real, V, E}

Sets a new meta value `new_value` for the vertex `id` in the hypegraph `h`.

"""
function set_vertex_meta!(h::Hypergraph{T, V, E}, new_value::Union{V,Nothing}, id::Int) where {T <: Real, V, E}
    checkbounds(h.v_meta, id)
    h.v_meta[id] = new_value
    h.v_meta
end

"""
    get_vertex_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}

Returns a meta value stored at the vertex `id` in the hypergraph `h`.

"""
function get_vertex_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}
    checkbounds(h.v_meta, id)
    h.v_meta[id]
end

"""
    set_hyperedge_meta!(h::Hypergraph{T, V, E}, new_value::Union{E,Nothing}, id::Int)
        where {T <: Real, V, E}

Sets a new meta value `new_value` for the hyperedge `id` in the hypegraph `h`.

"""
function set_hyperedge_meta!(h::Hypergraph{T, V, E}, new_value::Union{E,Nothing}, id::Int) where {T <: Real, V, E}
    checkbounds(h.he_meta, id)
    h.he_meta[id] = new_value
    h.he_meta
end

"""
    get_hyperedge_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}

Returns a meta value stored at the hyperedge `id` in the hypergraph `h`.

"""
function get_hyperedge_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}
    checkbounds(h.he_meta, id)
    h.he_meta[id]
end

"""
    nhe(h::Hypergraph{T, V, E}) where {T <: Real, V, E}

Return the number of hyperedges in the hypergraph `h`.
"""
function nhe(h::Hypergraph{T, V, E}) where {T <: Real, V, E}
    length(h.he2v)
end

"""
    nhv(h::Hypergraph{T, V, E}) where {T <: Real, V, E}

Return the number of vertices in the hypergraph `h`.
"""
function nhv(h::Hypergraph{T, V, E}) where {T <: Real, V, E}
    length(h.v2he)
end

function _default_heselect(h::Hypergraph, v::Int)
    hes = gethyperedges(h, v)
    sort!(collect(keys(hes))), ones(length(hes))
end

function _default_vselect(h::Hypergraph, he::Int)
    vs = getvertices(h, he)
    sort!(collect(keys(vs))), ones(length(vs))
end

"""
    random_walk(h::Hypergraph, start::Int; heselect::Function, vselect::Function)

Return a next vertex visited in assuming a random walk starting from vertex `start`.
First a hyperedge is sampled with weights proportional to `heselect` function
(by default each hyperedge is sampled with the same probability).
Next a vertex within hyperedge is with weights proportional to `vselect` function
(by default each vertex, including the source, is sampled with the same probability).

`heselect` and `vselect` functions take two arguments a `Hypergraph` and respectively
a vertex identifier or a hyperedge identifier. The return values of both functions
should be respectively a list of hyperedges or vertices and their weights.
"""

function random_walk(h::Hypergraph, start::Int;
                     heselect::Function=_default_heselect,
                     vselect::Function=_default_vselect)
    1 <= start <= nhv(h) || throw(ArgumentError("invalid start vertex index"))
    hes, hew = heselect(h, start)
    he = sample(hes, Weights(hew))
    ves, vw = vselect(h, he)
    return sample(ves, Weights(vw))
end

# TODO needs validate_hypergraph!(h::Hypergraph{T})
