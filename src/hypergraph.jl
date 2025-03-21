# TODO: think more carefully about ensuring that metadata vectors are of appropriate lengths
# TODO: use haskey instead of x in keys(...)
# TODO: Don't allow empty hyperedges, heads, tails

"""
    Hypergraph{T} <: AbstractSimpleHypergraph{Union{T, Nothing}}

An undirected hypergraph storing information about vertices and hyperedges.

**Constructors**

    Hypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    Hypergraph{T,V}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n)
        ) where {T<:Real, V}
    Hypergraph{T,E}(n::Integer, k::Integer;
        he_meta=Vector{Union{E,Nothing}}(nothing, n)
        ) where {T<:Real, E}
    Hypergraph{T,V,E}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, V, E}
    Hypergraph{T,V,E,D}(n::Integer, k::Integer,
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,V}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1))
        ) where {T<:Real, V}
    Hypergraph{T,E}(m::AbstractMatrix{Union{T, Nothing}};
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, E}
    Hypergraph{T,V,E}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, V, E}
    Hypergraph{T,V,E,D}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, V, E, D<:AbstractDict{Int,T}}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    Hypergraph(g::Graphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of Graphs.Graph.
A `SortedDict` will be used for internal data storage of the hypergraph.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a graph representation of the hypergraph
"""

struct Hypergraph{T<:Real,V,E,D<:AbstractDict{Int,T}} <: AbstractSimpleHypergraph{Union{T, Nothing}}
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
            v_meta, he_meta
        )
end


Hypergraph{T,V,E}(n::Integer, k::Integer) where {T<:Real, V, E} = Hypergraph{T,V,E,Dict{Int,T}}(n, k)

Hypergraph{T,V}(n::Integer, k::Integer) where {T<:Real, V} = Hypergraph{T,V,Nothing,Dict{Int,T}}(n, k)

Hypergraph{T}(n::Integer, k::Integer) where {T<:Real} =  Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(n, k)

Hypergraph(n::Integer, k::Integer) =  Hypergraph{Bool,Nothing,Nothing,Dict{Int,Bool}}(n, k)

function Hypergraph{T,V,E,D}(m::AbstractMatrix{Union{T, Nothing}};
                             v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
                             he_meta::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
                            ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}
    @assert length(v_meta) == size(m,1)
    @assert length(he_meta) == size(m,2)
    n, k = size(m)
    h = Hypergraph{T,V,E,D}(n, k, v_meta, he_meta)
    h .= m
    h
end

function Hypergraph{T,V,E}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
                        he_meta::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
                        ) where {T<:Real,V,E}
    Hypergraph{T,V,E,Dict{Int,T}}(m;v_meta=v_meta,he_meta=he_meta)
end

function Hypergraph{T,V}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1))
                        ) where {T<:Real,V}
    Hypergraph{T,V,Nothing,Dict{Int,T}}(m;v_meta=v_meta)
end

function Hypergraph{T}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(m)
end

function Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(m)
end


function Hypergraph(g::Graphs.Graph)
    h = Hypergraph{Bool,Nothing,Nothing,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end


"""
    BasicHypergraph{T} <: AbstractSimpleHypergraph{Union{T, Nothing}}

An undirected hypergraph storing only incidence and weight information about vertices and
hyperedges, without additional features/metadata.

**Constructors**

    BasicHypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    BasicHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for the internal data storage,
however a different dictionary such as `SortedDict` to ensure result replicability
can be used (e.g. when doing stochastic simulations on hypergraphs).

    BasicHypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    BasicHypergraph{T, D}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for the internal data storage,
however a different dictionary such as `SortedDict` to ensure result
replicability can be used (e.g. when doing stochastic simulations on
hypergraphs).

    BasicHypergraph(g::Graphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of Graphs.Graph.
A `SortedDict` will be used for internal data storage of the hypergraph.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a graph representation of the hypergraph
"""

struct BasicHypergraph{T<:Real,D<:AbstractDict{Int,T}} <: AbstractSimpleHypergraph{Union{T, Nothing}}
    v2he::Vector{D}
    he2v::Vector{D}

    BasicHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real,D<:AbstractDict{Int,T}} =
        new{T,D}([D() for i in 1:n],[D() for i in 1:k])
end

BasicHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = BasicHypergraph{T,Dict{Int,T}}(n, k)

BasicHypergraph(n::Integer, k::Integer) =  BasicHypergraph{Bool,Dict{Int,Bool}}(n, k)


function BasicHypergraph{T,D}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real,D<:AbstractDict{Int,T}}
    n, k = size(m)
    h = BasicHypergraph{T,D}(n, k)
    h .= m
    h
end

function BasicHypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    BasicHypergraph{T,Dict{Int,T}}(m)
end

function BasicHypergraph{T}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    BasicHypergraph{T,Dict{Int,T}}(m)
end


function BasicHypergraph(g::Graphs.Graph)
    h = BasicHypergraph{Bool,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e += 1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end

hasvertexmeta(::Type{Hypergraph}) = true
hasvertexmeta(X::Hypergraph) = true
hashyperedgemeta(::Type{Hypergraph}) = true
hashyperedgemeta(X::Hypergraph) = true

# AbstractArray interface functions

"""
    Base.size(h::AbstractHypergraph)

Returns the size of hypergraph `h`.
The result is a tuple of the number of vertices and the number of hyperedges

"""
Base.size(h::AbstractHypergraph) = (nhv(h), nhe(h))


"""
    Base.getindex(h::H, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}

Returns a value for a given vertex-hyperedge pair `idx` for an undirected hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::H, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end


"""
    Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}

Removes a vertex from a given hyperedge for an undirected hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2], nothing)
    pop!(h.he2v[idx[2]], idx[1], nothing)
    h
end


"""
    Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}

Adds a vertex to an undirected hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end


"""
    getvertices(h::H, he_id::Int) where {H <: AbstractSimpleHypergraph}

Returns vertices from an undirected hypergraph `a` for a given hyperedge `he_id`.

"""
@inline getvertices(h::H, he_id::Int) where {H <: AbstractSimpleHypergraph} = h.he2v[he_id]


"""
    gethyperedges(h::H, v_id::Int) where {H <: AbstractSimpleHypergraph}

Returns hyperedges for a given vertex `v_id` in an undirected hypergraph `h`.

"""
@inline gethyperedges(h::H, v_id::Int) where {H <: AbstractSimpleHypergraph} = h.v2he[v_id]


#TODO: should there be a way to add hyperedge metadata as well as vertex metadata?
"""
    add_vertex!(h::Hypergraph{T, V, E, D};
                hyperedges::D = D(), v_meta::Union{V,Nothing} = nothing
                ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a vertex to a given undirected hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the vertex using the `v_meta` keyword
parameter.

"""
function add_vertex!(h::Hypergraph{T, V, E, D};
                     hyperedges::D = D(), v_meta::Union{V,Nothing} = nothing
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
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
    add_vertex!(h::BasicHypergraph{T, D};
                hyperedges::D = D()
                ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a vertex to a given undirected hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.

"""
function add_vertex!(h::BasicHypergraph{T, D};
                     hyperedges::D = D()
                    ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,1,k) for k in keys(hyperedges))
    push!(h.v2he,hyperedges)
    ix = length(h.v2he)
    for k in keys(hyperedges)
        h[ix,k]=hyperedges[k]
    end
    ix
end


"""
    remove_vertex!(h::Hypergraph, v::Int)

Removes the vertex `v` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
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
    resize!(h.v_meta, length(h.v_meta) - 1)
    h
end

"""
    remove_vertex!(h::BasicHypergraph, v::Int)

Removes the vertex `v` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
"""
function remove_vertex!(h::BasicHypergraph, v::Int)
    n = nhv(h)
    if v < n
        h.v2he[v] = h.v2he[n]
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
    add_hyperedge!(h::Hypergraph{T, V, E, D};
                   vertices::D = D(), he_meta::Union{E,Nothing}=nothing
                   ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given undirected hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge.
The paramater `vertices` represents a dictionary of vertex identifiers and
values stored at the hyperedges. Additionally, a value can be stored with the
hyperedge using the `he_meta` keyword parameter.

"""
function add_hyperedge!(h::Hypergraph{T, V, E, D};
                        vertices::D = D(), he_meta::Union{E,Nothing}=nothing
                        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
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
    add_hyperedge!(h::BasicHypergraph{T, D};
                   vertices::D = D()
                   ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given undirected hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge.
The paramater `vertices` represents a dictionary of vertex identifiers and
values stored at the hyperedges.

"""
function add_hyperedge!(h::BasicHypergraph{T, D};
                        vertices::D = D()
                        ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,k,1) for k in keys(vertices))
    push!(h.he2v,vertices)
    ix = length(h.he2v)
    for k in keys(vertices)
        h[k,ix]=vertices[k]
    end

    ix
end


"""
    remove_hyperedge!(h::Hypergraph, e::Int)
Removes the hyperedge `e` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges (and hyperedge metadata) will be shrunk.
"""
function remove_hyperedge!(h::Hypergraph, e::Int)
    ne = nhe(h)
	@assert(e <= ne)
	if e < ne
	    h.he2v[e] = h.he2v[ne]
	    h.he_meta[e] = h.he_meta[ne]
	end

    for he in h.v2he
	    if e < ne && haskey(he, ne)
		    he[e] = he[ne]
            delete!(he, ne)
		else
			delete!(he, e)
		end
    end
    resize!(h.he2v, length(h.he2v) - 1)
    resize!(h.he_meta, length(h.he_meta) - 1)
    h
end


"""
    remove_hyperedge!(h::BasicHypergraph, e::Int)
Removes the hyperedge `e` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges will be shrunk.
"""
function remove_hyperedge!(h::BasicHypergraph, e::Int)
    ne = nhe(h)
	@assert(e <= ne)
	if e < ne
	    h.he2v[e] = h.he2v[ne]
	end

    for he in h.v2he
	    if e < ne && haskey(he, ne)
		    he[e] = he[ne]
            delete!(he, ne)
		else
			delete!(he, e)
		end
    end
    resize!(h.he2v, length(h.he2v) - 1)
    h
end


"""
    prune_hypergraph!(h::H) where {H <: AbstractSimpleHypergraph}

Remove all vertices with degree 0 and all hyperedges of size 0.

"""
function prune_hypergraph!(h::H) where {H <: AbstractSimpleHypergraph}
	for e in reverse(1:nhe(h))
        length(h.he2v[e]) == 0 && remove_hyperedge!(h,e)
    end
	for v in reverse(1:nhv(h))
    	length(h.v2he[v]) == 0 && remove_vertex!(h,v)
    end
	h
end


"""
    prune_hypergraph(h::H) where {H <: AbstractHypergraph}

Return a pruned copy of `h`, removing all vertices with degree 0 and
all hyperedges of size 0.

"""
function prune_hypergraph(h::H) where {H <: AbstractHypergraph}
    prune_hypergraph!(deepcopy(h))
end


"""
    set_vertex_meta!(h::Hypergraph{T, V, E, D}, new_value::Union{V,Nothing},
        id::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the vertex `id` in the hypergraph `h`.

"""
function set_vertex_meta!(h::Hypergraph{T, V, E, D},
        new_value::Union{V,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.v_meta, id)
    h.v_meta[id] = new_value
    h.v_meta
end


set_vertex_meta!(::BasicHypergraph, ::Any, ::Int) = throw("Not implemented!")


"""
    get_vertex_meta(h::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, id::Int
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Returns a meta value stored at the vertex `id` in the undirected hypergraph `h`.

"""
function get_vertex_meta(h::Hypergraph{T, V, E, D}, id::Int
                         ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.v_meta, id)
    h.v_meta[id]
end


get_vertex_meta(::BasicHypergraph, ::Int) = throw("Not implemented!")


"""
    set_hyperedge_meta!(h::Hypergraph{T, V, E, D},
        new_value::Union{E,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the hyperedge `id` in the undirected hypergraph `h`.

"""
function set_hyperedge_meta!(h::Hypergraph{T, V, E, D},
                            new_value::Union{E,Nothing}, id::Int
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta, id)
    h.he_meta[id] = new_value
    h.he_meta
end


set_hyperedge_meta!(::BasicHypergraph, ::Any, ::Int) = throw("Not implemented!")


"""
    get_hyperedge_meta(h::Hypergraph{T, V, E, D}, id::Int)
        where {T <: Real, V, E, D <: AbstractDict{Int,T}}
Returns a meta value stored at the hyperedge `id` in the undirected hypergraph `h`.

"""
function get_hyperedge_meta(h::Hypergraph{T, V, E, D}, id::Int
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta, id)
    h.he_meta[id]
end


get_hyperedge_meta(::BasicHypergraph, ::Int) = throw("Not implemented!")


"""
    nhe(h::H) where {H <: AbstractSimpleHypergraph}

Return the number of hyperedges in the undirected hypergraph `h`.
"""
function nhe(h::H) where {H <: AbstractSimpleHypergraph}
    length(h.he2v)
end


"""
    nhv(h::H) where {H <: AbstractSimpleHypergraph}

Return the number of vertices in the undirected hypergraph `h`.
"""
function nhv(h::H) where {H <: AbstractSimpleHypergraph}
    length(h.v2he)
end


function _default_heselect(h::H, v::Int) where {H <: AbstractSimpleHypergraph}
    hes = gethyperedges(h, v)
    sort!(collect(keys(hes))), ones(length(hes))
end


function _default_vselect(h::H, he::Int) where {H <: AbstractSimpleHypergraph}
    vs = getvertices(h, he)
    sort!(collect(keys(vs))), ones(length(vs))
end


"""
    random_walk(
        h::H,
        start::Int;
        heselect::Function,
        vselect::Function,
    ) where {H <: AbstractSimpleHypergraph}

Return a next vertex visited in assuming a random walk starting from vertex `start`.
First a hyperedge is sampled with weights proportional to `heselect` function
(by default each hyperedge is sampled with the same probability).
Next a vertex within hyperedge is with weights proportional to `vselect` function
(by default each vertex, including the source, is sampled with the same probability).

`heselect` and `vselect` functions take two arguments a `Hypergraph` and respectively
a vertex identifier or a hyperedge identifier. The return values of both functions
should be respectively a list of hyperedges or vertices and their weights.
"""
function random_walk(h::H, start::Int;
                     heselect::Function=_default_heselect,
                     vselect::Function=_default_vselect) where {H <: AbstractSimpleHypergraph}
    1 <= start <= nhv(h) || throw(ArgumentError("invalid start vertex index"))
    hes, hew = heselect(h, start)
    he = sample(hes, Weights(hew))
    ves, vw = vselect(h, he)
    return sample(ves, Weights(vw))
end


"""
    _walk!(h::H, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool}) where where {H <: AbstractSimpleHypergraph}

Appends the list of neighbors `s` of a given vertex `i` (an auxiliary function for `get_connected_components`).
"""
function _walk!(h::H, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool}) where {H <: AbstractSimpleHypergraph}
    visited[i] && return
    visited[i] = true
    push!(s, i)
    for he in keys(gethyperedges(h, i))
        for j in keys(getvertices(h, he))
            _walk!(h, s, j, visited)
        end
    end
end


"""
    get_connected_components(h::H) where {H <: AbstractSimpleHypergraph}

Return an array of connected components in the undirected hypergraph `h`
(array of vectors of vertices) using recurrence.
"""
function get_connected_components(h::H) where {H <: AbstractSimpleHypergraph}
    visited = falses(nhv(h))
    cc = Vector{Int}[]
    for i in 1:nhv(h)
        if !visited[i]
            s = Int[]
            _walk!(h, s, i, visited)
            push!(cc, s)
        end
    end
    cc
end


"""
    adjacency_matrix(M::AbstractMatrix{Union{T, Nothing}}; s::Int=1, weighted::Bool=false) where {T <: Real}

The sparse weighted `s`-adjacency matrix.

NOTE
The concept of `s`-adjacency matrix has been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/classes/classes.html#classes.hypergraph.Hypergraph.adjacency_matrix)
If weighted is `true` each off diagonal cell will equal the number
of edges shared by the nodes indexing the row and column if that number is
greater than `s`, otherwise the cell will equal 0. If weighted is `false`,
the off diagonal cell will equal 1 if the nodes indexed by the row and column
share at least `s` edges and 0 otherwise.

NOTE: information about the weight of a vertex in a hyperedge will be lost!

"""
function adjacency_matrix(M::AbstractMatrix{Union{T, Nothing}}; s::Int=1, weighted::Bool=true) where {T <: Real}
    _incidence_to_adjacency(M; s=s, weighted=weighted)
end


"""
    adjacency_matrix(h::H; s::Int=1, weighted::Bool=false) where {H <: AbstractSimpleHypergraph}

The sparse weighted `s`-adjacency matrix.

NOTE
The concept of `s`-adjacency matrix has been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/classes/classes.html#classes.hypergraph.Hypergraph.adjacency_matrix)
If weighted is `true` each off diagonal cell will equal the number
of edges shared by the nodes indexing the row and column if that number is
greater than `s`, otherwise the cell will equal 0. If weighted is `false`,
the off diagonal cell will equal 1 if the nodes indexed by the row and column
share at least `s` edges and 0 otherwise.

NOTE: information about the weight of a vertex in a hyperedge will be lost!

"""
function adjacency_matrix(h::H; s::Int=1, weighted::Bool=true) where {H <: AbstractSimpleHypergraph}
    M = Matrix(h)
    _incidence_to_adjacency(M; s=s, weighted=weighted)
end


"""
    edge_adjacency_matrix(h::Hypergraph; s::Int=1, weighted::Bool=false)

The sparse weighted `s`-adjacency matrix for the dual hypergraph.

NOTE
The concept of `s`-adjacency matrix has been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/classes/classes.html#classes.hypergraph.Hypergraph.edge_adjacency_matrix)
This is also the adjacency matrix for the line graph.
Two edges are `s`-adjacent if they share at least `s` nodes.

If weighted is `true` each off diagonal cell will equal the number
of nodes shared by the hyperedges indexing the row and column if that number is
greater than `s`, otherwise the cell will equal 0. If weighted is `false`,
the off diagonal cell will equal 1 if the hyperedges indexed by the row and column
share at least `s` nodes and 0 otherwise.

"""
function edge_adjacency_matrix(h; s::Int=1, weighted::Bool=true)
    M = Matrix(h)
	M[M .== nothing] .= 0
    _incidence_to_adjacency(transpose(M); s=s, weighted=weighted)
end


"""
    _incidence_to_adjacency(M; s::Int=1, weighted::Bool=true)

Helper method to obtain adjacency matrix from incidence matrix.

"""
function _incidence_to_adjacency(M; s::Int=1, weighted::Bool=true)
    M[M .== nothing] .= 0

    # TODO: Should this be M[M .!= 0] .= 1?
    M[M .> 0] .= 1

    A = *(M, transpose(M))
    A[diagind(A)] .= 0

    if s > 1
        A = A .* (A .>= s)
    end
    if !weighted
        A = (A .> 0) .* 1
    end

    A
end


# TODO needs validate_hypergraph!(h::Hypergraph{T})
