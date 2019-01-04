# We support two-way mapping as in different problems both might be needed.
# In the matrix representation rows are vertices and columns are hyperedges
# n is number of vertices and k is number of hyperedges
# if he[vertex, hyperedge] returns nothing this means that the vertex
# is not present in the hyperedge
# if he[vertex, hyperedge] returns a real number it is a weight
# of the vertex in this hyperedge

"""
    Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}

A hypergraph storing information about vertices and hyperedges.

**Constructors**

    Hypergraph{T}(n,k) where {T<:Real}

Construct a hypergraph with a given number of vertices and hyperedges.

    Hypergraph(m::AbstractMatrix{T}) where {T<:Real}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.

**Arguments**

* `T` : type of values stored in the hypergraph
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges

"""
struct Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}
    v2he::Vector{Dict{Int,T}}
    he2v::Vector{Dict{Int,T}}
    Hypergraph{T}(n, k) where {T<:Real} =
        new([Dict{Int,T}() for i in 1:n], [Dict{Int,T}() for i in 1:k])
end

function Hypergraph(m::AbstractMatrix{T}) where {T<:Real}
    h = Hypergraph{T}(size(m)...)
    h .= m
    h
end

"""
    Base.size(h::Hypergraph)

Returns the size of Hypergraph m.
The result is a tuple of the number of vertices and the number of hyperedges

**Arguments**

* `h` : a hypergraph

"""
Base.size(h::Hypergraph) = (length(h.v2he), length(h.he2v))


"""
    Base.getindex(h::Hypergraph, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair or `nothing` if a vertex does not belong to a hyperedge.

**Arguments**

* `h` : a hypergraph
* `idx` : an index where the first element is vertex and the second is a hyperedge
"""
@inline function Base.getindex(h::Hypergraph, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

"""
    Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})

Removes a vertex from a given hyperedge

**Arguments**

* `h` : a hypergraph
* `idx` : an index where the first element is vertex and the second is a hyperedge
"""
@inline function Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2])
    pop!(h.he2v[idx[2]], idx[1])
    h
end

"""
    Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})

Adds a vertex to a hyperedge and assigns value to be stored with that assignment.

**Arguments**

* `h` : a hypergraph
* `v` : a value to be stored with vertex-hyperedge assignment
* `idx` : an index where the first element is vertex and the second is a hyperedge
"""
@inline function Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end

"""
    getvertices(h::Hypergraph, he_id::Int)

Returns vertices for a given hyperedge

**Arguments**

* `h` : a hypergraph
* `he_id` : an identifier of a hyperedge
"""
@inline getvertices(h::Hypergraph, he_id::Int) = h.he2v[he_id]

"""
    gethyperedges(h::Hypergraph, v_id::Int)

Returns hyperedges for a given vertex

**Arguments**

* `h` : a hypergraph
* `v_id` : an identifier of a vertex
"""
@inline gethyperedges(h::Hypergraph, v_id::Int) = h.v2he[v_id]

function add_vertex!(h::Hypergraph{T};hyperedges::Dict{Int,T} = Dict{Int,T}()) where T <: Real
    push!(h.v2he,hyperedges)
    length(h.v2he)
end

function add_hyperedge!(h::Hypergraph{T};vertices::Dict{Int,T} = Dict{Int,T}()) where T <: Real
    push!(h.he2v,vertices)
    length(h.he2v)
end
