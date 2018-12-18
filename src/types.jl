# We support two-way mapping as in different problems both might be needed.
# In the matrix representation rows are vertices and columns are hyperedges
# n is number of vertices and k is number of hyperedges
# if he[vertex, hyperedge] returns nothing this means that the vertex
# is not present in the hyperedge
# if he[vertex, hyperedge] returns a real number it is a weight
# of the vertex in this hyperedge
struct Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}
    v2he::Vector{Dict{Int,T}}
    he2v::Vector{Dict{Int,T}}
    Hypergraph{T}(n,k) where {T<:Real} =
        new([Dict{Int,T}() for i in 1:n], [Dict{Int,T}() for i in 1:k])
end

function Hypergraph(m::AbstractMatrix{T}) where {T<:Real}
    h = Hypergraph{T}(size(m)...)
    h .= m
    h
end

Base.size(h::Hypergraph) = (length(h.v2he), length(h.he2v))

@inline function Base.getindex(h::Hypergraph, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

@inline function Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2])
    pop!(h.he2v[idx[2]], idx[1])
    h
end

@inline function Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end

@inline getvertices(h::Hypergraph, he_id::Int) = h.he2v[he_id]
@inline gethyperedges(h::Hypergraph, v_id::Int) = h.v2he[v_id]
