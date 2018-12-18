module SimpleHypergraphs

export Hypergraph, getvertices, gethyperedges

# We support two-way mapping as in different problems both might be needed.
# In the matrix representation rows are vertices and columns are hyperedges
# n is number of vertices and k is number of hyperedges
struct Hypergraph <: AbstractMatrix{Bool}
    v2he::Vector{Set{Int}}
    he2v::Vector{Set{Int}}
    Hypergraph(n,k) = new([Set{Int}() for i in 1:n],
                          [Set{Int}() for i in 1:k])
end

Base.size(h::Hypergraph) = (length(h.v2he), length(h.he2v))

@inline function Base.getindex(h::Hypergraph, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    idx[2] in h.v2he[idx[1]]
end

@inline function Base.setindex!(h::Hypergraph, v::Bool, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    if v
        push!(h.v2he[idx[1]], idx[2])
        push!(h.he2v[idx[2]], idx[1])
    else
        pop!(h.v2he[idx[1]], idx[2])
        pop!(h.he2v[idx[2]], idx[1])
    end
    h
end

getvertices(h::Hypergraph, he_id::Int) = h.he2v
gethyperedges(h::Hypergraph, v_id::Int) = h.v2he

end # module
