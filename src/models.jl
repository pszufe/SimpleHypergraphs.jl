"""
    Models for random hypergraphs building:
        * random model
        * k-uniform model
        * d-uniform model
        * preferential-attachment
"""


"""
    random_model(nVertices, nEdges)

Generate a *random* hypergraph without any structural constraints.

# The Algorithm

Given two integer parameters *nVertices* and *nEdges* (the number of nodes and hyperedges, respectively),
the algorithm computes - for each hyperedge *he={1,...,m}* -
a random number *s ϵ [1, n]* (i.e. the hyperedge size).
Then, the algorithm selects uniformly at random *s* vertices from *V* to be added in *he*.
"""
function random_model(nVertices::Int, nEdges::Int)
    mx = Matrix{Union{Nothing,Bool}}(nothing, nVertices, nEdges)
    if nEdges == 0
        return Hypergraph(nVertices, nEdges)
    end
    for e in 1:size(mx,2)
        nv = rand(1:size(mx,1))
        mx[sample(1:size(mx,1), nv;replace=false), e] .= true
    end

    h = Hypergraph(mx)
    if all(length.(h.v2he) .> 0)
        return h
    else
        return random_model(nVertices, nEdges)
    end
end


"""
    random_kuniform_model(nVertices, nEdges, k)

Generates a *k*-uniform hypergraph, i.e. an hypergraph where each hyperedge has size *k*.

# The Algorithm

The algorithm proceeds as the *randomH*, forcing the size of each hyperedge equal to *k*.
"""
function random_kuniform_model(nVertices::Int, nEdges::Int, k::Int)
    mx = Matrix{Union{Nothing,Bool}}(nothing, nVertices,nEdges)
    for e in 1:size(mx,2)
        mx[sample(1:size(mx,1), k;replace=false), e] .= true
    end
    Hypergraph(mx)
end


"""
    random_dregular_model(nVertices, nEdges, d)

Generates a *d*-regular hypergraph, where each node has degree *d*.

# The Algorithm

The algorithm exploits the *k*-uniform approach described for the *random_kuniform_model* method
to build a *d*-regular hypergraph *H* having *nVertices* nodes and *nEdges* edges.
It returns the hypergraph H^* dual of *H*.
"""
function random_dregular_model(nVertices::Int, nEdges::Int, d::Int)
    mx = Matrix{Union{Nothing,Bool}}(nothing, nVertices,nEdges)
    for v in 1:size(mx,1)
        mx[v, sample(1:size(mx,2), d;replace=false)] .= true
    end
    Hypergraph(mx)
end


"""
    random_preferential_model(nVertices::Int, p::Real; H = random_model(5,5))

Generate a hypergraph with a preferential attachment rule between nodes, as presented in
*Avin, C., Lotker, Z., and Peleg, D.Random preferential attachment hyper-graphs. Computer Science 23 (2015).*

# The Algorithm

The algorithm starts with a random graph with 5 nodes and 5 edges. For this reason,
the generated random graph has at least 5 nodes and 5 edges.
It iteratively adds a node or a edge, according to a given parameter *p*,
which defines the probability of creating a new node or a new hyperedge.

More in detail, the connections with the new node/hyperedge are generated according to
a preferential attachment policy defined by _p_.

The so-built hypergraph will have `nVertices` vertices.

The starting hypergraph can be instantiated as preferred.
"""
function random_preferential_model(nVertices::Int, p::Real; H = random_model(5,5))
    while nhv(H) < nVertices
        r = rand()
        y = rand(1:nhv(H))
        Y = Dict{Int,Bool}()
        if r < p
            v = SimpleHypergraphs.add_vertex!(H)
            push!(Y,v=>true)
            for v in next_nodes(H,y-1)
                push!(Y,v)
            end
        else
            for v in next_nodes(H,y)
                push!(Y,v)
            end
        end
        SimpleHypergraphs.add_hyperedge!(H, vertices=Y)
    end
    H
end


"""
    next_nodes(h::Hypergraph, size::Int)

Selects nodes to add to a hyperedge.
"""
function next_nodes(h::Hypergraph, size::Int)
    nodes = Dict{Int, Bool}()

    ids = collect(1:nhv(h))
    degrees = length.(h.v2he)
    
    for s=1:size
        psum = Vector{Int}(undef, length(ids))

        psum[1] = degrees[ids[1]]
        for j=2:length(ids)
            psum[j] = psum[j-1] + degrees[ids[j]]
        end

        number = rand(1:psum[length(psum)])
        local bucket::Int
        index=1
        for i=1:length(psum)
            if number <= psum[i]
                bucket = ids[i]
                index = i
                break
            end
        end

        nodes[bucket] = true
        deleteat!(ids, index)
    end
    nodes
end