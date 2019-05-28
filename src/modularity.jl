"""
    randompartition(hg::Hypergraph, n::Int)::Vector{Vector{Int}}

Generates a random partition for vertices of a hypergraph `hg` into `n` subsets.
"""
randompartition(hg::Hypergraph, n::Int) = randompartition(size(hg, 1), n)

"""
    randompartition(hg::Hypergraph, n::Int)::Vector{Vector{Int}}

Generates a random partition for graph having `N` vertices into `n` subsets.
"""
function randompartition(N::Int, n::Int)
    res = [Int[] for i in 1:n]
    for i in 1:N
        push!(res[rand(1:n)], i)
    end
    res
end

"""
    modularity(hg::Hypergraph, partition::Vector{Vector{Int}})

Calculates the modularity of a hypergraph `hg` for a given `partition`.
"""
@inline function modularity(hg::Hypergraph, partition::Vector{Vector{Int}})
    @boundscheck sum(length.(partition)) == size(hg, 1)
    @boundscheck sort!(union(partition...)) == axes(hg, 1)
    hes = [length(hg.he2v[i]) for i in axes(hg, 2)]
    max_hes = maximum(hes)
    Ed = zeros(Int, max_hes)
    for ii in 1:length(hes)
        if hes[ii] >= 2   #no single vertex hyperedges
            Ed[hes[ii]]+=1
        end
    end
    deg_vs = [length(hg.v2he[i]) for i in axes(hg, 1)]
    volV = 2*size(hg,2)
    volP_volV = [sum(deg_vs[i] for i in p)/volV for p in partition]
    eP = [count(i-> hes[i]>0 && (keys(hg.he2v[i]) ⊆ p), axes(hg,2)) for p in partition]
    (sum(eP) - sum( Ed[d]*sum(volP_volV.^d) for d in 2:max_hes)) / size(hg, 2)
end


"""
    modularity(hg::Hypergraph)

Calculates the modularity of a hypergraph `hg` assuming a single partition.
"""
modularity(hg::Hypergraph) = modularity(hg, [collect(1:size(hg, 1))])

"""
    findmodularity(hg::Hypergraph, n::Int, reps::Int)::NamedTuple{(:bp, :bm),Tuple{Vector{Vector{Int}}, Float64}}

Makes a random search over the hypergraph `hg` and finds
a `partition into `n` subsets having the maximum modularity value.
During the search `reps` random `n`-partitions will be evaluated.

Returns a `NamedTuple` where the field `bp` contains partition
and the field `bm` contains the modularity value for that partition.
"""
function findmodularity(hg::Hypergraph, n::Int, reps::Int)::NamedTuple{(:bp, :bm),Tuple{Vector{Vector{Int}}, Float64}}
    bp = [Int[]]
    bm = -Inf
    for i in 1:reps
        p = Vector{Vector{Int}}([[]])
        while  minimum(length.(p)) == 0
            p = randompartition(hg, n)
        end
        m = modularity(hg, p)
        if m > bm
            bm = m
            bp = p
        end
    end
    (bp=bp, bm=bm)
end
