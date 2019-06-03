"""
    randompartition(h::Hypergraph, n::Int)::Vector{Set{Int}}

Generates a random partition for vertices of a hypergraph `h` into `n` subsets.
"""
randompartition(h::Hypergraph, n::Int) = randompartition(nhv(h), n)

"""
    randompartition(h::Hypergraph, n::Int)::Vector{Set{Int}}

Generates a random partition for graph having `N` vertices into `n` subsets.
"""
function randompartition(N::Int, n::Int)
    res = [Set{Int}() for i in 1:n]
    for i in 1:N
        push!(res[rand(1:n)], i)
    end
    res
end

"""
HypergraphAggs(h::Hypergraph)

    Precomputes vertex and edge basic stats for a hypergraph.
    The stats are being used for efficiency reasons by community search algorithms.
"""
struct HypergraphAggs
    hes::Vector{Int}
    max_hes::Int
    Ed::Vector{Int}
    deg_vs::Vector{Int}
    volV::Int    
    HypergraphAggs(h::Hypergraph) = begin
        hes = [length(h.he2v[i]) for i in 1:nhe(h)]
        max_hes = maximum(hes)
        Ed = zeros(Int, max_hes)
        for ii in 1:length(hes)
            if hes[ii] >= 1
                Ed[hes[ii]]+=1
            end
        end
        deg_vs = [length(h.v2he[i]) for i in 1:nhv(h)]
        volV = sum(deg_vs)
        new(hes,max_hes,Ed,deg_vs,volV)
    end
end

"""
    modularity(h::Hypergraph, ha::HypergraphAggs, partition::Vector{Set{Int}})

Calculates the strict modularity of a hypergraph `h` for a given `partition` using 
the precomputed aggregates `ha`.
"""
@inline function modularity(h::Hypergraph, ha::HypergraphAggs, partition::Vector{Set{Int}})
    @boundscheck sum(length.(partition)) == nhv(h)
    @boundscheck union(partition...) == Set(1:nhv(h))
    volP_volV = [sum(ha.deg_vs[i] for i in p)/ha.volV for p in partition]
    eP = [count(i-> ha.hes[i]>0 && (keys(h.he2v[i]) ⊆ p), 1:nhe(h)) for p in partition]
    (sum(eP) - sum( ha.Ed[d]*sum(volP_volV.^d) for d in 1:ha.max_hes)) / nhe(h)
end


"""
    modularity(h::Hypergraph, partition::Vector{Set{Int}})

Calculates the strict modularity of a hypergraph `h` for a given `partition`.
"""
@inline function modularity(h::Hypergraph, partition::Vector{Set{Int}})
    @boundscheck sum(length.(partition)) == nhv(h)
    @boundscheck union(partition...) == Set(1:nhv(h))
    @inbounds modularity(h, HypergraphAggs(h), partition)
end


"""
The base type for all algorithms representing various community search patterns. 
"""
abstract type AbstractCommunityFinder end


"""
    CFModularityRandom(n::Int, reps::Int) <: AbstractCommunityFinder

Represents a random search over the hypergraph `h` that finds
a partition into `n` communities (subsets) having the maximum modularity value.
During the search `reps` random `n`-partitions will be evaluated.
If there are many partitions having the same value the first
one that was randomly found will be returned.

"""
struct CFModularityRandom <: AbstractCommunityFinder
    n::Int
    reps::Int
end




"""
    findcommunities(h::Hypergraph, method::CFModularityRandom)::NamedTuple{(:bp, :bm),Tuple{Vector{Vector{Int}}, Float64}}

Makes a random search over the hypergraph `h` and finds
a partition into `method.n` communities (subsets) having the maximum modularity value.
During the search `method.reps` random `n`-partitions will be evaluated.
If there are many partitions having the same value the first
one that was randomly found will be returned.

Returns a `NamedTuple` where the field `bp` contains partition
and the field `bm` contains the modularity value for that partition.
"""
function findcommunities(h::Hypergraph, method::CFModularityRandom)::NamedTuple{(:bp, :bm),Tuple{Vector{Set{Int}}, Float64}}
    bp = [Int[]]
    bm = -Inf
    ha = HypergraphAggs(h)
    for i in 1:method.reps
        p = Vector{Vector{Int}}([[]])
        while  minimum(length.(p)) == 0
            p = randompartition(h, method.n)
        end
        m = modularity(h, ha, p)
        if m > bm
            bm = m
            bp = p
        end
    end
    (bp=bp, bm=bm)
end
