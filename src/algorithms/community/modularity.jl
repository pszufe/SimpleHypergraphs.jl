"""
    randompartition(h::Hypergraph, n::Int)::Vector{Set{Int}}

Generates a random partition for vertices of a hypergraph `h` into `n` subsets.
"""
randompartition(h::Hypergraph, n::Int) = randompartition(nhv(h), n)


"""
    randompartition(N::Int, n::Int)::Vector{Set{Int}}

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
    Graphs.modularity(h::Hypergraph, partition::Vector{Set{Int}},
ha::HypergraphAggs=HypergraphAggs(h))

Calculates the strict modularity of a hypergraph `h` for a given `partition` using
the precomputed aggregates `ha`.
"""
@inline function Graphs.modularity(h::Hypergraph, partition::Vector{Set{Int}},
        ha::HypergraphAggs=HypergraphAggs(h))

    @boundscheck sum(length.(partition)) == nhv(h)
    @boundscheck union(partition...) == Set(1:nhv(h))
    volP_volV = [sum(ha.deg_vs[i] for i in p)/ha.volV for p in partition]
    eP = [count(i-> ha.hes[i]>0 && (keys(h.he2v[i]) ⊆ p), 1:nhe(h)) for p in partition]
    (sum(eP) - sum( ha.Ed[d]*sum(volP_volV.^d) for d in 1:ha.max_hes)) / nhe(h)
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
    findcommunities(h::Hypergraph, method::CFModularityRandom)

Makes a random search over the hypergraph `h` and finds
a partition into `method.n` communities (subsets) having the maximum modularity value.
During the search `method.reps` random `n`-partitions will be evaluated.
If there are many partitions having the same value the first
one that was randomly found will be returned.

Returns a `NamedTuple` where the field `bp` contains partition
and the field `bm` contains the modularity value for that partition.
"""
function findcommunities(h::Hypergraph, method::CFModularityRandom)
    bp = [Int[]]
    bm = -Inf
    ha = HypergraphAggs(h)
    for i in 1:method.reps
        p = Vector{Vector{Int}}([[]])
        while  minimum(length.(p)) == 0
            p = randompartition(h, method.n)
        end
        m = modularity(h, p, ha)
        if m > bm
            bm = m
            bp = p
        end
    end
    (bp=bp, bm=bm)
end


function find_first(c::Array{Set{Int}}, vals)
    for i in 1:length(c)
        for v in vals
            v in c[i] && return i
        end
    end
    throw("None of values in $vals found")
end


"""
    CFModularityCNMLike(n::Int, reps::Int) <: AbstractCommunityFinder

Represents a CNM-Like algorithm for finding communities.
In the algorithm we start with a partition where each node is in its own part.
Then in each step, we randomly select a hyperedge.
Subsequently, we consider merging each set of that parts it touches.
We actually merge the parts if the new best modularity is at least as high
as the modularity from the previous step.
The algortithm iterates through `reps` of repetitions.

For more information see `Algorithm 1` at:
Clustering via Hypergraph Modularity (submitted to Plos ONE), auhtors:
Bogumił Kamiński, Valerie Poulin, Paweł Prałat, Przemysław Szufel, Francois Theberge

"""
struct CFModularityCNMLike <: AbstractCommunityFinder
    reps::Int
end


"""
    findcommunities(h::Hypergraph, method::CFModularityCNMLike)

Iterates a CNM-Like algorithm for finding communities.
In the algorithm we start with a partition where each node is in its own part.
Then in each step, we randomly select a hyperedge.
Subsequently, we consider merging each set of that parts it touches.
We actually merge the parts if the new best modularity is at least as high
as the modularity from the previous step.

Returns a `NamedTuple` where the field `bp` contains partition
and the field `bm` contains the modularity value for that partition,
finally, the fiel `mod_history` represents modularities achieved
in subsequent steps of the algorithm.

For more information see `Algorithm 1` at:
Clustering via Hypergraph Modularity (submitted to Plos ONE), authors:
Bogumił Kamiński, Valerie Poulin, Paweł Prałat, Przemysław Szufel, Francois Theberge

"""
function findcommunities(h::Hypergraph, method::CFModularityCNMLike)
    ha = HypergraphAggs(h)
    best_modularity = 0
    comms = [Set(i) for i in 1:nhv(h)]
    mod_history = Vector{Float64}(undef, method.reps)
    for rep in 1:method.reps
        he = rand(1:nhe(h))
        vers = collect(keys(getvertices(h, he)))
        if length(vers) == 0
            continue
        end;
        c = deepcopy(comms)
        i0 = find_first(c, vers)
        max_i = length(c)
        i_cur = i0
        while i_cur < max_i
            i_cur += 1
            if length(intersect(c[i_cur],vers)) > 0
                union!(c[i0], c[i_cur])
                c[i_cur]=c[max_i]
                max_i += -1
            end
        end
        resize!(c,max_i)
        m = modularity(h, c, ha)
        if m > best_modularity
            best_modularity = m
            comms = c
        end
        mod_history[rep] = best_modularity
    end
    return (bm=best_modularity, bp=comms, mod_history=mod_history)
end
