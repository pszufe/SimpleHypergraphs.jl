"""
    randompartitioning(hg::Hypergraph, n::Int)::Vector{Vector{Int}}
    
Generates a random `n`-partitioning for a hypergraph `hg`. 
"""
function randompartitioning(hg::Hypergraph, n::Int)::Vector{Vector{Int}}
    x = rand(1:n, size(hg, 1))
    [findall(==(i), x) for i in 1:n]
end

"""
    modularity(hg::Hypergraph, partitioning::Vector{Vector{Int}})
    
Calculates the modularity of a hypergraph `hg` for a given `partitioning`.
"""
function modularity(hg::Hypergraph, partitioning::Vector{Vector{Int}})
    @assert sum(length.(partitioning)) == size(hg, 1)
    @assert sort!(union(partitioning...)) == axes(hg, 1)
    hes = [count(==(true), hg[:, i]) for i in axes(hg, 2)]
    Ed = Dict{Int,Int}()
    for i in 2:maximum(hes)
        Ed[i] = count(==(i), hes)
    end
    deg(hg, i) = count(==(true), hg[i, :])
    volV = 2*size(hg,2)
    volP = [sum(deg(hg, i) for i in p) for p in partitioning]
    eP = [count(i->issubset(findall(==(true), hg[:,i]), p)*any(==(true),hg[:,i]), axes(hg,2)) for p in partitioning]
    (sum(eP) - sum(Ed[d]*sum((v/volV)^d for v in volP) for d in keys(Ed))) / size(hg, 2)
end

"""
    modularity(hg::Hypergraph)
    
Calculates the modularity of a hypergraph `hg`.
"""
modularity(hg::Hypergraph) = modularity(hg, [collect(1:size(hg, 1))])

"""
    findmodularity(hg::Hypergraph, n::Int, reps::Int)::NamedTuple{(:bp, :bm),Tuple{Vector{Vector{Int}}, Float64}}

Makes a random search over the hypergraph `hg` and finds 
a `n`-partitioning with the maximum modularity value. 
During the search `reps` random `n`-partitionings will be evaluated. 

Returns a `NamedTuple` where the field `bp` contains partitioning 
and the field `bm` contains the modularity value for that partitioning.
"""
function findmodularity(hg::Hypergraph, n::Int, reps::Int)::NamedTuple{(:bp, :bm),Tuple{Vector{Vector{Int}}, Float64}}
    bp = Vector{Vector{Int}}([[]])
    bm = -Inf
    for i in 1:reps
        p = Vector{Vector{Int}}([[]])
        while  minimum(length.(p)) == 0 
            p = randompartitioning(hg, n)
        end       
        m = modularity(hg, p)
        if m > bm
            bm = m
            bp = p
        end
    end
    (bp=bp, bm=bm)
end
