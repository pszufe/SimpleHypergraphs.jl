"""
    CFLabelPropagationFinder() <: AbstractCommunityFinder

Represents a label propagation search over the hypergraph `h` that finds
a partition into communities (subsets).
"""
struct CFLabelPropagationFinder <: AbstractCommunityFinder
    max_iter::Int
    seed::Int
end


"""
    findcommunities(h::Hypergraph, method::CFLabelPropagationFinder)

Implements the label propagation algorithm over a hypergraph `h`.

NOTE
The algorithm works on a single connected component.
An AssertionError is thrown otherwise.

This algorithm generalizes the one proposed for graphs by Raghavan et al.
(Raghavan, U. N., Albert, R., and Kumara, S. Near linear time algorithm to detect
community structures in large-scale networks. Physical review. E, Statistical,
nonlinear, and soft matter physics 76 (2007).)

The proposed algorithm modifies the propagation rule, splitting it into two phases:
hyperedge labeling and vertex labeling.

For more information see `Section 4` in the paper
Alessia Antelmi, Gennaro Cordasco, Bogumił Kamiński, Paweł Prałat,
Vittorio Scarano, Carmine Spagnuolo, Przemyslaw Szufel
*Analyzing, Exploring, and Visualizing Complex Networks via Hypergraphs Using SimpleHypergraphs.jl.*
Journal Internet Mathematics (2020). https://doi.org/10.24166/im.01.2020
"""
function findcommunities(h::Hypergraph, method::CFLabelPropagationFinder)
    @assert length(get_connected_components(h)) == 1

    rng = MersenneTwister(method.seed)
    vlabels = Dict{Int, Int}()
    helabels = Dict{Int, Int}()

    for v in 1:nhv(h)
        push!(vlabels, v=>v)
    end

    vertices = collect(1:nhv(h))
    edges = collect(1:nhe(h))

    stop = false
    iter = 0

    while !stop && iter < method.max_iter
        stop = true
        shuffle!(rng, edges)

        for e in edges
            length(getvertices(h, e)) == 0 && continue
            l = SimpleHypergraphs.compute_edge_label(h, e, vlabels, helabels, rng)
            push!(helabels, e => l)
        end

        shuffle!(rng, vertices)
        for v in vertices
            l = SimpleHypergraphs.compute_vertex_label(h, v, vlabels, helabels, rng)
            if l != vlabels[v]
                stop = false
                push!(vlabels, v => l)
            end
        end

        iter+=1
    end

    np_vertices = unique(values(vlabels))
    np_edges = unique(values(helabels))

    comms_vertices = Dict{Int, Set{Int}}()
    comms_hyperedges = Dict{Int, Set{Int}}()

    for pv in vlabels
        push!(
            get!(comms_vertices, pv[2], Set{Int}()),
            pv[1]
        )
    end

    for pe in helabels
        push!(
            get!(comms_hyperedges, pe[2], Set{Int}()),
            pe[1]
        )
    end

    labels = Array{Int}(undef, nhv(h))
    for i in 1:nhv(h)
        labels[i] = vlabels[i]
    end

    hlabels = Array{Int}(undef, nhe(h))
    for i in 1:nhe(h)
        if length(getvertices(h, i)) == 0
            hlabels[i] = -1
        else
            hlabels[i] = helabels[i]
        end
    end

    (np=collect(values(comms_vertices)), hep=collect(values(comms_hyperedges)), vlabels=labels, helabels=hlabels, iter=iter)
end


"""
    compute_vertex_label(h::Hypergraph, v::Int, vlabels::Dict{Int,Int}, helabels::Dict{Int,Int}, rng::MersenneTwister)

Vertices labeling phase. Computes the label of each vertex according to the most
frequent label among the hyperedges it belongs to.
"""
function compute_vertex_label(h::Hypergraph, v::Int, vlabels::Dict{Int,Int}, helabels::Dict{Int,Int}, rng::MersenneTwister)
    hesᵥ = gethyperedges(h, v)
    vL = Dict{Int,Int}()

    max = 0
    maxL = Set{Int}()

    for e in shuffle!(rng, sort(collect(keys(hesᵥ))))
        l = helabels[e]

        if !haskey(vL, l)
            push!(vL, l=>0)
        end

        push!(
            vL,
            l => vL[l] + (length(getvertices(h, e)))
        )

        if vL[l] == max
            push!(maxL, l)
        elseif vL[l] > max
            max = vL[l]
            maxL = Set{Int}()
            push!(maxL, l)
        end
    end

    if in(vlabels[v], maxL)
        return vlabels[v]
    end

    return first(maxL)
end


"""
    compute_edge_label(h::Hypergraph, e::Int, vlabels::Dict{Int,Int}, helabels::Dict{Int,Int}, rng::MersenneTwister)

Hyperedges labeling phase. Computes the labels of the hyperedges according  to
the  most frequent label among the vertices contained in that hyperedge.
"""
function compute_edge_label(h::Hypergraph, e::Int, vlabels::Dict{Int,Int}, helabels::Dict{Int,Int}, rng::MersenneTwister)
    vₑ = getvertices(h,e)
    eL = Dict{Int,Int}()

    max = 0
    maxL = Set{Int}()

    for v in shuffle!(rng, sort(collect(keys(vₑ))))
        l = vlabels[v]

        if !haskey(eL, l)
            push!(eL, l=>0)
        end

        push!(
            eL,
            l => eL[l]+1
        )

        if eL[l] == max
            push!(maxL, l)
        elseif eL[l] > max
            max = eL[l]
            maxL = Set{Int}()
            push!(maxL, l)
        end
    end

    if haskey(helabels, e) && in(helabels[e], maxL)
        return helabels[e]
    end

    return first(maxL)
end
