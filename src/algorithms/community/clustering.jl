function _num_quads(hg::H, i::Int) where {H <: AbstractSimpleHypergraph}
    quads = 0
    nv = nhv(hg)
    ne = nhe(hg)
    # TODO: there must be a better implementation
    for α in 1:ne
        for β in α+1:ne
            for j in 1:nv
                if i == j
                    continue
                end

                if !(isnothing(hg[i,α]) || isnothing(hg[i,β]) || isnothing(hg[j,α]) || isnothing(hg[j,β]))
                    quads += 1
                end
            end
        end
    end
    quads
end

function _max_num_quads(hg::H, i::Int) where {H <: AbstractSimpleHypergraph}
    ne = nhe(hg)
    he_degrees = length.(hg.he2v)
    # TODO: there must be a better implementation
    qmax = 0
    for α in 1:ne
        for β in α+1:ne
            if !(isnothing(hg[i,α]) || isnothing(hg[i,β]))
                qmax += (min(he_degrees[α], he_degrees[β]) - 1)
            end
        end
    end
    qmax
end


"""
    quad_clustering_coefficient(hg::H, i::Int) where {H <: AbstractSimpleHypergraph}
    quad_clustering_coefficient(hg::H) where {H <: AbstractSimpleHypergraph}

    Implements the "quad clustering coefficient" (QCC), as described in:
    Ha, Neri, and Annibale, Chaos 34, 043102 (2024), DOI: 10.1063/5.0188246

    A *quad* is the shortest simple cycle in a hypergraph, consisting of two vertices `i` and `j` that are both
    incident on the same two hyperedges `α` and `β`. The QCC is a density, describing the fraction of all possible
    "quads" a particular vertex `i` participates in. It is always true that `0 <= QCC(inc, i) <= 1`, where `inc` is
    the *incidence matrix* of a hypergraph `hg`. Note that, if a vertex is incident on less than two hyperedges, its
    QCC must be 0.
"""
function quad_clustering_coefficient(hg::H, i::Int) where {H <: AbstractSimpleHypergraph}
    if length(hg.v2he[i]) < 2
        return 0.0
    end
    
    q = _num_quads(hg, i)
    qmax = _max_num_quads(hg, i)

    return q / qmax
end

function quad_clustering_coefficient(hg::H) where {H <: AbstractSimpleHypergraph}
    return [quad_clustering_coefficient(hg, i) for i in 1:nhv(hg)]
end