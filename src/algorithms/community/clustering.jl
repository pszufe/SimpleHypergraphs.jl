function _bool_incidence_matrix(hg::H) where {H <: AbstractSimpleHypergraph}
    M = zeros(Bool, nhv(hg), nhe(hg))
    M[hg .!== nothing] .= true

    M
end

function _num_quads(inc::Matrix{Bool}, i::Int)
    quads = 0
    nv, ne = size(inc)
    # TODO: there must be a better implementation
    for α in 1:ne
        for β in α+1:ne
            for j in 1:nv
                if i == j
                    continue
                end
                # 0 if quad does not exist between i and j using hyperedges α and β
                # 1 if that quad does exist
                quads += inc[i,α] * inc[i,β] * inc[j,α] * inc[j,β]
            end
        end
    end
    quads
end

function _max_num_quads(inc::Matrix{Bool}, i::Int)
    _, ne = size(inc)
    he_degrees = sum(inc, dims=1)
    # TODO: there must be a better implementation
    qmax = 0
    for α in 1:ne
        for β in α+1:ne
            qmax += (min(he_degrees[α], he_degrees[β]) - 1) * inc[i,α] * inc[i,β]
        end
    end
    qmax
end


"""
    quad_clustering_coefficient(inc::Matrix{Bool}, i::Int)
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
function quad_clustering_coefficient(inc::Matrix{Bool}, i::Int)
    if sum(inc[i,:]) < 2
        return 0.0
    end
    
    q = _num_quads(inc, i)
    qmax = _max_num_quads(inc, i)

    return q / qmax
end

function quad_clustering_coefficient(hg::H, i::Int) where {H <: AbstractSimpleHypergraph}
    inc = _bool_incidence_matrix(hg)
    quad_clustering_coefficient(inc, i)
end

function quad_clustering_coefficient(hg::H) where {H <: AbstractSimpleHypergraph}
    inc = _bool_incidence_matrix(hg)
    
    return [quad_clustering_coefficient(inc, i) for i in 1:nhv(hg)]
end