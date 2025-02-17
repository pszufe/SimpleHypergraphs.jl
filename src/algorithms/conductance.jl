"""
    conductance(h::Hypergraph, subset::Set{Int})::Float64

Calculate unweighted hypergraph conductance of `subset`.
note: ∅ ⊊ `subset` ⊊ `1:nhv(h)`

For more information see `1. Introduction` at:
Spectral Properties of Hypergraph Laplacian and Approximation Algorithms
auhtors: T-H. Hubert Chan, Anand Louis, Zhihao Gavin Tang, and Chenzi Zhang
"""
function conductance(h::Hypergraph, subset::Set{Int})::Float64
  if isempty(subset) error("`subset` is not allowed empty.") end
  if subset == Set(1:nhv(h)) error("`subset` is not allowed true subset of `h`.") end
  subset2 = setdiff(Set(1:nhv(h)), subset)
  volS = sum([sum(values(gethyperedges(h, node))) for node in subset])
  volV_S = sum([sum(values(gethyperedges(h, node))) for node in subset2])
  cutS = 0
  for he in 1:nhe(h)
    he_vertices = keys(getvertices(h, he))
    if isempty(intersect(he_vertices, subset)) continue end
    if isempty(intersect(he_vertices, subset2)) continue end
    cutS += 1
  end
  return cutS / min(volS, volV_S)
end
