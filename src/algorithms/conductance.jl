"""
    conductance(h::H, subset::Set{Int})::Float64 where {H<:AbstractUndirectedHypergraph}

Calculate unweighted hypergraph conductance of `subset`.
note: ∅ ⊊ `subset` ⊊ `1:nhv(h)`

For more information see `1. Introduction` at:
Generalizing the Hypergraph Laplacian via a Diffusion Process with Mediators,
authors: T-H. Hubert Chan, Xhibin Liang.
"""
function conductance(h::H, subset::Set{Int})::Float64 where {H<:AbstractUndirectedHypergraph}
  if isempty(subset) error("`subset` is not allowed empty.") end
  if subset == Set(1:nhv(h)) error("`subset` is not allowed true subset of `h`.") end
  subset2 = setdiff(Set(1:nhv(h)), subset)
  wₛ = sum([length(gethyperedges(h, node)) for node in subset])
  w∂s = 0
  for he in 1:nhe(h)
    he_vertices = keys(getvertices(h, he))
    if isempty(intersect(he_vertices, subset)) continue end
    if isempty(intersect(he_vertices, subset2)) continue end
    w∂s += length(getvertices(h, he))
  end
  return w∂s / wₛ
end

# TODO: conductance in directed hypergraphs