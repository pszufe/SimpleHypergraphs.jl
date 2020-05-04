"""
    conductance(h::Hypergraph, subset::Set{Int})::Float64

Calculate conductance of `subset`.

For more information see `1. Introduction` at:
Generalizing the Hypergraph Laplacian via a Diffusion Processwith Mediators,
auhtors: T-H. Hubert Chan, Xhibin Liang.

"""

function conductance(h::Hypergraph, subset::Set{Int})::Float64
  subset2 = setdiff(Set([node for node in 1:nhv(h)]), subset)
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
