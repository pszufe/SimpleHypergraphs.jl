module SimpleHypergraphs

using LightGraphs

export Hypergraph, getvertices, gethyperedges, hg_load, hg_save
export add_vertex!, add_hyperedge!
export set_vertex_value!, get_vertex_value
export set_hyperedge_value!, get_hyperedge_value
export BipartiteView, shortest_path
export TwoSectionView

include("hypergraph.jl")
include("bipartite.jl")
include("io.jl")
include("twosection.jl")

end # module
