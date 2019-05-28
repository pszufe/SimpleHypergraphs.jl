module SimpleHypergraphs

using LightGraphs

export Hypergraph, getvertices, gethyperedges, hg_load, hg_save
export add_vertex!, add_hyperedge!
export set_vertex_meta!, get_vertex_meta
export set_hyperedge_meta!, get_hyperedge_meta
export BipartiteView, shortest_path
export TwoSectionView
export modularity, randompartition, findmodularity

include("hypergraph.jl")
include("bipartite.jl")
include("io.jl")
include("twosection.jl")
include("modularity.jl")

end # module
