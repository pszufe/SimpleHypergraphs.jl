module SimpleHypergraphs

using LightGraphs

export Hypergraph, getvertices, gethyperedges, hg_load, hg_save
export add_vertex!, add_hyperedge!
export BipartiteView, shortest_path

include("hypergraph.jl")
include("bipartite.jl")
include("io.jl")

end # module
