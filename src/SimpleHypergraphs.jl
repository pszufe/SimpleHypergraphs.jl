module SimpleHypergraphs

using LightGraphs
using StatsBase
using DataStructures
using PyCall
using PyPlot
using JSON3
using JSON

export Hypergraph, getvertices, gethyperedges
export add_vertex!, add_hyperedge!, remove_vertex!
export set_vertex_meta!, get_vertex_meta
export set_hyperedge_meta!, get_hyperedge_meta
export BipartiteView, shortest_path
export TwoSectionView

export HGF_FORMAT, JSON_FORMAT
export hg_load, hg_save

export nhv, nhe
export modularity, randompartition
export AbstractCommunityFinder, CFModularityRandom, CFModularityCNMLike
export findcommunities
export random_walk
export get_connected_components

export HyperNetX, GraphBased
export draw

const hnx = PyNULL()
const nx = PyNULL()

function __init__()
    copy!(hnx, pyimport("hypernetx"))
    copy!(nx, pyimport("networkx"))
end


include("hypergraph.jl")
include("bipartite.jl")
include("io.jl")
include("twosection.jl")
include("modularity.jl")

include("viz/drawing.jl")
include("viz/widget.jl")

end # module
