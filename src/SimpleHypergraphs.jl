module SimpleHypergraphs

using Graphs
using StatsBase
using DataStructures
using PyCall
using Conda
using PyPlot
using JSON3
using Random
using LinearAlgebra
using SimpleTraits

export AbstractHypergraph, AbstractSimpleHypergraph
export isdirected, IsDirected, hasvertexmeta, HasVertexMeta, hashyperedgemeta, HasHyperedgeMeta
export Hypergraph, getvertices, gethyperedges
export add_vertex!, add_hyperedge!, remove_vertex!, remove_hyperedge!
export prune_hypergraph!, prune_hypergraph
export set_vertex_meta!, get_vertex_meta
export set_hyperedge_meta!, get_hyperedge_meta
export adjacency_matrix, edge_adjacency_matrix

export BipartiteView, TwoSectionView
export shortest_path
export get_twosection_adjacency_mx, get_twosection_weighted_adjacency_mx
export dual
export random_model, random_kuniform_model, random_dregular_model, random_preferential_model

export Abstract_HG_format, HGF_Format, JSON_Format, HIF_Format
export hg_load, hg_save

export modularity
export randompartition
export AbstractCommunityFinder, CFModularityRandom, CFModularityCNMLike, CFLabelPropagationFinder
export findcommunities

export nhv, nhe
export random_walk
export get_connected_components
export conductance
export AbstractDistance, SnodeDistanceDijkstra, SedgeDistanceDijkstra
export distance

export HyperNetX, GraphBased
export draw

const hnx  = PyNULL()
const nx = PyNULL()
const has_plotting = Ref(false)


function __init__()
    has_networkx = false
    has_hypernetx = false
    try
        copy!(nx, pyimport("networkx"))
        has_networkx = true
    catch e; end
    try
        copy!(hnx, pyimport("hypernetx"))
        has_hypernetx = true
    catch e; end
    has_plotting[] = has_networkx && has_hypernetx
    if !has_plotting[]
        @warn "The plotting functionality of HyperNetX will not work!\n"* 
		(has_networkx ? "" : "Conda Python networkx not found.\n")*
        (has_hypernetx ? "" : "Conda Python HyperNetX not found.\n")*        
        "To test your installation try running `using PyCall;pyimport(\"networkx\");pyimport(\"hypernetx\")`"
    end
end

function support_hypernetx()
    return has_plotting[]
end


include("abstracttypes.jl")
include("hypergraph.jl")
include("io.jl")

include("io_hif.jl")

include("models/bipartite.jl")
include("models/twosection.jl")
include("models/random-models.jl")
include("models/dual.jl")

include("algorithms/conductance.jl")
include("algorithms/distance.jl")

include("algorithms/community/modularity.jl")
include("algorithms/community/label-propagation.jl")

include("viz/drawing.jl")
include("viz/widget.jl")

end # module
