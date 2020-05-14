module SimpleHypergraphs

using LightGraphs
using StatsBase
using DataStructures
using PyCall
using Conda
using PyPlot
using JSON3
using JSON
using Random
using LinearAlgebra

export Hypergraph, getvertices, gethyperedges
export add_vertex!, add_hyperedge!, remove_vertex!, remove_hyperedge!, prune_hypergraph!, prune_hypergraph
export set_vertex_meta!, get_vertex_meta
export set_hyperedge_meta!, get_hyperedge_meta
export BipartiteView, shortest_path
export TwoSectionView
export get_twosection_adjacency_mx, get_twosection_weighted_adjacency_mx

export Abstract_HG_format, HGF_Format, JSON_Format
export hg_load, hg_save

export nhv, nhe
export modularity, nmi, randompartition
export AbstractCommunityFinder, CFModularityRandom, CFModularityCNMLike, CFLabelPropagationFinder
export findcommunities
export random_walk
export get_connected_components
export conductance

export distance, edge_distance, adjacency_matrix, edge_adjacency_matrix

export dual
export random_model, random_kuniform_model, random_dregular_model, random_preferential_model

export HyperNetX, GraphBased
export draw

const hnx = PyNULL()
const nx = PyNULL()
const pynull = PyNULL()


function __init__()
    plot_ok = true
    try
		copy!(nx, pyimport("networkx"))
    catch e
		@warn "Python networkx not found. Plotting functionality of HyperNetX will not work."
		plot_ok = false
	end
	try
		copy!(hnx, pyimport("hypernetx"))
    catch e
		@warn "Python HyperNetX not found. Plotting functionality will not work."
		plot_ok = false
	end
end

function support_hypernetx()
    return ((SimpleHypergraphs.nx !=  SimpleHypergraphs.pynull) &&
           (SimpleHypergraphs.hnx !=  SimpleHypergraphs.pynull))

end


include("hypergraph.jl")
include("io.jl")

include("models/bipartite.jl")
include("models/twosection.jl")
include("models/random-models.jl")
include("models/dual.jl")

include("algorithms/distance.jl")
include("algorithms/community/modularity.jl")
include("algorithms/community/label-propagation.jl")
include("algorithms/conductance.jl")

include("viz/drawing.jl")
include("viz/widget.jl")


end # module
