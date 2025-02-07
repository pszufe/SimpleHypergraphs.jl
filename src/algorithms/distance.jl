"""
The base type for all algorithms representing various distances metrics.
"""
abstract type AbstractDistance end


"""
    struct SnodeDistanceDijkstra(source_node::Int, target_node::Int, s::Int) <: AbstractDistance

Represent a distance between two nodes of the hypergraph `h`, which is
the minimum `s`-walk length between the two nodes. An `s`-walk between nodes
is a sequence of nodes that pairwise share at least `s` edges.
"""
struct SnodeDistanceDijkstra <: AbstractDistance
    source_node::Int
    target_node::Int
    s::Int
end


"""
    struct SedgeDistanceDijkstra(source_edge::Int, target_edge::Int, s::Int) <: AbstractDistance

Represent a distance between two hyperedges of the hypergraph `h`, which is
the minimum `s`-walk lenght between the two hyperedge. An `s`-walk between edges is a sequence
of edges such that consecutive pairwise edges intersect in at least `s` nodes.
"""
struct SedgeDistanceDijkstra <: AbstractDistance
    source_edge::Int
    target_edge::Int
    s::Int
end


"""
    distance(h::H, distance_method::SnodeDistanceDijkstra) where {H<:AbstractSimpleHypergraph}

Return the shortest `distance_method.s`-walk distance between the `distance_method.source_node` and
the node `distance_method.target_node` in the hypergraph `h`.

NOTE
The concepts of `s`-distance and `s`-walk have been defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/_modules/classes/hypergraph.html#Hypergraph.distance)
The `s`-distance is the shortest `s`-walk length between two nodes.
An `s`-walk between nodes is a sequence of nodes that pairwise share
at least `s` edges. The length of the shortest `s`-walk is 1 less than
the number of nodes in the path sequence. If no such path exists returns typemax(T).
"""
function distance(h::H, distance_method::SnodeDistanceDijkstra) where {H<:AbstractSimpleHypergraph}
    checkbounds(h.v2he, distance_method.source_node)
    checkbounds(h.v2he, distance_method.target_node)
    A = adjacency_matrix(h; s=distance_method.s)
    g = Graphs.Graph(A)
    dj = Graphs.dijkstra_shortest_paths(g, distance_method.source_node)
    dj.dists[distance_method.target_node]
end


"""
    distance(h::H, distance_method::SedgeDistanceDijkstra) where {H<:AbstractSimpleHypergraph}

Return the shortest `distance_method.s`-walk distance between the `distance_method.source_edge` and
the node `distance_method.target_edge` in the hypergraph `h`.

NOTE
The concepts of `s`-distance and `s`-walk have been defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/_modules/classes/hypergraph.html#Hypergraph.edge_distance)
The `s`-distance is the shortest `s`-walk length between the edges.
An `s`-walk between edges is a sequence of edges such that consecutive pairwise
edges intersect in at least `s` nodes. The length of the shortest `s`-walk is 1 less than
the number of edges in the path sequence. If no such path exists returns typemax(T).
"""
function distance(h::H, distance_method::SedgeDistanceDijkstra) where {H<:AbstractSimpleHypergraph}
    checkbounds(h.he2v, distance_method.source_edge)
    checkbounds(h.he2v, distance_method.target_edge)
    A = edge_adjacency_matrix(h; s=distance_method.s)
    g = Graphs.Graph(A)
    dj = Graphs.dijkstra_shortest_paths(g, distance_method.source_edge)
    dj.dists[distance_method.target_edge]
end


# TODO: distance in a directed hypergraph