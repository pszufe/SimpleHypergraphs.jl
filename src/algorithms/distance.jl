"""
    distance(h::Hypergraph, u::Int, v::Int; s::Int=1)

Return the shortest `s`-walk distance between the `u` and the node `v` in the hypergraph `h`.

NOTE
The concepts of `s`-distance and `s`-walk have been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/_modules/classes/hypergraph.html#Hypergraph.distance)
The `s`-distance is the shortest `s`-walk length between two nodes.
An `s`-walk between nodes is a sequence of nodes that pairwise share
at least `s` edges. The length of the shortest `s`-walk is 1 less than
the number of nodes in the path sequence. If no such path exists returns typemax(T).
"""
function distance(h::Hypergraph, source::Int, target::Int; s::Int=1)
    checkbounds(h.v2he, source)
    checkbounds(h.v2he, target)

    A = adjacency_matrix(h; s=s)
    g = LightGraphs.Graph(A)

    dj = LightGraphs.dijkstra_shortest_paths(g, source)
    dj.dists[target]
end


"""
    edge_distance(h::Hypergraph, he_source::Int, he_target::Int; s::Int=1)

Return the shortest `s`-walk distance between two edges in the hypergraph.

NOTE
The concepts of `s`-distance and `s`-walk have been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/_modules/classes/hypergraph.html#Hypergraph.edge_distance)
The `s`-distance is the shortest `s`-walk length between the edges.
An `s`-walk between edges is a sequence of edges such that consecutive pairwise
edges intersect in at least `s` nodes. The length of the shortest `s`-walk is 1 less than
the number of edges in the path sequence. If no such path exists returns typemax(T).
"""
function edge_distance(h::Hypergraph, he_source::Int, he_target::Int; s::Int=1)
    checkbounds(h.he2v, he_source)
    checkbounds(h.he2v, he_target)

    A = edge_adjacency_matrix(h; s=s)
    g = LightGraphs.Graph(A)

    dj = LightGraphs.dijkstra_shortest_paths(g, he_source)
    dj.dists[he_target]
end
