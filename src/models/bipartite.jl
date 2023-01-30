"""
    BipartiteView{T<:Real} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int}

Represents a bipartite view of a hypergraph `h`.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

**Constructors**

BipartiteView(::Hypergraph)

The bipartite view of a hypergraph is suitable for processing with the Graphs.jl package.
Several Graphs methods are provided for the compability.

"""
struct BipartiteView{T<:Real} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int}
    h::Hypergraph{T}
end


"""
  Return the number of vertices in a bipartite view `b` of a hypergraph.
"""
Graphs.nv(b::BipartiteView) = length(b.h.v2he)+length(b.h.he2v)

Graphs.vertices(b::BipartiteView) = Base.OneTo(Graphs.nv(b))

"""
  Return the number of edges in a bipartite view `b` of a hypergraph.
"""
Graphs.ne(b::BipartiteView) = sum(length.(b.h.v2he))

function Graphs.all_neighbors(b::BipartiteView, v::Integer)
    n1 = length(b.h.v2he)
    if v <= n1
      n1 .+ keys(b.h.v2he[v])
    else
      collect(keys(b.h.he2v[v-n1]))
    end
end

function Graphs.has_edge(b::BipartiteView, s, d)
    n1 = length(b.h.v2he)
    if s <= n1
        d > n1 && has_key(b.v2he[s], d - n1)
    else
        d <= n1 && has_key(b.h.he2v[s-n1], d)
    end
end

Graphs.has_vertex(b::BipartiteView, v::Integer) = 1 <= v <= Graphs.nv(b)


Graphs.outneighbors(b::BipartiteView, v::Integer) = Graphs.all_neighbors(b::BipartiteView, v)

Graphs.inneighbors(b::BipartiteView, v::Integer) = Graphs.all_neighbors(b::BipartiteView, v)

"""
    Graphs.SimpleGraph(b::BipartiteView)

Creates a `Graphs.SimpleGraph` representation of a `BipartiteView` b.

This creates a copy of the date. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function Graphs.SimpleGraph(b::BipartiteView)
    g = SimpleGraph(nv(b))
    for v in keys(b.h.v2he)
        for he in keys(b.h.v2he[v])
            add_edge!(g, v, length(b.h.v2he) + he)
        end
    end
    g
end

Graphs.is_directed(b::BipartiteView{T}) where T = false

Graphs.is_directed(::Type{BipartiteView{T}}) where T = false

Base.eltype(::BipartiteView{T}) where T = Int

"""
    shortest_path(b::BipartiteView,source::Int, target::Int)

Finds a single shortest path in a graph `b` between vertices
`source` and `target`.
Note that if several paths of the same length exist, only one
will be returned.

"""
function shortest_path(b::BipartiteView,source::Int, target::Int)
    checkbounds(b.h.v2he, source)
    checkbounds(b.h.v2he, target)
    dj = dijkstra_shortest_paths(b, source)
    enumerate_paths(dj)[target][1:2:end]
end

"""
    Graphs.SimpleGraphs.fadj(b::BipartiteView)

Generates an adjency list for this view of a hypergraph.
"""
function Graphs.SimpleGraphs.fadj(b::BipartiteView)
    res = Vector{Vector{Int}}(undef, Graphs.nv(b))

    h_nv = length(b.h.v2he)
    for i in 1:h_nv
       res[i] = h_nv .+ sort!(collect(keys(b.h.v2he[i])))
    end
    for i in 1:length(b.h.he2v)
        res[i+h_nv] = sort!(collect(keys(b.h.he2v[i])))
    end
    res
end

Graphs.SimpleGraphs.fadj(b::BipartiteView, v::Integer) = Graphs.all_neighbors(b,v)
Graphs.edges(b::BipartiteView) = Graphs.SimpleGraphs.SimpleEdgeIter(b)

Graphs.edgetype(b::BipartiteView{T}) where T = Graphs.SimpleGraphs.SimpleEdge{Int}

Graphs.zero(t::BipartiteView{T}) where T = BipartiteView(Hypergraph{T}(0,0))
Graphs.zero(::Type{BipartiteView{T}}) where T = BipartiteView(Hypergraph{T}(0,0))
