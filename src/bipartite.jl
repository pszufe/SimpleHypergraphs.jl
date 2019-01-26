"""
    BipartiteView{T<:Real} <: AbstractGraph{Int64}

Represents a bipartite view of a hypergraph `h`.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

**Constructors**

BipartiteView(::Hypergraph)

The bipartite view of a hypergraph is suitable for processing with the LightGraphs.jl package.
Several LightGraphs methods are provided for the compability.

"""
struct BipartiteView{T<:Real} <: AbstractGraph{Int}
    h::Hypergraph{T}
end


LightGraphs.nv(b::BipartiteView) = length(b.h.v2he)+length(b.h.he2v)

LightGraphs.vertices(b::BipartiteView) = Base.OneTo(LightGraphs.nv(b))


LightGraphs.ne(b::BipartiteView) = 2*sum(length.(b.h.v2he))

function LightGraphs.all_neighbors(b::BipartiteView, v::Integer)
    n1 = length(b.h.v2he)
    if v <= n1
      n1 .+ keys(b.h.v2he[v])
    else
      keys(b.h.he2v[v-n1])
    end
end

function LightGraphs.has_edge(b::BipartiteView, s, d)
    n1 = length(b.h.v2he)
    if s <= n1
        d > n1 && has_key(b.v2he[s], d - n1)
    else
        d <= n1 && has_key(b.h.he2v[s-n1], d)
    end
end


LightGraphs.outneighbors(b::BipartiteView, v::Integer) = LightGraphs.all_neighbors(b::BipartiteView, v)

LightGraphs.inneighbors(b::BipartiteView, v::Integer) = LightGraphs.all_neighbors(b::BipartiteView, v)

"""
    LightGraphs.SimpleGraph(b::BipartiteView)

Creates a `LightGraphs.SimpleGraph` representation of a `BipartiteView` b.

This creates a copy of the date. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function LightGraphs.SimpleGraph(b::BipartiteView)
    g = SimpleGraph(nv(b))
    for v in keys(b.h.v2he)
        for he in keys(b.h.v2he[v])
            add_edge!(g, v, length(b.h.v2he) + he)
        end
    end
    g
end



LightGraphs.is_directed(b::BipartiteView) = false

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
