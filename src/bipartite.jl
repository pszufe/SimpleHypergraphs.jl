"""
    BipartiteView{T<:Real} <: AbstractGraph{Int64}

Creates a bipartite view of a hypergraph.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

**Constructors**

    BipartiteView{T}(h::Hypergraph{T}) where {T<:Real}

Construct a bipartite view of a hypergraph for processing with LightGraphs


**Arguments**

* `h` : a hypergraph

"""
struct BipartiteView{T<:Real} <: AbstractGraph{Int}
    v2he::Vector{Dict{Int,T}}
    he2v::Vector{Dict{Int,T}}
    function BipartiteView{T}(h::Hypergraph{T}) where {T<:Real}
        new(h.v2he,h.he2v)
    end
end


LightGraphs.nv(b::BipartiteView) =
     length(b.v2he)+length(b.he2v)

LightGraphs.vertices(b::BipartiteView) =
     Base.OneTo(nv(b))


LightGraphs.ne(b::BipartiteView) =
    sum(length.(b.v2he))+sum(length.(b.he2v))

function LightGraphs.all_neighbors(b::BipartiteView,v::Integer)
    n1 = length(b.v2he)
    if v <= n1
      n1 .+ keys(b.v2he[v])
    else
      keys(b.he2v[v-n1])
    end
end

function LightGraphs.has_edge(b::BipartiteView,s,d)
    n1 = length(b.v2he)
    if s <= n1
        if d <= n1 return false; end
        has_key(b.v2he[s], d-n1)
    else
        if d > n1 return false; end
        has_key(b.he2v[s-n1], d)
    end
end


LightGraphs.outneighbors(b::BipartiteView,v::Integer) =
    LightGraphs.all_neighbors(b::BipartiteView,v)

LightGraphs.inneighbors(b::BipartiteView,v::Integer) =
    LightGraphs.all_neighbors(b::BipartiteView,v)


function LightGraphs.SimpleGraph(b::BipartiteView)
    g = SimpleGraph(length(b.v2he)+length(b.he2v))
    for v in keys(b.v2he)
        for he in keys(b.v2he[v])
            add_edge!(g,v,length(b.v2he)+he)
        end
    end
    g
end



LightGraphs.is_directed(b::BipartiteView) = false

"""
    shortest_path(g::BipartiteView,source, target)

Finds a single shortest path between two vertices

**Arguments**

* `b` : a bipartite view of a hypergraph
* `source` : a starting vertex
* `target` : a destination vertex
"""
function shortest_path(b::BipartiteView,source::Int, target::Int)
    @boundscheck source <= length(b.v2he) || throw(BoundsError(b.v2he, source))
    @boundscheck target <= length(b.v2he) || throw(BoundsError(b.v2he, target))    
    dj = dijkstra_shortest_paths(b, source)
    res = enumerate_paths(dj)[target]
    filter(n -> (n <= length(b.v2he)),res)
end
