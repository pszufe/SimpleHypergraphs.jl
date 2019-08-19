"""
    TwoSectionView{T<:Real} <: LightGraphs.SimpleGraphs.AbstractSimpleGraph{Int64}

Represents a 2-section view of a hypergraph `h`.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

**Constructors**

TwoSectionView(::Hypergraph)

The 2-section view of a hypergraph is suitable for processing with the LightGraphs.jl package.
Several LightGraphs methods are provided for the compability.

"""
struct TwoSectionView{T<:Real} <: LightGraphs.SimpleGraphs.AbstractSimpleGraph{Int}
    h::Hypergraph{T}
end

"""
  Return the number of vertices in 2-section view `t` of a hypergraph.
"""
LightGraphs.nv(t::TwoSectionView) = length(t.h.v2he)


LightGraphs.vertices(t::TwoSectionView) = Base.OneTo(nv(t))

"""
  Return the number of edges in 2-section view `t` of a hypergraph.
"""
function LightGraphs.ne(t::TwoSectionView)
    s = 0
    for x in t.h.he2v
        s += length(x) * (length(x) - 1) 
    end
    div(s, 2)
end

"""
    LightGraphs.all_neighbors(t::TwoSectionView, v::Integer)

Returns N(v) (the vertex v is not included in N(v))
"""
function LightGraphs.all_neighbors(t::TwoSectionView, v::Integer)
    neighbors = Set{Int}()
    for he in keys(t.h.v2he[v])
        union!(neighbors, keys(t.h.he2v[he]))
    end
    delete!(neighbors, v) #remove v from its neighborhood
    collect(neighbors) #returns the corresponding array
end

function LightGraphs.has_edge(t::TwoSectionView, s, d)
    s == d && return false
    !isempty(intersect(keys(t.h.v2he[s]), keys(t.h.v2he[d])))
end

LightGraphs.outneighbors(t::TwoSectionView, v::Integer) =
    LightGraphs.all_neighbors(t::TwoSectionView, v)

LightGraphs.inneighbors(t::TwoSectionView, v::Integer) = 
    LightGraphs.all_neighbors(t::TwoSectionView, v)

"""
    LightGraphs.SimpleGraph(t::TwoSectionView)

Creates a `LightGraphs.SimpleGraph` representation of a `TwoSectionView` t.

This creates a copy of the date. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function LightGraphs.SimpleGraph(t::TwoSectionView)
    g = SimpleGraph(nv(t))
    for v in LightGraphs.vertices(t)
        neighbors_v = LightGraphs.all_neighbors(t, v)
        for neighbor in neighbors_v
            add_edge!(g, v, neighbor)
        end
    end
    g
end

LightGraphs.is_directed(t::TwoSectionView{T}) where T = false

LightGraphs.is_directed(::Type{TwoSectionView{T}}) where T = false

Base.eltype(::TwoSectionView{T}) where T = T


"""
    shortest_path(t::TwoSectionView,source::Int, target::Int)

Finds a single shortest path in a graph `b` between vertices
`source` and `target`.
Note that if several paths of the same length exist, only one
will be returned.

"""
function shortest_path(t::TwoSectionView, source::Int, target::Int)
    checkbounds(t.h.v2he, source)
    checkbounds(t.h.v2he, target)
    dj = dijkstra_shortest_paths(t, source)
    enumerate_paths(dj)[target]
end

"""
    LightGraphs.SimpleGraphs.fadj(t::TwoSectionView)
    
Generates an adjency list for this view of a hypergraph. 
"""
function LightGraphs.SimpleGraphs.fadj(t::TwoSectionView)
    res = [Vector{Int}() for _ in 1:LightGraphs.nv(t)]
    for he in t.h.he2v
        vs = collect(keys(he))
        if length(vs) > 1
            for i in 1:length(vs)
                append!(res[vs[i]], vs[1:end .!= i])
            end
        end
    end
    sort!.(res)
end
LightGraphs.SimpleGraphs.fadj(t::TwoSectionView, v::Integer) = LightGraphs.all_neighbors(t,v)
LightGraphs.edges(t::TwoSectionView) = LightGraphs.SimpleGraphs.SimpleEdgeIter(t)