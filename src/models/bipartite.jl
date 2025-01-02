"""
    BipartiteView{T<:Real} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int}

Represents a bipartite view of a hypergraph `h`.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

**Constructors**

BipartiteView(::H) where {H<:AbstractHypergraph}

The bipartite view of a hypergraph is suitable for processing with the Graphs.jl package.
Several Graphs methods are provided for the compability.

"""
struct BipartiteView{H<:AbstractHypergraph, T<:Real} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int}
    h::H{T}
end


"""
  Return the number of vertices in a bipartite view `b` of a hypergraph.
"""
Graphs.nv(b::BipartiteView) =  nhv(b.h) + nhe(b.h)

Graphs.vertices(b::BipartiteView) = Base.OneTo(Graphs.nv(b))

"""
  Return the number of edges in a bipartite view `b` of a hypergraph.
"""
Graphs.ne(b::BipartiteView{H}) where {H<:AbstractUndirectedHypergraph} = sum(length.(b.h.v2he))
Graphs.ne(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph} = sum(length.(b.h.hg_tail.v2he)) + sum(length.(b.h.hg_head.v2he))


function Graphs.all_neighbors(b::BipartiteView{H}, v::Integer) where {H<:AbstractUndirectedHypergraph}
    n1 = length(b.h.v2he)
    if v <= n1
      n1 .+ keys(b.h.v2he[v])
    else
      collect(keys(b.h.he2v[v-n1]))
    end
end

function Graphs.all_neighbors(b::BipartiteView{H}, v::Integer) where {H<:AbstractDirectedHypergraph}
    n1 = nhv(b.h)

    if v <= n1
      t, h = gethyperedges(b.h, v)
      n1 .+ unique([collect(keys(t)); collect(keys(h))])
    else
      t, h = getvertices(b.h, v - n1)
      unique([collect(keys(t)); collect(keys(h))])
    end
end


function Graphs.has_edge(b::BipartiteView{H}, s, d) where {H<:AbstractUndirectedHypergraph}
    n1 = length(b.h.v2he)
    if s <= n1
        d > n1 && has_key(b.v2he[s], d - n1)
    else
        d <= n1 && has_key(b.h.he2v[s-n1], d)
    end
end

function Graphs.has_edge(b::BipartiteView{H}, s, d) where {H<:AbstractDirectedHypergraph}
    n1 = nhv(b.h)

    if s <= n1
        d > n1 && has_key(b.h.hg_tail.v2he[s], d - n1)
    else
        d <= n1 && has_key(b.h.hg_head.he2v[s-n1], d)
    end
end


Graphs.has_vertex(b::BipartiteView, v::Integer) = 1 <= v <= Graphs.nv(b)


Graphs.outneighbors(b::BipartiteView{H}, v::Integer) where {H<:AbstractUndirectedHypergraph} = Graphs.all_neighbors(b::BipartiteView{H}, v)

Graphs.inneighbors(b::BipartiteView{H}, v::Integer) where {H<:AbstractUndirectedHypergraph} = Graphs.all_neighbors(b::BipartiteView{H}, v)

function Graphs.outneighbors(
    b::BipartiteView{H},
    v::Integer
    ) where {H<:AbstractDirectedHypergraph}

    n1 = nhv(b.h)

    if v <= n1
      t, _ = gethyperedges(b.h, v)
      n1 .+ collect(keys(t))
    else
      _, h = getvertices(b.h, v - n1)
      collect(keys(h))
    end

end

function Graphs.inneighbors(
    b::BipartiteView{H},
    v::Integer
    ) where {H<:AbstractDirectedHypergraph}

    n1 = nhv(b.h)

    if v <= n1
      _, h = gethyperedges(b.h, v)
      n1 .+ collect(keys(h))
    else
      t, _ = getvertices(b.h, v - n1)
      collect(keys(t))
    end

end


"""
    Graphs.SimpleGraph(b::BipartiteView{H}) where {H<:AbstractUndirectedHypergraph}

Creates a `Graphs.SimpleGraph` representation of a `BipartiteView` b.

This creates a copy of the data. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function Graphs.SimpleGraph(b::BipartiteView{H}) where {H<:AbstractUndirectedHypergraph}
    g = SimpleGraph(nv(b))
    for v in keys(b.h.v2he)
        for he in keys(b.h.v2he[v])
            add_edge!(g, v, length(b.h.v2he) + he)
        end
    end
    g
end


"""
    Graphs.SimpleGraph(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph}

Creates a `Graphs.SimpleGraph` representation of a `BipartiteView` b.

This creates a copy of the data. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function Graphs.SimpleGraph(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph}
    g = SimpleGraph(nv(b))

    n1 = nhv(b.h)
    for v in 1:n1
        t, h = gethyperedges(b.h, v)

        for he in unique([collect(keys(t)); collect(keys(h))])
            add_edge!(g, v, n1 + he)
        end
    end
    g
end

# TODO: you are here
"""
    Graphs.SimpleDiGraph(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph}

Creates a `Graphs.SimpleDiGraph` representation of a `BipartiteView` b.

This creates a copy of the data. Note that the weights information is not stored
in the created `SimpleDiGraph`.
"""
function Graphs.SimpleDiGraph(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph}
    g = SimpleDiGraph(nv(b))

    n1 = nhv(b.h)

    for v in 1:n1
        t, h = gethyperedges(b.h, v)

        for he in keys(t)
            add_edge!(g, v, n1 + he)
        end

        for he in keys(h)
            add_edge!(g, n1 + he, v)
        end

    end

    g
end


Graphs.is_directed(b::BipartiteView{H}) where {H<:AbstractUndirectedHypergraph} = false

Graphs.is_directed(::Type{BipartiteView{H}}) where {H<:AbstractUndirectedHypergraph} = false

Graphs.is_directed(b::BipartiteView{H}) where {H<:AbstractDirectedHypergraph} = true

Graphs.is_directed(::Type{BipartiteView{H}}) where {H<:AbstractDirectedHypergraph} = true

Base.eltype(::BipartiteView{T}) where T = Int


"""
    shortest_path(b::BipartiteView{H}, source::Int, target::Int) where {H<:AbstractUndirectedHypergraph}

Finds a single shortest path in a graph `b` between vertices
`source` and `target`.
Note that if several paths of the same length exist, only one
will be returned.

"""
function shortest_path(b::BipartiteView{H}, source::Int, target::Int) where {H<:AbstractUndirectedHypergraph}
    checkbounds(b.h.v2he, source)
    checkbounds(b.h.v2he, target)
    dj = dijkstra_shortest_paths(b, source)
    enumerate_paths(dj)[target][1:2:end]
end

"""
    shortest_path(b::BipartiteView{H}, source::Int, target::Int) where {H<:AbstractDirectedHypergraph}

Finds a single shortest path in a graph `b` between vertices
`source` and `target`.
Note that if several paths of the same length exist, only one
will be returned.

"""
function shortest_path(b::BipartiteView{H}, source::Int, target::Int) where {H<:AbstractDirectedHypergraph}
    checkbounds(b.h.hg_tail.v2he, source)
    checkbounds(b.h.hg_tail.v2he, target)
    checkbounds(b.h.hg_head.v2he, source)
    checkbounds(b.h.hg_head.v2he, target)

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

Graphs.SimpleGraphs.fadj(b::BipartiteView{H}, v::Integer) where {H<:AbstractUndirectedHypergraph} = Graphs.all_neighbors(b,v)

Graphs.SimpleGraphs.fadj(b::BipartiteView{H}, v::Integer) where {H<:AbstractDirectedHypergraph} = Graphs.outneighbors(b,v)
Graphs.SimpleGraphs.badj(b::BipartiteView{H}, v::Integer) where {H<:AbstractDirectedHypergraph} = Graphs.inneighbors(b,v)

Graphs.edges(b::BipartiteView) = Graphs.SimpleGraphs.SimpleEdgeIter(b)

Graphs.edgetype(b::BipartiteView{H,T}) where H, T = Graphs.SimpleGraphs.SimpleEdge{Int}

Graphs.zero(t::BipartiteView{H,T}) where {H<:AbstractHypergraph, T} = BipartiteView(H{T}(0,0))
Graphs.zero(::Type{BipartiteView{H,T}}) where {H<:AbstractHypergraph, T} = BipartiteView(H{T}(0,0))
