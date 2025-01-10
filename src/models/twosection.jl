"""
    TwoSectionView{T<:Real, H<:AbstractHypergraph} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int64}

Represents a 2-section view of a hypergraph `h`.
Note (1) this is a view - changes to the original hypergraph will be automatically reflected in the view.

Note (2) The view will only work correctly for hypergraphs not having overlapping hyperedges.
To check whether a graph has overlapping edges try `has_overlapping_hedges(h)` - for such graph
you need to fully materialize it rather than use a view.
This can be achieved via the `get_twosection_adjacency_mx(h)` method.

**Constructors**

TwoSectionView(::H<:AbstractHypergraph)

The 2-section view of a hypergraph is suitable for processing with the Graphs.jl package.
Several Graphs methods are provided for the compability.

"""
struct TwoSectionView{T<:Real, H<:AbstractHypergraph} <: Graphs.SimpleGraphs.AbstractSimpleGraph{Int}
    h::H{T}
    TwoSectionView(h::H{T}) where {T<:Real, H<:AbstractHypergraph} = begin
        has_overlapping_hedges(h) && error("A two section view can be created only for a hypergraph with non-overlapping hyperedges")
        new{T, H}(h)
    end
end

"""
    has_overlapping_hedges(h::H{T}) where {T<:Real, H<:AbstractHypergraph}

Checks whether a hypergraph has hyperedges connecting the same pairs of vertices.
Such hypergraph cannot be presented as a two section view
"""

function has_overlapping_hedges(h::H{T}) where {T<:Real, H<:AbstractHypergraph}
    minimum(size(h)) == 0 && return false
    maximum(get_twosection_adjacency_mx(h;replace_weights=1)) > 1
end

"""
  Return the number of vertices in 2-section view `t` of a hypergraph.
"""
Graphs.nv(t::TwoSectionView) = nhv(t.h)

Graphs.vertices(t::TwoSectionView) = Base.OneTo(nv(t))


"""
  Return the number of edges in 2-section view `t` of an undirected hypergraph.
"""
function Graphs.ne(t::TwoSectionView{T, H}) where {T<:Real, H<:AbstractUndirectedHypergraph}
    s = 0
    for x in t.h.he2v
        s += length(x) * (length(x) - 1)
    end
    div(s, 2)
end

"""
  Return the number of edges in 2-section view `t` of a directed hypergraph.
"""
function Graphs.ne(t::TwoSectionView{T, H}) where {T<:Real, H<:AbstractDirectedHypergraph}
    s = 0
    for x in 1:nhe(t.h)
        s += length(t.h.hg_tail[x]) * length(t.h.hg_head[x])
    end
    s
end


# TODO: you are here
"""
    Graphs.all_neighbors(t::TwoSectionView, v::Integer)

Returns N(v) (the vertex v is not included in N(v))
"""
function Graphs.all_neighbors(t::TwoSectionView, v::Integer)
    neighbors = Set{Int}()
    for he in keys(t.h.v2he[v])
        union!(neighbors, keys(t.h.he2v[he]))
    end
    delete!(neighbors, v) #remove v from its neighborhood
    collect(neighbors) #returns the corresponding array
end

function Graphs.has_edge(t::TwoSectionView, s, d)
    s == d && return false
    !isempty(intersect(keys(t.h.v2he[s]), keys(t.h.v2he[d])))
end


Graphs.has_vertex(t::TwoSectionView, v::Integer) = 1 <= v <= Graphs.nv(t)

Graphs.outneighbors(t::TwoSectionView, v::Integer) =
    Graphs.all_neighbors(t::TwoSectionView, v)

Graphs.inneighbors(t::TwoSectionView, v::Integer) =
    Graphs.all_neighbors(t::TwoSectionView, v)

"""
    Graphs.SimpleGraph(t::TwoSectionView)

Creates a `Graphs.SimpleGraph` representation of a `TwoSectionView` t.

This creates a copy of the date. Note that the weights information is not stored
in the created `SimpleGraph`.
"""
function Graphs.SimpleGraph(t::TwoSectionView)
    g = SimpleGraph(nv(t))
    for v in Graphs.vertices(t)
        neighbors_v = Graphs.all_neighbors(t, v)
        for neighbor in neighbors_v
            add_edge!(g, v, neighbor)
        end
    end
    g
end

Graphs.is_directed(t::TwoSectionView{T}) where T = false

Graphs.is_directed(::Type{TwoSectionView{T}}) where T = false

Base.eltype(::TwoSectionView{T}) where T = Int


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
    Graphs.SimpleGraphs.fadj(t::TwoSectionView)

Generates an adjency list for this view of a hypergraph.
"""
function Graphs.SimpleGraphs.fadj(t::TwoSectionView)
    res = [Vector{Int}() for _ in 1:Graphs.nv(t)]
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
Graphs.SimpleGraphs.fadj(t::TwoSectionView, v::Integer) = Graphs.all_neighbors(t,v)
Graphs.edges(t::TwoSectionView) = Graphs.SimpleGraphs.SimpleEdgeIter(t)

Graphs.edgetype(t::TwoSectionView{T}) where T = Graphs.SimpleGraphs.SimpleEdge{Int}

Graphs.zero(t::TwoSectionView{T}) where T = TwoSectionView(Hypergraph{T}(0,0))
Graphs.zero(::Type{TwoSectionView{T}}) where T = TwoSectionView(Hypergraph{T}(0,0))

"""
    get_twosection_adjacency_mx(h::H{T}; count_self_loops::Bool=false,
                                replace_weights::Union{Nothing,Real}=nothing ) where {H<:AbstractUndirectedHypergraph, T<:Real}

Returns an adjacency matrix for a two section view of a hypergraph `h`.
"""
function get_twosection_adjacency_mx(
    h::H{T};
    count_self_loops::Bool=false,
    replace_weights::Union{Nothing,Real}=nothing
    ) where {H<:AbstractUndirectedHypergraph, T<:Real}
    mx = zeros(replace_weights === nothing ? T : typeof(replace_weights), nhv(h), nhv(h))
    for he in 1:nhe(h)
        for v1 in keys(h.he2v[he])
            for v2 in keys(h.he2v[he])
                v1 == v2 && !count_self_loops && continue
                mx[v1,v2] += replace_weights === nothing ? h.he2v[he][v1] : replace_weights
            end
        end
    end
    mx
end

"""
    get_twosection_adjacency_mx(h::H{T}; count_self_loops::Bool=false,
                                replace_weights::Union{Nothing,Real}=nothing) where {H<:AbstractDirectedHypergraph, T<:Real}

Returns an adjacency matrix for a two section view of a hypergraph `h`.
"""
function get_twosection_adjacency_mx(
    h::H{T};
    count_self_loops::Bool=false,
    replace_weights::Union{Nothing,Real}=nothing
    ) where {H<:AbstractDirectedHypergraph, T<:Real}
    mx = zeros(replace_weights === nothing ? Tuple{T,T} : typeof(replace_weights), nhv(h), nhv(h))
    for he in 1:nhe(h)
        for vt in keys(h.hg_tail.he2v[he])
            for vh in keys(h.hg_head.he2v[he])
                vt == vh && !count_self_loops && continue
                mx[vt,vh] += replace_weights === nothing ? (h.hg_tail.he2v[he][vt], h.hg_head.he2v[he][vh]) : replace_weights
            end
        end
    end
    mx
end