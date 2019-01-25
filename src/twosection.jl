"""
    TwoSectionView{T<:Real} <: AbstractGraph{Int64}

Create a 2-section view of a hypergraph `h`.
Note this is a view - changes to the original hypergraph will be automatically reflected in the view.

The 2-section view of a hypergraph is suitable for processing with the LightGraphs.jl package.
Several LightGraphs methods are provided for the compability.

"""
struct TwoSectionView{T<:Real} <: AbstractGraph{Int}
    h::Hypergraph{T}
    function TwoSectionView{T}(h::Hypergraph{T}) where {T<:Real}
        new(h)
    end
end

LightGraphs.nv(t::TwoSectionView) = length(t.h.v2he)

LightGraphs.vertices(t::TwoSectionView) = Base.OneTo(nv(t))

LightGraphs.ne(t::TwoSectionView)::Int64 = 0.5*sum(length.(t.h.he2v) .* (length.(t.h.he2v) .- 1))

"""
    LightGraphs.all_neighbors(t::TwoSectionView, v::Integer)

Returns N(v) (the vertex v is not included in N(v))
"""
function LightGraphs.all_neighbors(t::TwoSectionView, v::Integer)
    neighbors = Set()
    for he in keys(t.h.v2he[v])
        union!(neighbors, keys(t.h.he2v[he]))
    end
    delete!(neighbors, v) #remove v from its neighborhood
    convert(Array{Int64}, collect(neighbors)) #returns the corresponding array
    #neighbors
end

function LightGraphs.has_edge(t::TwoSectionView, s, d)
    he_s = keys(t.h.v2he[s])
    he_d = keys(t.h.v2he[d])

    !issetequal(intersect(he_s, he_d), Set())
end

LightGraphs.outneighbors(t::TwoSectionView, v::Integer) = LightGraphs.all_neighbors(t::TwoSectionView, v)

LightGraphs.inneighbors(t::TwoSectionView, v::Integer) = LightGraphs.all_neighbors(t::TwoSectionView, v)

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

LightGraphs.is_directed(t::TwoSectionView) = false

"""
    shortest_path(t::TwoSectionView,source::Int, target::Int)

Finds a single shortest path in a graph `b` between vertices
`source` and `target`.
Note that if several paths of the same length exist, only one
will be returned.

"""
function shortest_path(t::TwoSectionView,source::Int, target::Int)
    @boundscheck source <= length(t.h.v2he) || throw(BoundsError(t.h.v2he, source))
    @boundscheck target <= length(t.h.v2he) || throw(BoundsError(t.h.v2he, target))
    dj = dijkstra_shortest_paths(t, source)
    enumerate_paths(dj)[target]#[1:2:end]
end
