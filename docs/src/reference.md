Reference
=========

```@meta
CurrentModule = SimpleHypergraphs
DocTestSetup = quote
    using SimpleHypergraphs
end
```

Abstract types
---------------------

```@docs

AbstractHypergraph{T} <: AbstractMatrix{T}
AbstractUndirectedHypergraph{T} <: AbstractHypergraph{Union{T, Nothing}}
AbstractDirectedHypergraph{T} <: AbstractHypergraph{Tuple{Union{T, Nothing}, Union{T, Nothing}}}
```

Creating an undirected hypergraph
---------------------

```@docs

Hypergraph
BasicHypergraph
random_model(::Int, ::Int, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
random_kuniform_model(::Int, ::Int, ::Int, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
random_dregular_model(::Int, ::Int, ::Int, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
random_preferential_model(::Int, ::Real, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
```

Creating a directed hypergraph
---------------------

```@docs

DirectedHypergraph
BasicDirectedHypergraph
random_model(::Int, ::Int, ::Type{H}; ::Bool) where {H <: AbstractDirectedHypergraph}
random_dregular_model(::Int, ::Int, ::Int, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
random_preferential_model(::Int, ::Real, ::Type{H}) where {H <: AbstractUndirectedHypergraph}
```

Manipulating vertices and hyperedges
------------------------------------
```@docs

add_hyperedge!(::Hypergraph{T, V, E, D}; ::D, ::Union{E,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
add_hyperedge!(::BasicHypergraph{T, D}; ::D) where {T <: Real, D <: AbstractDict{Int,T}}
add_hyperedge!(::DirectedHypergraph{T, V, E, D}; ::D, ::D, ::Union{E,Nothing}, ::Union{E,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
add_hyperedge!(::BasicDirectedHypergraph{T, D}; ::D, ::D) where {T <: Real, D <: AbstractDict{Int,T}}

add_vertex!(::Hypergraph{T, V, E, D}; ::D, ::Union{V,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
add_vertex!(::BasicHypergraph{T, D}; ::D) where {T <: Real, D <: AbstractDict{Int,T}}
add_vertex!(::DirectedHypergraph{T, V, E, D}; ::D, ::D, ::Union{V,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
add_vertex!(::BasicDirectedHypergraph{T, D}; ::D, ::D) where {T <: Real, D <: AbstractDict{Int,T}}

set_vertex_meta!(::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, ::Union{V,Nothing}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
get_vertex_meta(::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
set_hyperedge_meta!(::Hypergraph{T, V, E, D}, ::Union{E,Nothing}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
set_hyperedge_meta!(::DirectedHypergraph{T, V, E, D}, ::Union{E,Nothing}, ::Union{E,Nothing}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
get_hyperedge_meta(::Hypergraph{T, V, E, D}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
get_hyperedge_meta(::DirectedHypergraph{T, V, E, D}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

remove_vertex!(::Hypergraph, ::Int)
remove_vertex!(::BasicHypergraph, ::Int)
remove_vertex!(::DirectedHypergraph, ::Int)
remove_vertex!(::BasicDirectedHypergraph, ::Int)

remove_hyperedge!(::Hypergraph, ::Int)
remove_hyperedge!(::BasicHypergraph, ::Int)
remove_hyperedge!(::DirectedHypergraph, ::Int)
remove_hyperedge!(::BasicDirectedHypergraph, ::Int)
```

Hypergraph array getters and setters
------------------------------------

Normally you work with a hypergraph via array setters, for example the code below craete an Hypergraph and add vertex one to hyperedges 2 and 3 with weight 5:
```jldoctest
h = Hypergraph{Int64}(2,3);
h[1, 2:3] .= 5;
h

# output

2×3 Hypergraph{Int64, Nothing, Nothing, Dict{Int64, Int64}}:
 nothing  5         5
 nothing   nothing   nothing
```

For a directed hypergraph, an additional index can refer to either the tail (1) or head (2) of a directed hyperedge:
```jldoctest
dh = BasicDirectedHypergraph{Int64}(2,3);
dh[1,1,1] = 1;
dh[2,2,1] = 2;
dh

# output

2×3 BasicDirectedHypergraph{Int64, Dict{Int64, Int64}}:
 (1, nothing)  (nothing, nothing)  (nothing, nothing)
 (nothing, 2)  (nothing, nothing)  (nothing, nothing)
```

Setting with slices is not currently possible, and users must currently use slices on the component undirected hypergraphs instead. However, accessing using slices is allowed:
```jldoctest
dh = BasicDirectedHypergraph{Int64}(4,2);
dh.hg_tail[1:2,1] .= 1;
dh.hg_head[3:4,1] .= 2;
dh[:,1]

# output

4-element Vector{Tuple{Union{Nothing, Int64}, Union{Nothing, Int64}}}:
 (1, nothing)
 (1, nothing)
 (nothing, 2)
 (nothing, 2)
```

```@docs
Base.getindex(::H, ::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
Base.getindex(::H, ::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}

Base.setindex!(::H, ::Nothing, ::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
Base.setindex!(::H, ::Real, ::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
Base.setindex!(::H, ::Nothing, ::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
Base.setindex!(::H, ::Real, ::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
Base.setindex!(::H, ::Tuple{Union{Real, Nothing}, Union{Real, Nothing}}, ::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
Base.setindex!(::H, ::Nothing, ::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}
Base.setindex!(::H, ::Real, ::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}
```

Hypergraph representation as Graphs.jl simple graphs
-------------------------------------------------------

The goal of those methods is to provide a way to manipulate a hypergraph using
the methods from the [Graphs.jl](https://github.com/JuliaGraphs/Graphs.jl) library.
This has been achieved by providing types that are subtypes of the
`Graphs.SimpleGraphs.AbstractSimpleGraph{Int}` type along with appropiate methods.

```@docs
BipartiteView
shortest_path(::BipartiteView, ::Int, ::Int)

TwoSectionView
shortest_path(::TwoSectionView, ::Int, ::Int)
```

Hypergraph info
---------------
```@docs
size(::H) where {H <: AbstractHypergraph}
nhv(::H) where {H <: AbstractUndirectedHypergraph}
nhv(::H) where {H <: AbstractDirectedHypergraph}
nhe(::H) where {H <: AbstractUndirectedHypergraph}
nhe(::H) where {H <: AbstractDirectedHypergraph}

getvertices(h::H, he_id::Int) where {H <: AbstractUndirectedHypergraph}
getvertices(h::H, he_id::Int) where {H <: AbstractDirectedHypergraph}
gethyperedges(::Hypergraph, ::Int) where {H <: AbstractUndirectedHypergraph}
gethyperedges(::Hypergraph, ::Int) where {H <: AbstractDirectedHypergraph}

get_connected_components(::H) where {H <: AbstractUndirectedHypergraph}
get_weakly_connected_components(::H) where {H <: AbstractDirectedHypergraph}
get_strongly_connected_components(::H) where {H <: AbstractDirectedHypergraph}

conductance(::Hypergraph, ::Set{Int})
get_twosection_adjacency_mx(::H; ::Bool, ::Union{Nothing,Real}) where {H<:AbstractUndirectedHypergraph}
get_twosection_adjacency_mx(::H; ::Bool, ::Union{Nothing,Real}) where {H<:AbstractDirectedHypergraph}
random_walk(::H, ::Int; ::Function, ::Function) where {H <: AbstractUndirectedHypergraph}
random_walk(::H, ::Int; ::Function, ::Function, ::Bool) where {H <: AbstractDirectedHypergraph}

dual(h::Hypergraph)
dual(h::BasicHypergraph)
dual(h::DirectedHypergraph)
dual(h::BasicDirectedHypergraph)

modularity(::H, ::Vector{Set{Int}}, ::SimpleHypergraphs.HypergraphAggs) {H <: AbstractUndirectedHypergraph}

SimpleHypergraphs.HypergraphAggs

randompartition(::Int64,::Int64)
randompartition(::H, ::Int) where {H <: AbstractHypergraph}

AbstractCommunityFinder
CFModularityRandom
CFModularityCNMLike

findcommunities(::H, ::CFModularityRandom) where {H <: AbstractUndirectedHypergraph}
findcommunities(::H, ::CFModularityCNMLike) where {H <: AbstractUndirectedHypergraph}
```

I/O
---

Undirected hypergraphs can be saved as and loaded from JSON- and HGF-formatted files. Directed hypergraphs can be saved as and loaded from JSON- and EHGF-formatted files, where the EHGF format is a close derivative of HGF.

```@docs
hg_save
hg_load
```

Hypergraph Visualization
------------------------

Currently, visualization is only supported for undirected hypergraphs.

```@docs
draw
```

```@meta
DocTestSetup = nothing
```
