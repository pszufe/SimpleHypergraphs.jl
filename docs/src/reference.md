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

AbstractHypergraph
AbstractSimpleHypergraph
```

Creating an undirected hypergraph
---------------------

```@docs

Hypergraph
random_model(::Int, ::Int, ::Type{H}) where {H <: AbstractSimpleHypergraph}
random_kuniform_model(::Int, ::Int, ::Int, ::Type{H}) where {H <: AbstractSimpleHypergraph}
random_dregular_model(::Int, ::Int, ::Int, ::Type{H}) where {H <: AbstractSimpleHypergraph}
random_preferential_model(::Int, ::Real, ::Type{H}) where {H <: AbstractSimpleHypergraph}
```

Manipulating vertices and hyperedges
------------------------------------
```@docs

add_hyperedge!(::Hypergraph{T, V, E, D}; ::D, ::Union{E,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

add_vertex!(::Hypergraph{T, V, E, D}; ::D, ::Union{V,Nothing}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

set_vertex_meta!(::Hypergraph{T, V, E, D}, ::Union{V,Nothing}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
get_vertex_meta(::Hypergraph{T, V, E, D}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
set_hyperedge_meta!(::Hypergraph{T, V, E, D}, ::Union{E,Nothing}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
get_hyperedge_meta(::Hypergraph{T, V, E, D}, ::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

remove_vertex!(::Hypergraph, ::Int)

remove_hyperedge!(::Hypergraph, ::Int)
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

```@docs
Base.getindex(::H, ::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}

Base.setindex!(::H, ::Nothing, ::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}
Base.setindex!(::H, ::Real, ::Vararg{Int,2}) where {H <: AbstractSimpleHypergraph}
```

Hypergraph representation as Graphs.jl simple graphs
-------------------------------------------------------

The goal of those methods is to provide a way to manipulate a hypergraph using
the methods from the [Graphs.jl](https://github.com/JuliaGraphs/Graphs.jl) library.
This has been achieved by providing types that are subtypes of the
`Graphs.SimpleGraphs.AbstractSimpleGraph{Int}` type along with appropiate methods.

```@docs
BipartiteView
shortest_path(::BipartiteView{H}, ::Int, ::Int) where {H<:AbstractSimpleHypergraph}

TwoSectionView
shortest_path(::TwoSectionView{H}, ::Int, ::Int) where {H<:AbstractSimpleHypergraph}
```

Hypergraph info
---------------
```@docs
size(::H) where {H <: AbstractHypergraph}
nhv(::H) where {H <: AbstractSimpleHypergraph}
nhe(::H) where {H <: AbstractSimpleHypergraph}

getvertices(h::H, he_id::Int) where {H <: AbstractSimpleHypergraph}
gethyperedges(::Hypergraph, ::Int) where {H <: AbstractSimpleHypergraph}

get_connected_components(::H) where {H <: AbstractSimpleHypergraph}

conductance(::Hypergraph, ::Set{Int})
get_twosection_adjacency_mx(::H; ::Bool, ::Union{Nothing,Real}) where {T<:Real, H<:AbstractSimpleHypergraph{Union{T, Nothing}}}
random_walk(::H, ::Int; ::Function, ::Function) where {H <: AbstractSimpleHypergraph}

dual(h::Hypergraph)

modularity(::H, ::Vector{Set{Int}}, ::SimpleHypergraphs.HypergraphAggs) where {H <: AbstractSimpleHypergraph}

SimpleHypergraphs.HypergraphAggs

randompartition(::Int64,::Int64)
randompartition(::H, ::Int) where {H <: AbstractHypergraph}

AbstractCommunityFinder
CFModularityRandom
CFModularityCNMLike

findcommunities(::H, ::CFModularityRandom) where {H <: AbstractSimpleHypergraph}
findcommunities(::H, ::CFModularityCNMLike) where {H <: AbstractSimpleHypergraph}
```

I/O
---

Undirected hypergraphs can be saved as and loaded from JSON- and HGF-formatted files.

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
