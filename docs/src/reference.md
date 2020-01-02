Reference
=========

```@meta
CurrentModule = SimpleHypergraphs
DocTestSetup = quote
    using SimpleHypergraphs
end
```

Creating a hypergraph
---------------------

```@docs
Hypergraph
```

Manipulating vertices and hyperedges
------------------------------------
```@docs

add_hyperedge!(::Hypergraph{T, V, E}; ::Dict{Int,T}, ::Union{E,Nothing} ) where {T <: Real, V, E}
add_vertex!(::Hypergraph{T, V, E};::Dict{Int,T},::Union{V,Nothing} ) where {T <: Real, V, E}
set_vertex_meta!(::Hypergraph{T, V, E}, ::Union{V,Nothing}, ::Int) where {T <: Real, V, E}
get_vertex_meta(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}
set_hyperedge_meta!(::Hypergraph{T, V, E}, ::Union{E,Nothing}, ::Int) where {T <: Real, V, E}
get_hyperedge_meta(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}
remove_vertex!(::Hypergraph, ::Int)
```

Hypergraph array getters and setters
------------------------------------

Normally you work with a hypergraph via array setters, for example the code below craete an Hypergraph and add vertex one to hyperedges 2 and 3 with weight 5:
```jldoctest
h = Hypergraph{Int64}(2,3);
h[1, 2:3] .= 5;  
h

# output

2×3 Hypergraph{Int64,Nothing,Nothing}:
 nothing  5         5
 nothing   nothing   nothing
```

```@docs
getindex(::Hypergraph, ::Vararg{Int,2})
setindex!(::Hypergraph, ::Nothing, ::Vararg{Int,2})
setindex!(::Hypergraph, ::Real, ::Vararg{Int,2})
```

Hypergraph representation as LightGraphs' simple graphs
-------------------------------------------------------

The goal of those methods is to provide a way to manipulate a hypergraph using
the methods from the [LightGraphs.jl](https://github.com/JuliaGraphs/LightGraphs.jl) library.
This has been achieved by providing types that are subtypes of the
`LightGraphs.SimpleGraphs.AbstractSimpleGraph{Int}` type along with appropiate methods.

```@docs
BipartiteView
shortest_path(::BipartiteView, ::Int, ::Int)

TwoSectionView
shortest_path(::TwoSectionView, ::Int, ::Int)
```

Hypergraph info
---------------
```@docs
size(::Hypergraph)
nhv(::Hypergraph)
nhe(::Hypergraph)
getvertices(::Hypergraph, ::Int)
gethyperedges(::Hypergraph, ::Int)
get_connected_components(::Hypergraph)

modularity(::Hypergraph, ::Vector{Set{Int}}, ::SimpleHypergraphs.HypergraphAggs)
random_walk(::Hypergraph, ::Int; heselect::Function, vselect::Function)

SimpleHypergraphs.HypergraphAggs

randompartition(::Int64,::Int64)
randompartition(::Hypergraph,::Int64)

AbstractCommunityFinder
CFModularityRandom
CFModularityCNMLike

findcommunities(::Hypergraph, ::CFModularityRandom)
findcommunities(::Hypergraph, ::CFModularityCNMLike)
```

I/O
---
```@docs
hg_save
hg_load
```

```@meta
DocTestSetup = nothing
```
