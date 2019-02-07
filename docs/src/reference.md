Reference
=========

```@meta
CurrentModule = SimpleHypergraphs
DocTestSetup = quote
    using SimpleHypergraphs
end
```

Types
-----

```@docs
Hypergraph
```

Manipulating vertices and hyperedges
------------------------------------
```@docs

add_hyperedge!(::Hypergraph{T, V, E}; ::Dict{Int,T}, ::Union{UndefInitializer, E}) where {T <: Real, V, E}
add_vertex!(::Hypergraph{T, V, E};::Dict{Int,T},::Union{UndefInitializer,V}) where {T <: Real, V, E}
set_vertex_value!(::Hypergraph{T, V, E}, ::V, ::Int) where {T <: Real, V, E}
get_vertex_value(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}
set_hyperedge_value!(::Hypergraph{T, V, E}, ::E, ::Int) where {T <: Real, V, E}
get_hyperedge_value(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}
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

Hypergraph info
---------------
```@docs
size(::Hypergraph)
getvertices(::Hypergraph, ::Int)
gethyperedges(::Hypergraph, ::Int)

BipartiteView
shortest_path(::BipartiteView, ::Int, ::Int)

TwoSectionView
shortest_path(::TwoSectionView, ::Int, ::Int)
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
