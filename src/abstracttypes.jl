"""
    AbstractHypergraph{T} <: AbstractMatrix{T}

An abstract hypergraph type storing information about vertices and hyperedges.
"""
abstract type AbstractHypergraph{T} <: AbstractMatrix{T} end

"""
    AbstractSimpleHypergraph{T} <: AbstractHypergraph{T}

An abstract undirected hypergraph type storing information about vertices and hyperedges.
"""
abstract type AbstractSimpleHypergraph{T} <: AbstractHypergraph{T} end


# fundamental traits
# Inspired by HyperGraphs.jl
@traitdef IsDirected{X <: AbstractHypergraph}
@traitdef HasVertexMeta{X <: AbstractHypergraph}
@traitdef HasHyperedgeMeta{X <: AbstractHypergraph}

# functions and default behaviour: direction
@traitimpl IsDirected{T} <- isdirected(T)
isdirected(::Type{T}) where {T} = false
isdirected(X::T) where {T} = isdirected(T)

# functions and default behaviour: vertex/hyperedge metadata
@traitimpl HasVertexMeta{T} <- hasvertexmeta(T)
hasvertexmeta(::Type{T}) where {T} = false
hasvertexmeta(X::T) where {T} = hasvertexmeta(T)

@traitimpl HasHyperedgeMeta{T} <- hashyperedgemeta(T)
hashyperedgemeta(::Type{T}) where {T} = false
hashyperedgemeta(X::T) where {T} = hashyperedgemeta(T)
