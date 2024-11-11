"""
    AbstractHypergraph{T} <: AbstractMatrix{Union{T, Nothing}}

An abstract hypergraph type storing information about vertices and hyperedges.
"""
# TODO: should this be just `abstract type AbstractHypergraph{T} end`?
# How important is subtyping AbstractMatrix?
abstract type AbstractHypergraph{T} <: AbstractMatrix{Union{T, Nothing}} end

# TODO: interface, similar to Graphs.jl

# fundamental traits
# Inspired by HyperGraphs.jl
@traitdef IsDirected{X <: AbstractHypergraph}
@traitdef HasMeta{X <: AbstractHypergraph}

# functions and default behaviour: direction
@traitimpl IsDirected{T} <- isoriented(T)
isdirected(::Type{T}) where {T} = false
isdirected(X::T) where {T} = isoriented(T)

# functions and default behaviour: vertex/hyperedge metadata
@traitimpl HasMeta{T} <- hasmeta(T)
hasmeta(::Type{T}) where {T} = false
hasmeta(X::T) where {T} = hasmeta(T)