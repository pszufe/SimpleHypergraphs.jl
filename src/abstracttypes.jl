"""
    AbstractHypergraph{T} <: AbstractMatrix{Union{T, Nothing}}

An abstract hypergraph type storing information about vertices and hyperedges.
"""
# TODO: should this be just `abstract type AbstractHypergraph{T} end`?
# How important is subtyping AbstractMatrix?
abstract type AbstractHypergraph{T} <: AbstractMatrix{Union{T, Nothing}} end

# TODO: interface, similar to Graphs.jl