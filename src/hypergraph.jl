# We support two-way mapping as in different problems both might be needed.
# In the matrix representation rows are vertices and columns are hyperedges
# n is number of vertices and k is number of hyperedges
# if he[vertex, hyperedge] returns nothing this means that the vertex
# is not present in the hyperedge
# if he[vertex, hyperedge] returns a real number it is a weight
# of the vertex in this hyperedge

"""
    Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}

A hypergraph storing information about vertices and hyperedges.

**Constructors**

    Hypergraph{T}(n,k) where {T<:Real}
    Hypergraph{T,V}(n, k) where {T<:Real, V}
    Hypergraph{T,V,E}(n, k) where {T<:Real, V, E}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges.

    Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{V}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V}
    Hypergraph{V, E}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V, E}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges.

    Hypergraph(g::LightGraphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of LightGraphs.Graph


**Arguments**

* `T` : type of weight values stored in the hypergraph
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges

"""
struct Hypergraph{T,V,E} <: AbstractMatrix{Union{T, Nothing}}
    v2he::Vector{Dict{Int,T}}
    he2v::Vector{Dict{Int,T}}
    v_meta::Vector{Union{V,Nothing}}
    he_meta::Vector{Union{E,Nothing}}
    Hypergraph{T,V,E}(n, k) where {T<:Real, V, E} =
        new{T,V,E}([Dict{Int,T}() for i in 1:n], [Dict{Int,T}() for i in 1:k],
                          Vector{Union{V,Nothing}}(nothing, n), Vector{Union{E,Nothing}}(nothing, k))
end

Hypergraph{T,V}(n, k) where {T<:Real, V} = Hypergraph{T,V,Nothing}(n, k)
Hypergraph{T}(n, k) where {T<:Real} =  Hypergraph{T,Nothing,Nothing}(n, k)


function Hypergraph{V, E}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V, E}
    n, k = size(m)
    h = Hypergraph{T, V, E}(n,k)
    h .= m
    h
end

function Hypergraph{V}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V}
    Hypergraph{V, Nothing}(m)
end

function Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{Nothing, Nothing}(m)
end


function Hypergraph(g::LightGraphs.Graph)
    h = Hypergraph{Bool}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end



"""
    Base.size(h::Hypergraph)

Returns the size of Hypergraph `h`.
The result is a tuple of the number of vertices and the number of hyperedges

"""
Base.size(h::Hypergraph) = (nhv(h), nhe(h))

"""
    Base.getindex(h::Hypergraph, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair `idx` for a hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::Hypergraph, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

"""
    Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})

Removes a vertex from a given hyperedge for a hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2], nothing)
    pop!(h.he2v[idx[2]], idx[1], nothing)
    h
end

"""
    Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})

Adds a vertex to a hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end

"""
    getvertices(h::Hypergraph, he_id::Int)

Returns vertices from a hypergraph `a` for a given hyperedge `he_id`.

"""
@inline getvertices(h::Hypergraph, he_id::Int) = h.he2v[he_id]

"""
    gethyperedges(h::Hypergraph, v_id::Int)

Returns hyperedges for a given vertex `v_id` in a hypergraph `h`.

"""
@inline gethyperedges(h::Hypergraph, v_id::Int) = h.v2he[v_id]

"""
    add_vertex!(h::Hypergraph{T, V, E}; hyperedges::Dict{Int,T} = Dict{Int,T}(),
                vertex_meta::Union{V,Nothing} nothing undef) where {T <: Real, V, E}

Adds a vertex to a given hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the vertex using the `vertex_meta` keyword parameter.

"""
function add_vertex!(h::Hypergraph{T, V, E}; hyperedges::Dict{Int,T} = Dict{Int,T}(),
                     vertex_meta::Union{V, Nothing} = nothing) where {T <: Real, V, E}
    @boundscheck (checkbounds(h,1,k) for k in keys(hyperedges))
    push!(h.v2he,hyperedges)
    ix = length(h.v2he)
    for k in keys(hyperedges)
        h[ix,k]=hyperedges[k]
    end
    push!(h.v_meta, vertex_meta)
    ix
end

"""
    add_hyperedge!(h::Hypergraph{T, V, E}; vertices::Dict{Int,T} = Dict{Int,T}(),
                   hyperedge_meta::Union{E,Nothing}=nothing) where {T <: Real, V, E}

Adds a hyperedge to a given hypergraph `h`. Optionally, existing vertices can be added
to the created hyperedge. The paramater `vertices` represents a dictionary
of vertex identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the hyperedge using the `hyperedge_meta` keyword parameter.

"""
function add_hyperedge!(h::Hypergraph{T, V, E}; vertices::Dict{Int,T} = Dict{Int,T}(),
                        hyperedge_meta::Union{E,Nothing}=nothing) where {T <: Real, V, E}
    @boundscheck (checkbounds(h,k,1) for k in keys(vertices))
    push!(h.he2v,vertices)
    ix = length(h.he2v)
    for k in keys(vertices)
        h[k,ix]=vertices[k]
    end
    push!(h.he_meta, hyperedge_meta)
    ix
end

"""
    set_vertex_meta!(h::Hypergraph{T, V, E}, new_value::Union{V,Nothing}, id::Int)
        where {T <: Real, V, E}

Sets a new meta value `new_value` for the vertex `id` in the hypegraph `h`.

"""
function set_vertex_meta!(h::Hypergraph{T, V, E}, new_value::Union{V,Nothing}, id::Int) where {T <: Real, V, E}
    checkbounds(h.v_meta, id)
    h.v_meta[id] = new_value
    h.v_meta
end

"""
    get_vertex_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}

Returns a meta value stored at the vertex `id` in the hypergraph `h`.

"""
function get_vertex_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}
    checkbounds(h.v_meta, id)
    h.v_meta[id]
end

"""
    set_hyperedge_meta!(h::Hypergraph{T, V, E}, new_value::Union{E,Nothing}, id::Int)
        where {T <: Real, V, E}

Sets a new meta value `new_value` for the hyperedge `id` in the hypegraph `h`.

"""
function set_hyperedge_meta!(h::Hypergraph{T, V, E}, new_value::Union{E,Nothing}, id::Int) where {T <: Real, V, E}
    checkbounds(h.he_meta, id)
    h.he_meta[id] = new_value
    h.he_meta
end

"""
    get_hyperedge_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}

Returns a meta value stored at the hyperedge `id` in the hypergraph `h`.

"""
function get_hyperedge_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}
    checkbounds(h.he_meta, id)
    h.he_meta[id]
end

"""
    nhe(h::Hypergraph{T, V, E}) where {T <: Real, V, E}

Return the number of hyperedges in the hypergraph `h`.
"""
function nhe(h::Hypergraph{T, V, E}) where {T <: Real, V, E}
    length(h.he2v)
end



"""
    nhv(h::Hypergraph{T, V, E}) where {T <: Real, V, E}

Return the number of vertices in the hypergraph `h`.
"""
function nhv(h::Hypergraph{T, V, E}) where {T <: Real, V, E}
    length(h.v2he)
end



# TODO needs remove_vertex!(h::Hypergraph{T}, v_id::Int)

# TODO needs add_hyperedge!(h::Hypergraph{T}, he_id::Int)

# TODO needs validate_hypergraph!(h::Hypergraph{T})
