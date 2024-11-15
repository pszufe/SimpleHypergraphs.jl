# TODO: think more carefully about ensuring that metadata vectors are of appropriate lengths

"""
    Hypergraph{T} <: AbstractUndirectedHypergraph{T}

An undirected hypergraph storing information about vertices and hyperedges.

**Constructors**

    Hypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    Hypergraph{T,V}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n)
        ) where {T<:Real, V}
    Hypergraph{T,E}(n::Integer, k::Integer;
        he_meta=Vector{Union{E,Nothing}}(nothing, n)
        ) where {T<:Real, E}
    Hypergraph{T,V,E}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, V, E}
    Hypergraph{T,V,E,D}(n::Integer, k::Integer,
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,V}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1))
        ) where {T<:Real, V}
    Hypergraph{T,E}(m::AbstractMatrix{Union{T, Nothing}};
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, E}
    Hypergraph{T,V,E}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, V, E}
    Hypergraph{T,V,E,D}(m::AbstractMatrix{Union{T, Nothing}};
        v_meta=Vector{Union{V,Nothing}}(nothing, size(m,1)),
        he_meta=Vector{Union{E,Nothing}}(nothing, size(m,2))
        ) where {T<:Real, V, E, D<:AbstractDict{Int,T}}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    Hypergraph(g::Graphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of Graphs.Graph.
A `SortedDict` will be used for internal data storage of the hypergraph.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a graph representation of the hypergraph
"""

struct Hypergraph{T<:Real,V,E,D<:AbstractDict{Int,T}} <: AbstractUndirectedHypergraph{T}
    v2he::Vector{D}
    he2v::Vector{D}
    v_meta::Vector{Union{V,Nothing}}
    he_meta::Vector{Union{E,Nothing}}

    Hypergraph{T,V,E,D}(n::Integer, k::Integer,
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta=Vector{Union{E,Nothing}}(nothing, k)
    ) where {T<:Real,V,E,D<:AbstractDict{Int,T}} =
        new{T,V,E,D}(
            [D() for i in 1:n],[D() for i in 1:k],
            v_meta, he_meta
        )
end


Hypergraph{T,V,E}(n::Integer, k::Integer) where {T<:Real, V, E} = Hypergraph{T,V,E,Dict{Int,T}}(n, k)

Hypergraph{T,V}(n::Integer, k::Integer; v_meta=Vector{Union{V,Nothing}}(nothing, n)) where {T<:Real, V} = Hypergraph{T,V,Nothing,Dict{Int,T}}(n, k; v_meta=v_meta)

# Why was this not included before?
Hypergraph{T,E}(n::Integer, k::Integer; he_meta=Vector{Union{E,Nothing}}(nothing, k)) where {T<:Real, E} = Hypergraph{T,Nothing,E,Dict{Int,T}}(n, k; he_meta=he_meta)

Hypergraph{T}(n::Integer, k::Integer) where {T<:Real} =  Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(n, k)

Hypergraph(n::Integer, k::Integer) =  Hypergraph{Bool,Nothing,Nothing,Dict{Int,Bool}}(n, k)

function Hypergraph{T,V,E,D}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
                        he_meta::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
                        ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}
    @assert length(v_meta) == size(m,1)
    @assert length(he_meta) == size(m,2)
    n, k = size(m)
    h = Hypergraph{T,V,E,D}(n, k, v_meta, he_meta)
    h .= m
    h
end

function Hypergraph{T,V,E}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
                        he_meta::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
                        ) where {T<:Real,V,E}
    Hypergraph{T,V,E,Dict{Int,T}}(m;v_meta=v_meta,he_meta=he_meta)
end

function Hypergraph{T,V}(m::AbstractMatrix{Union{T, Nothing}};
                        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1))
                        ) where {T<:Real,V}
    Hypergraph{T,V,Nothing,Dict{Int,T}}(m;v_meta=v_meta)
end

function Hypergraph{T,E}(m::AbstractMatrix{Union{T, Nothing}};
    he_meta::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E}
    Hypergraph{T,Nothing,E,Dict{Int,T}}(m;he_meta=he_meta)
end

function Hypergraph{T}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(m)
end

function Hypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    Hypergraph{T,Nothing,Nothing,Dict{Int,T}}(m)
end


function Hypergraph(g::Graphs.Graph)
    h = Hypergraph{Bool,Nothing,Nothing,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end


"""
    BasicHypergraph{T} <: AbstractUndirectedHypergraph{T}

An undirected hypergraph storing only incidence and weight information about vertices and
hyperedges, without additional features/metadata.

**Constructors**

    BasicHypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    BasicHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for the internal data storage,
however a different dictionary such as `SortedDict` to ensure result replicability
can be used (e.g. when doing stochastic simulations on hypergraphs).

    BasicHypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    BasicHypergraph{T, D}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for the internal data storage,
however a different dictionary such as `SortedDict` to ensure result
replicability can be used (e.g. when doing stochastic simulations on
hypergraphs).

    BasicHypergraph(g::Graphs.Graph)

Constructs a hypergraph of degree 2 by making a deep copy of Graphs.Graph.
A `SortedDict` will be used for internal data storage of the hypergraph.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a graph representation of the hypergraph
"""

struct BasicHypergraph{T<:Real,D<:AbstractDict{Int,T}} <: AbstractUndirectedHypergraph{T}
    v2he::Vector{D}
    he2v::Vector{D}

    BasicHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real,D<:AbstractDict{Int,T}} =
        new{T,D}([D() for i in 1:n],[D() for i in 1:k])
end

BasicHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = BasicHypergraph{T,Dict{Int,T}}(n, k)

BasicHypergraph(n::Integer, k::Integer) =  BasicHypergraph{Bool,Dict{Int,Bool}}(n, k)


function BasicHypergraph{T,D}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real,D<:AbstractDict{Int,T}}
    n, k = size(m)
    h = BasicHypergraph{T,D}(n, k)
    h .= m
    h
end

function BasicHypergraph{T}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}
    BasicHypergraph{T,Dict{Int,T}}(m)
end


function BasicHypergraph(g::Graphs.Graph)
    h = BasicHypergraph{Bool,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e += 1
        h[edge.src,e] = true
        h[edge.dst,e] = true
    end
    h
end

"""
    DirectedHypergraph{T} <: AbstractDirectedHypergraph{T}

A directed hypergraph storing information about vertices and hyperedges.

This implementation is based on guidance from Przemysław Szufel;
    see https://github.com/pszufe/SimpleHypergraphs.jl/issues/45
This allows us to manipulate DirectedHypergraphs using Hypergraph functionality
There is danger of a user manipulating individual `hg_in` and `hg_out` (undirected) hypergraphs
Is there a smart way to prevent this?
TODO: reconsider this design choice

**Constructors**

    DirectedHypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    DirectedHypergraph{T,V}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n)
        ) where {T<:Real, V}
    DirectedHypergraph{T,E}(n::Integer, k::Integer;
        he_meta_in=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_out=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, E}
    DirectedHypergraph{T,V,E}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta_in=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_out=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, V, E}
    DirectedHypergraph{T,V,E,D}(n::Integer, k::Integer,
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta_in=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_out=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    DirectedHypergraph(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}    
    DirectedHypergraph{T}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    DirectedHypergraph{T,V}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V}
    DirectedHypergraph{T,E}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}};
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E}
    DirectedHypergraph{T,V,E}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E}
    DirectedHypergraph{T,V,E,D}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

Construct a directed hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    DirectedHypergraph(g::Graphs.DiGraph)

Constructs a directed hypergraph of degree 2 by making a deep copy of a
Graphs.DiGraph. A `SortedDict` will be used for internal data storage of the
hypergraph.

    DirectedHypergraph{T,V,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V,D<:AbstractDict{Int, T}}
    DirectedHypergraph{T,E,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D};
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E,D<:AbstractDict{Int, T}}
    DirectedHypergraph{T,V,E,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E,D<:AbstractDict{Int, T}}

Constructs a directed hypergraph from two undirected basic hypergraphs, one with hyperedges
containing "incoming" vertices and one with hyperedges containing "outgoing"
verticies.

    DirectedHypergraph{T,V,E,D}(
        hg_in::Hypergraph{T,V,E,D},
        hg_out::Hypergraph{T,V,E,D}
    ) where {T<:Real,V,E,D<:AbstractDict{Int, T}}

Constructs a directed hypergraph from two hypergraphs potentially containing metadata. Throws
an error if the vertex metadata of the two hypergraphs is not element-for-element identical.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a (directed) graph representation of the hypergraph
* `hg_in`: an undirected hypergraph representing the incoming half of
    the directed hypergraph
* `hg_out`: an undirected hypergraph representing the outgoing half of
    the directed hypergraph
"""

struct DirectedHypergraph{T<:Real,V,E,D<:AbstractDict{Int, T}} <: AbstractDirectedHypergraph{Tuple{T, T}}
    hg_in::BasicHypergraph{T,D}
    hg_out::BasicHypergraph{T,D}

    v_meta::Vector{Union{V,Nothing}}
    he_meta_in::Vector{Union{E,Nothing}}
    he_meta_out::Vector{Union{E,Nothing}}

    DirectedHypergraph{T,V,E,D}(
        n::Integer, k::Integer,
        v_meta=Vector{Union{V, Nothing}}(nothing, n),
        he_meta_in=Vector{Union{E, Nothing}}(nothing, k),
        he_meta_out=Vector{Union{E, Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int, T}} = 
        new{T,V,E,D}(
            BasicHypergraph(n, k),
            BasicHypergraph(n, k),
            v_meta, he_meta_in, he_meta_out
        )

    function DirectedHypergraph{T,V,E,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
        ) where {T<:Real,V,E,D<:AbstractDict{Int, T}}
        @assert size(hg_in) == size(hg_out)

        @assert length(v_meta) == size(hg_in,1)
        @assert length(he_meta_in) == size(hg_in,2)
        @assert length(he_meta_out) == size(hg_out,2)

        new{T,V,E,D}(
            hg_in,
            hg_out,
            v_meta,
            he_meta_in,
            he_meta_out
        )
    end
end

DirectedHypergraph{T,V,E}(
    n::Integer, k::Integer;
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real, V, E} = DirectedHypergraph{T,V,E,Dict{Int,T}}(
        n, k;
        v_meta=v_meta,
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )

DirectedHypergraph{T,V}(
    n::Integer, k::Integer;
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real, V} = DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        n, k;
        v_meta=v_meta
    )

DirectedHypergraph{T,E}(
    n::Integer, k::Integer;
    he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real, V} = DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        n, k;
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )

DirectedHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real, D<:AbstractDict{Int, T}} = DirectedHypergraph{T,Nothing,Nothing,D}(n, k)

DirectedHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(n, k)

DirectedHypergraph(n::Integer, k::Integer) = DirectedHypergraph{Bool,Nothing,Nothing,Dict{Int,Bool}}(n, k)


function DirectedHypergraph{T,D}(
    hg_in::BasicHypergraph{T,D},
    hg_out::BasicHypergraph{T,D}
    ) where {T<:Real,D<:AbstractDict{Int, T}}

    DirectedHypergraph{T,Nothing,Nothing,D}(
        hg_in,
        hg_out
    )
end

function DirectedHypergraph{T,V,D}(
    hg_in::BasicHypergraph{T,D},
    hg_out::BasicHypergraph{T,D};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V,D<:AbstractDict{Int, T}}

    DirectedHypergraph{T,V,Nothing,D}(
        hg_in,
        hg_out;
        v_meta=v_meta
    )
end

function DirectedHypergraph{T,E,D}(
    hg_in::BasicHypergraph{T,D},
    hg_out::BasicHypergraph{T,D};
    he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E,D<:AbstractDict{Int, T}}

    DirectedHypergraph{T,Nothing,E,D}(
        hg_in,
        hg_out;
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )
end


function DirectedHypergraph{T,V,E,D}(
    hg_in::Hypergraph{T,V,E,D},
    hg_out::Hypergraph{T,V,E,D}
) where {T<:Real,V,E,D<:AbstractDict{Int, T}}
    @assert size(hg_in) == size(hg_out)

    n, k = size(hg_in)
    sgh_in = BasicHypergraph(n, k)
    sgh_out = BasicHypergraph(n, k)

    # TODO: test behavior on this
    sgh_in .= hg_in
    sgh_out .= hg_out

    if all(hg_in.v_meta .== hg_out.v_meta)
        DirectedHypergraph{T,V,E,D}(shg_in, shg_out; hg_in.v_meta, hg_in.he_meta, hg_out.he_meta)
    else
        throw(ArgumentError("Vertex metadata `v_meta` is different for ingoing and outgoing hypergraphs!"))
    end
end


function DirectedHypergraph{T,V,E,D}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,D}(n, k)
    hg_in .= m_in
    
    hg_out = BasicHypergraph{T,D}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,V,E,D}(
        hg_in, hg_out;
        v_meta=v_meta,
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )
end

function DirectedHypergraph{T,V,E}(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
) where {T<:Real,V,E}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,V,E,Dict{Int,T}}(
        hg_in, hg_out;
        v_meta=v_meta,
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )
end

function DirectedHypergraph{T,V}(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
) where {T<:Real,V}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        hg_in,
        hg_out;
        v_meta=v_meta
    )
end

function DirectedHypergraph{T,E}(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}};
    he_meta_in::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_out::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
) where {T<:Real,V}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        hg_in,
        hg_out;
        he_meta_in=he_meta_in,
        he_meta_out=he_meta_out
    )
end

function DirectedHypergraph{T}(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(hg_in, hg_out)
end

function DirectedHypergraph(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(
        hg_in,
        hg_out
    )
end


function DirectedHypergraph(g::Graphs.DiGraph)
    h = DirectedHypergraph{Bool,Nothing,Nothing,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[1,edge.src,e] = true
        h[2,edge.dst,e] = true
    end
    h
end


"""
    BasicDirectedHypergraph{T} <: AbstractDirectedHypergraph{T}

A directed hypergraph storing vertices and hyperedges, with no additional
information other than weight.

This implementation is based on guidance from Przemysław Szufel;
    see https://github.com/pszufe/SimpleHypergraphs.jl/issues/45
This allows us to manipulate DirectedHypergraphs using Hypergraph functionality
There is danger of a user manipulating individual `hg_in` and `hg_out` (undirected) hypergraphs
Is there a smart way to prevent this?
TODO: reconsider this design choice

**Constructors**

    BasicDirectedHypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    BasicDirectedHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    BasicDirectedHypergraph(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    BasicDirectedHypergraph{T}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    BasicDirectedHypergraph{T,D}(
        m_in::AbstractMatrix{Union{T, Nothing}},
        m_out::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real,D<:AbstractDict{Int,T}}

Construct a directed hypergraph using its matrix representation.
In the matrix representation rows are vertices and columns are hyperedges.
By default the hypergraph uses a `Dict{Int,T}` for the internal data storage,
however a different dictionary such as `SortedDict` to ensure result
replicability can be used (e.g. when doing stochastic simulations on hypergraphs).

    DirectedHypergraph(g::Graphs.DiGraph)

Constructs a directed hypergraph of degree 2 by making a deep copy of a
Graphs.DiGraph. A `SortedDict` will be used for internal data storage of the
hypergraph.

    DirectedHypergraph{T,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D}
    ) where {T<:Real,D<:AbstractDict{Int, T}}

Constructs a directed hypergraph from two undirected basic hypergraphs, one with hyperedges
containing "incoming" vertices and one with hyperedges containing "outgoing"
verticies.

**Arguments**

* `T` : type of weight values stored in the hypergraph's adjacency matrix
* `V` : type of values stored in the vertices of the hypergraph
* `E` : type of values stored in the edges of the hypergraph
* `D` : dictionary for storing values the default is `Dict{Int, T}`
* `n` : number of vertices
* `k` : number of hyperedges
* `m` : a matrix representation rows are vertices and columns are hyperedges
* `g` : a (directed) graph representation of the hypergraph
* `hg_in`: an undirected hypergraph representing the incoming half of
    the directed hypergraph
* `hg_out`: an undirected hypergraph representing the outgoing half of
    the directed hypergraph
"""
struct BasicDirectedHypergraph{T<:Real,D<:AbstractDict{Int, T}} <: AbstractDirectedHypergraph{T}
    hg_in::BasicHypergraph{T,D}
    hg_out::BasicHypergraph{T,D}

    BasicDirectedHypergraph{T,D}(
        n::Integer, k::Integer,
        ) where {T<:Real,D<:AbstractDict{Int, T}} =
        new{T,D}(
            BasicHypergraph(n, k),
            BasicHypergraph(n, k)
        )

    function BasicDirectedHypergraph{T,D}(
        hg_in::BasicHypergraph{T,D},
        hg_out::BasicHypergraph{T,D}
        ) where {T<:Real,D<:AbstractDict{Int, T}}
        
        @assert size(hg_in) == size(hg_out)

        new{T,D}(hg_in, hg_out)
    end
end


BasicDirectedHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = BasicDirectedHypergraph{T,Dict{Int,T}}(n, k)

BasicDirectedHypergraph(n::Integer, k::Integer) = BasicDirectedHypergraph{Bool,Dict{Int,Bool}}(n, k)


function BasicDirectedHypergraph{T}(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    BasicDirectedHypergraph{T,Dict{Int,T}}(hg_in, hg_out)
end

function BasicDirectedHypergraph(
    m_in::AbstractMatrix{Union{T, Nothing}},
    m_out::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_in)

    hg_in = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_in .= m_in

    hg_out = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_out .= m_out

    BasicDirectedHypergraph{T,Dict{Int,T}}(
        hg_in,
        hg_out
    )
end


function BasicDirectedHypergraph(g::Graphs.DiGraph)
    h = BasicDirectedHypergraph{Bool,SortedDict{Int,Bool}}(maximum(vertices(g)), ne(g))
    e = 0
    for edge in edges(g)
        e+=1
        h[1,edge.src,e] = true
        h[2,edge.dst,e] = true
    end
    h
end


# build unions of types for easier dispatch
const HasMetaHGs = Union{Hypergraph, DirectedHypergraph}
const NoMetaHGs = Union{BasicHypergraph, BasicDirectedHypergraph}

# implementing traits on types
@traitimpl HasMeta{HasMetaStructs}
hasmeta(::Type{T}) where {T<:HasMetaStructs} = true

# Do I need these, or can I just use the abstract types and assume that all implementations will follow a similar structure
const ConcreteUndirectedHGs = Union{Hypergraph, BasicHypergraph}
const ConcreteDirectedHGs = Union{DirectedHypergraph, BasicDirectedHypergraph}

# TODO: god this is awkward...
const DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES = [1,2]

# TODO: can this entirely replace the above? Index setting seems problematic...
@enum HyperedgeDirection begin
    incoming = 1
    outgoing = 2
end


# AbstractArray interface functions

"""
    Base.size(h::AbstractHypergraph)

Returns the size of hypergraph `h`.
The result is a tuple of the number of vertices and the number of hyperedges

"""
Base.size(h::AbstractHypergraph) = (nhv(h), nhe(h))


"""
    Base.getindex(h::Union{Hypergraph, BasicHypergraph}, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair `idx` for an undirected hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::ConcreteUndirectedHGs, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

"""
    Base.getindex(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair `idx` for a directed hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::ConcreteDirectedHGs, idx::Vararg{Int,2})
    @boundscheck checkbounds(h.hg_in, idx...)
    @boundscheck checkbounds(h.hg_out, idx...)

    in_value = get(h.hg_in.v2he[idx[1]], idx[2], nothing)
    out_value = get(h.hg_out.v2he[idx[1]], idx[2], nothing)

    (in_value, out_value)
end

#TODO: getindex implementation with idx::Vararg{Int, 2}?


"""
    Base.setindex!(h::Union{Hypergraph, BasicHypergraph}, ::Nothing, idx::Vararg{Int,2})

Removes a vertex from a given hyperedge for an undirected hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::ConcreteUndirectedHGs, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2], nothing)
    pop!(h.he2v[idx[2]], idx[1], nothing)
    h
end


"""
    Base.setindex!(h::Union{Hypergraph, BasicHypergraph}, v::Real, idx::Vararg{Int,2})

Adds a vertex to an undirected hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::ConcreteUndirectedHGs, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h, idx...)
    h.v2he[idx[1]][idx[2]] = v
    h.he2v[idx[2]][idx[1]] = v
    h
end


"""
    Base.setindex!(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, ::Nothing, idx::Vararg{Int,2})

Removes a vertex from a given hyperedge for a directed hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::ConcreteDirectedHGs, ::Nothing, idx::Vararg{Int,2})
    @boundscheck checkbounds(h.hg_in, idx...)
    @boundscheck checkbounds(h.hg_out, idx...)
    setindex!(h.hg_in, nothing, idx)
    setindex!(h.hg_out, nothing, idx)
    h
end


"""
    Base.setindex!(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, v::Real, idx::Vararg{Int,2})

Adds a vertex to a hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::ConcreteDirectedHGs, v::Real, idx::Vararg{Int,2})
    @boundscheck checkbounds(h.hg_in, idx...)
    @boundscheck checkbounds(h.hg_out, idx...)

    setindex!(h.hg_in, v, idx)
    setindex!(h.hg_out, v, idx)
    h
end


"""
    Base.setindex!(h::ConcreteDirectedHGs, v::Tuple{Union{Real, Nothing}, Union{Real, Nothing}}, idx::Vararg{Int,2})

Manipulates a hyperedge (represented by indices `idx`), either adding a vertex to the 
ingoing and/or outgoing sides of the hyperedge and assigning a value associated with that assignment,
or else removing a vertex from the ingoing/outgoing sides of the hyperedge.

Here, `v` is a 2-tuple where the first element is the value that will be assigned to the ingoing part of the hyperedge
and the second element is the value that will be assigned to the outgoing part. A value of `nothing` means that the
vertex will be removed from that side of the hyperedge.

"""
@inline function Base.setindex!(h::ConcreteDirectedHGs, v::Tuple{Union{Real, Nothing}, Union{Real, Nothing}}, idx::Vararg{Int,2})
    @boundscheck checkbounds(h.hg_in, idx...)
    @boundscheck checkbounds(h.hg_out, idx...)
    
    setindex!(h.hg_in, v[1], idx)
    setindex!(h.hg_out, v[2], idx)

    h
end


"""
    Base.setindex!(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, ::Nothing, idx::Vararg{Int,3})

Removes a vertex from a given hyperedge for a directed hypergraph `h` and a given side-vertex-hyperedge pair `idx`.
If the first index of `idx` is 1, then the vertex will be removed from the incoming hyperedge; if `idx` is 2, then
the vertex will be removed from the outgoing hyperedge. 
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::ConcreteDirectedHGs, ::Nothing, idx::Vararg{Int,3})
    @boundscheck checkbounds(DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES, idx[1])

    if idx[1] == 1
        side = h.hg_in
    else
        side = h.hg_out
    end
    
    @boundscheck checkbounds(side, idx[2:end]...)
    
    setindex!(side, nothing, idx[2:end])

    h
end


"""
    Base.setindex!(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, v::Real, idx::Vararg{Int,3})

Adds a vertex to a hyperedge (represented by indices `idx`, where the first index must be either
1 - referring to an incoming hyperedge - or 2 - referring to an outgoing hyperedge) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::ConcreteDirectedHGs, v::Real, idx::Vararg{Int,3})
    @boundscheck checkbounds(DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES, idx[1])

    if idx[1] == 1
        side = h.hg_in
    else
        side = h.hg_out
    end
    
    @boundscheck checkbounds(side, idx[2:end]...)
    
    setindex!(side, v, idx[2:end])

    h
end


"""
    getvertices(h::Union{Hypergraph, BasicHypergraph}, he_id::Int)

Returns vertices from an undirected hypergraph `a` for a given hyperedge `he_id`.

"""
@inline getvertices(h::ConcreteUndirectedHGs, he_id::Int) = h.he2v[he_id]


"""
    getvertices(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, he_id::Int)

Returns vertices from a directed hypergraph `a` for a given hyperedge `he_id`.
Vertex indices are given in a tuple `(in, out)`, where `in` are the incoming vertices
and `out` are the outgoing vertices

"""
@inline getvertices(h::ConcreteDirectedHGs, he_id::Int) = (h.hg_in.he2v[he_id], h.hg_out.he2v[he_id])


"""
    gethyperedges(h::Union{Hypergraph, BasicHypergraph}, v_id::Int)

Returns hyperedges for a given vertex `v_id` in an undirected hypergraph `h`.

"""
@inline gethyperedges(h::ConcreteUndirectedHGs, v_id::Int) = h.v2he[v_id]


"""
    gethyperedges(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, v_id::Int)

Returns hyperedges for a given vertex `v_id` in a directed hypergraph `h`.
Hyperedge indices are given in a tuple `(in, out)`, where `in` are the hyperedges where
vertex `v_ind` is on the ingoing side and `out` are the hyperedges where `v_ind` is on
the outgoing side.

"""
@inline gethyperedges(h::ConcreteDirectedHGs, v_id::Int) = (h.hg_in.v2he[v_id], h.hg_out.v2he[v_id])



#TODO: should there be a way to add hyperedge metadata as well as vertex metadata?
"""
    add_vertex!(h::Hypergraph{T, V, E, D};
                hyperedges::D = D(), v_meta::Union{V,Nothing} = nothing
                ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a vertex to a given undirected hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.
Additionally, a value can be stored with the vertex using the `v_meta` keyword
parameter.

"""
function add_vertex!(h::Hypergraph{T, V, E, D};
                     hyperedges::D = D(), v_meta::Union{V,Nothing} = nothing
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,1,k) for k in keys(hyperedges))
    push!(h.v2he,hyperedges)
    ix = length(h.v2he)
    for k in keys(hyperedges)
        h[ix,k]=hyperedges[k]
    end
    push!(h.v_meta, v_meta)
    ix
end

"""
    add_vertex!(h::BasicHypergraph{T, D};
                hyperedges::D = D()
                ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a vertex to a given undirected hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges` parameter presents a dictionary
of hyperedge identifiers and values stored at the hyperedges.

"""
function add_vertex!(h::BasicHypergraph{T, D};
                     hyperedges::D = D()
                    ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,1,k) for k in keys(hyperedges))
    push!(h.v2he,hyperedges)
    ix = length(h.v2he)
    for k in keys(hyperedges)
        h[ix,k]=hyperedges[k]
    end
    ix
end


"""
    add_vertex!(h::DirectedHypergraph{T, V, E, D};
                hyperedges_in::D = D(), hyperedges_out::D = D(), v_meta::Union{V,Nothing} = nothing
                ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a vertex to a given directed hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges_in` parameter presents a dictionary
of hyperedge identifiers and values stored at the ingoing side of hyperedges, and
the `hyperedges_out` parameter presents a dictionary of hyperedge identifiers and
values stored at the outgoing side of hyperedges.
Additionally, a value can be stored with the vertex using the `v_meta` keyword
parameter.

"""
function add_vertex!(h::DirectedHypergraph{T, V, E, D};
                     hyperedges_in::D = D(), hyperedges_out::D = D(), v_meta::Union{V,Nothing} = nothing
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_in,1,k) for k in keys(hyperedges_in))
    @boundscheck (checkbounds(h.hg_out,1,k) for k in keys(hyperedges_out))

    push!(h.hg_in.v2he,hyperedges_in)
    push!(h.hg_out.v2he,hyperedges_out)

    # Should always be identical to h.hg_out.v2he
    ix = length(h.hg_in.v2he)

    for k in keys(hyperedges_in)
        h[1,ix,k]=hyperedges_in[k]
    end

    for k in keys(hyperedges_out)
        h[2,ix,k]=hyperedges_out[k]
    end

    push!(h.v_meta, v_meta)
    ix
end

"""
    add_vertex!(h::BasicDirectedHypergraph{T, D};
                hyperedges_in::D = D(), hyperedges_out::D = D()
                ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a vertex to a given directed hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges_in` parameter presents a dictionary
of hyperedge identifiers and values stored at the ingoing side of hyperedges, and
the `hyperedges_out` parameter presents a dictionary of hyperedge identifiers and
values stored at the outgoing side of hyperedges.

"""
function add_vertex!(h::BasicDirectedHypergraph{T, D};
                     hyperedges::D = D()
                    ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_in,1,k) for k in keys(hyperedges_in))
    @boundscheck (checkbounds(h.hg_out,1,k) for k in keys(hyperedges_out))

    push!(h.hg_in.v2he,hyperedges_in)
    push!(h.hg_out.v2he,hyperedges_out)
    
    ix = length(h.hg_in.v2he)

    for k in keys(hyperedges_in)
        h[1,ix,k]=hyperedges_in[k]
    end

    for k in keys(hyperedges_out)
        h[2,ix,k]=hyperedges_out[k]
    end

    ix
end


"""
    remove_vertex!(h::Hypergraph, v::Int)

Removes the vertex `v` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
"""

function remove_vertex!(h::Hypergraph, v::Int)
    n = nhv(h)
    if v < n
        h.v2he[v] = h.v2he[n]
        h.v_meta[v] = h.v_meta[n]
    end

    for hv in h.he2v
        if v < n && haskey(hv, n)
            hv[v] = hv[n]
            delete!(hv, n)
        else
            delete!(hv, v)
        end
    end
    resize!(h.v2he, length(h.v2he) - 1)
    resize!(h.v_meta, length(h.v_meta) - 1)
    h
end

"""
    remove_vertex!(h::BasicHypergraph, v::Int)

Removes the vertex `v` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
"""
function remove_vertex!(h::BasicHypergraph, v::Int)
    n = nhv(h)
    if v < n
        h.v2he[v] = h.v2he[n]
    end

    for hv in h.he2v
        if v < n && haskey(hv, n)
            hv[v] = hv[n]
            delete!(hv, n)
        else
            delete!(hv, v)
        end
    end
    resize!(h.v2he, length(h.v2he) - 1)
    h
end


"""
    remove_vertex!(h::DirectedHypergraph, v::Int)

Removes the vertex `v` from a given directed hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
"""
function remove_vertex!(h::DirectedHypergraph, v::Int)
    n = nhv(h)
    if v < n
        h.v_meta[v] = h.v_meta[n]
    end
    
    remove_vertex!(h.hg_in, v)
    remove_vertex!(h.hg_out, v)

    resize!(h.v_meta, length(h.v_meta) - 1)

    h
end


"""
    remove_vertex!(h::BasicDirectedHypergraph, v::Int)

Removes the vertex `v` from a given directed hypergraph `h`.
Note that running this function will cause reordering of vertices in the
hypergraph; the vertex `v` will replaced by the last vertex of the hypergraph
and the list of vertices will be shrunk.
"""
function remove_vertex!(h::BasicDirectedHypergraph, v::Int)
    remove_vertex!(h.hg_in, v)
    remove_vertex!(h.hg_out, v)

    h
end


"""
    add_hyperedge!(h::Hypergraph{T, V, E, D};
                   vertices::D = D(), he_meta::Union{E,Nothing}=nothing
                   ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given undirected hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge.
The paramater `vertices` represents a dictionary of vertex identifiers and
values stored at the hyperedges. Additionally, a value can be stored with the
hyperedge using the `he_meta` keyword parameter.

"""
function add_hyperedge!(h::Hypergraph{T, V, E, D};
                        vertices::D = D(), he_meta::Union{E,Nothing}=nothing
                        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,k,1) for k in keys(vertices))
    push!(h.he2v,vertices)
    ix = length(h.he2v)
    for k in keys(vertices)
        h[k,ix]=vertices[k]
    end
    push!(h.he_meta, he_meta)
    ix
end


"""
    add_hyperedge!(h::BasicHypergraph{T, D};
                   vertices::D = D()
                   ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given undirected hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge.
The paramater `vertices` represents a dictionary of vertex identifiers and
values stored at the hyperedges.

"""
function add_hyperedge!(h::BasicHypergraph{T, D};
                        vertices::D = D()
                        ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h,k,1) for k in keys(vertices))
    push!(h.he2v,vertices)
    ix = length(h.he2v)
    for k in keys(vertices)
        h[k,ix]=vertices[k]
    end

    ix
end


"""
    add_hyperedge!(h::DirectedHypergraph{T, V, E, D};
                   vertices_in::D = D(), vertices_out::D = D(),
                   he_meta_in::Union{E,Nothing}=nothing, he_meta_out::Union{E,Nothing}=nothing
                   ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given directed hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge in the
incoming or outgoing directions.
The paramater `vertices_in` represents a dictionary of vertex identifiers and
values stored at the incoming hyperedge; `vertices_out` represented the vertex
identifiers and values stored at the outcoming side of the hyperedge. Additionally, 
a value can be stored with the hyperedge using the `he_meta_in` and `he_meta_out`
keyword parameters.

"""
function add_hyperedge!(h::DirectedHypergraph{T, V, E, D};
                        vertices_in::D = D(), vertices_out::D = D(),
                        he_meta_in::Union{E,Nothing}=nothing, he_meta_out::Union{E,Nothing}=nothing
                        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_in,k,1) for k in keys(vertices_in))
    @boundscheck (checkbounds(h.hg_out,1,k) for k in keys(vertices_out))
    
    push!(h.hg_in.he2v,vertices_in)
    push!(h.hg_out.he2v, vertices_out)

    # Should always be identical to length(h.hg_out.he2v)
    ix = length(h.hg_in.he2v)
    for k in keys(vertices_in)
        h[1,k,ix]=vertices_in[k]
    end
    for k in keys(vertices_out)
        h[2,k,ix]=vertices_out[k]
    end
    push!(h.he_meta_in, he_meta_in)
    push!(h.he_meta_out, he_meta_out)
    ix
end


"""
    add_hyperedge!(h::BasicDirectedHypergraph{T, D};
                   vertices::D = D()
                   ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given directed hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge in the
incoming or outgoing directions.
The paramater `vertices_in` represents a dictionary of vertex identifiers and
values stored at the incoming hyperedge; `vertices_out` represented the vertex
identifiers and values stored at the outcoming side of the hyperedge.

"""
function add_hyperedge!(h::BasicDirectedHypergraph{T, D};
                        vertices_in::D = D(), vertices_out::D = D()
                        ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_in,k,1) for k in keys(vertices_in))
    @boundscheck (checkbounds(h.hg_out,1,k) for k in keys(vertices_out))
    
    push!(h.hg_in.he2v,vertices_in)
    push!(h.hg_out.he2v, vertices_out)

    # Should always be identical to length(h.hg_out.he2v)
    ix = length(h.hg_in.he2v)
    for k in keys(vertices_in)
        h[1,k,ix]=vertices_in[k]
    end
    for k in keys(vertices_out)
        h[2,k,ix]=vertices_out[k]
    end

    ix
end


"""
    remove_hyperedge!(h::Hypergraph, e::Int)
Removes the hyperedge `e` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges (and hyperedge metadata) will be shrunk.
"""
function remove_hyperedge!(h::Hypergraph, e::Int)
    ne = nhe(h)
	@assert(e <= ne)
	if e < ne
	    h.he2v[e] = h.he2v[ne]
	    h.he_meta[e] = h.he_meta[ne]
	end

    for he in h.v2he
	    if e < ne && haskey(he, ne)
		    he[e] = he[ne]
            delete!(he, ne)
		else
			delete!(he, e)
		end
    end
    resize!(h.he2v, length(h.he2v) - 1)
    resize!(h.he_meta, length(h.he_meta) - 1)
    h
end


"""
    remove_hyperedge!(h::BasicHypergraph, e::Int)
Removes the hyperedge `e` from a given undirected hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges will be shrunk.
"""
function remove_hyperedge!(h::BasicHypergraph, e::Int)
    ne = nhe(h)
	@assert(e <= ne)
	if e < ne
	    h.he2v[e] = h.he2v[ne]
	end

    for he in h.v2he
	    if e < ne && haskey(he, ne)
		    he[e] = he[ne]
            delete!(he, ne)
		else
			delete!(he, e)
		end
    end
    resize!(h.he2v, length(h.he2v) - 1)
    h
end


"""
    remove_hyperedge!(h::DirectedHypergraph, e::Int)
Removes the hyperedge `e` from a given directed hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges (and hyperedge metadata) will be shrunk.
"""
function remove_hyperedge!(h::DirectedHypergraph, e::Int)
    ne = nhe(h)
	@assert(e <= ne)
	if e < ne
	    h.he_meta_in[e] = h.he_meta_in[ne]
        h.he_meta_out[e] = h.he_meta_out[ne]
	end

    remove_hyperedge!(h.hg_in, e)
    remove_hyperedge!(h.hg_out, e)

    resize!(h.he_meta_in, length(h.he_meta_in) - 1)
    resize!(h.he_meta_out, length(h.he_meta_out) - 1)

    h
end


"""
    remove_hyperedge!(h::BasicDirectedHypergraph, e::Int)
Removes the hyperedge `e` from a given directed hypergraph `h`.
Note that running this function will cause reordering of hyperedges in the
hypergraph: the hyperedge `e` will replaced by the last hyperedge of the hypergraph
and the list of hyperedges (and hyperedge metadata) will be shrunk.
"""
function remove_hyperedge!(h::BasicDirectedHypergraph, e::Int)
    remove_hyperedge!(h.hg_in, e)
    remove_hyperedge!(h.hg_out, e)

    h
end


"""
    prune_hypergraph!(h::Union{Hypergraph, BasicHypergraph})

Remove all vertices with degree 0 and all hyperedges of size 0.

"""
function prune_hypergraph!(h::ConcreteUndirectedHGs)
	for e in reverse(1:nhe(h))
        length(h.he2v[e]) == 0 && remove_hyperedge!(h,e)
    end
	for v in reverse(1:nhv(h))
    	length(h.v2he[v]) == 0 && remove_vertex!(h,v)
    end
	h
end


"""
    prune_hypergraph!(h::Union{DirectedHypergraph, BasicDirectedHypergraph})

Remove all vertices with degree 0 and all hyperedges of size 0.

"""
function prune_hypergraph!(h::ConcreteDirectedHGs)
	for e in reverse(1:nhe(h))
        length(h.hg_in.he2v[e]) == 0 && length(h.hg_out.he2v[e]) && remove_hyperedge!(h,e)
    end
	for v in reverse(1:nhv(h))
    	length(h.hg_in.v2he[v]) == 0 && length(h.hg_in.v2he[v]) == 0 && remove_vertex!(h,v)
    end
	h
end


"""
    prune_hypergraph(h::H) where {H <: AbstractHypergraph}

Return a pruned copy of `h`, removing all vertices with degree 0 and
all hyperedges of size 0.

"""
function prune_hypergraph(h::H) where {H <: AbstractHypergraph}
    prune_hypergraph!(deepcopy(h))
end


"""
    set_vertex_meta!(h::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, new_value::Union{V,Nothing},
        id::Int) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the vertex `id` in the hypergraph `h`.

"""
function set_vertex_meta!(h::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}},
        new_value::Union{V,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.v_meta, id)
    h.v_meta[id] = new_value
    h.v_meta
end


set_vertex_meta!(::NoMetaHGs, ::Any, ::Int) = throw("Not implemented!")


"""
    get_vertex_meta(h::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, id::Int
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Returns a meta value stored at the vertex `id` in the undirected hypergraph `h`.

"""
function get_vertex_meta(h::Union{Hypergraph{T, V, E, D}, DirectedHypergraph{T, V, E, D}}, id::Int
                         ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.v_meta, id)
    h.v_meta[id]
end


get_vertex_meta(::NoMetaHGs, ::Int) = throw("Not implemented!")


"""
    set_hyperedge_meta!(h::Hypergraph{T, V, E, D},
        new_value::Union{E,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the hyperedge `id` in the undirected hypergraph `h`.

"""
function set_hyperedge_meta!(h::Hypergraph{T, V, E, D},
                            new_value::Union{E,Nothing}, id::Int
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta, id)
    h.he_meta[id] = new_value
    h.he_meta
end


"""
    set_hyperedge_meta!(h::DirectedHypergraph{T, V, E, D},
        new_value_in::Union{E,Nothing}, new_value_out::Union{E,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the hyperedge `id` in the directed hypergraph `h`.

"""
function set_hyperedge_meta!(h::DirectedHypergraph{T, V, E, D},
                             new_value_in::Union{E,Nothing}, new_value_out::Union{E,Nothing}, id::Int
                             ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta_in, id)
    checkbounds(h.he_meta_out, id)

    h.he_meta_in[id] = new_value_in
    h.he_meta_out[id] = new_value_out

    (h.he_meta_in, h.he_meta_out)
end


"""
    set_hyperedge_meta!(h::DirectedHypergraph{T, V, E, D},
        new_value::Union{E,Nothing}, id::Int, side::HyperedgeDirection
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the hyperedge `id` in the direction `side`
    for a directed hypergraph `h`.

"""
function set_hyperedge_meta!(h::DirectedHypergraph{T, V, E, D},
                             new_value::Union{E,Nothing}, id::Int, side::HyperedgeDirection
                             ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    
    if side == incoming
        checkbounds(h.he_meta_in, id)
        h.he_meta_in[id] = new_value
        h.he_meta_in
    else
        checkbounds(h.he_meta_out, id)
        h.he_meta_out[id] = new_value
        h.he_meta_out

end


set_hyperedge_meta!(::BasicHypergraph, ::Any, ::Int) = throw("Not implemented!")
set_hyperedge_meta!(::BasicDirectedHypergraph, ::Any, ::Any, ::Int) = throw("Not implemented!")
set_hyperedge_meta!(::BasicDirectedHypergraph, ::Any, ::Int, ::HyperedgeDirection) = throw("Not implemented!")


"""
    get_hyperedge_meta(h::Hypergraph{T, V, E, D}, id::Int)
        where {T <: Real, V, E, D <: AbstractDict{Int,T}}
Returns a meta value stored at the hyperedge `id` in the undirected hypergraph `h`.

"""
function get_hyperedge_meta(h::Hypergraph{T, V, E, D}, id::Int
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta, id)
    h.he_meta[id]
end


"""
    get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int)
        where {T <: Real, V, E, D <: AbstractDict{Int,T}}
Returns a meta value stored at the hyperedge `id` in the directed hypergraph `h`.

"""
function get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta_in, id)
    checkbounds(h.he_meta_out, id)

    (h.he_meta_in[id], h.he_meta_out[id])
end


"""
    get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int, side::HyperedgeDirection)
        where {T <: Real, V, E, D <: AbstractDict{Int,T}}
Returns a meta value stored at the hyperedge `id` in the directed hypergraph `h`.

"""
function get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int, side::HyperedgeDirection
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

    if side == incoming
        checkbounds(h.he_meta_in, id)
        h.he_meta_in[id]
    else
        checkbounds(h.he_meta_out, id)
        h.he_meta_out[id]
end

get_hyperedge_meta(::BasicHypergraph, ::Int) = throw("Not implemented!")
get_hyperedge_meta(::BasicDirectedHypergraph, ::Int) = throw("Not implemented!")
get_hyperedge_meta(::BasicDirectedHypergraph, ::Int, ::HyperedgeDirection) = throw("Not implemented!")


# TODO: you are here
"""
    nhe(h::Hypergraph)

Return the number of hyperedges in the hypergraph `h`.
"""
function nhe(h::Hypergraph)
    length(h.he2v)
end


"""
    nhv(h::Hypergraph)

Return the number of vertices in the hypergraph `h`.
"""
function nhv(h::Hypergraph)
    length(h.v2he)
end


function _default_heselect(h::Hypergraph, v::Int)
    hes = gethyperedges(h, v)
    sort!(collect(keys(hes))), ones(length(hes))
end


function _default_vselect(h::Hypergraph, he::Int)
    vs = getvertices(h, he)
    sort!(collect(keys(vs))), ones(length(vs))
end


"""
    random_walk(h::Hypergraph, start::Int; heselect::Function, vselect::Function)

Return a next vertex visited in assuming a random walk starting from vertex `start`.
First a hyperedge is sampled with weights proportional to `heselect` function
(by default each hyperedge is sampled with the same probability).
Next a vertex within hyperedge is with weights proportional to `vselect` function
(by default each vertex, including the source, is sampled with the same probability).

`heselect` and `vselect` functions take two arguments a `Hypergraph` and respectively
a vertex identifier or a hyperedge identifier. The return values of both functions
should be respectively a list of hyperedges or vertices and their weights.
"""
function random_walk(h::Hypergraph, start::Int;
                     heselect::Function=_default_heselect,
                     vselect::Function=_default_vselect)
    1 <= start <= nhv(h) || throw(ArgumentError("invalid start vertex index"))
    hes, hew = heselect(h, start)
    he = sample(hes, Weights(hew))
    ves, vw = vselect(h, he)
    return sample(ves, Weights(vw))
end


"""
    _walk!(h::Hypergraph, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool})

Appends the list of neighbors `s` of a given vertex `i` (an auxiliary function for `get_connected_components`).
"""
function _walk!(h::Hypergraph, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool})
    visited[i] && return
    visited[i] = true
    push!(s, i)
    for he in keys(gethyperedges(h, i))
        for j in keys(getvertices(h, he))
            _walk!(h, s, j, visited)
        end
    end
end


"""
    get_connected_components(h::Hypergraph)

Return an array of connected components in the hypergraph `h`
(array of vectors of vertices) using recurrence.
"""
function get_connected_components(h::Hypergraph)
    visited = falses(nhv(h))
    cc = Vector{Int}[]
        for i in 1:nhv(h)
            if !visited[i]
                s = Int[]
                _walk!(h, s, i, visited)
                push!(cc, s)
        end
    end
    cc
end


"""
    adjacency_matrix(h::Hypergraph; s::Int=1, weighted::Bool=false)

The sparse weighted `s`-adjacency matrix.

NOTE
The concept of `s`-adjacency matrix has been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/classes/classes.html#classes.hypergraph.Hypergraph.adjacency_matrix)
If weighted is `true` each off diagonal cell will equal the number
of edges shared by the nodes indexing the row and column if that number is
greater than `s`, otherwise the cell will equal 0. If weighted is `false`,
the off diagonal cell will equal 1 if the nodes indexed by the row and column
share at least `s` edges and 0 otherwise.

! information about the weight of a vertex in a he will be lost.

"""
function adjacency_matrix(h; s::Int=1, weighted::Bool=true)
    M = Matrix(h)
    _incidence_to_adjacency(M; s=s, weighted=weighted)
end


"""
    edge_adjacency_matrix(h::Hypergraph; s::Int=1, weighted::Bool=false)

The sparse weighted `s`-adjacency matrix for the dual hypergraph.

NOTE
The concept of `s`-adjacency matrix has been firstly defined in the
Python library [HyperNetX](https://github.com/pnnl/HyperNetX)

From [HyperNetX](https://pnnl.github.io/HyperNetX/build/classes/classes.html#classes.hypergraph.Hypergraph.edge_adjacency_matrix)
This is also the adjacency matrix for the line graph.
Two edges are `s`-adjacent if they share at least `s` nodes.

If weighted is `true` each off diagonal cell will equal the number
of nodes shared by the hyperedges indexing the row and column if that number is
greater than `s`, otherwise the cell will equal 0. If weighted is `false`,
the off diagonal cell will equal 1 if the hyperedges indexed by the row and column
share at least `s` nodes and 0 otherwise.

"""
function edge_adjacency_matrix(h; s::Int=1, weighted::Bool=true)
    M = Matrix(h)
	M[M .== nothing] .= 0
    _incidence_to_adjacency(transpose(M); s=s, weighted=weighted)
end


"""
    _incidence_to_adjacency(M; s::Int=1, weighted::Bool=true)

Helper method to obtain adjacency matrix from incidence matrix.

"""
function _incidence_to_adjacency(M; s::Int=1, weighted::Bool=true)
    M[M .== nothing] .= 0
    M[M .> 0] .= 1

    A = *(M, transpose(M))
    A[diagind(A)] .= 0

    if s > 1
        A = A .* (A .>= s)
    end
    if !weighted
        A = (A .> 0) .* 1
    end

    A
end


# TODO find connected components without recurrence
# TODO needs validate_hypergraph!(h::Hypergraph{T})
