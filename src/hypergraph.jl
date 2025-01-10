# TODO: think more carefully about ensuring that metadata vectors are of appropriate lengths
# TODO: use haskey instead of x in keys(...)

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
There is danger of a user manipulating individual `hg_tail` and `hg_head` (undirected) hypergraphs
Is there a smart way to prevent this?
TODO: reconsider this design choice

**Constructors**

    DirectedHypergraph{T}(n::Integer,k::Integer) where {T<:Real}
    DirectedHypergraph{T,V}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n)
        ) where {T<:Real, V}
    DirectedHypergraph{T,E}(n::Integer, k::Integer;
        he_meta_tail=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_head=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, E}
    DirectedHypergraph{T,V,E}(n::Integer, k::Integer;
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta_tail=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_head=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real, V, E}
    DirectedHypergraph{T,V,E,D}(n::Integer, k::Integer,
        v_meta=Vector{Union{V,Nothing}}(nothing, n),
        he_meta_tail=Vector{Union{E,Nothing}}(nothing, k),
        he_meta_head=Vector{Union{E,Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

Construct a hypergraph with a given number of vertices and hyperedges.
Optionally, values of type `V` can be stored at vertices and values of type `E`
can be stored at hyperedges. By default the hypergraph uses a `Dict{Int,T}` for
the internal data storage, however a different dictionary such as `SortedDict`
to ensure result replicability can be used (e.g. when doing stochastic
simulations on hypergraphs).

    DirectedHypergraph(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}    
    DirectedHypergraph{T}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    DirectedHypergraph{T,V}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V}
    DirectedHypergraph{T,E}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}};
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E}
    DirectedHypergraph{T,V,E}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E}
    DirectedHypergraph{T,V,E,D}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
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
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V,D<:AbstractDict{Int, T}}
    DirectedHypergraph{T,E,D}(
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D};
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,E,D<:AbstractDict{Int, T}}
    DirectedHypergraph{T,V,E,D}(
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E,D<:AbstractDict{Int, T}}

Constructs a directed hypergraph from two undirected basic hypergraphs, one with hyperedges
containing "tail" vertices and one with hyperedges containing "head"
verticies.

    DirectedHypergraph{T,V,E,D}(
        hg_tail::Hypergraph{T,V,E,D},
        hg_head::Hypergraph{T,V,E,D}
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
* `hg_tail`: an undirected hypergraph representing the tail half of
    the directed hypergraph
* `hg_head`: an undirected hypergraph representing the head half of
    the directed hypergraph
"""

struct DirectedHypergraph{T<:Real,V,E,D<:AbstractDict{Int, T}} <: AbstractDirectedHypergraph{Tuple{T, T}}
    hg_tail::BasicHypergraph{T,D}
    hg_head::BasicHypergraph{T,D}

    v_meta::Vector{Union{V,Nothing}}
    he_meta_tail::Vector{Union{E,Nothing}}
    he_meta_head::Vector{Union{E,Nothing}}

    DirectedHypergraph{T,V,E,D}(
        n::Integer, k::Integer,
        v_meta=Vector{Union{V, Nothing}}(nothing, n),
        he_meta_tail=Vector{Union{E, Nothing}}(nothing, k),
        he_meta_head=Vector{Union{E, Nothing}}(nothing, k)
        ) where {T<:Real,V,E,D<:AbstractDict{Int, T}} = 
        new{T,V,E,D}(
            BasicHypergraph(n, k),
            BasicHypergraph(n, k),
            v_meta, he_meta_tail, he_meta_head
        )

    function DirectedHypergraph{T,V,E,D}(
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
        ) where {T<:Real,V,E,D<:AbstractDict{Int, T}}
        @assert size(hg_tail) == size(hg_head)

        @assert length(v_meta) == size(hg_tail,1)
        @assert length(he_meta_tail) == size(hg_tail,2)
        @assert length(he_meta_head) == size(hg_head,2)

        new{T,V,E,D}(
            hg_tail,
            hg_head,
            v_meta,
            he_meta_tail,
            he_meta_head
        )
    end
end

DirectedHypergraph{T,V,E}(
    n::Integer, k::Integer;
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real, V, E} = DirectedHypergraph{T,V,E,Dict{Int,T}}(
        n, k;
        v_meta=v_meta,
        he_meta_tail=he_meta_tail,
        he_meta_head=he_meta_head
    )

DirectedHypergraph{T,V}(
    n::Integer, k::Integer;
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real, V} = DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        n, k;
        v_meta=v_meta
    )

DirectedHypergraph{T,D}(n::Integer, k::Integer) where {T<:Real, D<:AbstractDict{Int, T}} = DirectedHypergraph{T,Nothing,Nothing,D}(n, k)

DirectedHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(n, k)

DirectedHypergraph(n::Integer, k::Integer) = DirectedHypergraph{Bool,Nothing,Nothing,Dict{Int,Bool}}(n, k)


function DirectedHypergraph{T,D}(
    hg_tail::BasicHypergraph{T,D},
    hg_head::BasicHypergraph{T,D}
    ) where {T<:Real,D<:AbstractDict{Int, T}}

    DirectedHypergraph{T,Nothing,Nothing,D}(
        hg_tail,
        hg_head
    )
end

function DirectedHypergraph{T,V,D}(
    hg_tail::BasicHypergraph{T,D},
    hg_head::BasicHypergraph{T,D};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    ) where {T<:Real,V,D<:AbstractDict{Int, T}}

    DirectedHypergraph{T,V,Nothing,D}(
        hg_tail,
        hg_head;
        v_meta=v_meta
    )
end

function DirectedHypergraph{T,V,E,D}(
    hg_tail::Hypergraph{T,V,E,D},
    hg_head::Hypergraph{T,V,E,D}
) where {T<:Real,V,E,D<:AbstractDict{Int, T}}
    @assert size(hg_tail) == size(hg_head)

    n, k = size(hg_tail)
    sgh_tail = BasicHypergraph(n, k)
    sgh_head = BasicHypergraph(n, k)

    # TODO: test behavior on this
    sgh_tail .= hg_tail
    sgh_head .= hg_head

    if all(hg_tail.v_meta .== hg_head.v_meta)
        DirectedHypergraph{T,V,E,D}(shg_tail, shg_head; v_meta=hg_tail.v_meta, he_meta_tail=hg_tail.he_meta, he_meta_head=hg_head.he_meta)
    else
        throw(ArgumentError("Vertex metadata `v_meta` is different for ingoing and head hypergraphs!"))
    end
end


function DirectedHypergraph{T,V,E,D}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}};
        v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
        he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
        he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
    ) where {T<:Real,V,E,D<:AbstractDict{Int,T}}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,D}(n, k)
    hg_tail .= m_tail
    
    hg_head = BasicHypergraph{T,D}(n, k)
    hg_head .= m_head

    DirectedHypergraph{T,V,E,D}(
        hg_tail, hg_head;
        v_meta=v_meta,
        he_meta_tail=he_meta_tail,
        he_meta_head=he_meta_head
    )
end

function DirectedHypergraph{T,V,E}(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
    he_meta_tail::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2)),
    he_meta_head::Vector{Union{Nothing,E}}=Vector{Union{Nothing,E}}(nothing, size(m,2))
) where {T<:Real,V,E}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    DirectedHypergraph{T,V,E,Dict{Int,T}}(
        hg_tail, hg_head;
        v_meta=v_meta,
        he_meta_tail=he_meta_tail,
        he_meta_head=he_meta_head
    )
end

function DirectedHypergraph{T,V}(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}};
    v_meta::Vector{Union{Nothing,V}}=Vector{Union{Nothing,V}}(nothing, size(m,1)),
) where {T<:Real,V}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    DirectedHypergraph{T,V,Nothing,Dict{Int,T}}(
        hg_tail,
        hg_head;
        v_meta=v_meta
    )
end

function DirectedHypergraph{T}(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(hg_tail, hg_head)
end

function DirectedHypergraph(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    DirectedHypergraph{T,Nothing,Nothing,Dict{Int,T}}(
        hg_tail,
        hg_head
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
There is danger of a user manipulating individual `hg_tail` and `hg_head` (undirected) hypergraphs
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
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    BasicDirectedHypergraph{T}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}}
    ) where {T<:Real}
    BasicDirectedHypergraph{T,D}(
        m_tail::AbstractMatrix{Union{T, Nothing}},
        m_head::AbstractMatrix{Union{T, Nothing}}
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
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D}
    ) where {T<:Real,D<:AbstractDict{Int, T}}

Constructs a directed hypergraph from two undirected basic hypergraphs, one with hyperedges
containing "tail" vertices and one with hyperedges containing "head"
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
* `hg_tail`: an undirected hypergraph representing the tail half of
    the directed hypergraph
* `hg_head`: an undirected hypergraph representing the head half of
    the directed hypergraph
"""
struct BasicDirectedHypergraph{T<:Real,D<:AbstractDict{Int, T}} <: AbstractDirectedHypergraph{Tuple{T, T}}
    hg_tail::BasicHypergraph{T,D}
    hg_head::BasicHypergraph{T,D}

    BasicDirectedHypergraph{T,D}(
        n::Integer, k::Integer,
        ) where {T<:Real,D<:AbstractDict{Int, T}} =
        new{T,D}(
            BasicHypergraph(n, k),
            BasicHypergraph(n, k)
        )

    function BasicDirectedHypergraph{T,D}(
        hg_tail::BasicHypergraph{T,D},
        hg_head::BasicHypergraph{T,D}
        ) where {T<:Real,D<:AbstractDict{Int, T}}
        
        @assert size(hg_tail) == size(hg_head)

        new{T,D}(hg_tail, hg_head)
    end
end


BasicDirectedHypergraph{T}(n::Integer, k::Integer) where {T<:Real} = BasicDirectedHypergraph{T,Dict{Int,T}}(n, k)

BasicDirectedHypergraph(n::Integer, k::Integer) = BasicDirectedHypergraph{Bool,Dict{Int,Bool}}(n, k)


function BasicDirectedHypergraph{T}(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    BasicDirectedHypergraph{T,Dict{Int,T}}(hg_tail, hg_head)
end

function BasicDirectedHypergraph(
    m_tail::AbstractMatrix{Union{T, Nothing}},
    m_head::AbstractMatrix{Union{T, Nothing}}
) where {T<:Real}

    # Arbitrary, since sizes are identical
    n, k = size(m_tail)

    hg_tail = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_tail .= m_tail

    hg_head = BasicHypergraph{T,Dict{Int,T}}(n, k)
    hg_head .= m_head

    BasicDirectedHypergraph{T,Dict{Int,T}}(
        hg_tail,
        hg_head
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
@traitimpl HasMeta{HasMetaHGs}
hasmeta(::Type{T}) where {T<:HasMetaHGs} = true


# TODO: this is awkward...
const DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES = [1,2]

# TODO: can this entirely replace the above? Index setting seems problematic...
@enum HyperedgeDirection begin
    tail = 1
    head = 2
end


# AbstractArray interface functions

"""
    Base.size(h::AbstractHypergraph)

Returns the size of hypergraph `h`.
The result is a tuple of the number of vertices and the number of hyperedges

"""
Base.size(h::AbstractHypergraph) = (nhv(h), nhe(h))


"""
    Base.getindex(h::H, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}

Returns a value for a given vertex-hyperedge pair `idx` for an undirected hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::H, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
    @boundscheck checkbounds(h, idx...)
    get(h.v2he[idx[1]], idx[2], nothing)
end

"""
    Base.getindex(h::Union{DirectedHypergraph, BasicDirectedHypergraph}, idx::Vararg{Int,2})

Returns a value for a given vertex-hyperedge pair `idx` for a directed hypergraph `h`.
If a vertex does not belong to a hyperedge `nothing` is returned.

"""
@inline function Base.getindex(h::H, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(h.hg_tail, idx...)
    @boundscheck checkbounds(h.hg_head, idx...)

    tail_value = get(h.hg_tail.v2he[idx[1]], idx[2], nothing)
    head_value = get(h.hg_head.v2he[idx[1]], idx[2], nothing)

    (tail_value, head_value)
end



"""
    Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}

Removes a vertex from a given hyperedge for an undirected hypergraph `h` and a given vertex-hyperedge pair `idx`.
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
    @boundscheck checkbounds(h, idx...)
    pop!(h.v2he[idx[1]], idx[2], nothing)
    pop!(h.he2v[idx[2]], idx[1], nothing)
    h
end


"""
    Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}

Adds a vertex to an undirected hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractUndirectedHypergraph}
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
@inline function Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(h.hg_tail, idx...)
    @boundscheck checkbounds(h.hg_head, idx...)
    setindex!(h.hg_tail, nothing, idx)
    setindex!(h.hg_head, nothing, idx)
    h
end


"""
    Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}

Adds a vertex to a hyperedge (represented by indices `idx`) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::H, v::Real, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(h.hg_tail, idx...)
    @boundscheck checkbounds(h.hg_head, idx...)

    setindex!(h.hg_tail, v, idx)
    setindex!(h.hg_head, v, idx)
    h
end


"""
    Base.setindex!(h::H, v::Tuple{Union{Real, Nothing}, Union{Real, Nothing}}, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}

Manipulates a hyperedge (represented by indices `idx`), either adding a vertex to the 
ingoing and/or head sides of the hyperedge and assigning a value associated with that assignment,
or else removing a vertex from the ingoing/head sides of the hyperedge.

Here, `v` is a 2-tuple where the first element is the value that will be assigned to the ingoing part of the hyperedge
and the second element is the value that will be assigned to the head part. A value of `nothing` means that the
vertex will be removed from that side of the hyperedge.

"""
@inline function Base.setindex!(h::H, v::Tuple{Union{Real, Nothing}, Union{Real, Nothing}}, idx::Vararg{Int,2}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(h.hg_tail, idx...)
    @boundscheck checkbounds(h.hg_head, idx...)
    
    setindex!(h.hg_tail, v[1], idx)
    setindex!(h.hg_head, v[2], idx)

    h
end


"""
    Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}

Removes a vertex from a given hyperedge for a directed hypergraph `h` and a given side-vertex-hyperedge pair `idx`.
If the first index of `idx` is 1, then the vertex will be removed from the tail hyperedge; if `idx` is 2, then
the vertex will be removed from the head hyperedge. 
Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.

"""
@inline function Base.setindex!(h::H, ::Nothing, idx::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES, idx[1])

    if idx[1] == 1
        side = h.hg_tail
    else
        side = h.hg_head
    end
    
    @boundscheck checkbounds(side, idx[2:end]...)
    
    setindex!(side, nothing, idx[2:end])

    h
end


"""
    Base.setindex!(h::H, v::Real, idx::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}

Adds a vertex to a hyperedge (represented by indices `idx`, where the first index must be either
1 - referring to an tail hyperedge - or 2 - referring to an head hyperedge) and assigns value
`v` to be stored with that assignment.

"""
@inline function Base.setindex!(h::H, v::Real, idx::Vararg{Int,3}) where {H <: AbstractDirectedHypergraph}
    @boundscheck checkbounds(DIRECTED_HYPERGRAPH_VALID_FIRST_INDICES, idx[1])

    if idx[1] == 1
        side = h.hg_tail
    else
        side = h.hg_head
    end
    
    @boundscheck checkbounds(side, idx[2:end]...)
    
    setindex!(side, v, idx[2:end])

    h
end


"""
    getvertices(h::H, he_id::Int) where {H <: AbstractUndirectedHypergraph}

Returns vertices from an undirected hypergraph `a` for a given hyperedge `he_id`.

"""
@inline getvertices(h::H, he_id::Int) where {H <: AbstractUndirectedHypergraph} = h.he2v[he_id]


"""
    getvertices(h::H, he_id::Int) where {H <: AbstractDirectedHypergraph}

Returns vertices from a directed hypergraph `a` for a given hyperedge `he_id`.
Vertex indices are given in a tuple `(in, out)`, where `in` are the tail vertices
and `out` are the head vertices

"""
@inline getvertices(h::H, he_id::Int) where {H <: AbstractDirectedHypergraph} = (h.hg_tail.he2v[he_id], h.hg_head.he2v[he_id])


"""
    gethyperedges(h::H, v_id::Int) where {H <: AbstractUndirectedHypergraph}

Returns hyperedges for a given vertex `v_id` in an undirected hypergraph `h`.

"""
@inline gethyperedges(h::H, v_id::Int) where {H <: AbstractUndirectedHypergraph} = h.v2he[v_id]


"""
    gethyperedges(h::H, v_id::Int) where {H <: AbstractDirectedHypergraph}

Returns hyperedges for a given vertex `v_id` in a directed hypergraph `h`.
Hyperedge indices are given in a tuple `(tail, head)`, where `tail` are the hyperedges where
vertex `v_ind` is on the tail side and `head` are the hyperedges where `v_ind` is on
the head side.

"""
@inline gethyperedges(h::H, v_id::Int) where {H <: AbstractDirectedHypergraph} = (h.hg_tail.v2he[v_id], h.hg_head.v2he[v_id])

"""
    to_undirected(h::DirectedHypergraph)

Converts a directed hypergraph into an undirected hypergraph.
Tail and head hyperedges are combined; that is, for all hyperedges he_orig in
the directed hypergraph h, all vertices in the head or tail are added to a
corresponding undirected hyperedge he_new in the undirected hypergraph h'.

Metadata is combined into tuples; i.e., if there was originally tail metadata
t_meta and head metadata h_meta for a given directed hyperedge, the new
undirected hyperedge will have metadata (t_meta, h_meta).

Because vertex-hyperedge weights are restricted to real numbers, we cannot
combine the weights, so we simply set the values to 1.0 if a given vertex
is in a given hyperedge 

"""
function to_undirected(h::DirectedHypergraph{T,V,E,D}) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    incidence = Matrix{Union{Tuple{Union{T, Nothing}, Union{T, Nothing}}, Nothing}}(undef, nhv(h), nhe(h))
    fill!(incidence, nothing)

    this_nhe = nhe(h)

    for row in 1:nhv(h)
        for column in 1:this_nhe
            tail_val, head_val = h[row, column]
            if tail_val === nothing && head_val === nothing
                incidence[row, column] = nothing
            else
                incidence[row, column] = convert(T, 1.0)
            end
        end
    end

    combined_he_meta = Vector{Union{Tuple{Union{E, Nothing}, Union{E, Nothing}}, Nothing}}(undef, this_nhe)
    fill!(combined_he_meta, nothing)
    for he_index in 1:this_nhe
        tail_meta = h.he_meta_tail[he_index]
        head_meta = h.he_meta_head[he_index]

        if tail_meta !== nothing || head_meta !== nothing
            combined_he_meta[he_index] = (tail_meta, head_meta)
        end
    end

    Hypergraph{T, V, Tuple{Union{E, Nothing},Union{E, Nothing}}, D}(
        incidence,
        v_meta=h.v_meta,
        he_meta=combined_he_meta
    )

end

"""
    to_undirected(h::BasicDirectedHypergraph)

Converts a directed hypergraph into an undirected hypergraph.
Tail and head hyperedges are combined; that is, for all hyperedges he_orig in
the directed hypergraph h, all vertices in the head or tail are added to a
corresponding undirected hyperedge he_new in the undirected hypergraph h'.

Because vertex-hyperedge weights are restricted to real numbers, we cannot
combine the weights, so we simply set the values to 1.0 if a given vertex
is in a given hyperedge 
"""
function to_undirected(h::BasicDirectedHypergraph{T, D}) where {T <: Real, D <: AbstractDict{Int,T}}
    incidence = Matrix{Union{T, Nothing}}(undef, nhv(h), nhe(h))
    fill!(incidence, nothing)

    this_nhe = nhe(h)

    for row in 1:nhv(h)
        for column in 1:this_nhe
            tail_val, head_val = h[row, column]
            if tail_val === nothing && head_val === nothing
                incidence[row, column] = nothing
            else
                incidence[row, column] = convert(T, 1.0)
            end
        end
    end

    BasicHypergraph{T, D}(incidence)

end


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
                hyperedges_tail::D = D(), hyperedges_head::D = D(), v_meta::Union{V,Nothing} = nothing
                ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a vertex to a given directed hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges_tail` parameter presents a dictionary
of hyperedge identifiers and values stored at the ingoing side of hyperedges, and
the `hyperedges_head` parameter presents a dictionary of hyperedge identifiers and
values stored at the head side of hyperedges.
Additionally, a value can be stored with the vertex using the `v_meta` keyword
parameter.

"""
function add_vertex!(h::DirectedHypergraph{T, V, E, D};
                     hyperedges_tail::D = D(), hyperedges_head::D = D(), v_meta::Union{V,Nothing} = nothing
                    ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_tail,1,k) for k in keys(hyperedges_tail))
    @boundscheck (checkbounds(h.hg_head,1,k) for k in keys(hyperedges_head))

    push!(h.hg_tail.v2he,hyperedges_tail)
    push!(h.hg_head.v2he,hyperedges_head)

    # Should always be identical to h.hg_head.v2he
    ix = length(h.hg_tail.v2he)

    for k in keys(hyperedges_tail)
        h[1,ix,k]=hyperedges_tail[k]
    end

    for k in keys(hyperedges_head)
        h[2,ix,k]=hyperedges_head[k]
    end

    push!(h.v_meta, v_meta)
    ix
end

"""
    add_vertex!(h::BasicDirectedHypergraph{T, D};
                hyperedges_tail::D = D(), hyperedges_head::D = D()
                ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a vertex to a given directed hypergraph `h`. Optionally, the vertex can be added
to existing hyperedges. The `hyperedges_tail` parameter presents a dictionary
of hyperedge identifiers and values stored at the ingoing side of hyperedges, and
the `hyperedges_head` parameter presents a dictionary of hyperedge identifiers and
values stored at the head side of hyperedges.

"""
function add_vertex!(h::BasicDirectedHypergraph{T, D};
                     hyperedges_tail::D = D(), hyperedges_head::D = D()
                    ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_tail,1,k) for k in keys(hyperedges_tail))
    @boundscheck (checkbounds(h.hg_head,1,k) for k in keys(hyperedges_head))

    push!(h.hg_tail.v2he,hyperedges_tail)
    push!(h.hg_head.v2he,hyperedges_head)
    
    ix = length(h.hg_tail.v2he)

    for k in keys(hyperedges_tail)
        h[1,ix,k]=hyperedges_tail[k]
    end

    for k in keys(hyperedges_head)
        h[2,ix,k]=hyperedges_head[k]
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
    
    remove_vertex!(h.hg_tail, v)
    remove_vertex!(h.hg_head, v)

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
    remove_vertex!(h.hg_tail, v)
    remove_vertex!(h.hg_head, v)

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
                   vertices_tail::D = D(), vertices_head::D = D(),
                   he_meta_tail::Union{E,Nothing}=nothing, he_meta_head::Union{E,Nothing}=nothing
                   ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given directed hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge in the
tail or head directions.
The paramater `vertices_tail` represents a dictionary of vertex identifiers and
values stored at the tail hyperedge; `vertices_head` represented the vertex
identifiers and values stored at the outcoming side of the hyperedge. Additionally, 
a value can be stored with the hyperedge using the `he_meta_tail` and `he_meta_head`
keyword parameters.

"""
function add_hyperedge!(h::DirectedHypergraph{T, V, E, D};
                        vertices_tail::D = D(), vertices_head::D = D(),
                        he_meta_tail::Union{E,Nothing}=nothing, he_meta_head::Union{E,Nothing}=nothing
                        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_tail,k,1) for k in keys(vertices_tail))
    @boundscheck (checkbounds(h.hg_head,1,k) for k in keys(vertices_head))
    
    push!(h.hg_tail.he2v,vertices_tail)
    push!(h.hg_head.he2v, vertices_head)

    # Should always be identical to length(h.hg_head.he2v)
    ix = length(h.hg_tail.he2v)
    for k in keys(vertices_tail)
        h[1,k,ix]=vertices_tail[k]
    end
    for k in keys(vertices_head)
        h[2,k,ix]=vertices_head[k]
    end
    push!(h.he_meta_tail, he_meta_tail)
    push!(h.he_meta_head, he_meta_head)
    ix
end


"""
    add_hyperedge!(h::BasicDirectedHypergraph{T, D};
                   vertices::D = D()
                   ) where {T <: Real, D <: AbstractDict{Int,T}}

Adds a hyperedge to a given directed hypergraph `h`.
Optionally, existing vertices can be added to the created hyperedge in the
tail or head directions.
The paramater `vertices_tail` represents a dictionary of vertex identifiers and
values stored at the tail hyperedge; `vertices_head` represented the vertex
identifiers and values stored at the outcoming side of the hyperedge.

"""
function add_hyperedge!(h::BasicDirectedHypergraph{T, D};
                        vertices_tail::D = D(), vertices_head::D = D()
                        ) where {T <: Real, D <: AbstractDict{Int,T}}
    @boundscheck (checkbounds(h.hg_tail,k,1) for k in keys(vertices_tail))
    @boundscheck (checkbounds(h.hg_head,1,k) for k in keys(vertices_head))
    
    push!(h.hg_tail.he2v,vertices_tail)
    push!(h.hg_head.he2v, vertices_head)

    # Should always be identical to length(h.hg_head.he2v)
    ix = length(h.hg_tail.he2v)
    for k in keys(vertices_tail)
        h[1,k,ix]=vertices_tail[k]
    end
    for k in keys(vertices_head)
        h[2,k,ix]=vertices_head[k]
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
	    h.he_meta_tail[e] = h.he_meta_tail[ne]
        h.he_meta_head[e] = h.he_meta_head[ne]
	end

    remove_hyperedge!(h.hg_tail, e)
    remove_hyperedge!(h.hg_head, e)

    resize!(h.he_meta_tail, length(h.he_meta_tail) - 1)
    resize!(h.he_meta_head, length(h.he_meta_head) - 1)

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
    remove_hyperedge!(h.hg_tail, e)
    remove_hyperedge!(h.hg_head, e)

    h
end


"""
    prune_hypergraph!(h::H) where {H <: AbstractUndirectedHypergraph}

Remove all vertices with degree 0 and all hyperedges of size 0.

"""
function prune_hypergraph!(h::H) where {H <: AbstractUndirectedHypergraph}
	for e in reverse(1:nhe(h))
        length(h.he2v[e]) == 0 && remove_hyperedge!(h,e)
    end
	for v in reverse(1:nhv(h))
    	length(h.v2he[v]) == 0 && remove_vertex!(h,v)
    end
	h
end


"""
    prune_hypergraph!(h::H) where {H <: AbstractDirectedHypergraph}

Remove all vertices with degree 0 and all hyperedges of size 0.

"""
function prune_hypergraph!(h::H) where {H <: AbstractDirectedHypergraph}
	for e in reverse(1:nhe(h))
        length(h.hg_tail.he2v[e]) == 0 && length(h.hg_head.he2v[e]) && remove_hyperedge!(h,e)
    end
	for v in reverse(1:nhv(h))
    	length(h.hg_tail.v2he[v]) == 0 && length(h.hg_tail.v2he[v]) == 0 && remove_vertex!(h,v)
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
        new_value_tail::Union{E,Nothing}, new_value_head::Union{E,Nothing}, id::Int
        ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

Sets a new meta value `new_value` for the hyperedge `id` in the directed hypergraph `h`.

"""
function set_hyperedge_meta!(h::DirectedHypergraph{T, V, E, D},
                             new_value_tail::Union{E,Nothing}, new_value_head::Union{E,Nothing}, id::Int
                             ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}
    checkbounds(h.he_meta_tail, id)
    checkbounds(h.he_meta_head, id)

    h.he_meta_tail[id] = new_value_tail
    h.he_meta_head[id] = new_value_head

    (h.he_meta_tail, h.he_meta_head)
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
    
    if side == tail
        checkbounds(h.he_meta_tail, id)
        h.he_meta_tail[id] = new_value
        h.he_meta_tail
    else
        checkbounds(h.he_meta_head, id)
        h.he_meta_head[id] = new_value
        h.he_meta_head
    end

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
    checkbounds(h.he_meta_tail, id)
    checkbounds(h.he_meta_head, id)

    (h.he_meta_tail[id], h.he_meta_head[id])
end


"""
    get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int, side::HyperedgeDirection)
        where {T <: Real, V, E, D <: AbstractDict{Int,T}}
Returns a meta value stored at the hyperedge `id` in the directed hypergraph `h`.

"""
function get_hyperedge_meta(h::DirectedHypergraph{T, V, E, D}, id::Int, side::HyperedgeDirection
                            ) where {T <: Real, V, E, D <: AbstractDict{Int,T}}

    if side == tail
        checkbounds(h.he_meta_tail, id)
        h.he_meta_tail[id]
    else
        checkbounds(h.he_meta_head, id)
        h.he_meta_head[id]
    end
end

get_hyperedge_meta(::BasicHypergraph, ::Int) = throw("Not implemented!")
get_hyperedge_meta(::BasicDirectedHypergraph, ::Int) = throw("Not implemented!")
get_hyperedge_meta(::BasicDirectedHypergraph, ::Int, ::HyperedgeDirection) = throw("Not implemented!")


"""
    nhe(h::Union{Hypergraph,BasicHypergraph})

Return the number of hyperedges in the undirected hypergraph `h`.
"""
function nhe(h::H) where {H <: AbstractUndirectedHypergraph}
    length(h.he2v)
end


"""
    nhe(h::Union{DirectedHypergraph,BasicDirectedHypergraph})

Return the number of hyperedges in the directed hypergraph `h`.
"""
function nhe(h::H) where {H <: AbstractDirectedHypergraph}
    (length(h.hg_tail.he2v) == length(h.hg_head.he2v)) ? length(h.hg_tail.he2v) : throw("Tail and head sides of hypergraph have different numbers of hyperedges!")
end


"""
    nhv(h::Union{Hypergraph,BasicHypergraph})

Return the number of vertices in the undirected hypergraph `h`.
"""
function nhv(h::H) where {H <: AbstractUndirectedHypergraph}
    length(h.v2he)
end


"""
    nhv(h::Union{DirectedHypergraph,BasicDirectedHypergraph})

Return the number of vertices in the directed hypergraph `h`.
"""
function nhv(h::H) where {H <: AbstractDirectedHypergraph}
    (length(h.hg_tail.v2he) == length(h.hg_head.v2he)) ? length(h.hg_tail.v2he) : throw("Tail and head sides of hypergraph have different numbers of hyperedges!")
end


function _default_heselect(h::H, v::Int) where {H <: AbstractUndirectedHypergraph}
    hes = gethyperedges(h, v)
    sort!(collect(keys(hes))), ones(length(hes))
end

function _default_heselect(h::H, v::Int; reverse::Bool=false) where {H <: AbstractDirectedHypergraph}
    he_tail, he_head = gethyperedges(h, v)

    if reverse
        hes = he_tail
    else
        hes = he_head
    end

    sort!(collect(keys(hes))), ones(length(hes))
end


function _default_vselect(h::H, he::Int) where {H <: AbstractUndirectedHypergraph}
    vs = getvertices(h, he)
    sort!(collect(keys(vs))), ones(length(vs))
end

function _default_vselect(h::H, he::Int; reverse::Bool=false) where {H <: AbstractDirectedHypergraph}
    vs_tail, vs_head = getvertices(h, he)

    if reverse
        vs = vs_tail
    else
        vs = vs_head
    end

    sort!(collect(keys(vs))), ones(length(vs))

end


"""
    random_walk(
        h::H,
        start::Int;
        heselect::Function,
        vselect::Function,
    ) where {H <: AbstractUndirectedHypergraph}

Return a next vertex visited in assuming a random walk starting from vertex `start`.
First a hyperedge is sampled with weights proportional to `heselect` function
(by default each hyperedge is sampled with the same probability).
Next a vertex within hyperedge is with weights proportional to `vselect` function
(by default each vertex, including the source, is sampled with the same probability).

`heselect` and `vselect` functions take two arguments a `Hypergraph` and respectively
a vertex identifier or a hyperedge identifier. The return values of both functions
should be respectively a list of hyperedges or vertices and their weights.
"""
function random_walk(h::H, start::Int;
                     heselect::Function=_default_heselect,
                     vselect::Function=_default_vselect) where {H <: AbstractUndirectedHypergraph}
    1 <= start <= nhv(h) || throw(ArgumentError("invalid start vertex index"))
    hes, hew = heselect(h, start)
    he = sample(hes, Weights(hew))
    ves, vw = vselect(h, he)
    return sample(ves, Weights(vw))
end

"""
    random_walk(
        h::H,
        start::Int;
        heselect::Function,
        vselect::Function,
        reverse::bool
    ) where {H <: AbstractDirectedHypergraph}

Return a next vertex visited in assuming a random walk starting from vertex `start`.
First a hyperedge is sampled with weights proportional to `heselect` function
(by default each hyperedge is sampled with the same probability).
Next a vertex within hyperedge is with weights proportional to `vselect` function
(by default each vertex, including the source, is sampled with the same probability).

`heselect` and `vselect` functions take two arguments a `Hypergraph` and respectively
a vertex identifier or a hyperedge identifier. The return values of both functions
should be respectively a list of hyperedges or vertices and their weights.
"""
function random_walk(h::H, start::Int;
                     heselect::Function=_default_heselect,
                     vselect::Function=_default_vselect,
                     reverse::Bool=false) where {H <: AbstractDirectedHypergraph}
    1 <= start <= nhv(h) || throw(ArgumentError("invalid start vertex index"))
    hes, hew = heselect(h, start, reverse=reverse)
    he = sample(hes, Weights(hew))
    ves, vw = vselect(h, he, reverse=reverse)
    return sample(ves, Weights(vw))
end


"""
    _walk!(h::H, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool}) where where {H <: AbstractUndirectedHypergraph}

Appends the list of neighbors `s` of a given vertex `i` (an auxiliary function for `get_connected_components`).
"""
function _walk!(h::H, s::AbstractVector{Int}, i::Int, visited::AbstractVector{Bool}) where {H <: AbstractUndirectedHypergraph}
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
    get_connected_components(h::H) where {H <: AbstractUndirectedHypergraph}

Return an array of connected components in the undirected hypergraph `h`
(array of vectors of vertices) using recurrence.
"""
function get_connected_components(h::H) where {H <: AbstractUndirectedHypergraph}
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
    get_weakly_connected_components(h::H) where {H <: AbstractDirectedHypergraph}

Return an array of weakly connected components in the directed hypergraph `h`
(array of vectors of vertices) by first converting the directed hypergraph
into an undirected hypergraph and then obtaining the conected components of
that hypergraph.
"""
function get_weakly_connected_components(h::H) where {H <: AbstractDirectedHypergraph}
    undirected = to_undirected(h)
    get_connected_components(undirected)
end


"""
    _visit!(h::H, v::Int) where {H <: AbstractDirectedHypergraph}

Determines the B-connected component of a vertex `v` in directed hypergraph `h`.
This is an auxiliary function for `get_strongly_connected_components`, which
determines the strongly connected components of a directed hypergraph.
"""
function _visit(
    h::H,
    v::Int
) where {H <: AbstractDirectedHypergraph}
    visited = zeros(Bool, nhv(h))
    visited_tail_nodes = zeros(Int, nhe(h))

    q = Queue{Int}()
    bcc = Set{Int}()
    enqueue!(q, v)

    visited[v] = true

    while length(q) > 0
        u = dequeue!(q)
        push!(bcc, u)

        tail_hes = gethyperedges(h, u)[1]

        for tail_he in keys(tail_hes)
            visited_tail_nodes[tail_he] += 1

            tail_vs, head_vs = getvertices(h, tail_he)

            if visited_tail_nodes[tail_he] == length(tail_vs)
                for head_v in keys(head_vs)
                    if !visited[head_v]
                        visited[head_v] = true
                        enqueue!(q, head_v)
                    end
                end
            end
        end
    end

    bcc
end


"""
    get_strongly_connected_components(h::H) where {H <: AbstractDirectedHypergraph}

Return an array of strongly connected components in the directed hypergraph `h`
(array of vectors of vertices), based on the "naive" algorithm of
Francisco José Martín-Recuerda Moyano (PhD dissertation, 2016).

"""
function get_strongly_connected_components(h::H) where {H <: AbstractDirectedHypergraph}

    T = Dict{Vector{Int}, Set{Int}}()

    for v in 1:nhv(h)
        bcc_v = _visit(h, v)
        bcc_sorted = sort(collect(bcc_v))
        for i in 1:length(bcc_sorted)
            if !haskey(T, bcc_sorted[1:i])
                T[bcc_sorted[1:i]] = Set{Int}()
            end
        end
        push!(T[bcc_sorted], v)
    end

    [v for (k, v) in T if length(v) != 0]
end

"""
    adjacency_matrix(h::H; s::Int=1, weighted::Bool=false) where {H <: AbstractUndirectedHypergraph}

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

NOTE: information about the weight of a vertex in a hyperedge will be lost!

"""
function adjacency_matrix(h::H; s::Int=1, weighted::Bool=true) where {H <: AbstractUndirectedHypergraph}
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

    # TODO: Should this be M[M .!= 0] .= 1?
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


# TODO needs validate_hypergraph!(h::Hypergraph{T})
