var documenterSearchIndex = {"docs": [

{
    "location": "#",
    "page": "SimpleHypergraphs.jl",
    "title": "SimpleHypergraphs.jl",
    "category": "page",
    "text": ""
},

{
    "location": "#SimpleHypergraphs.jl-1",
    "page": "SimpleHypergraphs.jl",
    "title": "SimpleHypergraphs.jl",
    "category": "section",
    "text": "Documentation for SimpleHypergraphs.jl"
},

{
    "location": "reference/#",
    "page": "Reference",
    "title": "Reference",
    "category": "page",
    "text": ""
},

{
    "location": "reference/#Reference-1",
    "page": "Reference",
    "title": "Reference",
    "category": "section",
    "text": "CurrentModule = SimpleHypergraphs\nDocTestSetup = quote\n    using SimpleHypergraphs\nend"
},

{
    "location": "reference/#SimpleHypergraphs.Hypergraph",
    "page": "Reference",
    "title": "SimpleHypergraphs.Hypergraph",
    "category": "type",
    "text": "Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}\n\nA hypergraph storing information about vertices and hyperedges.\n\nConstructors\n\nHypergraph{T}(n,k) where {T<:Real}\n\nConstruct a hypergraph with a given number of vertices and hyperedges.\n\nHypergraph(m::AbstractMatrix{T}) where {T<:Real}\n\nConstruct a hypergraph using its matrix representation. In the matrix representation rows are vertices and columns are hyperedges.\n\nArguments\n\nT : type of values stored in the hypergraph\nn : number of vertices\nk : number of hyperedges\nm : a matrix representation rows are vertices and columns are hyperedges\n\n\n\n\n\n"
},

{
    "location": "reference/#Types-1",
    "page": "Reference",
    "title": "Types",
    "category": "section",
    "text": "SimpleHypergraphs.Hypergraph"
},

{
    "location": "reference/#Base.getindex-Tuple{Hypergraph,Int64,Int64}",
    "page": "Reference",
    "title": "Base.getindex",
    "category": "method",
    "text": "Base.getindex(h::Hypergraph, idx::Vararg{Int,2})\n\nReturns a value for a given vertex-hyperedge pair or nothing if a vertex does not belong to a hyperedge.\n\nArguments\n\nh : a hypergraph\nidx : an index where the first element is vertex and the second is a hyperedge\n\n\n\n\n\n"
},

{
    "location": "reference/#Base.setindex!-Tuple{Hypergraph,Nothing,Int64,Int64}",
    "page": "Reference",
    "title": "Base.setindex!",
    "category": "method",
    "text": "Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})\n\nRemoves a vertex from a given hyperedge\n\nArguments\n\nh : a hypergraph\nidx : an index where the first element is vertex and the second is a hyperedge\n\n\n\n\n\n"
},

{
    "location": "reference/#Base.setindex!-Tuple{Hypergraph,Real,Int64,Int64}",
    "page": "Reference",
    "title": "Base.setindex!",
    "category": "method",
    "text": "Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})\n\nAdds a vertex to a hyperedge and assigns value to be stored with that assignment.\n\nArguments\n\nh : a hypergraph\nv : a value to be stored with vertex-hyperedge assignment\nidx : an index where the first element is vertex and the second is a hyperedge\n\n\n\n\n\n"
},

{
    "location": "reference/#Hypergraph-array-getters-and-setters-1",
    "page": "Reference",
    "title": "Hypergraph array getters and setters",
    "category": "section",
    "text": "Normally you work with a hypergraph via array setters, for example the code below craete an Hypergraph and add vertex one to hyperedges 2 and 3 with weight 5:h = Hypergraph{Int64}(2,3);\nh[1, 2:3] .= 5;  \nh\n\n# output\n\n2×3 Hypergraph{Int64}:\n nothing  5         5\n nothing   nothing   nothinggetindex(::Hypergraph, ::Vararg{Int,2})\nsetindex!(::Hypergraph, ::Nothing, ::Vararg{Int,2})\nsetindex!(::Hypergraph, ::Real, ::Vararg{Int,2})"
},

{
    "location": "reference/#Base.size-Tuple{Hypergraph}",
    "page": "Reference",
    "title": "Base.size",
    "category": "method",
    "text": "Base.size(h::Hypergraph)\n\nReturns the size of Hypergraph m. The result is a tuple of the number of vertices and the number of hyperedges\n\nArguments\n\nh : a hypergraph\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.getvertices-Tuple{Hypergraph,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.getvertices",
    "category": "method",
    "text": "getvertices(h::Hypergraph, he_id::Int)\n\nReturns vertices for a given hyperedge\n\nArguments\n\nh : a hypergraph\nhe_id : an identifier of a hyperedge\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.gethyperedges-Tuple{Hypergraph,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.gethyperedges",
    "category": "method",
    "text": "gethyperedges(h::Hypergraph, v_id::Int)\n\nReturns hyperedges for a given vertex\n\nArguments\n\nh : a hypergraph\nv_id : an identifier of a vertex\n\n\n\n\n\n"
},

{
    "location": "reference/#Hypergraph-info-1",
    "page": "Reference",
    "title": "Hypergraph info",
    "category": "section",
    "text": "size(::Hypergraph)\ngetvertices(::Hypergraph, ::Int)\ngethyperedges(::Hypergraph, ::Int)"
},

{
    "location": "reference/#SimpleHypergraphs.hg_save",
    "page": "Reference",
    "title": "SimpleHypergraphs.hg_save",
    "category": "function",
    "text": "hg_save(io::IO, h::Hypergraph)\n\nSaves a hypergraph to a stream.\n\nArguments\n\nio : the output stream to be written to\nh : a hypergraph\n\n\n\n\n\nhg_save(fname::AbstractString, h::Hypergraph)\n\nSaves a hypergraph to a file.\n\nArguments\n\nfname : a file name to save to\nh : a hypergraph\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.hg_load",
    "page": "Reference",
    "title": "SimpleHypergraphs.hg_load",
    "category": "function",
    "text": "hg_load(fname::AbstractString, T::Type{<:Real})\n\nLoads a hypergraph from a stream.\n\nArguments\n\nio : an input stream from which to load the data\nT : type of data in the hypegraph\n\n\n\n\n\nhg_load(fname::AbstractString, T::Type{<:Real})\n\nLoads a hypergraph from a file.\n\nArguments\n\nfname : a file from which to load the data\nT : type of data in the hypegraph\n\n\n\n\n\n"
},

{
    "location": "reference/#I/O-1",
    "page": "Reference",
    "title": "I/O",
    "category": "section",
    "text": "hg_save\nhg_loadDocTestSetup = nothing"
},

]}
