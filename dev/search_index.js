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
    "text": "Documentation for SimpleHypergraphs.jlFor details please go to the Reference section."
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
    "text": "Hypergraph{T} <: AbstractMatrix{Union{T, Nothing}}\n\nA hypergraph storing information about vertices and hyperedges.\n\nConstructors\n\nHypergraph{T}(n,k) where {T<:Real}\nHypergraph{T,V}(n, k) where {T<:Real, V}\nHypergraph{T,V,E}(n, k) where {T<:Real, V, E}\n\nConstruct a hypergraph with a given number of vertices and hyperedges. Optionally, values of type V can be stored at vertices and values of type E can be stored at hyperedges.\n\nHypergraph(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real}\nHypergraph{V}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V}\nHypergraph{V, E}(m::AbstractMatrix{Union{T, Nothing}}) where {T<:Real, V, E}\n\nConstruct a hypergraph using its matrix representation. In the matrix representation rows are vertices and columns are hyperedges. Optionally, values of type V can be stored at vertices and values of type E can be stored at hyperedges.\n\nArguments\n\nT : type of weight values stored in the hypergraph\nV : type of values stored in the vertices of the hypergraph\nE : type of values stored in the edges of the hypergraph\nn : number of vertices\nk : number of hyperedges\nm : a matrix representation rows are vertices and columns are hyperedges\n\n\n\n\n\n"
},

{
    "location": "reference/#Types-1",
    "page": "Reference",
    "title": "Types",
    "category": "section",
    "text": "Hypergraph"
},

{
    "location": "reference/#SimpleHypergraphs.add_hyperedge!-Union{Tuple{Hypergraph{T,V,E}}, Tuple{E}, Tuple{V}, Tuple{T}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.add_hyperedge!",
    "category": "method",
    "text": "add_hyperedge!(h::Hypergraph{T, V, E}; vertices::Dict{Int,T} = Dict{Int,T}(),\n               hyperedge_meta::Union{E,Nothing}=nothing) where {T <: Real, V, E}\n\nAdds a hyperedge to a given hypergraph h. Optionally, existing vertices can be added to the created hyperedge. The paramater vertices represents a dictionary of vertex identifiers and values stored at the hyperedges. Additionally, a value can be stored with the hyperedge using the hyperedge_meta keyword parameter.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.add_vertex!-Union{Tuple{Hypergraph{T,V,E}}, Tuple{E}, Tuple{V}, Tuple{T}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.add_vertex!",
    "category": "method",
    "text": "add_vertex!(h::Hypergraph{T, V, E}; hyperedges::Dict{Int,T} = Dict{Int,T}(),\n            vertex_meta::Union{V,Nothing} nothing undef) where {T <: Real, V, E}\n\nAdds a vertex to a given hypergraph h. Optionally, the vertex can be added to existing hyperedges. The hyperedges parameter presents a dictionary of hyperedge identifiers and values stored at the hyperedges. Additionally, a value can be stored with the vertex using the vertex_meta keyword parameter.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.set_vertex_meta!-Union{Tuple{E}, Tuple{V}, Tuple{T}, Tuple{Hypergraph{T,V,E},Union{Nothing, V},Int64}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.set_vertex_meta!",
    "category": "method",
    "text": "set_vertex_meta!(h::Hypergraph{T, V, E}, new_value::Union{V,Nothing}, id::Int)\n    where {T <: Real, V, E}\n\nSets a new meta value new_value for the vertex id in the hypegraph h.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.get_vertex_meta-Union{Tuple{E}, Tuple{V}, Tuple{T}, Tuple{Hypergraph{T,V,E},Int64}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.get_vertex_meta",
    "category": "method",
    "text": "get_vertex_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}\n\nReturns a meta value stored at the vertex id in the hypergraph h.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.set_hyperedge_meta!-Union{Tuple{E}, Tuple{V}, Tuple{T}, Tuple{Hypergraph{T,V,E},Union{Nothing, E},Int64}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.set_hyperedge_meta!",
    "category": "method",
    "text": "set_hyperedge_meta!(h::Hypergraph{T, V, E}, new_value::Union{E,Nothing}, id::Int)\n    where {T <: Real, V, E}\n\nSets a new meta value new_value for the hyperedge id in the hypegraph h.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.get_hyperedge_meta-Union{Tuple{E}, Tuple{V}, Tuple{T}, Tuple{Hypergraph{T,V,E},Int64}} where E where V where T<:Real",
    "page": "Reference",
    "title": "SimpleHypergraphs.get_hyperedge_meta",
    "category": "method",
    "text": "get_hyperedge_meta(h::Hypergraph{T, V, E}, id::Int) where {T <: Real, V, E}\n\nReturns a meta value stored at the hyperedge id in the hypergraph h.\n\n\n\n\n\n"
},

{
    "location": "reference/#Manipulating-vertices-and-hyperedges-1",
    "page": "Reference",
    "title": "Manipulating vertices and hyperedges",
    "category": "section",
    "text": "\nadd_hyperedge!(::Hypergraph{T, V, E}; ::Dict{Int,T}, ::Union{E,Nothing} ) where {T <: Real, V, E}\nadd_vertex!(::Hypergraph{T, V, E};::Dict{Int,T},::Union{V,Nothing} ) where {T <: Real, V, E}\nset_vertex_meta!(::Hypergraph{T, V, E}, ::Union{V,Nothing}, ::Int) where {T <: Real, V, E}\nget_vertex_meta(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}\nset_hyperedge_meta!(::Hypergraph{T, V, E}, ::Union{E,Nothing}, ::Int) where {T <: Real, V, E}\nget_hyperedge_meta(::Hypergraph{T, V, E}, ::Int) where {T <: Real, V, E}"
},

{
    "location": "reference/#Base.getindex-Tuple{Hypergraph,Int64,Int64}",
    "page": "Reference",
    "title": "Base.getindex",
    "category": "method",
    "text": "Base.getindex(h::Hypergraph, idx::Vararg{Int,2})\n\nReturns a value for a given vertex-hyperedge pair idx for a hypergraph h. If a vertex does not belong to a hyperedge nothing is returned.\n\n\n\n\n\n"
},

{
    "location": "reference/#Base.setindex!-Tuple{Hypergraph,Nothing,Int64,Int64}",
    "page": "Reference",
    "title": "Base.setindex!",
    "category": "method",
    "text": "Base.setindex!(h::Hypergraph, ::Nothing, idx::Vararg{Int,2})\n\nRemoves a vertex from a given hyperedge for a hypergraph h and a given vertex-hyperedge pair idx. Note that trying to remove a vertex from a hyperedge when it is not present will not throw an error.\n\n\n\n\n\n"
},

{
    "location": "reference/#Base.setindex!-Tuple{Hypergraph,Real,Int64,Int64}",
    "page": "Reference",
    "title": "Base.setindex!",
    "category": "method",
    "text": "Base.setindex!(h::Hypergraph, v::Real, idx::Vararg{Int,2})\n\nAdds a vertex to a hyperedge (represented by indices idx) and assigns value v to be stored with that assignment.\n\n\n\n\n\n"
},

{
    "location": "reference/#Hypergraph-array-getters-and-setters-1",
    "page": "Reference",
    "title": "Hypergraph array getters and setters",
    "category": "section",
    "text": "Normally you work with a hypergraph via array setters, for example the code below craete an Hypergraph and add vertex one to hyperedges 2 and 3 with weight 5:h = Hypergraph{Int64}(2,3);\nh[1, 2:3] .= 5;  \nh\n\n# output\n\n2×3 Hypergraph{Int64,Nothing,Nothing}:\n nothing  5         5\n nothing   nothing   nothinggetindex(::Hypergraph, ::Vararg{Int,2})\nsetindex!(::Hypergraph, ::Nothing, ::Vararg{Int,2})\nsetindex!(::Hypergraph, ::Real, ::Vararg{Int,2})"
},

{
    "location": "reference/#Base.size-Tuple{Hypergraph}",
    "page": "Reference",
    "title": "Base.size",
    "category": "method",
    "text": "Base.size(h::Hypergraph)\n\nReturns the size of Hypergraph h. The result is a tuple of the number of vertices and the number of hyperedges\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.getvertices-Tuple{Hypergraph,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.getvertices",
    "category": "method",
    "text": "getvertices(h::Hypergraph, he_id::Int)\n\nReturns vertices from a hypergraph a for a given hyperedge he_id.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.gethyperedges-Tuple{Hypergraph,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.gethyperedges",
    "category": "method",
    "text": "gethyperedges(h::Hypergraph, v_id::Int)\n\nReturns hyperedges for a given vertex v_id in a hypergraph h.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.BipartiteView",
    "page": "Reference",
    "title": "SimpleHypergraphs.BipartiteView",
    "category": "type",
    "text": "BipartiteView{T<:Real} <: AbstractGraph{Int64}\n\nRepresents a bipartite view of a hypergraph h. Note this is a view - changes to the original hypergraph will be automatically reflected in the view.\n\nConstructors\n\nBipartiteView(::Hypergraph)\n\nThe bipartite view of a hypergraph is suitable for processing with the LightGraphs.jl package. Several LightGraphs methods are provided for the compability.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.shortest_path-Tuple{BipartiteView,Int64,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.shortest_path",
    "category": "method",
    "text": "shortest_path(b::BipartiteView,source::Int, target::Int)\n\nFinds a single shortest path in a graph b between vertices source and target. Note that if several paths of the same length exist, only one will be returned.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.TwoSectionView",
    "page": "Reference",
    "title": "SimpleHypergraphs.TwoSectionView",
    "category": "type",
    "text": "TwoSectionView{T<:Real} <: AbstractGraph{Int64}\n\nRepresents a 2-section view of a hypergraph h. Note this is a view - changes to the original hypergraph will be automatically reflected in the view.\n\nConstructors\n\nTwoSectionView(::Hypergraph)\n\nThe 2-section view of a hypergraph is suitable for processing with the LightGraphs.jl package. Several LightGraphs methods are provided for the compability.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.shortest_path-Tuple{TwoSectionView,Int64,Int64}",
    "page": "Reference",
    "title": "SimpleHypergraphs.shortest_path",
    "category": "method",
    "text": "shortest_path(t::TwoSectionView,source::Int, target::Int)\n\nFinds a single shortest path in a graph b between vertices source and target. Note that if several paths of the same length exist, only one will be returned.\n\n\n\n\n\n"
},

{
    "location": "reference/#Hypergraph-info-1",
    "page": "Reference",
    "title": "Hypergraph info",
    "category": "section",
    "text": "size(::Hypergraph)\ngetvertices(::Hypergraph, ::Int)\ngethyperedges(::Hypergraph, ::Int)\n\nBipartiteView\nshortest_path(::BipartiteView, ::Int, ::Int)\n\nTwoSectionView\nshortest_path(::TwoSectionView, ::Int, ::Int)"
},

{
    "location": "reference/#SimpleHypergraphs.hg_save",
    "page": "Reference",
    "title": "SimpleHypergraphs.hg_save",
    "category": "function",
    "text": "hg_save(io::IO, h::Hypergraph)\n\nSaves a hypergraph h to an output stream io.\n\n\n\n\n\nhg_save(fname::AbstractString, h::Hypergraph)\n\nSaves a hypergraph h to a file fname.\n\n\n\n\n\n"
},

{
    "location": "reference/#SimpleHypergraphs.hg_load",
    "page": "Reference",
    "title": "SimpleHypergraphs.hg_load",
    "category": "function",
    "text": "hg_load(fname::AbstractString, T::Type{<:Real})\n\nLoads a hypergraph from a stream io. The second argument T represents type of data in the hypegraph.\n\nSkips an initial comment.\n\n\n\n\n\nhg_load(fname::AbstractString, T::Type{<:Real})\n\nLoads a hypergraph from a file fname. The second argument T represents type of data in the hypegraph\n\n\n\n\n\n"
},

{
    "location": "reference/#I/O-1",
    "page": "Reference",
    "title": "I/O",
    "category": "section",
    "text": "hg_save\nhg_loadDocTestSetup = nothing"
},

]}
