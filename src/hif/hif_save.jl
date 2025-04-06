using JSON3

HIFEntryType = Dict{String, Union{String, Number, Dict{String, Any}}}

"""
    hg_save(io::IO, h::Hypergraph, format::HIF_Format)

Saves a hypergraph `h` to an output stream `io` in `HIF` format.

If `h` has `Composite Types` either for vertex metadata or hyperedges metadata,
the user has to explicit tell the JSON3 package about it, for instance using:

`JSON3.StructType(::Type{MyType}) = JSON3.Struct()`.

See the (JSON3.jl documentation)[https://github.com/quinnj/JSON3.jl] for more details.

"""
function hg_save(io::IO, h::Hypergraph{T, V, E, D}, format::HIF_Format) where {T, V, E, D}
    _ = format

    json_hg = Dict{Symbol,Any}()

    nodes_meta = prepare_metadata(h.v_meta, handle_node)
    edges_meta = prepare_metadata(h.he_meta, handle_edge)

    incidences = prepare_incidences(h)
    
    json_hg[:incidences] = incidences

    if !isempty(nodes_meta)
        json_hg[:nodes] = nodes_meta
    end

    if !isempty(edges_meta)
        json_hg[:edges] = edges_meta
    end

    JSON3.write(io, json_hg)
end


function prepare_incidences(h::Hypergraph{T, V, E, D}) where {T, V, E, D}
    incidences = Vector{HIFEntryType}()

    for node_idx in eachindex(h.v_meta)
        edges = gethyperedges(h, node_idx)

        node = isnothing(h.v_meta[node_idx]) ? node_idx : h.v_meta[node_idx]

        _node = (V == Dict{String, Any}) ? node["node"] : node

        for (edge, weight) in edges
            _edge = isnothing(h.he_meta[edge]) ? edge : h.he_meta[edge]
            if T == Bool
                push!(incidences, Dict("edge" => _edge, "node" => _node))
            else
                push!(incidences, Dict("edge" => _edge, "node" => _node, "weight" => weight))
            end
        end
    end

    return incidences
end


function prepare_metadata(
    metadata::Vector{Union{T, Nothing}}, 
    handling_func::Function
) where {T}
    result = Vector{HIFEntryType}()

    for item in metadata
        if isnothing(item)
            continue
        end

        handled = handling_func(item)
        push!(result, handled)
    end

    return result
end


function handle_node(node::Union{String, Int})
    return Dict{String, Union{String, Int}}(
        "node" => node
    )
end

function handle_node(node::Dict{String, Any})
    result = HIFEntryType(
        "node" => node["node"]
    )

    add_optional_params!(result, node)

    return result
end


function handle_edge(edge::Union{String, Int})
    return Dict{String, Union{String, Int}}(
        "edge" => edge
    )
end


function handle_edge(edge::Dict{String, Any})
    result = HIFEntryType(
        "edge" => edge["edge"]
    )

    add_optional_params!(result, edge)

    return result
end


function add_optional_params!(result::HIFEntryType, item::Dict{String, Any})
    if haskey(item, "weight")
        result["weight"] = item["weight"]
    end

    if haskey(item, "attrs")
        result["attrs"] = item["attrs"]
    end
end
