using JSON3

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
    incidences = Vector{Dict{String, Union{String, Number}}}()
    v_meta = handle_metadata(h.v_meta)

    node_dict = Dict(i => val for (i, val) in pairs(v_meta))

    for node_idx = eachindex(v_meta)
        edges = gethyperedges(h, node_idx)
        node = cast_value(node_dict[node_idx], V)
        
        for (_edge, weight) in edges
            edge = cast_value(_edge, E)
            push!(incidences, Dict("edge" => edge, "node" => node, "weight" => weight))
        end
    end

    json_hg[:incidences] = incidences

    JSON3.write(io, json_hg)
end

function cast_value(val::Union{String, Int}, t::Type{String}) 
    return string(val)
end


function cast_value(val::Union{Int, JSON3.Object}, t::Type{Union{Int, JSON3.Object}})
    return val
end


function handle_metadata(metadata::Array)
    result = Vector{Union{String, Int, JSON3.Object}}()

    if any(isnothing, metadata)
        append!(result, 1:length(metadata))
    else
        append!(result, metadata)
    end

    return result
end
