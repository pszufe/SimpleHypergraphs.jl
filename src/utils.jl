function cast_value(val::Union{String, Int}, t::Type{String}) 
    return string(val)
end


function cast_value(val::Int, t::Type{Int})
    return val
end


function handle_metadata(metadata::Array)
    result = Vector{Union{String, Int}}()

    if any(isnothing, metadata)
        append!(result, 1:length(metadata))
    else
        append!(result, metadata)
    end

    return result
end
