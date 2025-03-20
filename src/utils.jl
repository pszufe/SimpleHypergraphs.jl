function cast_value(val::Union{String, Int}, t::Type{String}) 
    return string(val)
end


function cast_value(val::Int, t::Type{Int})
    return val
end
