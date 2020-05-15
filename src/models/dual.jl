
"""
    dual(h::Hypergraph)

Return the dual of the hypergraph `h`.

NOTE
`h` needs to have at least one dimension greater than 0.
"""

function dual(h::Hypergraph)
    @assert(nhv(h)>0 || nhe(h)>0)

    T = nhv(h) > 0 ? eltype(values(h.v2he[1])) : eltype(values(h.he2v[1]))
    V = isa(eltype(h.he_meta), Union) ? eltype(h.he_meta).b : Nothing
    E = isa(eltype(h.v_meta), Union) ? eltype(h.v_meta).b : Nothing

    mx = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))

    for v=1:nhv(h)
        for he in keys(h.v2he[v])
            mx[he, v] = h.v2he[v][he]
        end
    end

    Hypergraph{T, V, E}(mx; v_meta=h.he_meta, he_meta=h.v_meta)
end
