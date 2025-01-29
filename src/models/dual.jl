
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

"""
    dual(h::BasicHypergraph)

Return the dual of the basic hypergraph `h`.

NOTE
`h` needs to have at least one dimension greater than 0.
"""
function dual(h::BasicHypergraph)
    @assert(nhv(h)>0 || nhe(h)>0)

    T = nhv(h) > 0 ? eltype(values(h.v2he[1])) : eltype(values(h.he2v[1]))

    mx = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))

    for v=1:nhv(h)
        for he in keys(h.v2he[v])
            mx[he, v] = h.v2he[v][he]
        end
    end

    BasicHypergraph{T}(mx)
end

"""
    dual(h::DirectedHypergraph)

Return the dual of the directed hypergraph `h`.

NOTE
`h` needs to have at least one dimension greater than 0.
"""
function dual(h::DirectedHypergraph)
    @assert(nhv(h)>0 || nhe(h)>0)

    T = nhv(h) > 0 ? eltype(values(h.hg_tail.v2he[1])) : eltype(values(h.hg_tail.he2v[1]))
    V = isa(eltype(h.he_meta_tail), Union) ? eltype(h.he_meta_tail).b : Nothing
    E = isa(eltype(h.v_meta), Union) ? eltype(h.v_meta).b : Nothing

    mx_tail = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))
    mx_head = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))

    for v=1:nhv(h)
        the, hhe = gethyperedges(h, v)

        for he in keys(the)
            mx_tail[he, v] = h.hg_tail[v,he]
        end

        for he in keys(hhe)
            mx_head[he, v] = h.hg_head[v,he]
        end
    end

    # This is awkward, but it allows us to combine tail and head metadata
    new_v_meta = Vector{Union{Tuple{Union{V, Nothing}, Union{V, Nothing}}, Nothing}}(nothing, nhe(h))

    for e=1:nhe(h)
        new_v_meta[e] = (h.he_meta_tail[e], h.he_meta_head[e])
    end

    DirectedHypergraph{T, Tuple{Union{V, Nothing}, Union{V, Nothing}}, E}(mx_tail, mx_head; v_meta=new_v_meta, he_meta_tail=h.v_meta, he_meta_head=h.v_meta)
end

"""
    dual(h::BasicDirectedHypergraph)

Return the dual of the basic directed hypergraph `h`.

NOTE
`h` needs to have at least one dimension greater than 0.
"""
function dual(h::BasicDirectedHypergraph)
    @assert(nhv(h)>0 || nhe(h)>0)

    T = nhv(h) > 0 ? eltype(values(h.hg_tail.v2he[1])) : eltype(values(h.hg_tail.he2v[1]))

    mx_tail = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))
    mx_head = Matrix{Union{Nothing,T}}(nothing, nhe(h), nhv(h))

    for v=1:nhv(h)
        the, hhe = gethyperedges(h, v)

        for he in keys(the)
            mx_tail[he, v] = h.hg_tail[v,he]
        end

        for he in keys(hhe)
            mx_head[he, v] = h.hg_head[v,he]
        end
    end

    BasicDirectedHypergraph{T}(mx_tail, mx_head)
end