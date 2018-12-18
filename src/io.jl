# TODO: maybe more fancy file format and correctness checking should be done

function hg_save(io::IO, h::Hypergraph)
    println(io, length(h.v2he), " ", length(h.he2v))
    foreach(he -> println(io, join(sort(collect(he)), ' ')), h.he2v)
end

hg_save(fname::AbstractString, h::Hypergraph) =
    open(io -> hg_save(io, h), fname, "w")

function hg_load(io::IO)
    l = split(readline(io))
    length(l) == 2 || throw(ArgumentError("expected two integers"))
    n, k = parse.(Int, l)
    h = Hypergraph(n, k)
    for i in 1:k
        he = parse.(Int, split(readline(io)))
        if length(unique(he)) != length(he)
            throw(ArgumentError("non-unique entries found in $i-th hyperedge"))
        end
        h[he, i] .= true
    end
    # we ignore lines beyond k+1 in the file
    h
end

hg_load(fname::AbstractString) = open(io -> hg_load(io), fname, "r")
