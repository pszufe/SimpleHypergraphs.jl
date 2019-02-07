# TODO: maybe more fancy file format and correctness checking should be done


"""
    hg_save(io::IO, h::Hypergraph)

Saves a hypergraph `h` to an output stream `io`.

"""
function hg_save(io::IO, h::Hypergraph)
    println(io, length(h.v2he), " ", length(h.he2v))
    for he in h.he2v
        skeys = sort(collect(keys(he)))
        println(io, join(["$k=$(he[k])" for k in skeys], ' '))
    end
end

"""
    hg_save(fname::AbstractString, h::Hypergraph)

Saves a hypergraph `h` to a file `fname`.

"""
hg_save(fname::AbstractString, h::Hypergraph) =
    open(io -> hg_save(io, h), fname, "w")

"""
    hg_load(fname::AbstractString, T::Type{<:Real})

Loads a hypergraph from a stream `io`. The second argument
`T` represents type of data in the hypegraph

Skips an initial comment.

"""
function hg_load(io::IO, T::Type{<:Real})
    line = readline(io)

    if startswith(line, "\"\"\"")
      singleline = true
        while(
            !( (!singleline && endswith(line, "\"\"\"")) ||
            (singleline && endswith(line, "\"\"\"") && length(line)>5)
            ) &&
            !eof(io)
            )
                line = readline(io)
                singleline = false
        end
        if eof(io)
            throw(ArgumentError("malformed input"))
        end
       line = readline(io)
    end
    
    l = split(line)
    length(l) == 2 || throw(ArgumentError("expected two integers"))
    n, k = parse.(Int, l)
    h = Hypergraph{T}(n, k)
    lastv = 0
    for i in 1:k
        for pos in split(readline(io))
            entry = split(pos, '=')
            length(entry) == 2 || throw(ArgumentError("expected vertex=weight"))
            v = parse(Int, entry[1])
            w = parse(T, entry[2])
            if v > lastv
                lasti = v
            else
                throw(ArgumentError("vertices in hyperedge must be sorted"))
            end
            h[v, i] = w
        end
    end
    # we ignore lines beyond k+1 in the file
    h
end

"""
    hg_load(fname::AbstractString, T::Type{<:Real})

Loads a hypergraph from a file `fname`. The second argument
`T` represents type of data in the hypegraph

"""
hg_load(fname::AbstractString, T::Type{<:Real}) =
    open(io -> hg_load(io, T), fname, "r")
