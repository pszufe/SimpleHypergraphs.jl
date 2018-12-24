# TODO: maybe more fancy file format and correctness checking should be done


"""
    hg_save(io::IO, h::Hypergraph)

Saves a hypergraph to a stream.

**Arguments**

* `io` : the output stream to be written to
* `h` : a hypergraph

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

Saves a hypergraph to a file.

**Arguments**

* `fname` : a file name to save to
* `h` : a hypergraph

"""
hg_save(fname::AbstractString, h::Hypergraph) =
    open(io -> hg_save(io, h), fname, "w")

"""
    hg_load(fname::AbstractString, T::Type{<:Real})

Loads a hypergraph from a stream.

**Arguments**

* `io` : an input stream from which to load the data
* `T` : type of data in the hypegraph

"""
function hg_load(io::IO, T::Type{<:Real})
    l = split(readline(io))
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

Loads a hypergraph from a file.

**Arguments**

* `fname` : a file from which to load the data
* `T` : type of data in the hypegraph

"""
hg_load(fname::AbstractString, T::Type{<:Real}) =
    open(io -> hg_load(io, T), fname, "r")
