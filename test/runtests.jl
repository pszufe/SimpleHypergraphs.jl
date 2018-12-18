using Test, SimpleHypergraphs

h = hg_load("data/test1.hgf", Int)
@test size(h) == (4, 4)
m = Matrix(h)
@test m == h
@test h == [1       nothing 4       nothing
            2       3       nothing nothing
            nothing nothing 5       nothing
            nothing nothing 6       nothing]
mktemp("data") do path, _
    println(path)
    hg_save(path, h)
    @test read(path, String) == read("data/test1.hgf", String)
end
