using Test, SimpleHypergraphs

h = hg_load("data/test1.hgf")
@test size(h) == (4, 4)
m = Matrix(h)
@test m == h
@test h == [true  false true  false
            true  true  false false
            false false true  false
            false false true  false]
mktemp("data") do path, _
    println(path)
    hg_save(path, h)
    @test read(path, String) == read("data/test1.hgf", String)
end
