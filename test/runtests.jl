using Test, SimpleHypergraphs
import LightGraphs

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
    @test read(path, String) == replace(read("data/test1.hgf", String), "\r\n" => "\n")
end

h1 = Hypergraph{Float64}(5,4)
h1[1:3,1] .= 1.5
h1[3,4] = 2.5
h1[2,3] = 3.5
h1[4,3:4] .= 4.5
h1[5,4] = 5.5
h1[5,2] = 6.5

h2 = Hypergraph{Float64}(0,0)
for i in 1:4 add_vertex!(h2) end
add_hyperedge!(h2;vertices=Dict(1:3 .=> 1.5))
add_hyperedge!(h2)
add_vertex!(h2;hyperedges=Dict(2=>6.5))
add_hyperedge!(h2;vertices=Dict(2 => 3.5, 4 => 4.5))
add_hyperedge!(h2;vertices=Dict(3:5 .=> (2.5,4.5,5.5)))
@test h1 == h2



b = BipartiteView(h1)

@test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(b))) == 18

@test sort(collect(LightGraphs.outneighbors(b,5))) == [7,9]
@test sort(collect(LightGraphs.outneighbors(b,1))) == [6]
@test sort(collect(LightGraphs.inneighbors(b,9))) == [3,4,5]

@test Set(LightGraphs.vertices(b)) == Set(1:LightGraphs.nv(b))

@test shortest_path(b,1,5) == [1,3,5]
@test LightGraphs.is_weakly_connected(b) == true

@test SimpleHypergraphs.add_vertex!(h1) == 6
@test add_hyperedge!(h1) == 5
h1[5,5] = 1
h1[6,5] = 1

@test shortest_path(b,1,6) == [1,3,5,6]
