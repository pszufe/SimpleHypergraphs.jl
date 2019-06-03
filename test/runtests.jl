using Test, SimpleHypergraphs
using Random
import LightGraphs

h1 = Hypergraph{Float64}(5,4)
h1[1:3,1] .= 1.5
h1[3,4] = 2.5
h1[2,3] = 3.5
h1[4,3:4] .= 4.5
h1[5,4] = 5.5
h1[5,2] = 6.5


@testset "SimpleHypergraphs Hypergraph    " begin

    h = hg_load("data/test1.hgf", Int)
    @test size(h) == (4, 4)
    @test nhv(h) == 4
    @test nhe(h) == 4
    m = Matrix(h)
    @test m == h
    @test h == [1       nothing 4       nothing
                2       3       nothing nothing
                nothing nothing 5       nothing
                nothing nothing 6       nothing]
    mktemp("data") do path, _
        println(path)
        hg_save(path, h)

        loaded_hg = replace(read(path, String), r"\n*$" => "")

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/test1.hgf", String)) #no comments

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/test_singlelinecomment.hgf", String)) #single line comment

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/test_multiplelinescomment.hgf", String)) #multiple lines comment
    end

    @test_throws ArgumentError hg_load("data/test_malformedcomment.hgf", Int)

    h2 = Hypergraph{Float64}(0,0)
    for i in 1:4 add_vertex!(h2) end
    add_hyperedge!(h2;vertices=Dict(1:3 .=> 1.5))
    add_hyperedge!(h2)
    add_vertex!(h2;hyperedges=Dict(2=>6.5))
    add_hyperedge!(h2;vertices=Dict(2 => 3.5, 4 => 4.5))
    add_hyperedge!(h2;vertices=Dict(3:5 .=> (2.5,4.5,5.5)))
    @test h1 == h2
    m = Matrix(h1)
    @test  m == Matrix(h2)
    @test h1 == Hypergraph(m)
    @test h1 == Hypergraph{Nothing}(m)
    @test h1 == Hypergraph{Nothing, Nothing}(m)
    @test getindex(h1,3,1) == 1.5

    h3 = Hypergraph{Float64,String,Nothing}(1,1)
    @test add_vertex!(h3,vertex_meta="test") == 2
    @test set_vertex_meta!(h3,"t",1) == ["t","test"]
    @test get_vertex_meta(h3,2) == "test"
    @test get_hyperedge_meta(h3,1) == nothing
    @test_throws BoundsError get_hyperedge_meta(h3,2)

    h4 = Hypergraph{Float64,Nothing,String}(1,1)
    @test add_hyperedge!(h4,hyperedge_meta="test") == 2
    @test set_hyperedge_meta!(h4,"t",1) == ["t","test"]
    @test get_hyperedge_meta(h4,2) == "test"
    @test get_vertex_meta(h4,1) == nothing
    @test_throws BoundsError get_vertex_meta(h4,2)
end;

@testset "SimpleHypergraphs BipartiteView " begin
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

end;

@testset "SimpleHypergraphs TwoSectionView" begin

    t = TwoSectionView(h1)

    @test LightGraphs.nv(t) == 6
    @test LightGraphs.ne(t) == 8

    @test sort(LightGraphs.all_neighbors(t, 1)) == [2,3]
    @test sort(LightGraphs.outneighbors(t, 5)) == [3,4,6]
    @test sort(LightGraphs.inneighbors(t, 4)) == [2,3,5]
    @inferred LightGraphs.all_neighbors(t, 1)

    @test LightGraphs.has_edge(t, 1, 2) == true
    @test LightGraphs.has_edge(t, 1, 5) == false

    @test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(t))) == 16
    @test shortest_path(t,1,5) == [1,3,5]
    @test LightGraphs.is_weakly_connected(t) == true

    @test SimpleHypergraphs.add_vertex!(h1) == 7
    h1[7,5] = 1

    @test shortest_path(t,1,6) == [1,3,5,6]

    @test LightGraphs.nv(t) == 7
    @test LightGraphs.ne(t) == 10
    @test sort(LightGraphs.outneighbors(t, 5)) == [3,4,6,7]

    @test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(t))) == 20

end;



@testset "SimpleHypergraphs Modularity    " begin
    Random.seed!(1234);
    hg = Hypergraph{Bool}(10, 12)
    for i in eachindex(hg)
        if rand() < 0.2
            hg[i] = true
        end
    end
   
    cfmr = CFModularityRandom(3,10000)   
    
    
    @test findcommunities(hg,cfmr) ==
         (bp = Set.([[4, 5, 9], [1, 3, 6, 7], [2, 8, 10]]), bm = 0.21505688117829677)
    @test modularity(hg,  Set.([1:10])) == 0.0
    Random.seed!(1234);
    @test randompartition(hg, 2) == Set.([[1, 5, 6, 7, 9], [2, 3, 4, 8, 10]])

    hh = Hypergraph{Bool}(7,4)
    hh[1,1] = true
    hh[2,1:2] .= true
    hh[3,1:3] .= true
    hh[4,4] = true
    hh[5:6,3] .= true
    @test nhv(hh) == 7
    @test nhe(hh) == 4

    @test modularity(hh,Set.([[1,2,3],[4],[5],[6],[7]])) ≈ 223/972
    @test modularity(hh,Set.([[1,2,3],[4,5,6,7]])) ≈ 14/72
    @test modularity(hh,Set.([[1,2,3,5,6],[4,7]])) ≈ 16/81
    @test modularity(hh,Set.([[1,2,3,5,6],[4],[7]])) ≈ 16/81
    @test modularity(hh, Set.([1:nhv(hh)])) == 0.0    
    ha = SimpleHypergraphs.HypergraphAggs(hh)
    @test ha.hes == [3, 2, 3, 1]
    @test ha.max_hes == 3
    @test ha.deg_vs == [1, 2, 3, 1, 1, 1, 0]
    @test ha.volV == 9
    @test modularity(hh, ha, Set.([[1,2,3],[4],[5],[6],[7]])) ≈ 223/972
    cfmr = CFModularityRandom(2,10000)
    @test findcommunities(hh,cfmr).bm ≈ 16/81

end;
