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


@testset "SimpleHypergraphs Hypergraph      " begin

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
    @test add_vertex!(h3;v_meta="test") == 2
    @test set_vertex_meta!(h3,"t",1) == ["t","test"]
    @test get_vertex_meta(h3,2) == "test"
    @test get_hyperedge_meta(h3,1) == nothing
    @test_throws BoundsError get_hyperedge_meta(h3,2)

    h4 = Hypergraph{Float64,Nothing,String}(1,1)
    @test add_hyperedge!(h4;he_meta="test") == 2
    @test set_hyperedge_meta!(h4,"t",1) == ["t","test"]
    @test get_hyperedge_meta(h4,2) == "test"
    @test get_vertex_meta(h4,1) == nothing
    @test_throws BoundsError get_vertex_meta(h4,2)

    h1_0 = deepcopy(h1)
    @test add_vertex!(h1_0) == 6
    h1_0[6,:] = h1_0[5,:]
    @test remove_vertex!(h1_0,5) == h1

end;

@testset "SimpleHypergraphs BipartiteView   " begin
    h2 = deepcopy(h1)

    @test LightGraphs.nv(LightGraphs.zero(BipartiteView{Int})) == 0

    b = BipartiteView(h2)
    @test LightGraphs.edgetype(b) == LightGraphs.SimpleGraphs.SimpleEdge{Int}
    @test LightGraphs.has_vertex(b, 0) == false
    @test LightGraphs.has_vertex(b, 1) == true
    @test LightGraphs.has_edge(b, 1, 1) == false
    @test LightGraphs.nv(LightGraphs.zero(b)) == 0

    @test LightGraphs.is_directed(b) == false
    @test LightGraphs.is_directed(typeof(b)) == false
    @test LightGraphs.eltype(b) == Int


    @test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(b))) == 18

    @test sort(collect(LightGraphs.outneighbors(b,5))) == [7,9]
    @test sort(collect(LightGraphs.outneighbors(b,1))) == [6]
    @test sort(collect(LightGraphs.inneighbors(b,9))) == [3,4,5]

    @test Set(LightGraphs.vertices(b)) == Set(1:LightGraphs.nv(b))

    @test shortest_path(b,1,5) == [1,3,5]
    @test LightGraphs.is_weakly_connected(b) == true


    @test add_vertex!(h2) == 6
    @test add_hyperedge!(h2) == 5
    h2[5,5] = 1
    h2[6,5] = 1

    @test shortest_path(b,1,6) == [1,3,5,6]

    bipartite_graph = LightGraphs.SimpleGraph(b)

    @test LightGraphs.SimpleGraphs.fadj(bipartite_graph)==LightGraphs.SimpleGraphs.fadj(b)
    @test LightGraphs.nv(b) == 11
    @test LightGraphs.ne(b) == 11

    @test sort!(LightGraphs.SimpleGraphs.fadj(b,1)) == [7]
    @test sort!(LightGraphs.SimpleGraphs.fadj(b,2)) == [7,9]
end;

@testset "SimpleHypergraphs TwoSectionView  " begin

    ht = Hypergraph{Float64}(3,3)
    ht[1:2,1:2] .= 2.
    ht[:, 3] .= 2.

    add_vertex!(h1)
    add_hyperedge!(h1)
    h1[5,5] = 1
    h1[6,5] = 1

    @test LightGraphs.nv(LightGraphs.zero(TwoSectionView{Int})) == 0

    t = TwoSectionView(h1)
    @test LightGraphs.edgetype(t) == LightGraphs.SimpleGraphs.SimpleEdge{Int}
    @test LightGraphs.has_vertex(t, 0) == false
    @test LightGraphs.has_vertex(t, 1) == true
    @test LightGraphs.nv(LightGraphs.zero(t)) == 0

    @test LightGraphs.is_directed(t) == false
    @test LightGraphs.is_directed(typeof(t)) == false
    @test LightGraphs.eltype(t) == Int

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

    Random.seed!(0);
    g = LightGraphs.erdos_renyi(8, 0.3)
    h_from_g = Hypergraph(g)
    @test LightGraphs.adjacency_matrix(g) == LightGraphs.adjacency_matrix(TwoSectionView(h_from_g))
    @test minimum([sum((h_from_g .== true)[:,n]) for n in 1:6] .== 2)
    @test LightGraphs.modularity(g,[1,1,2,2,3,3,4,4]) ≈ modularity(h_from_g, Set.([[1,2],[3,4],[5,6],[7,8]]))
    @test LightGraphs.SimpleGraphs.fadj(g) == LightGraphs.SimpleGraphs.fadj(TwoSectionView(h_from_g))
end;



@testset "SimpleHypergraphs Modularity      " begin
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
    @test modularity(hh, Set.([[1,2,3],[4],[5],[6],[7]]), ha) ≈ 223/972
    cfmr = CFModularityRandom(2,10000)
    @test cfmr.n==2
    @test cfmr.reps == 10000
    @test findcommunities(hh,cfmr).bm ≈ 16/81
    Random.seed!(1234);
    cnm = CFModularityCNMLike(100)
    @test cnm.reps == 100
    @test findcommunities(hh, CFModularityRandom(4,10000)).bm ≈ findcommunities(hh, cnm).bm
    Random.seed!(0);
    @test findcommunities(hh, cnm).bm ≈ 223/972


end;
                                     #
@testset "SimpleHypergraphs randomized tests" begin
    Random.seed!(0)
    N = 100
    res = Vector{Bool}(undef, N)
    for i in 1:N
        m1 = CFModularityCNMLike(100)
        m2 = CFModularityRandom(2,100)
        r = rand([repeat([nothing],6)..., true], 12, 8)
        hh = Hypergraph(r)
        bm1 = findcommunities(hh, m1).bm
        bm2 = findcommunities(hh, m2).bm
        res[i] = (bm1 > bm2)
    end
    @test sum(res) >= N*0.80
end

@testset "randomwalk" begin
    h1 = Hypergraph{Float64}(5,4)
    h1[1:3,1] .= 1.5
    h1[3,4] = 2.5
    h1[2,3] = 3.5
    h1[4,3:4] .= 4.5
    h1[5,4] = 5.5
    h1[5,2] = 6.5
    # randomized, unseeded tests
    w1 = countmap([random_walk(h1, 1) for _ in 1:10^6])
    @test keys(w1) == Set([1,2,3])
    @test -(extrema(values(w1))...) > -10000
    w2 = countmap([random_walk(h1, 2) for _ in 1:10^6])
    @test keys(w2) == Set([1,2,3,4])
    @test abs((w2[2]-w2[4]) - w2[1]) < 10000
    @test abs(w2[1]-w2[3]) < 10000
    w5 = countmap([random_walk(h1, 5) for _ in 1:10^6])
    @test keys(w5) == Set([3,4,5])
    @test abs(w5[3]-w5[4]) < 10000
    @test abs(w5[5]-w5[4]-500000) < 10000
    @test_throws ArgumentError random_walk(h1, 0)
end

@testset "SimpleHypergraphs connected components" begin
    bip = LightGraphs.SimpleGraph(BipartiteView(h1))
    cc = LightGraphs.connected_components(bip)
    filter!.(x -> x <= nhv(h1), cc)
    filter!(!isempty, cc)

    cc2 = SimpleHypergraphs.get_connected_components(h1)
    @test sort!(sort!.(cc)) == sort!(sort!.(cc2))
    @test typeof(cc2) == Vector{Vector{Int}}
end
