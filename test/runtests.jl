ENV["MPLBACKEND"]="agg" # no GUI
using PyPlot, PyCall
@info("SimpleHypergraphs is using Matplotlib $(PyPlot.version) with Python $(PyCall.pyversion)")

using Test, SimpleHypergraphs, StatsBase
using Random
using DataStructures
import Graphs


h1 = Hypergraph{Float64, Int, String}(5,4)
h1[1:3,1] .= 1.5
h1[3,4] = 2.5
h1[2,3] = 3.5
h1[4,3:4] .= 4.5
h1[5,4] = 5.5
h1[5,2] = 6.5

h1_basic = BasicHypergraph{Float64}(h1)

dh1 = DirectedHypergraph{Float64, Int, String}(7,6)
dh1[1,1,1] = 1.0
dh1.hg_head[2:3,1] .= 2.5  # Assignment on a directed hypergraph directly with slices is currently awkward
dh1[1,3,3] = 4.0
dh1[2,4,3] = 5.5
dh1[1,5,4] = 7.0
dh1[2,6,4] = 8.5
dh1[1,6,5] = 10.0
dh1[2,7,5] = -1.5
dh1[1,7,6] = 0.0
dh1[2,5,6] = 1.5

dh1_basic = BasicDirectedHypergraph{Float64}(dh1.hg_tail, dh1.hg_head)


@testset "SimpleHypergraphs Hypergraph             " begin

    h = hg_load("data/test1.hgf"; T=Int, HType=Hypergraph)
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

        for v=1:nhv(h)
            set_vertex_meta!(h1, v, v)
        end

        for he=1:nhe(h)
            set_hyperedge_meta!(h1, string(he), he)
        end

        hg_save(path, h1; format=JSON_Format())
        loaded_hg = hg_load(path; format=JSON_Format(), HType=Hypergraph, T=Float64, V=Int, E=String)

        @test h1 == loaded_hg
        @test h1.v_meta == loaded_hg.v_meta
        @test h1.he_meta == loaded_hg.he_meta

        @test get_vertex_meta(h1, 1) == get_vertex_meta(loaded_hg, 1)
        @test get_hyperedge_meta(h1, 2) == get_hyperedge_meta(loaded_hg, 2)

    end

    @test_throws ArgumentError hg_load("data/test_malformedcomment.hgf"; T=Int)
    @test_throws ArgumentError hg_load("data/test_argumenterror.hgf"; T=Int)

    h2 = Hypergraph{Float64}(0,0)
    @test h2 == Hypergraph{Float64,Nothing}(0,0)
    @test h2 == Hypergraph{Float64,Nothing,Nothing}(0,0)
    @test h2 == Hypergraph{Float64,Nothing,Nothing,Dict{Int,Float64}}(0,0)

    h3 = Hypergraph(0,0)
    @test h3 == Hypergraph{Bool, Nothing, Nothing, Dict{Int, Bool}}(0,0)

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
    @test h1 == Hypergraph{Float64}(m)
    @test h1 == Hypergraph{Float64,Nothing}(m)
    @test h1 == Hypergraph{Float64,Nothing, Nothing}(m)
    @test h1 == Hypergraph{Float64,Nothing, Nothing,Dict{Int,Float64}}(m)
    @test all(Matrix(h1) .== Matrix(Hypergraph{Float64,Nothing, Nothing,SortedDict{Int,Float64}}(m)))
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

    h5 = Hypergraph{Float64,String,String,SortedDict{Int,Float64}}(1,1)
    @test typeof(h5.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(h5.he2v[1]) <: SortedDict{Int,Float64}
    @test add_vertex!(h5;v_meta="test") == 2
    @test set_vertex_meta!(h5,"t",1) == ["t","test"]
    @test get_vertex_meta(h5,2) == "test"
    @test get_hyperedge_meta(h5,1) == nothing
    @test add_hyperedge!(h5;he_meta="test") == 2
    @test set_hyperedge_meta!(h5,"t",1) == ["t","test"]
    @test get_hyperedge_meta(h5,2) == "test"
    @test_throws BoundsError get_vertex_meta(h5,3)
    @test_throws BoundsError get_hyperedge_meta(h5,3)
    h5 .= [1.0 2.0;3.0 4.0]
    @test h5[2,2] == 4

    h1_0 = deepcopy(h1)
    @test add_vertex!(h1_0) == 6
    h1_0[6,:] = h1_0[5,:]
    @test remove_vertex!(h1_0,5) == h1
    setindex!(h1_0, nothing, 1, 1)
    @test h1_0[1,1] == nothing
    @test_throws BoundsError setindex!(h1_0, nothing, 10, 9)

    h1_1 = Hypergraph([nothing nothing nothing nothing
                       1       1       nothing nothing
                       nothing nothing 1       nothing
                       nothing nothing 1       nothing])
    @test add_hyperedge!(h1_1) == 5
    @test size(remove_hyperedge!(h1_1, 5))[2] == 4
    @test add_vertex!(h1_1) == 5
    @test add_hyperedge!(h1_1) == 5
    hp = prune_hypergraph(h1_1)
    @test size(hp)[1] == 3 && size(h)[1] == 4
    @test size(hp)[2] == 3 && size(h)[1] == 4
    prune_hypergraph!(h1_1)
    @test size(h1_1)[1] == 3
    @test size(h1_1)[2] == 3
end;

@testset "SimpleHypergraphs BasicHypergraph        " begin
    h = hg_load("data/test1.hgf"; T=Int, HType=BasicHypergraph)
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

        hg_save(path, h1_basic; format=JSON_Format())
        loaded_hg = hg_load(path; format=JSON_Format(), HType=BasicHypergraph, T=Float64)

        @test h1_basic == loaded_hg

    end

    h2 = BasicHypergraph{Float64}(0,0)
    @test h2 == BasicHypergraph{Float64, Dict{Int,Float64}}(0,0)

    h3 = BasicHypergraph(0,0)
    @test h3 == BasicHypergraph{Bool, Dict{Int, Bool}}(0,0)

    for i in 1:4 add_vertex!(h2) end
    add_hyperedge!(h2;vertices=Dict(1:3 .=> 1.5))
    add_hyperedge!(h2)
    add_vertex!(h2;hyperedges=Dict(2=>6.5))
    add_hyperedge!(h2;vertices=Dict(2 => 3.5, 4 => 4.5))
    add_hyperedge!(h2;vertices=Dict(3:5 .=> (2.5,4.5,5.5)))
    @test h1_basic == h2
    m = Matrix(h1)
    @test  m == Matrix(h2)
    @test h1_basic == Hypergraph(m)
    @test h1_basic == Hypergraph{Float64}(m)
    @test h1_basic == Hypergraph{Float64,Dict{Int,Float64}}(m)
    @test all(Matrix(h1_basic) .== Matrix(Hypergraph{Float64, SortedDict{Int,Float64}}(m)))
    @test getindex(h1_basic,3,1) == 1.5

    h5 = BasicHypergraph{Float64,SortedDict{Int,Float64}}(1,1)
    @test typeof(h5.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(h5.he2v[1]) <: SortedDict{Int,Float64}
    @test add_vertex!(h5) == 2
    @test add_hyperedge!(h5) == 2
    h5 .= [1.0 2.0;3.0 4.0]
    @test h5[2,2] == 4

    h1_0 = deepcopy(h1_basic)
    @test add_vertex!(h1_0) == 6
    h1_0[6,:] = h1_0[5,:]
    @test remove_vertex!(h1_0,5) == h1_basic
    setindex!(h1_0, nothing, 1, 1)
    @test h1_0[1,1] === nothing
    @test_throws BoundsError setindex!(h1_0, nothing, 10, 9)

    h1_1 = BasicHypergraph([nothing nothing nothing nothing
                       1       1       nothing nothing
                       nothing nothing 1       nothing
                       nothing nothing 1       nothing])
    @test add_hyperedge!(h1_1) == 5
    @test size(remove_hyperedge!(h1_1, 5))[2] == 4
    @test add_vertex!(h1_1) == 5
    @test add_hyperedge!(h1_1) == 5
    hp = prune_hypergraph(h1_1)
    @test size(hp)[1] == 3 && size(h)[1] == 4
    @test size(hp)[2] == 3 && size(h)[1] == 4
    prune_hypergraph!(h1_1)
    @test size(h1_1)[1] == 3
    @test size(h1_1)[2] == 3
end;

@testset "SimpleHypergraphs DirectedHypergraph     " begin
    h = hg_load("data/test_dhg.ehgf"; format=EHGF_Format(), T=Int, HType=DirectedHypergraph)
    @test size(h) == (6, 3)
    @test nhv(h) == 6
    @test nhe(h) == 3
    m = Matrix(h)
    @test m == h
    @test h == [
        (1, nothing)        (nothing, nothing)  (nothing, nothing)
        (2, nothing)        (3, nothing)        (nothing, nothing)
        (nothing, nothing)  (nothing, 0)        (nothing, nothing)
        (nothing, 4)        (nothing, nothing)  (1, nothing)
        (nothing, 5)        (12, nothing)       (nothing, nothing)
        (nothing, nothing)  (nothing, nothing)  (nothing, 4)
   ]
    mktemp("data") do path, _
        println(path)
        hg_save(path, h; format=EHGF_Format())

        loaded_hg = replace(read(path, String), r"\n*$" => "")

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/test_dhg.ehgf", String)) #no comments

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/singlelinecomment.ehgf", String)) #single line comment

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/multilinecomment.ehgf", String)) #multiple lines comment

        for v=1:nhv(dh1)
            set_vertex_meta!(dh1, v, v)
        end

        for he=1:nhe(dh1)
            set_hyperedge_meta!(dh1, string(he), string(he), he)
        end

        hg_save(path, dh1; format=JSON_Format())
        loaded_hg = hg_load(path; format=JSON_Format(), HType=DirectedHypergraph, T=Float64, V=Int, E=String)

        @test dh1 == loaded_hg
        @test dh1.v_meta == loaded_hg.v_meta
        @test dh1.he_meta_tail == loaded_hg.he_meta_tail
        @test dh1.he_meta_head == loaded_hg.he_meta_head

        @test get_vertex_meta(dh1, 1) == get_vertex_meta(loaded_hg, 1)
        @test get_hyperedge_meta(dh1, 2) == get_hyperedge_meta(loaded_hg, 2)

    end

    @test_throws ArgumentError hg_load("data/malformedcomment.ehgf"; format=EHGF_Format(), HType=DirectedHypergraph, T=Int)
    @test_throws ArgumentError hg_load("data/argumenterror.ehgf"; format=EHGF_Format(), HType=DirectedHypergraph, T=Int)

    dh2 = DirectedHypergraph{Float64}(0,0)
    @test dh2 == DirectedHypergraph{Float64,Nothing}(0,0)
    @test dh2 == DirectedHypergraph{Float64,Nothing,Nothing}(0,0)
    @test dh2 == DirectedHypergraph{Float64,Nothing,Nothing,Dict{Int,Float64}}(0,0)

    dh3 = DirectedHypergraph(0,0)
    @test dh3 == DirectedHypergraph{Bool, Nothing, Nothing, Dict{Int, Bool}}(0,0)

    for i in 1:6 add_vertex!(dh2) end
    add_hyperedge!(dh2;vertices_tail=Dict(1 => 1.0), vertices_head=Dict(2:3 .=> 2.5))
    add_hyperedge!(dh2)
    add_hyperedge!(dh2;vertices_tail=Dict(3 => 4.0), vertices_head=Dict(4 => 5.5))
    add_hyperedge!(dh2;vertices_tail=Dict(5 => 7.0), vertices_head=Dict(6 => 8.5))
    add_hyperedge!(dh2;vertices_tail=Dict(6 => 10.0))
    add_hyperedge!(dh2;vertices_head=Dict(5 => 1.5))
    add_vertex!(dh2;hyperedges_tail=Dict(6 => 0.0), hyperedges_head=Dict(5 => -1.5))
    @test dh1 == dh2
    mtail = Matrix(dh1.hg_tail)
    mhead = Matrix(dh1.hg_head)
    @test mtail == Matrix(dh2.hg_tail)
    @test mhead == Matrix(dh2.hg_head)
    @test dh1 == DirectedHypergraph(mtail, mhead)
    @test dh1 == DirectedHypergraph{Float64}(mtail, mhead)
    @test dh1 == DirectedHypergraph{Float64,Nothing}(mtail, mhead)
    @test dh1 == DirectedHypergraph{Float64,Nothing, Nothing}(mtail, mhead)
    @test dh1 == DirectedHypergraph{Float64,Nothing, Nothing,Dict{Int,Float64}}(mtail, mhead)
    @test all(Matrix(dh1.hg_tail) .== Matrix(DirectedHypergraph{Float64,Nothing, Nothing,SortedDict{Int,Float64}}(mtail, mhead).hg_tail))
    @test all(Matrix(dh1.hg_head) .== Matrix(DirectedHypergraph{Float64,Nothing, Nothing,SortedDict{Int,Float64}}(mtail, mhead).hg_head))
    @test getindex(dh1,5,4) == (7.0, nothing)

    dh4 = DirectedHypergraph{Float64,String,Nothing}(1,1)
    @test add_vertex!(dh4;v_meta="test") == 2
    @test set_vertex_meta!(dh4,"t",1) == ["t","test"]
    @test get_vertex_meta(dh4,2) == "test"
    @test get_hyperedge_meta(dh4,1) == (nothing, nothing)
    @test_throws BoundsError get_hyperedge_meta(dh4,2)

    dh5 = DirectedHypergraph{Float64,Nothing,String}(1,1)
    @test add_hyperedge!(dh5;he_meta_tail="test") == 2
    @test set_hyperedge_meta!(dh5,"t", "h", 1) == (["t", "test"], ["h", nothing])
    @test get_hyperedge_meta(dh5,2) == ("test", nothing)
    @test get_vertex_meta(dh5,1) === nothing
    @test_throws BoundsError get_vertex_meta(dh5,2)

    dh6 = DirectedHypergraph{Float64,String,String,SortedDict{Int,Float64}}(1,1)
    @test typeof(dh6.hg_tail.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(dh6.hg_head.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(dh6.hg_tail.he2v[1]) <: SortedDict{Int,Float64}
    @test typeof(dh6.hg_head.he2v[1]) <: SortedDict{Int,Float64}
    @test add_vertex!(dh6;v_meta="test") == 2
    @test set_vertex_meta!(dh6,"t",1) == ["t","test"]
    @test get_vertex_meta(dh6,2) == "test"
    @test get_hyperedge_meta(dh6,1) == (nothing, nothing)
    @test add_hyperedge!(dh6;he_meta_tail="test") == 2
    @test set_hyperedge_meta!(dh6,"t", "h", 1) == (["t", "test"], ["h", nothing])
    @test get_hyperedge_meta(dh6,2) == ("test", nothing)
    @test_throws BoundsError get_vertex_meta(dh6,3)
    @test_throws BoundsError get_hyperedge_meta(dh6,3)
    dh6.hg_tail .= [1.0 2.0;3.0 4.0]
    @test dh6[2,2] == (4.0, nothing)

    dh1_0 = deepcopy(dh1)
    @test add_vertex!(dh1_0) == 8
    dh1_0.hg_tail[8,:] = dh1_0.hg_tail[7,:]
    dh1_0.hg_head[8,:] = dh1_0.hg_head[7,:]
    @test remove_vertex!(dh1_0,8) == dh1
    setindex!(dh1_0, nothing, 1, 1)
    @test dh1_0[1,1] == (nothing, nothing)
    @test_throws BoundsError setindex!(dh1_0, nothing, 10, 9)

    dh1_1 = DirectedHypergraph(
        [
            1 nothing nothing
            nothing nothing 2
            nothing nothing nothing
        ],
        [
            nothing nothing 2
            2 nothing 1
            nothing nothing nothing
        ]
    )
    @test add_hyperedge!(dh1_1) == 4
    @test size(remove_hyperedge!(dh1_1, 4))[2] == 3
    @test add_vertex!(dh1_1) == 4
    @test add_hyperedge!(dh1_1) == 4
    hp = prune_hypergraph(dh1_1)
    @test size(hp)[1] == 2 && size(dh1_1)[1] == 4
    @test size(hp)[2] == 2 && size(dh1_1)[2] == 4
    prune_hypergraph!(dh1_1)
    @test size(dh1_1)[1] == 2
    @test size(dh1_1)[2] == 2
end;


@testset "SimpleHypergraphs BasicDirectedHypergraph" begin
    h = hg_load("data/test_dhg.ehgf"; format=EHGF_Format(), T=Int, HType=BasicDirectedHypergraph)
    @test size(h) == (6, 3)
    @test nhv(h) == 6
    @test nhe(h) == 3
    m = Matrix(h)
    @test m == h
    @test h == [
        (1, nothing)        (nothing, nothing)  (nothing, nothing)
        (2, nothing)        (3, nothing)        (nothing, nothing)
        (nothing, nothing)  (nothing, 0)        (nothing, nothing)
        (nothing, 4)        (nothing, nothing)  (1, nothing)
        (nothing, 5)        (12, nothing)       (nothing, nothing)
        (nothing, nothing)  (nothing, nothing)  (nothing, 4)
   ]
    mktemp("data") do path, _
        println(path)
        hg_save(path, h; format=EHGF_Format())

        loaded_hg = replace(read(path, String), r"\n*$" => "")

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/test_dhg.ehgf", String)) #no comments

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/singlelinecomment.ehgf", String)) #single line comment

        @test loaded_hg ==
            reduce(replace,
                ["\r\n"=>"\n",
                r"^\"\"\"(?s).*\"\"\"\n"=>"", #remove initial comments
                r"\n*$"=>""], #remove final \n*
                init=read("data/multilinecomment.ehgf", String)) #multiple lines comment

        hg_save(path, dh1; format=JSON_Format())
        loaded_hg = hg_load(path; format=JSON_Format(), HType=BasicDirectedHypergraph, T=Float64)

        @test dh1 == loaded_hg

    end

    @test_throws ArgumentError hg_load("data/malformedcomment.ehgf"; format=EHGF_Format(), HType=BasicDirectedHypergraph, T=Int)
    @test_throws ArgumentError hg_load("data/argumenterror.ehgf"; format=EHGF_Format(), HType=BasicDirectedHypergraph, T=Int)

    dh2 = BasicDirectedHypergraph{Float64}(0,0)
    @test dh2 == BasicDirectedHypergraph{Float64}(0,0)
    @test dh2 == BasicDirectedHypergraph{Float64}(0,0)
    @test dh2 == BasicDirectedHypergraph{Float64,Dict{Int,Float64}}(0,0)

    dh3 = BasicDirectedHypergraph(0,0)
    @test dh3 == BasicDirectedHypergraph{Bool, Dict{Int, Bool}}(0,0)

    for i in 1:6 add_vertex!(dh2) end
    add_hyperedge!(dh2;vertices_tail=Dict(1 => 1.0), vertices_head=Dict(2:3 .=> 2.5))
    add_hyperedge!(dh2)
    add_hyperedge!(dh2;vertices_tail=Dict(3 => 4.0), vertices_head=Dict(4 => 5.5))
    add_hyperedge!(dh2;vertices_tail=Dict(5 => 7.0), vertices_head=Dict(6 => 8.5))
    add_hyperedge!(dh2;vertices_tail=Dict(6 => 10.0))
    add_hyperedge!(dh2;vertices_head=Dict(5 => 1.5))
    add_vertex!(dh2;hyperedges_tail=Dict(6 => 0.0), hyperedges_head=Dict(5 => -1.5))
    @test dh1 == dh2
    mtail = Matrix(dh1.hg_tail)
    mhead = Matrix(dh1.hg_head)
    @test mtail == Matrix(dh2.hg_tail)
    @test mhead == Matrix(dh2.hg_head)
    @test dh1 == BasicDirectedHypergraph(mtail, mhead)
    @test dh1 == BasicDirectedHypergraph{Float64}(mtail, mhead)
    @test dh1 == BasicDirectedHypergraph{Float64,Dict{Int,Float64}}(mtail, mhead)
    @test all(Matrix(dh1.hg_tail) .== Matrix(BasicDirectedHypergraph{Float64,SortedDict{Int,Float64}}(mtail, mhead).hg_tail))
    @test all(Matrix(dh1.hg_head) .== Matrix(BasicDirectedHypergraph{Float64,SortedDict{Int,Float64}}(mtail, mhead).hg_head))
    @test getindex(dh1,5,4) == (7.0, nothing)

    dh3 = BasicDirectedHypergraph{Float64,SortedDict{Int,Float64}}(1,1)
    @test typeof(dh3.hg_tail.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(dh3.hg_head.v2he[1]) <: SortedDict{Int,Float64}
    @test typeof(dh3.hg_tail.he2v[1]) <: SortedDict{Int,Float64}
    @test typeof(dh3.hg_head.he2v[1]) <: SortedDict{Int,Float64}

    dh1_0 = deepcopy(dh1)
    @test add_vertex!(dh1_0) == 8
    dh1_0.hg_tail[8,:] = dh1_0.hg_tail[7,:]
    dh1_0.hg_head[8,:] = dh1_0.hg_head[7,:]
    @test remove_vertex!(dh1_0,8) == dh1
    setindex!(dh1_0, nothing, 1, 1)
    @test dh1_0[1,1] == (nothing, nothing)
    @test_throws BoundsError setindex!(dh1_0, nothing, 10, 9)

    dh1_1 = BasicDirectedHypergraph(
        [
            1 nothing nothing
            nothing nothing 2
            nothing nothing nothing
        ],
        [
            nothing nothing 2
            2 nothing 1
            nothing nothing nothing
        ]
    )
    @test add_hyperedge!(dh1_1) == 4
    @test size(remove_hyperedge!(dh1_1, 4))[2] == 3
    @test add_vertex!(dh1_1) == 4
    @test add_hyperedge!(dh1_1) == 4
    hp = prune_hypergraph(dh1_1)
    @test size(hp)[1] == 2
    @test size(dh1_1)[1] == 4
    @test size(hp)[2] == 2
    @test size(dh1_1)[1] == 4
    prune_hypergraph!(dh1_1)
    @test size(dh1_1)[1] == 2
    @test size(dh1_1)[2] == 2
end;


@testset "SimpleHypergraphs BipartiteView          " begin
    h2 = deepcopy(h1)

    @test Graphs.nv(Graphs.zero(BipartiteView{Hypergraph{Int}})) == 0

    b = BipartiteView(h2)
    @test Graphs.edgetype(b) == Graphs.SimpleGraphs.SimpleEdge{Int}
    @test Graphs.has_vertex(b, 0) == false
    @test Graphs.has_vertex(b, 1) == true
    @test Graphs.has_edge(b, 1, 1) == false
    @test Graphs.nv(Graphs.zero(b)) == 0

    @test Graphs.is_directed(b) == false
    @test Graphs.is_directed(typeof(b)) == false
    @test Graphs.eltype(b) == Int


    @test sum(Graphs.adjacency_matrix(Graphs.SimpleGraph(b))) == 18

    @test sort(collect(Graphs.outneighbors(b,5))) == [7,9]
    @test sort(collect(Graphs.outneighbors(b,1))) == [6]
    @test sort(collect(Graphs.inneighbors(b,9))) == [3,4,5]

    @test Set(Graphs.vertices(b)) == Set(1:Graphs.nv(b))

    @test shortest_path(b,1,5) == [1,3,5]
    @test Graphs.is_weakly_connected(b) == true


    @test add_vertex!(h2) == 6
    @test add_hyperedge!(h2) == 5
    h2[5,5] = 1
    h2[6,5] = 1

    @test shortest_path(b,1,6) == [1,3,5,6]

    bipartite_graph = Graphs.SimpleGraph(b)

    @test Graphs.SimpleGraphs.fadj(bipartite_graph)==Graphs.SimpleGraphs.fadj(b)
    @test Graphs.nv(b) == 11
    @test Graphs.ne(b) == 11

    @test sort!(Graphs.SimpleGraphs.fadj(b,1)) == [7]
    @test sort!(Graphs.SimpleGraphs.fadj(b,2)) == [7,9]


    dh2 = deepcopy(dh1)

    @test Graphs.nv(Graphs.zero(BipartiteView{BasicDirectedHypergraph{Int}})) == 0

    b = BipartiteView(dh2)
    @test Graphs.edgetype(b) == Graphs.SimpleGraphs.SimpleEdge{Int}
    @test Graphs.has_vertex(b, 0) == false
    @test Graphs.has_vertex(b, 1) == true
    @test Graphs.has_edge(b, 1, 1) == false
    @test Graphs.nv(Graphs.zero(b)) == 0

    @test Graphs.is_directed(b) == true
    @test Graphs.is_directed(typeof(b)) == true
    @test Graphs.eltype(b) == Int


    @test sum(Graphs.adjacency_matrix(Graphs.SimpleDiGraph(b))) == 11

    @test sort(collect(Graphs.outneighbors(b,5))) == [11]
    @test sort(collect(Graphs.outneighbors(b,1))) == [8]
    @test sort(collect(Graphs.inneighbors(b,2))) == [8]

    @test Set(Graphs.vertices(b)) == Set(1:Graphs.nv(b))

    @test shortest_path(b,1,4) == [1, 3, 4]
    @test shortest_path(b,1,5) == Int64[]
    @test Graphs.is_weakly_connected(b) == false

    @test add_vertex!(dh2) == 8
    @test add_hyperedge!(dh2) == 7
    dh2[1,4,7] = 1
    dh2[2,8,7] = 1

    @test shortest_path(b,1,8) == [1,3,4,8]

    bipartite_graph = Graphs.SimpleDiGraph(b)

    @test Graphs.SimpleGraphs.fadj(bipartite_graph)==Graphs.SimpleGraphs.fadj(b)
    @test Graphs.nv(b) == 15
    @test Graphs.ne(b) == 13

    @test sort!(Graphs.SimpleGraphs.fadj(b,1)) == [9]
    @test sort!(Graphs.SimpleGraphs.fadj(b,2)) == Int64[]
    @test sort!(Graphs.SimpleGraphs.badj(b,2)) == [9]
end;


@testset "SimpleHypergraphs TwoSectionView         " begin

    ht = Hypergraph{Float64}(3,3)
    ht[1:2,1:2] .= 2.
    ht[:, 3] .= 2.

    add_vertex!(h1)
    add_hyperedge!(h1)
    h1[5,5] = 1
    h1[6,5] = 1

    @test Graphs.nv(Graphs.zero(TwoSectionView{BasicHypergraph{Int64}})) == 0

    t = TwoSectionView(h1)
    @test Graphs.edgetype(t) == Graphs.SimpleGraphs.SimpleEdge{Int}
    @test Graphs.has_vertex(t, 0) == false
    @test Graphs.has_vertex(t, 1) == true
    @test Graphs.nv(Graphs.zero(t)) == 0

    @test Graphs.is_directed(t) == false
    @test Graphs.is_directed(typeof(t)) == false
    @test Graphs.eltype(t) == Int

    @test Graphs.nv(t) == 6
    @test Graphs.ne(t) == 8

    @test sort(Graphs.all_neighbors(t, 1)) == [2,3]
    @test sort(Graphs.outneighbors(t, 5)) == [3,4,6]
    @test sort(Graphs.inneighbors(t, 4)) == [2,3,5]
    @inferred Graphs.all_neighbors(t, 1)

    @test Graphs.has_edge(t, 1, 2) == true
    @test Graphs.has_edge(t, 1, 5) == false

    @test sum(Graphs.adjacency_matrix(Graphs.SimpleGraph(t))) == 16
    @test shortest_path(t,1,5) == [1,3,5]
    @test Graphs.is_weakly_connected(t) == true

    @test SimpleHypergraphs.add_vertex!(h1) == 7
    h1[7,5] = 1

    @test shortest_path(t,1,6) == [1,3,5,6]

    @test Graphs.ne(t) == 10
    @test Graphs.nv(t) == 7
    @test sort(Graphs.outneighbors(t, 5)) == [3,4,6,7]

    @test sum(Graphs.adjacency_matrix(Graphs.SimpleGraph(t))) == 20

    Random.seed!(0);
    g = Graphs.erdos_renyi(8, 0.3)
    h_from_g = Hypergraph(g)
    @test Graphs.adjacency_matrix(g) == Graphs.adjacency_matrix(TwoSectionView(h_from_g))
    @test minimum([sum((h_from_g .== true)[:,n]) for n in 1:6] .== 2)
    @test Graphs.modularity(g,[1,1,2,2,3,3,4,4]) ≈ modularity(h_from_g, Set.([[1,2],[3,4],[5,6],[7,8]]))
    @test Graphs.SimpleGraphs.fadj(g) == Graphs.SimpleGraphs.fadj(TwoSectionView(h_from_g))

    ht = BasicDirectedHypergraph{Float64}(3,3)
    ht[1,1,1] = 1
    ht.hg_head[2:3,1] .= 2
    ht[1,2,2] = 2
    ht[2,1,2] = 2
    ht[1,3,3] = 3
    ht[2,1,3] = 3

    @test Graphs.nv(Graphs.zero(TwoSectionView{BasicDirectedHypergraph{Int64}})) == 0

    t = TwoSectionView(dh1)
    @test Graphs.edgetype(t) == Graphs.SimpleGraphs.SimpleEdge{Int}
    @test Graphs.has_vertex(t, 0) == false
    @test Graphs.has_vertex(t, 1) == true
    @test Graphs.nv(Graphs.zero(t)) == 0

    @test Graphs.is_directed(t) == true
    @test Graphs.is_directed(typeof(t)) == true
    @test Graphs.eltype(t) == Int

    @test Graphs.nv(t) == 7
    @test Graphs.ne(t) == 6

    @test sort(Graphs.all_neighbors(t, 1)) == [2,3]
    @test sort(Graphs.outneighbors(t, 5)) == [6]
    @test sort(Graphs.inneighbors(t, 4)) == [3]
    @inferred Graphs.all_neighbors(t, 1)

    @test Graphs.has_edge(t, 1, 2) == true
    @test Graphs.has_edge(t, 1, 5) == false

    @test sum(Graphs.adjacency_matrix(Graphs.SimpleDiGraph(t))) == 6
    @test shortest_path(t,1,4) == [1,3,4]
    @test Graphs.is_weakly_connected(t) == false
    @test Graphs.is_strongly_connected(t) == false

    add_vertex!(dh1)
    add_hyperedge!(dh1)
    dh1[1,4,7] = 1
    dh1[2,8,7] = 1

    @test Graphs.ne(t) == 7
    @test Graphs.nv(t) == 8
    @test sort(Graphs.outneighbors(t, 4)) == [8]

    @test shortest_path(t,1,8) == [1,3,4,8]

    @test sum(Graphs.adjacency_matrix(Graphs.SimpleDiGraph(t))) == 7

    Random.seed!(0);
    g = Graphs.erdos_renyi(8, 0.3; is_directed=true)
    h_from_g = BasicDirectedHypergraph(g)
    @test Graphs.adjacency_matrix(g) == Graphs.adjacency_matrix(TwoSectionView(h_from_g))
    @test minimum([sum((h_from_g.hg_tail .== true)[:,n]) for n in 1:6] .== 1)
    @test minimum([sum((h_from_g.hg_head .== true)[:,n]) for n in 1:6] .== 1)
    @test Graphs.SimpleGraphs.fadj(g) == Graphs.SimpleGraphs.fadj(TwoSectionView(h_from_g))
    @test Graphs.SimpleGraphs.badj(g) == Graphs.SimpleGraphs.badj(TwoSectionView(h_from_g))
    
end;


@testset "SimpleHypergraphs random-models          " begin

    Hᵣ = random_model(5, 5, BasicHypergraph)
    @test nhv(Hᵣ) == 5
    @test nhe(Hᵣ) == 5
    @test  all(length.(Hᵣ.v2he) .> 0)
    @test  all(length.(Hᵣ.v2he) .<= 5)

    Hᵣ2 = random_model(5, 0, BasicHypergraph)
    add_hyperedge!(Hᵣ2;vertices=Dict(2 => true, 4 => true))
    @test nhv(Hᵣ2) == 5
    @test nhe(Hᵣ2) == 1

    Hᵣ3  = random_model(5,1,BasicHypergraph)
    @test nhe(Hᵣ3) == 1

    Hκ = random_kuniform_model(5, 5, 3, BasicHypergraph)
    @test nhv(Hκ) == 5
    @test nhe(Hκ) == 5
    @test all(length.(Hκ.he2v) .== 3)

    Hδ = random_dregular_model(5, 5, 3, BasicHypergraph)
    @test nhv(Hδ) == 5
    @test nhe(Hδ) == 5
    @test all(length.(Hδ.v2he) .== 3)

    # TODO: you are here
    H∂ = random_preferential_model(20, 0.5, BasicHypergraph)
    @test nhv(H∂) == 20

    DHᵣ = random_model(5, 5, BasicDirectedHypergraph)
    @test nhv(DHᵣ) == 5
    @test nhe(DHᵣ) == 5
    @test  all(length.(DHᵣ.hg_tail.v2he) .> 0)
    @test  all(length.(DHᵣ.hg_head.v2he) .> 0)
    @test  all(length.(DHᵣ.hg_tail.v2he) .<= 5)
    @test  all(length.(DHᵣ.hg_head.v2he) .<= 5)

    @test_throws ErrorException random_model(1, 2, BasicDirectedHypergraph; no_self_loops=true)

    DHr_nsl = random_model(5, 5, BasicDirectedHypergraph; no_self_loops=true)
    for i in 1:5
        @test length(intersect(keys(DHr_nsl.hg_tail.v2he[i]), keys(DHr_nsl.hg_head.v2he[i]))) == 0
    end

    DHᵣ2 = random_model(5, 0, BasicDirectedHypergraph)
    add_hyperedge!(DHᵣ2;vertices_tail=Dict(2 => true, 4 => true),vertices_head=Dict(1 => true, 5 => true))
    @test nhv(DHᵣ2) == 5
    @test nhe(DHᵣ2) == 1

    DHκ = random_kuniform_model(5, 5, 3, BasicDirectedHypergraph)
    @test nhv(Hκ) == 5
    @test nhe(Hκ) == 5
    @test all(length.(DHκ.hg_tail.he2v) .+ length.(DHκ.hg_head.he2v) .== 3)

    @test_throws ErrorException random_kuniform_model(1, 3, 1, BasicDirectedHypergraph; no_self_loops=true)

    DHκ_nsl = random_kuniform_model(5, 5, 3, BasicDirectedHypergraph; no_self_loops=true)
    for i in 1:5
        @test length(intersect(keys(DHκ_nsl.hg_tail.v2he[i]), keys(DHκ_nsl.hg_head.v2he[i]))) == 0
    end

    DHδ = random_dregular_model(5, 5, 3, BasicDirectedHypergraph)
    @test nhv(DHδ) == 5
    @test nhe(DHδ) == 5
    @test all(length.(DHδ.hg_tail.v2he) .+ length.(DHδ.hg_head.v2he) .== 3)

    @test_throws ErrorException random_dregular_model(1, 3, 1, BasicDirectedHypergraph; no_self_loops=true)

    DHδ_nsl = random_kuniform_model(5, 5, 3, BasicDirectedHypergraph; no_self_loops=true)
    for i in 1:5
        @test length(intersect(keys(DHκ_nsl.hg_tail.v2he[i]), keys(DHκ_nsl.hg_head.v2he[i]))) == 0
    end

end;


@testset "SimpleHypergraphs modularity          " begin
    Random.seed!(1234);
    hg = Hypergraph{Bool}(10, 12)
    for i in eachindex(hg)
        if rand() < 0.2
            hg[i] = true
        end
    end

    cfmr = CFModularityRandom(3,10000)

    @test 0.18 < findcommunities(hg,cfmr).bm < 0.22
    @test modularity(hg,  Set.([1:10])) == 0.0
    Random.seed!(1234);
    @test typeof(randompartition(hg, 2)) == Vector{Set{Int}}
    @test length(randompartition(hg, 2)) == 2
    @test sort(vcat(collect.(randompartition(hg, 2))...))==1:nhv(hg)
    
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
    cnm = CFModularityCNMLike(1000)
    @test cnm.reps == 1000
    @test findcommunities(hh, CFModularityRandom(4,10000)).bm ≈ findcommunities(hh, cnm).bm
    Random.seed!(0);
    @test any(isapprox.(findcommunities(hh, cnm).bm, [16/81, 223/972]))
end;


@testset "SimpleHypergraphs randomized tests       " begin
    Random.seed!(0)
    N = 500
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
    @test sum(res) >= N*0.75
end


@testset "SimpleHypergraphs label propagation      " begin
    Random.seed!(1234);
    hg = Hypergraph{Bool}(10, 12)
    for i in eachindex(hg)
        if rand() < 0.2
            hg[i] = true
        end
    end

    cflp = CFLabelPropagationFinder(100, 1234)
    @test_throws AssertionError findcommunities(hg, cflp)

    h = Hypergraph(11, 2)
    h[1:5, 1] .= true
    h[5:11, 2] .= true

    comms = findcommunities(h, cflp)
    @test typeof(comms.np) <: Vector{Set{Int}}
    @test typeof(comms.hep) <: Vector{Set{Int}}
    #@test comms.np == [Set([7, 9, 10, 11, 8, 5, 6]), Set([4, 2, 3, 1])]
    #@test comms.hep == Set[Set([2]), Set([1])]

    add_hyperedge!(h)
    comms = findcommunities(h, cflp)
    @test typeof(comms.helabels) <: Vector{Int64}
    #@test comms.helabels == [4, 7, -1]
end;


@testset "SimpleHypergraphs randomwalk             " begin
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

    dh = BasicDirectedHypergraph{Float64}(8,9)
    dh[1,1,1] = 1.0
    dh.hg_head[2:3,1] .= 2.5
    dh[1,3,2] = 0.5
    dh[2,4,2] = 1.5
    dh.hg_tail[3:4,3] .= 2.0
    dh[2,1,3] = 0.5
    dh[1,4,4] = 2.5
    dh[2,5,4] = 1.0
    dh[1,3,5] = 2.0
    dh[2,5,5] = 1.0
    dh[1,5,6] = 1.5
    dh[2,3,6] = 0.5
    dh[1,6,7] = 1.0
    dh[2,7,7] = 2.5
    dh[1,7,8] = 2.0
    dh[2,8,8] = 1.5
    dh[1,8,9] = 1.0
    dh[2,6,9] = 1.5

    dw1 = countmap([random_walk(dh, 1) for _ in 1:10^6])
    @test keys(dw1) == Set([2,3])
    @test -(extrema(values(dw1))...) > -10000

    dw1_rev = countmap([random_walk(dh, 1; reverse=true) for _ in 1:10^6])
    @test keys(dw1_rev) == Set([3, 4])
    @test -(extrema(values(dw1_rev))...) > -10000

    dw2 = countmap([random_walk(dh, 6) for _ in 1:10^6])
    @test keys(dw2) == Set([7])
    @test -(extrema(values(dw2))...) > -10000

    @test_throws ArgumentError random_walk(dh, 0)

end

@testset "SimpleHypergraphs connected components" begin
    bip = Graphs.SimpleGraph(BipartiteView(h1))
    cc = Graphs.connected_components(bip)
    filter!.(x -> x <= nhv(h1), cc)
    filter!(!isempty, cc)

    cc2 = SimpleHypergraphs.get_connected_components(h1)
    @test sort!(sort!.(cc)) == sort!(sort!.(cc2))
    @test typeof(cc2) == Vector{Vector{Int}}

    weak_conn = sort!(get_weakly_connected_components(dh1))
    @test length(weak_conn) == 2
    @test weak_conn[1] == [1,2,3,4,8]
    @test weak_conn[2] == [5,6,7]

    strong_conn = sort!(get_strongly_connected_components(dh1))
    @test length(strong_conn) == 6
    @test strong_conn[1] == [1]
    @test strong_conn[2] == [2]
    @test strong_conn[3] == [3]
    @test strong_conn[4] == [4]
    @test strong_conn[5] == [5,6,7]
    @test strong_conn[6] == [8]
end


@testset "SimpleHypergraphs hypernetx bridge       " begin

    if (!SimpleHypergraphs.support_hypernetx())
        @warn "HyperNetX is not installed. Skipping hypernetx tests"
        return
    end

    h_hnx = SimpleHypergraphs._convert_to_hnx(h1)
    data = Dict{String, Array{Int, 1}}(
        "1" => [1, 2, 3],
        "2" => [5],
        "3" => [2, 4],
        "4" => [3, 4, 5],
        "5" => [5, 6, 7]
    )
    h2 = SimpleHypergraphs.hnx.Hypergraph(data)
	#TODO update for HyperNetX 2.0.0
    #@test h_hnx == h2

    h_hnx =
        SimpleHypergraphs._convert_to_hnx(
            h1,
            node_labels = Dict{Int, String}(
                1=>"A", 2=>"B", 3=>"C", 4=>"D", 5=>"E", 6=>"F", 7=>"G"),
            edge_labels = Dict{Int, String}(
                1=>"HE1", 2=>"HE2", 3=>"HE3", 4=>"HE4", 5=>"HE5"
            ))
    data = Dict{String, Array{String, 1}}(
        "HE1" => ["A", "B", "C"],
        "HE2" => ["E"],
        "HE3" => ["B", "D"],
        "HE4" => ["C", "D", "E"],
        "HE5" => ["E", "F", "G"]
    )
    h2 = SimpleHypergraphs.hnx.Hypergraph(data)
	#TODO update for HyperNetX 2.0.0
    #@test h_hnx == h2

    @test SimpleHypergraphs.get_next_div_id() == 1
    @test SimpleHypergraphs.get_next_div_id() == 2
end;


@testset "SimpleHypergraphs conductance            " begin
  h = Hypergraph{Float64, Int}(5,4)
  h[1:3,1] .= 1
  h[3,4] = 1
  h[2,3] = 1
  h[4,3:4] .= 1
  h[5,4] = 1
  h[5,2] = 1
  @test SimpleHypergraphs.conductance(h, Set(1:3)) == 5 / 5
  @test SimpleHypergraphs.conductance(h, Set(1)) == 3 / 1
  @test SimpleHypergraphs.conductance(h, Set([1, 4])) == 8 / 3
  @test SimpleHypergraphs.conductance(h, Set(2:5)) == 3 / 8
  h = Hypergraph{Int}(6, 7)
  h[1:3, 1] .= 1
  h[1:2, 2] .= 1
  h[1, 3] = 1
  h[3, 3] = 1
  h[3:4, 4] .= 1
  h[4:6, 5] .= 1
  h[4:5, 6] .= 1
  h[5:6, 7] .= 1
  @test SimpleHypergraphs.conductance(h, Set(1:3)) == 2 / 8
  @test SimpleHypergraphs.conductance(h, Set([1, 4])) == 14 / 6
  @test_throws ErrorException SimpleHypergraphs.conductance(h, Set{Int}())
  @test_throws ErrorException SimpleHypergraphs.conductance(h, Set(1:nhv(h)))
end;


@testset "SimpleHypergraphs dual                   " begin
    m = [
          1         nothing   nothing   4
          1         2         3         nothing
          1         2         3         4
          nothing   2         3         nothing
          nothing   nothing   3         nothing
          nothing   nothing   nothing   4
    ]

    v_meta = Array{Union{Nothing, Char}, 1}(collect('a':'f'))
    he_meta = Array{Union{Nothing, Symbol}, 1}(Symbol.(collect('A':'D')))

    h = Hypergraph{Int, Char, Symbol}(m; v_meta=v_meta, he_meta=he_meta)
    h_dual = dual(h)

    @test nhv(h_dual) == nhe(h)
    @test nhe(h_dual) == nhv(h)

    @test h.v_meta == h_dual.he_meta
    @test h.he_meta == h_dual.v_meta

    m_dual = Matrix(h_dual)
    m_dual[m_dual .== nothing] .= 0
    m[m .== nothing] .= 0

    @test m == transpose(m_dual)

    @test_throws AssertionError dual(Hypergraph(0, 0))

    m_tail = [
        1         nothing      nothing      nothing
        nothing    2           nothing      nothing
        nothing    nothing     3            3
        4          nothing     nothing      4
    ]

    m_head = [
        nothing    5          5            nothing
        nothing    nothing    nothing      6
        7          nothing    nothing      nothing
        nothing    nothing    8            nothing
    ]

    v_meta = Array{Union{Nothing, Char}, 1}(collect('a':'d'))
    he_meta_tail = Array{Union{Nothing, Symbol}, 1}(Symbol.(collect('A':'D')))
    he_meta_head = Array{Union{Nothing, Symbol}, 1}(Symbol.(collect('E':'H')))

    dh = DirectedHypergraph{Int,Char,Symbol}(m_tail, m_head; v_meta=v_meta, he_meta_tail=he_meta_tail, he_meta_head=he_meta_head)
    dh_dual = dual(dh)

    @test nhv(dh_dual) == nhe(dh)
    @test nhe(dh_dual) == nhv(dh)

    m_dual_tail = Matrix(dh_dual.hg_tail)
    m_dual_head = Matrix(dh_dual.hg_head)

    m_dual_tail[m_dual_tail .== nothing] .= 0
    m_dual_head[m_dual_head .== nothing] .= 0
    m_tail[m_tail .== nothing] .= 0
    m_head[m_head .== nothing] .= 0

    @test m_tail == transpose(m_dual_tail)
    @test m_head == transpose(m_dual_head)

    @test dh_dual.he_meta_tail == dh.v_meta
    @test dh_dual.he_meta_head == dh.v_meta
    @test all(dh_dual.v_meta .== [(dh.he_meta_tail[i], dh.he_meta_head[i]) for i in 1:nhe(dh)])

    @test_throws AssertionError dual(DirectedHypergraph(0,0))

    h = BasicHypergraph{Int}(m)
    h_dual = dual(h)

    @test nhv(h_dual) == nhe(h)
    @test nhe(h_dual) == nhv(h)

    m_dual = Matrix(h_dual)
    m_dual[m_dual .== nothing] .= 0
    m[m .== nothing] .= 0

    @test m == transpose(m_dual)

    @test_throws AssertionError dual(BasicHypergraph(0, 0))

    dh = BasicDirectedHypergraph{Int}(m_tail, m_head)
    dh_dual = dual(dh)

    @test nhv(dh_dual) == nhe(dh)
    @test nhe(dh_dual) == nhv(dh)

    m_dual_tail = Matrix(dh_dual.hg_tail)
    m_dual_head = Matrix(dh_dual.hg_head)

    m_dual_tail[m_dual_tail .== nothing] .= 0
    m_dual_head[m_dual_head .== nothing] .= 0
    m_tail[m_tail .== nothing] .= 0
    m_head[m_head .== nothing] .= 0

    @test m_tail == transpose(m_dual_tail)
    @test m_head == transpose(m_dual_head)

    @test_throws AssertionError dual(BasicDirectedHypergraph(0,0))
end;


@testset "SimpleHypergraphs s-distance             " begin
    h = Hypergraph{Int}(6,4)

    h[1:3, 1] .= 1
    h[2:4, 2] .= 2
    h[2:5, 3] .= 3
    h[1, 4] = 4
    h[3, 4] = 4
    h[6, 4] = 4

    @test adjacency_matrix(Matrix(h); s=1) == [
                                                0  1  2  0  0  1
                                                1  0  3  2  1  0
                                                2  3  0  2  1  1
                                                0  2  2  0  1  0
                                                0  1  1  1  0  0
                                                1  0  1  0  0  0
                                               ]

    @test adjacency_matrix(Matrix(h); s=1, weighted=false) == [
                                                            0  1  1  0  0  1
                                                            1  0  1  1  1  0
                                                            1  1  0  1  1  1
                                                            0  1  1  0  1  0
                                                            0  1  1  1  0  0
                                                            1  0  1  0  0  0
                                                        ]

    @test edge_adjacency_matrix(h; s=1, weighted=false) == [
                                                            0  1  1  1
                                                            1  0  1  1
                                                            1  1  0  1
                                                            1  1  1  0
                                                        ]

    @test edge_adjacency_matrix(h; s=1) == [
                                            0  2  2  2
                                            2  0  3  1
                                            2  3  0  1
                                            2  1  1  0
                                        ]

    @test adjacency_matrix(Matrix(h); s=2) == [
                                                 0  0  2  0  0  0
                                                 0  0  3  2  0  0
                                                 2  3  0  2  0  0
                                                 0  2  2  0  0  0
                                                 0  0  0  0  0  0
                                                 0  0  0  0  0  0
                                             ]


    @test distance(h, SnodeDistanceDijkstra(1, 5, 1)) == 2
    @test distance(h, SnodeDistanceDijkstra(3, 6, 1)) == 1

    @test distance(h, SnodeDistanceDijkstra(1, 2, 2)) == 2
    @test distance(h, SnodeDistanceDijkstra(1, 6, 2)) == typemax(Int)

    @test distance(h, SedgeDistanceDijkstra(1, 3, 1)) == 1
    @test distance(h, SedgeDistanceDijkstra(2, 3, 3)) == 1
    @test distance(h, SedgeDistanceDijkstra(1, 3, 3)) == typemax(Int)
end;
