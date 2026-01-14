using Test
using StatsBase
using Random
using DataStructures
import Graphs
using SimpleHypergraphs


@testset "HIF load-save basic tests" begin
    h1 = Hypergraph{Float64, Int, String}(5,4)
    h1[1:3,1] .= 1.5
    h1[3,4] = 2.5
    h1[2,3] = 3.5
    h1[4,3:4] .= 4.5
    h1[5,4] = 5.5
    h1[5,2] = 6.5

    path1, _ = mktemp()
    hg_save(path1, h1; format=HIF_Format(), pretty=true)

    loaded_hg = hg_load(path1; format=HIF_Format(), HType=Hypergraph, T=Float64, V=String, E=String)

    @test loaded_hg == h1

    hg = Hypergraph{Int, String, String}(
        [1 nothing        2 nothing;
        3        1 nothing        4])

    set_vertex_meta!(hg, "vertex 1", 1)
    set_hyperedge_meta!(hg, "h-edge 2", 2)
    hg_save(path1, hg; format=HIF_Format(), pretty=true)
    loaded_hg = hg_load(
        path1;
        format=HIF_Format(),
        HType=Hypergraph,
        T=Int, V=String, E=String
    )
    loaded_hg_auto = hg_load(
        path1;
        format=HIF_Format(),
        HType=Hypergraph,
        T=Int, V=:auto, E=:auto
    )
    @test hg == loaded_hg == loaded_hg_auto
    @test hg.v_meta == loaded_hg.v_meta == loaded_hg_auto.v_meta
    @test hg.he_meta == loaded_hg.he_meta == loaded_hg_auto.he_meta
end
@testset "HIF load-save on HIF standard files" begin
    # note SimpleHypergraphs.jl uses 1-based indexing so node and edge ids are regenerated accordingly
    dir = "data/HIF-standard"
    files = filter!(endswith(".json"), readdir(dir))
    for file in files
        full_path = joinpath(dir, file)
        println("Testing HIF file: $full_path")
        flush(stdout)
        flush(stderr)
        h = hg_load(full_path; format=HIF_Format(), T=Int, show_warning=false); 
        io_h = IOBuffer()
        hg_save(io_h, h, HIF_Format(); pretty=true)
        h_loaded = hg_load(seekstart(io_h), HIF_Format(); T=Int, show_warning=true)
        @test h == h_loaded
        h2 = hg_load(full_path; format=HIF_Format(), T=Int, show_warning=false, add_original_id_to_meta=:id); 
        io_h2 = IOBuffer()
        hg_save(io_h2, h2, HIF_Format(); pretty=true)
        h_loaded2 = hg_load(seekstart(io_h2), HIF_Format(); T=Int, show_warning=true, add_original_id_to_meta=nothing)
        @test h2 == h_loaded2 == h
        @test h2.v_meta == h_loaded2.v_meta
        @test h2.he_meta == h_loaded2.he_meta 
    end
end


@testset "Tutorials JSON Data          " begin
    pth = joinpath(dirname(dirname(pathof(SimpleHypergraphs))), "tutorials", "basics", "data");
    files = String[]
    for (root, dirs, filenames) in walkdir(pth)
        for file in filenames
            if endswith(file, ".json")
                if contains(file, "hif.json")
                    continue
                end
                push!(files, joinpath(root, file))
            end
        end
    end
    for file in files
        println("Testing tutorials JSON file: $(basename(file))")

        flush(stdout)
        flush(stderr)
        if !endswith(file, "hg_seasons_min.json")
            loaded_hg = hg_load(file; format=JSON_Format(), HType=Hypergraph, T=Bool, V=Symbol, E=Symbol)
        else 
            loaded_hg = hg_load(file; format=JSON_Format(), HType=Hypergraph, T=Int, V=Symbol, E=Symbol)
        end
        io_h = IOBuffer()
        hg_save(io_h, loaded_hg, JSON_Format(); pretty=true)
        if !endswith(file, "hg_seasons_min.json")
            h_loaded = hg_load(seekstart(io_h), JSON_Format(); HType=Hypergraph, T=Bool, V=Symbol, E=Symbol)
        else 
            h_loaded = hg_load(seekstart(io_h), JSON_Format(); HType=Hypergraph, T=Int, V=Symbol, E=Symbol)
        end
        @test loaded_hg == h_loaded
    end

end;

@testset "Tutorials as HIF Data          " begin
    pth = joinpath(dirname(dirname(pathof(SimpleHypergraphs))), "tutorials", "basics", "data");
    files = String[]
    for (root, dirs, filenames) in walkdir(pth)
        for file in filenames
            if endswith(file, "hif.json")
                push!(files, joinpath(root, file))
            end
        end
    end
    for file in files
        println("Testing tutorials as HIF file: $(basename(file))")
        flush(stdout)
        flush(stderr)
        if !endswith(file, "hg_seasons_min.json")
            loaded_hg = hg_load(
                    file;
                    format=HIF_Format(),
                    HType=Hypergraph,
                    T=Bool, V=:auto, E=:auto
                )
        else 
            loaded_hg = hg_load(
                    file;
                    format=HIF_Format(),
                    HType=Hypergraph,
                    T=Int, V=:auto, E=:auto
                )
        end
        io_h = IOBuffer()
        hg_save(io_h, loaded_hg, HIF_Format(); pretty=true)
        
        if !endswith(file, "hg_seasons_min.json")
            h_loaded = hg_load(seekstart(io_h), HIF_Format(); HType=Hypergraph, T=Bool, V=:auto, E=:auto)
        else 
            h_loaded = hg_load(seekstart(io_h), HIF_Format(); HType=Hypergraph, T=Int, V=:auto, E=:auto)
        end

        @test loaded_hg == h_loaded
    end

end;

#==
using Revise
using SimpleHypergraphs
pathof(SimpleHypergraphs)
pth = joinpath(dirname(pathof(SimpleHypergraphs)),"..","tutorials", "basics", "data", "hg_seasons_all.json");
h = SimpleHypergraphs.hg_load(pth; format=JSON_Format(), T=Int, V=Symbol, E=Symbol);

cd("test")
full_path = "data/HIF-standard/missing_direction.json"
run(`cat $full_path`);
h2 = hg_load(full_path; format=HIF_Format(), T=Int, show_warning=false, add_original_id_to_meta=:id); 
io_h2 = IOBuffer()
hg_save(io_h2, h2, HIF_Format(); pretty=true)
println(String(read(seekstart(io_h2))))
h_loaded2 = hg_load(seekstart(io_h2), HIF_Format(); T=Int, show_warning=true, add_original_id_to_meta=nothing)
h2 == h_loaded2
h2.v_meta == h_loaded2.v_meta
h2.he_meta == h_loaded2.he_meta
==#
