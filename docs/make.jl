using Documenter


try
    using SimpleHypergraphs
catch
    if !("../src/" in LOAD_PATH)
	   push!(LOAD_PATH,"../src/")
	   @info "Added \"../src/\"to the path: $LOAD_PATH "
	   using SimpleHypergraphs
    end
end




makedocs(
    sitename = "SimpleHypergraphs",
    format = Documenter.HTML(),
    modules = [SimpleHypergraphs],
	pages = ["index.md", "reference.md"],
	doctest = true	
)

deploydocs(
    repo ="https://github.com/pszufe/SimpleHypergraphs.jl.git",
	target="build"
)
