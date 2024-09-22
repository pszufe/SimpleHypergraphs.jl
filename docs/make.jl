using Documenter
using Pkg

#endswith(pwd(), "SimpleHypergraphs.jl") && Pkg.activate(".")

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
	checkdocs=:exports,
	doctest = true	
)

deploydocs(
    repo ="github.com/pszufe/SimpleHypergraphs.jl.git",
	target="build"
)
