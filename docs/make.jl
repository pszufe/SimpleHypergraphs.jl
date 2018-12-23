using Documenter
using SimpleHypergraphs

makedocs(
    sitename = "SimpleHypergraphs",
    format = :html,
    modules = [SimpleHypergraphs]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo ="https://github.com/bkamins/SimpleHypergraphs.jl.git"
)=#
