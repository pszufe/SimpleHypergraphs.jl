using JSON3


struct HypergraphDimensions
    num_nodes::Int
    num_edges::Int
end


function get_hg_dims_from_hif(data::JSON3.Object, V, E)
    num_nodes = length(get(data, "nodes", []))
    num_edges = length(get(data, "edges", []))

    if num_nodes != 0 && num_edges != 0
        return HypergraphDimensions(num_nodes, num_edges)
    end

    nodes = Set{V}()
    edges = Set{E}()

    for i in 1:length(data.incidences)
        inc = data.incidences[i]

        if inc.node ∉ nodes
            push!(nodes, inc.node)
        end

        if inc.edge ∉ edges
            push!(edges, inc.edge)
        end        
    end

    HypergraphDimensions(
        length(nodes),
        length(edges)
    )

end