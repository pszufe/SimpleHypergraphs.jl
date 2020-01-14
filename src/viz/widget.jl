struct Widget
    type
    body
end

function display(w::Widget)
    HTML(w.body)
end

function widget_graph(
        json_hg::String,
        div_id::Int;
        width::Int=500,
        height::Int=500,
        radius::Real=10,
        node_radii::Union{Vector{<:Real}, Nothing}=nothing,
        node_color::String="#999",
        node_colors::Union{Vector{String}, Nothing}=nothing,
        node_stroke::Union{String, Nothing} = nothing,
        node_strokes::Union{Vector{String}, Nothing}=nothing,
        stroke_width::Real=0,
        stroke_widths::Union{Vector{<:Real}, Nothing}=nothing,
        node_opacity::Real=1,
        node_opacities::Union{Vector{<:Real}, Nothing}=nothing,
        stroke_opacity::Real=1,
        stroke_opacities::Union{Vector{<:Real}, Nothing}=nothing,
        with_node_labels::Bool=false,
        node_labels::Union{Vector{String}, Nothing}=nothing,
        with_node_metadata_hover::Bool=false,
        with_node_weight::Bool=false,
        he_colors::Union{Vector{String}, Nothing}=nothing,
        with_he_labels::Bool=false,
        he_labels::Union{Vector{String}, Nothing}=nothing,
        with_he_metadata_hover::Bool=false
        )

    w = Widget("graph", """
                <head>
                </head>

                <div id="div$(div_id)"></div>

                <script src="https://aleant93.github.io/hypergraphs-drawing/hypergraphsdrawing.js"></script>

                <script>

                    hgd.draw(
                        $(json_hg),
                        "div$(div_id)",
                        width=$(JSON.json(width)),
                        height=$(JSON.json(height)),
                        strength=-60,
                        linkDistance=40,
                        linkStrength=1,
                        theta=0.8,
                        radius=$(JSON.json(radius)),
                        nodeRadii=$(JSON.json(node_radii)),
                        nodeColor=$(JSON.json(node_color)),
                        nodeColors=$(JSON.json(node_colors)),
                        nodeStroke=$(JSON.json(node_stroke)),
                        nodeStrokes=$(JSON.json(node_strokes)),
                        strokeWidth=$(JSON.json(stroke_width)),
                        strokeWidths=$(JSON.json(stroke_widths)),
                        nodeOpacity=$(JSON.json(node_opacity)),
                        nodeOpacities=$(JSON.json(node_opacities)),
                        strokeOpacity=$(JSON.json(stroke_opacity)),
                        strokeOpacities=$(JSON.json(stroke_opacities)),
                        withNodeLabels=$(JSON.json(with_node_labels)),
                        nodeLabels=$(JSON.json(node_labels)),
                        nodeLabelsAttr=null,
                        nodeLabelsStyle=null,
                        withNodeMetadataOnHover=$(JSON.json(with_node_metadata_hover)),
                        withNodeWeight=$(JSON.json(with_node_weight)),
                        edgeColors=$(JSON.json(he_colors)),
                        withEdgeLabels=$(JSON.json(with_he_labels)),
                        edgeLabels=$(JSON.json(he_labels)),
                        withHyperedgesMetadataOnHover=$(JSON.json(with_he_metadata_hover))
                        );
                </script>
                </footer>
            """)
    return w
end
