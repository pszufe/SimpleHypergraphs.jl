struct Widget
    type
    body
end

function display(w::Widget)
    HTML(w.body)
end

function widget_graph(
        v2he::String,
        he2v::String,
        div_id::Int;
        v_meta::Union{AbstractVector, Nothing}=nothing,
        he_meta::Union{AbstractVector, Nothing}=nothing,
        width::Int=500,
        height::Int=500,
        radius::Real=10,
        node_radii::Union{AbstractVector{<:Real}, Nothing}=nothing,
        node_color::String="#999",
        node_colors::Union{AbstractVector{String}, Nothing}=nothing,
        node_stroke::Union{String, Nothing} = nothing,
        node_strokes::Union{AbstractVector{String}, Nothing}=nothing,
        stroke_width::Real=0,
        stroke_widths::Union{AbstractVector{<:Real}, Nothing}=nothing,
        node_opacity::Real=1,
        node_opacities::Union{AbstractVector{<:Real}, Nothing}=nothing,
        stroke_opacity::Real=1,
        stroke_opacities::Union{AbstractVector{<:Real}, Nothing}=nothing,
        with_node_labels::Bool=false,
        node_labels::Union{AbstractVector{String}, Nothing}=nothing,
        with_node_metadata_hover::Bool=false,
        with_node_weight::Bool=false,
        he_colors::Union{AbstractVector{String}, Nothing}=nothing,
        with_he_labels::Bool=false,
        he_labels::Union{AbstractVector{String}, Nothing}=nothing,
        with_he_metadata_hover::Bool=false
        )

    w = Widget("graph", """
                <head>
                </head>

                <div id="div$(div_id)"></div>

                <script src="https://aleant93.github.io/hypergraphs-drawing/hypergraphsdrawing.js"></script>

                <script>

                    hgd.draw(
                        $(v2he),
                        $(he2v),
                        "div$(div_id)",
                        vmeta=$(JSON3.write(v_meta)),
                        hemeta=$(JSON3.write(he_meta)),
                        width=$(JSON3.write(width)),
                        height=$(JSON3.write(height)),
                        strength=-60,
                        linkDistance=40,
                        linkStrength=1,
                        theta=0.8,
                        radius=$(JSON3.write(radius)),
                        nodeRadii=$(JSON3.write(node_radii)),
                        nodeColor=$(JSON3.write(node_color)),
                        nodeColors=$(JSON3.write(node_colors)),
                        nodeStroke=$(JSON3.write(node_stroke)),
                        nodeStrokes=$(JSON3.write(node_strokes)),
                        strokeWidth=$(JSON3.write(stroke_width)),
                        strokeWidths=$(JSON3.write(stroke_widths)),
                        nodeOpacity=$(JSON3.write(node_opacity)),
                        nodeOpacities=$(JSON3.write(node_opacities)),
                        strokeOpacity=$(JSON3.write(stroke_opacity)),
                        strokeOpacities=$(JSON3.write(stroke_opacities)),
                        withNodeLabels=$(JSON3.write(with_node_labels)),
                        nodeLabels=$(JSON3.write(node_labels)),
                        nodeLabelsAttr=null,
                        nodeLabelsStyle=null,
                        withNodeMetadataOnHover=$(JSON3.write(with_node_metadata_hover)),
                        withNodeWeight=$(JSON3.write(with_node_weight)),
                        edgeColors=$(JSON3.write(he_colors)),
                        withEdgeLabels=$(JSON3.write(with_he_labels)),
                        edgeLabels=$(JSON3.write(he_labels)),
                        withHyperedgesMetadataOnHover=$(JSON3.write(with_he_metadata_hover))
                        );
                </script>
                </footer>
            """)
    return w
end
