using Test, SimpleHypergraphs
import LightGraphs

h = Hypergraph{Float64}(5,4)
h[1:3,1] .= 1.5
h[3,4] = 2.5
h[2,3] = 3.5
h[4,3:4] .= 4.5
h[5,4] = 5.5
h[5,2] = 6.5

t = TwoSectionView{Float64}(h)

@test LightGraphs.nv(t) == 5
@test LightGraphs.ne(t) == 7

@test sort(LightGraphs.all_neighbors(t, 1)) == [2,3]
@test sort(LightGraphs.outneighbors(t, 5)) == [3,4]
@test sort(LightGraphs.inneighbors(t, 4)) == [2,3,5]

@test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(t))) == 14

@test shortest_path(t,1,5) == [1,3,5]

@test LightGraphs.is_weakly_connected(t) == true

@test SimpleHypergraphs.add_vertex!(h) == 6
@test add_hyperedge!(h) == 5
h[5,5] = 1
h[6,5] = 1

@test shortest_path(t,1,6) == [1,3,5,6]

@test LightGraphs.nv(t) == 6
@test LightGraphs.ne(t) == 8
@test sort(LightGraphs.outneighbors(t, 5)) == [3,4, 6]

@test sum(LightGraphs.adjacency_matrix(LightGraphs.SimpleGraph(t))) == 16
