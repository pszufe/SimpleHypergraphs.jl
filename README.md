# SimpleHypergraphs.jl

*A simple hypergraphs package for the Julia programming language.*

| **Documentation** | **Build Status** |
|---------------|--------------|
|[![][docs-stable-img]][docs-stable-url] <br/> [![][docs-latest-img]][docs-dev-url]<br/>[**tutorial**][tutorial-url] | [![Build Status][travis-img]][travis-url]  [![Coverage Status][codecov-img]][codecov-url] <br/> Linux and macOS |


## Installation instructions
### Prerequisites for plotting
`Simplehypergraphs` can optionally use (among other options) the Python's [HyperNetX](https://github.com/pnnl/HyperNetX) library so if you want the `hypernetx` plotting to work you need to install it first along with its all dependent libraries. In Julia command line REPL press `]` for the Julia package manager and run:
```
(v1.10) pkg> add PythonCall CondaPkg
```
HypernetX will get installed as a dependency described in `CondaPkg.toml`


For tutorial we also use the Jupyter notebook. The installation instructions are [here](https://github.com/JuliaLang/IJulia.jl#installation)

Note that recently a new version of HyperNetX has been released, as of today SimpleHyprgraphs.jl requires HyperNetX > 2.4.0.

### Installation
In Julia command line REPL press `]` for the Julia package manager and run:
```
(v1.10) pkg> add SimpleHypergraphs
```
## Documentation

- [**Tutorial**][tutorial-url] &mdash; for a quick start with the library see our Jupyter Notebook tutorial (raw `*.ipynb` version can be also [downloaded][tutorial-raw])
- [**Visualizing hypegraphs - quick overview**][viz-url] &mdash; insight about visualization functionalities.
- [**STABLE**][docs-stable-url] &mdash; **documentation of the most recently tagged version.**
- [**DEV**][docs-dev-url] &mdash; **documentation of the development version.**
- [**A Game of Thrones use case**][got-url] &mdash; check out how you can exploit the library to gather insights into real-world networks through a case study of the Game of Thrones TV series (raw `*.ipynb` version can be also [downloaded][got-raw])
- [**Working with HIF hypegraph exchange format**][viz-hif] &mdash; use case scenario showing a use of the [Hypergraph Interchange Format (HIF)](https://github.com/HIF-org/HIF-standard).
    
## Citing
If you use this library in your research please cite us:

Antelmi, A., Cordasco, G., Kamiński, B., Prałat, P., Scarano, V., Spagnuolo, C. and Szufel, P., "Analyzing, exploring, and visualizing complex networks via hypergraphs using SimpleHypergraphs. jl.",  Internet Mathematics, vol. 1 iss. 1 (2020), March 31, 2020

The full paper is available at [https://doi.org/10.24166/im.01.2020](https://doi.org/10.24166/im.01.2020)

## Acknowledgement
- The research was initially financed by NAWA - The Polish National Agency for Academic Exchange.
- The development of HIF standard format support for exchange of hypergraphs was financed by the National Science Centre (NCN), Poland (grant number: 2021/41/B/HS4/03349).

[docs-latest-img]: https://img.shields.io/badge/docs-latest-blue.svg
[docs-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[docs-dev-url]: https://pszufe.github.io/SimpleHypergraphs.jl/dev
[docs-stable-url]: https://pszufe.github.io/SimpleHypergraphs.jl/stable
[tutorial-url]: https://nbviewer.jupyter.org/github/pszufe/SimpleHypergraphs.jl/blob/master/tutorials/basics/SimpleHypergraphs_tutorial_v5.ipynb
[got-url]: https://nbviewer.jupyter.org/github/pszufe/SimpleHypergraphs.jl/blob/master/tutorials/basics/A%20case%20study%20-%20Game%20of%20Thrones.ipynb

[viz-url]: https://nbviewer.jupyter.org/github/pszufe/SimpleHypergraphs.jl/blob/master/tutorials/viz/Visualizing_hypergraphs.ipynb
[viz-hif]: https://nbviewer.jupyter.org/github/pszufe/SimpleHypergraphs.jl/blob/master/tutorials/hif-standard/HIF-SimpleHypergraphs-demo.ipynb

[tutorial-raw]: https://github.com/pszufe/SimpleHypergraphs.jl/raw/master/tutorials/basics/SimpleHypergraphs_tutorial_v5.ipynb
[got-raw]: https://github.com/pszufe/SimpleHypergraphs.jl/raw/master/tutorials/basics/A%20case%20study%20-%20Game%20of%20Thrones.ipynb

[travis-img]: https://travis-ci.org/pszufe/SimpleHypergraphs.jl.svg?branch=master
[travis-url]: https://travis-ci.org/pszufe/SimpleHypergraphs.jl

[codecov-img]: https://coveralls.io/repos/github/pszufe/SimpleHypergraphs.jl/badge.svg?branch=master
[codecov-url]: https://coveralls.io/github/pszufe/SimpleHypergraphs.jl?branch=master
