CellEngine R API Toolkit
-----

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
We are currently working on several breaking changes that will be released in
our first stable release, [v1.0](https://github.com/primitybio/cellengine-r-toolkit/milestone/1).

To install the v0.x version (no breaking changes will be made):
```R
library("devtools")
install_github("primitybio/cellengine-r-toolkit")
```

To install a preview of the upcoming v1.0 version (unstable):
```R
library("devtools")
install_github("primitybio/cellengine-r-toolkit", ref="v1-dev")
```

Quick start:

```R
library("cellengine")
authenticate("username", Sys.getenv("CELLENGINE_PASSWORD"))

experiments = getExperiments()
```
