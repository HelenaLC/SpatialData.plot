# `SpatialData.plot`

[![R-universe](https://github.com/HelenaLC/SpatialData.plot/actions/workflows/r-universe.yaml/badge.svg?branch=main&event=push)](https://github.com/HelenaLC/SpatialData.plot/actions/workflows/r-universe.yaml)

Visualization capabilities for `SpatialData` object elements 
from the `spatialdataR`: an R interface to Python's 
[spatialdata](https://spatialdata.scverse.org) framework
([Marconato et al. (2024)](https://doi.org/10.1038/s41592-024-02212-x)).

> [DEMO](https://helenalc.github.io/SpatialData.plot/articles/SpatialData.plot.html)

## Resources

- [SpatialData class](https://helenalc.github.io/spatialdataR) documentation.
- [SpatialData.demo](https://helenalc.github.io/SpatialData.demo): Biotechnology workflows.
- [SpatialData.data](https://github.com/HelenaLC/SpatialData.data): Example `SpatialData`sets.

## Installation

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
    
# install the development version from GitHub
BiocManager::install("HelenaLC/spatialdataR")
BiocManager::install("HelenaLC/SpatialData.plot")
```

## Quick Start

```r
library(spatialdataR)
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
(sd <- readSpatialData(zs))

plotSpatialData() +
  plotImage(sd) +
  plotLabel(sd, a=0.8) +
  plotShape(sd, fill="pink") +
  plotPoint(sd, col="genes", size=1.2)
```

