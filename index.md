# `SpatialData.plot`

[![R-universe](https://github.com/HelenaLC/SpatialData.plot/actions/workflows/r-universe.yaml/badge.svg?branch=main&event=push)](https://github.com/HelenaLC/SpatialData.plot/actions/workflows/r-universe.yaml)

`SpatialData.plot` is one of several companion packages to
`spatialdataR`, an R interface to scverse/Python’s
[spatialdata](https://spatialdata.scverse.org) framework ([Marconato et
al. (2024)](https://doi.org/10.1038/s41592-024-02212-x)), providing a
visualization suit for `SpatialData` objects in `ggplot2`-style. This
allows layering of elements into composite plots using `+`, where each
layer can be customized using familiar aesthetics (e.g., `fill` or
`size`).

## Resources

- **[SpatialData.plot](https://helenalc.github.io/SpatialData.plot):
  visualization.**
- [SpatialData class](https://helenalc.github.io/spatialdataR)
  documentation.
- [SpatialData.demo](https://helenalc.github.io/SpatialData.demo):
  Biotechnology workflows.
- [SpatialData.data](https://helenalc.github.io/SpatialData.data):
  Example `SpatialData`sets.

## Installation

``` r

if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
    
# install the development version from GitHub
BiocManager::install("HelenaLC/SpatialData.plot")
```

## Quick Start

In order to demonstrate the package’s basic functionality, we here

1.  load a toy dataset from the `spatialdataR` package
2.  render one element per layer on top of each other

``` r

library(spatialdataR)
library(SpatialData.plot)

zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
(sd <- readSpatialData(zs))
#> class: SpatialData
#> - images(2): blobs_image blobs_multiscale_image
#> - labels(2): blobs_labels blobs_multiscale_labels
#> - points(1):
#>   - blobs_points (200)
#> - shapes(3):
#>   - blobs_circles (5,circle)
#>   - blobs_multipolygons (2,polygon)
#>   - blobs_polygons (5,polygon)
#> - tables(1):
#>   - table (3,10) [blobs_labels]
#> coordinate systems(5):
#> - global(8): blobs_image blobs_multiscale_image ... blobs_polygons
#>   blobs_points
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels

plotSpatialData(sd) +
  plotImage() +
  plotLabel(a=2/3, pal="gold") +
  plotShape(col="white", lwd=2) +
  plotPoint(col="genes", size=1) 
```

![](reference/figures/README-fig-readme-1.png)

Figure 1
