# Plot a SpatialData object

Initialize an empty ggplot for a SpatialData object. This function is
typically combined with one or more calls to add specific plot layers.

## Usage

``` r
plotSpatialData(x = NULL, ct = NULL)
```

## Arguments

- x:

  A SpatialData object.

- ct:

  The name of a coordinate transformation to use for the plot.

## Value

A ggplot object.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)
ms <- lapply(seq(3), \(.) plotSpatialData(x) + plotImage(i=2, k=.))
patchwork::wrap_plots(ms)

```
