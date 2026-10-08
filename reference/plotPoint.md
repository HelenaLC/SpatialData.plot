# Add point layer to SpatialData plot

Add point layer to SpatialData plot

## Usage

``` r
plotPoint(x = NULL, i = 1, j = NULL, ...)
```

## Arguments

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.html)
  object. If `NULL`, the object will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- i:

  Index or name of point to plot.

- j:

  Index or name of coordinate transformation to use. If `NULL`, the
  coordinate transformation will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- ...:

  Optional aesthetic arguments passed to `geom_sf`.

## Value

An object of type `sd_point`, which can be added to an existing
`ggplot`.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

i <- "blobs_points"
p <- plotSpatialData(x)
p + plotPoint(i=i)                       # simple
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(i=i, colour="genes")       # discrete
#> Don't know how to automatically pick scale for object of type <NULL>.
#> Defaulting to continuous.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(i=i, colour="instance_id") # continuous
#> Don't know how to automatically pick scale for object of type <NULL>.
#> Defaulting to continuous.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

```
