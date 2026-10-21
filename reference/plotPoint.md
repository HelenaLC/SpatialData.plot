# Add point layer to SpatialData plot

Add point layer to SpatialData plot

## Usage

``` r
plotPoint(i = 1, j = NULL, x = NULL, n = NULL, key = NULL, ...)
```

## Arguments

- i:

  Index or name of point to plot.

- j:

  Index or name of coordinate system to use. If `NULL`, will be
  inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.html)
  object. If `NULL`, the object will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- n:

  Positive integer scalar; if not `NULL`, will randomly sample `n`
  points for plotting. Use
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before plotting to
  get a reproducible subset.

- key:

  Character vector; if not `NULL`, only plot points where
  `feature_key(point(x, i))` is a value in `key`.

- ...:

  Optional aesthetic arguments (for example, `fill` or `color`) passed
  to `geom_sf`.

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
#> Using coordinate system 'global'
p + plotPoint(i=i)                     # simple
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(i=i, colour="genes")     # discrete
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(i, colour="instance_id") # continuous
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

```
