# `SpatialData` point/shape viz.

`SpatialData` point/shape viz.

## Usage

``` r
# S4 method for class 'SpatialData'
plotShape(x, i = 1, j = 1, assay = 1, ...)

# S4 method for class 'SpatialData'
plotPoint(x, i = 1, j = 1, ...)
```

## Arguments

- x:

  `SpatialData` object.

- i:

  character string or index; the label element to plot.

- assay:

  character string; in case of `c` denoting a row name, specifies which
  `assay` data to use (see `valTable`). (ignored when `x` is a
  `PointFrame`).

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

# shapes
p <- plotSpatialData()
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
a <- p + plotShape(x, "blobs_polygons")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
b <- p + plotShape(x, "blobs_multipolygons")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
c <- p + plotShape(x, "blobs_circles")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
patchwork::wrap_plots(a, b, c)


# layered
p +
  plotShape(x, "blobs_circles", fill="pink") +
  plotShape(x, "blobs_polygons", colour="red")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

patchwork::wrap_plots(a, b)


# points
i <- "blobs_points"
p <- plotSpatialData()
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
p + plotPoint(x, i)                       # simple
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(x, i, colour="genes")       # discrete
#> Don't know how to automatically pick scale for object of type <NULL>.
#> Defaulting to continuous.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

p + plotPoint(x, i, colour="instance_id") # continuous
#> Don't know how to automatically pick scale for object of type <NULL>.
#> Defaulting to continuous.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
```
