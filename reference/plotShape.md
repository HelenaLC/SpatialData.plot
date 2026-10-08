# Add shape layer to SpatialData plot

Add shape layer to SpatialData plot

## Usage

``` r
plotShape(x = NULL, i = 1, j = NULL, assay = 1, ...)
```

## Arguments

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.html)
  object. If `NULL`, the object will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- i:

  Index or name of shape to plot.

- j:

  Index or name of coordinate transformation to use. If `NULL`, the
  coordinate transformation will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- assay:

  Character string; in case of `c` denoting a row name, specifies which
  `assay` data to use (see
  [`getTable`](https://helenalc.github.io/spatialdataR/reference/table-utils.html)).
  (ignored when `x` is a `SpatialDataPoint`).

- ...:

  Optional aesthetic arguments passed to `geom_sf`.

## Value

An object of type `sd_shape`, which can be added to an existing
`ggplot`.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

# shapes
p <- plotSpatialData(x)
a <- p + plotShape(i="blobs_polygons")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
b <- p + plotShape(i="blobs_multipolygons")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
c <- p + plotShape(i="blobs_circles")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
patchwork::wrap_plots(a, b, c)


# layered
p +
  plotShape(i="blobs_circles", fill="pink") +
  plotShape(i="blobs_polygons", colour="red")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

patchwork::wrap_plots(a, b)

```
