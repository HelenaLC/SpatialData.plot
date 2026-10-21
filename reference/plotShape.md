# Add shape layer to SpatialData plot

Add shape layer to SpatialData plot

## Usage

``` r
plotShape(
  i = 1,
  j = NULL,
  assay = 1,
  x = NULL,
  n = NULL,
  as = c("border", "solid", "centroid"),
  ...
)
```

## Arguments

- i:

  Index or name of shape to plot.

- j:

  Index or name of coordinate system to use. If `NULL`, will be
  inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- assay:

  Index or name of an assay; in case of `fill` or `colour` denoting a
  row name, specifies which `assay` data to use (see
  [`getTable`](https://helenalc.github.io/spatialdataR/reference/table-utils.html)).
  Ignored when `x` is a `SpatialDataPoint`.

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.html)
  object. If `NULL`, the object will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- n:

  Positive integer scalar; if not `NULL`, will randomly sample `n`
  points for plotting. Use
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before plotting to
  get a reproducible subset.

- as:

  Character scalar defining how to plot shapes. Must be one of
  `"border"` (just plot the outline, the default), `"solid"` (plot
  filled shapes) or `"centroid"` (just plot a point at the coordinates
  of the centroid of the shape).

- ...:

  Optional aesthetic arguments (for example, `fill` or `color`) passed
  to `geom_sf`.

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
#> Using coordinate system 'global'
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
  plotShape("blobs_circles", fill="pink") +
  plotShape("blobs_polygons", colour="red")
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.
#> Coordinate system already present.
#> ℹ Adding new coordinate system, which will replace the existing one.

patchwork::wrap_plots(a, b)

```
