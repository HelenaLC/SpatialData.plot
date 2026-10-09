# Add label layer to SpatialData plot

Add label layer to SpatialData plot

## Usage

``` r
plotLabel(
  i = 1,
  j = NULL,
  k = NULL,
  c = NULL,
  a = 0.5,
  pal = NULL,
  nan = NA,
  assay = 1,
  t = NULL,
  z = NULL,
  x = NULL
)
```

## Arguments

- i:

  Index or name of label to plot.

- j:

  Index or name of coordinate transformation to use. If `NULL`, the
  coordinate transformation will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

- k:

  Index of the scale to render; by default (NULL), will auto-select
  scale in order to minimize memory-usage and blurring for a target size
  of 800 x 800px; use Inf to plot the lowest resolution available.

- c:

  determines label colors; the default (NULL), gives a binary image of
  whether or not a pixel is non-zero; alternatively, a character string
  specifying a `colData` column or row name in an annotation `table`.

- a:

  scalar numeric in \[0, 1\]; alpha value passed to `geom_tile`.

- pal:

  character vector; color for discrete/continuous values (interpolated
  automatically when insufficient values are provided). When left
  unspecified, color will be sampled at random.

- nan:

  character string; color for missing values (hidden by default).

- assay:

  character string; in case of `c` denoting a row name, specifies which
  `assay` data to use (see
  [`getTable`](https://helenalc.github.io/spatialdataR/reference/table-utils.html)).

- t, z:

  Integer scalar to indicate a specific time- or z-slice; if left
  unspecified (default NULL), will perform a max-projection.

- x:

  [`SpatialData`](https://helenalc.github.io/spatialdataR/reference/SpatialData.html)
  object. If `NULL`, the object will be inherited from
  [`plotSpatialData()`](https://HelenaLC.github.io/SpatialData.plot/reference/plotSpatialData.md).

## Value

An object of type `sd_label`, which can be added to an existing
`ggplot`.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

i <- "blobs_labels"
p <- plotSpatialData(x)

# simple binary image
p + plotLabel(i)


# mock up some extra data
t <- getTable(x, i)
t$id <- sample(letters, ncol(t))
table(x) <- t

# coloring by 'colData'
t <- getTable(x, i)
t$id <- sample(letters, ncol(t))
table(x) <- t
plotSpatialData(x) + plotLabel(i=i, c="id")


# coloring by 'assay' data
plotSpatialData(x) + plotLabel(i=i, 
  c="channel_1_sum", 
  pal=c("lavender", "blue"))

```
