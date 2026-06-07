# `SpatialData` label viz.

`SpatialData` label viz.

## Usage

``` r
# S4 method for class 'SpatialData'
plotLabel(
  x,
  i = 1,
  j = 1,
  k = NULL,
  c = NULL,
  a = 0.5,
  pal = NULL,
  nan = NA,
  assay = 1,
  t = NULL,
  z = NULL
)
```

## Arguments

- x:

  `SpatialData` object.

- i:

  character string or index; the label element to plot.

- j:

  index or name of target coordinate system.

- k:

  index of the scale to render; by default (NULL), will auto-select
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
  [`getTable`](https://helenalc.github.io/SpatialData/reference/table-utils.html)).

- t, z:

  integer scalar to indicate a specific time- or z-slice; if left
  unspecified (default NULL), will perform a max-projection.

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

i <- "blobs_labels"
p <- plotSpatialData()

# simple binary image
p + plotLabel(x, i)


# mock up some extra data
t <- getTable(x, i)
t$id <- sample(letters, ncol(t))
table(x) <- t

# coloring by 'colData'
p + plotLabel(x, i, c="id")


# coloring by 'assay' data
p + plotLabel(x, i, 
  c="channel_1_sum", 
  pal=c("lavender", "blue"))
```
