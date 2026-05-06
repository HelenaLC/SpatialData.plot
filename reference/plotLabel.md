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
  pal = c("red", "green"),
  nan = NA,
  assay = 1
)
```

## Arguments

- x:

  `SpatialData` object.

- i:

  character string or index; the label element to plot.

- j:

  name of target coordinate system.

- k:

  index of the scale of an image; by default (NULL), will auto-select
  scale in order to minimize memory-usage and blurring for a target size
  of 800 x 800px; use Inf to plot the lowest resolution available.

- c:

  the default, NULL, gives a binary image of whether or not a given
  pixel is non-zero; alternatively, a character string specifying a
  `colData` column or row name in a `table` annotating `i`.

- a:

  scalar numeric in \[0, 1\]; alpha value passed to `geom_tile`.

- pal:

  character vector; color for discrete/continuous values (interpolated
  automatically when insufficient values are provided).

- nan:

  character string; color for missing values (hidden by default).

- assay:

  character string; in case of `c` denoting a row name, specifies which
  `assay` data to use (see `valTable`).

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
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
n <- length(unique(t$id))

# pal <- hcl.colors(n, "Spectral")
pal_d <- hcl.colors(10, "Spectral")
p + plotLabel(x, i, c="id", pal=pal_d)
#> Warning: Removed 3324 rows containing missing values or values outside the scale range
#> (`geom_raster()`).


# coloring by 'assay' data
p + plotLabel(x, i, c="channel_1_sum")
#> Warning: Removed 3324 rows containing missing values or values outside the scale range
#> (`geom_raster()`).
```
