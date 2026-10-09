# Add scalebar to plot

`scalebar` will get the axis unit and scale information from the closest
preceding layer where this information is present. Therefore, the
placement of `scalebar` in the sequence of layers is important.

## Usage

``` r
scalebar(len = NULL, col = "red", lwd = 1, xrel = 0.05, yrel = 0.05)
```

## Arguments

- len:

  scalar numeric giving the length of the scalebar in physical
  coordinate space; the unit will be extracted from the data's Zarr
  specifications (see `axes(x)`).

- col:

  string indicating the color to use for the scalebar.

- lwd:

  scalar numeric indicating the linewidth to use for the scalebar.

- xrel, yrel:

  scalar numeric indicating relative position of the scalebar.

## Value

length-two list of
[`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html)
layers corresponding to scalebar line (`geom="segment"`) and unit label
(`geom="text"`)

## Examples

``` r
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs, tables=FALSE)

# mock unit (data misses specification!)
md <- meta(image(sd, 2))
md$multiscales[[1]]$axes[[3]]$unit <- "micron"
sd$images[[2]]@meta <- md

plotSpatialData(sd) + 
  plotImage(i=2) + 
  scalebar(len=10)

```
