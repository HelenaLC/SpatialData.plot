#' Plot a SpatialData object
#' 
#' Initialize an empty ggplot for a SpatialData object. This function is 
#' typically combined with one or more calls to add specific plot layers.
#' 
#' @param x A SpatialData object.
#' @param ct The name of a coordinate transformation to use for the plot.
#' 
#' @returns A ggplot object.
#' 
#' @examples 
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x, tables=FALSE)
#' ms <- lapply(seq(3), \(.) plotSpatialData(x) + plotImage(i=2, k=.))
#' patchwork::wrap_plots(ms)
#' 
#' @export
#' @importFrom ggplot2 ggplot coord_sf
plotSpatialData <- \(x=NULL, ct=NULL) {
    p <- ggplot() + coord_sf(expand=FALSE, reverse="y") + .theme
    if (!is.null(x)) {
        if (is.null(ct)) {
            ct <- CTname(x)[1]
        } else if (is.numeric(ct)) {
            ct <- CTname(x)[ct]
        }
    }
    p@meta$sd_args <- list(sd=x, ct_name=ct)
    p
}
