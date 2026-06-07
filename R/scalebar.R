#' @title \code{SpatialDataArray} scalebar
#' 
#' @param x a \code{SpatialDataArray} object (i.e.,
#'   image or label element from a \code{SpatialData} object).
#' @param len scalar numeric giving the length of the scalebar 
#'   in physical coordinate space; the unit will be extracted
#'   from the data's Zarr specifications (see \code{axes(x)}).
#' @param col string indicating the color to use for the scalebar.
#' @param lwd scalar numeric indicating the linewidth to use for the scalebar.
#' @param xrel,yrel scalar numeric indicating relative position of the scalebar.
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' sd <- readSpatialData(zs, tables=FALSE)
#' 
#' # mock unit (data misses specification!)
#' md <- meta(image(sd, 2))
#' md$multiscales[[1]]$axes[[3]]$unit <- "micron"
#' sd$images[[2]]@meta <- md
#' 
#' plotSpatialData() + 
#'   plotImage(sd, i=2) + 
#'   scalebar(image(sd, i=2), len=10)
#' 
#' @importFrom ggplot2 annotate
#' @importFrom methods is
#' @export
scalebar <- function(x, len=NULL, col="red", lwd=1, xrel=0.05, yrel=0.05) {
    # validity
    if (!is(x, "SpatialDataArray")) 
        stop("'x' should be a 'SpatialDataArray' object, i.e., an",
            " image or label element from a 'SpatialData' object")
    ok <- \(x) is.numeric(x) && is.finite(x) && length(x) == 1
    if (!is.null(len)) stopifnot(ok(len), len > 0)
    stopifnot(ok(xrel), ok(yrel))
    
    xi <- which(axes(x, "name") == "x")
    unit <- axes(x)[[xi]]$unit
    if (is.null(unit)) 
        stop("'axes(x)' list element ", xi, 
            " (X dimension) missing 'unit'")
    if (unit %in% names(.unit_map))
        unit <- .unit_map[unit]

    wh <- .get_wh(x)
    if (is.null(len)) len <- 0.05*diff(wh$w)
    if (xrel <= 0.5) {
        xmin <- diff(wh$w) * xrel + wh$w[1]
        xmax <- diff(wh$w) * xrel + wh$w[1] + len
    } else {
        xmin <- wh$w[2] - diff(wh$w) * (1 - xrel) - len
        xmax <- wh$w[2] - diff(wh$w) * (1 - xrel)
    }
    y <- wh$h[2] - diff(wh$h) * yrel
    
    line <- annotate(
        geom="segment", 
        color=col, linewidth=lwd,
        x=xmin, xend=xmax, y=y, yend=y)
    text <- annotate(
        geom="text", 
        x=(xmin+xmax)/2, y=y, 
        vjust=ifelse(yrel > 0.5, 1.5, -0.5),
        color=col, label=paste0(round(len, 1), unit))
    return(list(line, text))
}
