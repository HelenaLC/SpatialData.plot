#' @name plotPoint
#' @title \code{SpatialData} point viz.
#' 
#' @param x \code{SpatialData} object or \code{PointFrame}.
#' @param i character string or index; the label element to plot.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}})
#'   (ignored when \code{x} is a \code{PointFrame}).
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=FALSE)
#' 
#' i <- "blobs_points"
#' p <- plotSpatialData()
#' 
#' # mock up a 'table'
#' f <- list(
#'   numbers=\(n) runif(n),
#'   letters=\(n) sample(letters, n, TRUE))
#' x <- setTable(x, i, f)
#' 
#' p + plotPoint(x, i) # simple dots
#' p + plotPoint(x, i, "numbers") # discrete coloring
#' p + plotPoint(x, i, "letters") # continuous coloring
NULL

#' @importFrom methods is
#' @importFrom grDevices hcl.colors
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom ggplot2 
#'   geom_point scale_color_gradientn
#'   aes theme guides guide_legend scale_type
.gg_p <- \(x, j, ...) {
    dot <- list(...)
    i <- dot$i
    ik <- dot$ik
    assay <- dot$assay
    pf <- is(x, "PointFrame")
    y <- if (pf) x else point(x, i)
    # transformation
    #y <- SpatialData::transform(y, j)
    df <- as.data.frame(data(y))
    aes <- aes(.data[["x"]], .data[["y"]])
    for (arg in names(dot)) {
                val <- dot[[arg]]
                if (is.character(val)) {
                    z <- tryCatch(
                        error=\(e) NULL,
                        valTable(x, i, val, assay=assay))
                    if (!is.null(z)) {
                        fd <- data.frame(z)
                        names(fd) <- val
                        df <- cbind(df, fd)
                    }
                    if (val %in% names(df)) {
                        if (scale_type(df[[arg]]) == "discrete")
                            df[[val]] <- factor(df[[arg]])
                        aes[[arg]] <- aes(.data[[val]])[[1]]
                        dot[[arg]] <- NULL
                    }
                }
            }
    list(
        do.call(geom_point, c(list(data=df, mapping=aes), dot))
    )
}

#' @export
#' @rdname plotPoint
#' @importFrom SpatialData instance_key
setMethod("plotPoint", "SpatialData", \(x, i=1, j=1, c=NULL, s=1, a=1, assay=1, f=NULL) {
    ik <- instance_key(point(x, i))
    .gg_p(x, j, ...)
})

#' @rdname plotPoint
#' @export
setMethod("plotPoint", "PointFrame", \(x, j=1, ...) {
    .gg_p(x, j, ...)
})