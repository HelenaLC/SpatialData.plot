#' @name plotFrame
#' @aliases plotShape plotPoint
#' @title \code{SpatialData} point/shape viz.
#'
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#'   (ignored when \code{x} is a \code{PointFrame}).
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#'
#' # shapes
#' p <- plotSpatialData()
#' a <- p + plotShape(x, "blobs_polygons")
#' b <- p + plotShape(x, "blobs_multipolygons")
#' c <- p + plotShape(x, "blobs_circles")
#' patchwork::wrap_plots(a, b, c)
#'
#' # layered
#' p +
#'   plotShape(x, "blobs_circles", fill="pink") +
#'   plotShape(x, "blobs_polygons", colour="red")
#' patchwork::wrap_plots(a, b)
#' 
#' # points
#' i <- "blobs_points"
#' p <- plotSpatialData()
#' p + plotPoint(x, i)                       # simple
#' p + plotPoint(x, i, colour="genes")       # discrete
#' p + plotPoint(x, i, colour="instance_id") # continuous
NULL

#' @importFrom ggplot2 aes theme scale_type geom_sf coord_sf
#' @importFrom sf st_as_sf st_coordinates st_geometry_type
#' @importFrom SpatialData transform
#' @importFrom ggforce geom_circle
#' @importFrom utils tail
.plot <- \(x, y, key=NULL, n=Inf, assay=1, i=1, ...) {
    if (is(y, "PointFrame")) {
        if (!is.null(key)) {
            fk <- feature_key(y)
            y@data <- filter(data(y), .data[[fk]] %in% key)
        }
    }
    if (is.finite(n)) {
        n <- min(length(y), n)
        y <- y[sample(length(y), n)]
        if (is(y, "ShapeFrame")) {
            shape(x, i) <- y
        } else {
            point(x, i) <- y
        }
    }
    df <- st_as_sf(data(y))
    aes <- aes()
    dot <- list(...)
    for (arg in names(dot)) {
        val <- dot[[arg]]
        if (is.character(val)) {
            z <- tryCatch(
                error=\(e) NULL,
                getTable(x, i, val, assay=assay))
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
        do.call(geom_sf, c(list(data=df, mapping=aes), c(dot))),
        theme(legend.key.size=unit(0.5, "lines")),
        coord_sf(expand=FALSE, reverse="y"))
}
#' @export
#' @rdname plotFrame
setMethod("plotShape", "SpatialData", \(x, i=1, j=1, assay=1, ...) {
    if (is.numeric(i)) i <- shapeNames(x)[i]
    y <- shape(x, i)
    y <- SpatialData::transform(y, j)
    .plot(x, y, assay=assay, i=i, ...)
})
#' @export
#' @rdname plotFrame
setMethod("plotPoint", "SpatialData", \(x, i=1, j=1, ...) {
    if (is.numeric(i)) i <- pointNames(x)[i]
    y <- point(x, i)
    y <- SpatialData::transform(y, j)
    .plot(x, y, i=i, ...)
})