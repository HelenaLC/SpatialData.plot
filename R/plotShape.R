#' @name plotShape
#' @title \code{SpatialData} shape viz.
#'
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param c the default, NULL, gives a binary image of whether or not
#'   a given pixel is non-zero; alternatively, a character string specifying
#'   a \code{colData} column or row name in a \code{table} annotating \code{i}.
#' @param f,s,a used to control plotting aesthetics;
#'   fill, size, alpha value passed to \code{geom_polygon}.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#'
#' p <- plotSpatialData()
#' a <- p + plotShape(x, "blobs_polygons")
#' b <- p + plotShape(x, "blobs_multipolygons")
#' c <- p + plotShape(x, "blobs_circles")
#' patchwork::wrap_plots(a, b, c)
#'
#' # layered
#' p +
#'   plotShape(x, "blobs_circles", f="pink") +
#'   plotShape(x, "blobs_polygons", c="red")
#'
#' patchwork::wrap_plots(a, b)
NULL

#' @rdname plotShape
#' @importFrom sf st_as_sf st_coordinates st_geometry_type
#' @importFrom ggplot2 aes theme scale_type geom_sf
#' @importFrom SpatialData transform
#' @importFrom ggforce geom_circle
#' @importFrom utils tail
#' @export
setMethod("plotShape", "SpatialData", \(x, i=1, j=1, s="radius", assay=1, ...) {
    if (is.numeric(i)) i <- shapeNames(x)[i]
    y <- shape(x, i)
    #y <- SpatialData::transform(y, j)
    df <- st_as_sf(data(y))
    typ <- as.character(st_geometry_type(df)[1])
    aes <- aes()
    dot <- list(...)
    # TODO: need separate plotting for different types of shapes
    switch(typ,
        # POINT means circle
        POINT={
            names(xs) <- xs <- setdiff(names(df), "geometry")
            df <- data.frame(xy, lapply(xs, \(.) df[[.]]))
            names(df) <- c("x", "y", xs)
            if (is.numeric(s)) {
                geo <- geom_point
                dot$size <- s
            } else if (!is.null(s)) {
                geo <- geom_circle
                aes$x0 <- df$x
                aes$y0 <- df$y
                aes$r <- aes(.data[[s]])[[1]]
            } else stop("invalid 's'")
            if (.str_is_col(c)) {
                dot$col <- c
            } else if (is.character(c)) {
                if (c %in% names(df)) {
                    aes$colour <- aes(.data[[c]])[[1]]
                } else {
                    df[[c]] <- valTable(x, i, c, assay=assay)
                    if (scale_type(df[[c]]) == "discrete")
                        df[[c]] <- factor(df[[c]])
                    aes$colour <- aes(.data[[c]])[[1]]
                }
            } else stop("invalid 'c'")
        },{
            geo <- geom_sf
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
        })
    list(
        theme(legend.key.size=unit(0.5, "lines")),
        do.call(geo, c(list(data=df, mapping=aes), dot)))
})