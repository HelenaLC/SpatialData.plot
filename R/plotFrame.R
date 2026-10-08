#' Add shape layer to SpatialData plot
#'
#' @param x \code{\link[spatialdataR]{SpatialData}} object. If \code{NULL}, 
#'   the object will be inherited from \code{plotSpatialData()}.
#' @param i Index or name of shape to plot.
#' @param j Index or name of coordinate transformation to use. If \code{NULL}, 
#'   the coordinate transformation will be inherited from 
#'   \code{plotSpatialData()}.
#' @param assay Character string; in case of \code{c} 
#'   denoting a row name, specifies which \code{assay} 
#'   data to use (see \code{\link[spatialdataR]{getTable}}).
#'   (ignored when \code{x} is a \code{SpatialDataPoint}).
#' @param ... Optional aesthetic arguments passed to \code{geom_sf}.
#'
#' @returns An object of type \code{sd_shape}, which can be added to an 
#' existing \code{ggplot}.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#'
#' # shapes
#' p <- plotSpatialData(x)
#' a <- p + plotShape(i="blobs_polygons")
#' b <- p + plotShape(i="blobs_multipolygons")
#' c <- p + plotShape(i="blobs_circles")
#' patchwork::wrap_plots(a, b, c)
#'
#' # layered
#' p +
#'   plotShape(i="blobs_circles", fill="pink") +
#'   plotShape(i="blobs_polygons", colour="red")
#' patchwork::wrap_plots(a, b)
#' 
#' @export
plotShape <- function(x=NULL, i=1, j=NULL, assay=1, ...) {
    structure(c(mget(names(formals())), list(...)), class = "sd_shape")
}

#' Add point layer to SpatialData plot
#'
#' @inheritParams plotShape
#' @param i Index or name of point to plot.
#'
#' @returns An object of type \code{sd_point}, which can be added to an 
#' existing \code{ggplot}.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#'
#' i <- "blobs_points"
#' p <- plotSpatialData(x)
#' p + plotPoint(i=i)                       # simple
#' p + plotPoint(i=i, colour="genes")       # discrete
#' p + plotPoint(i=i, colour="instance_id") # continuous
#' 
#' @export
plotPoint <- function(x=NULL, i=1, j=NULL, ...) {
    structure(c(mget(names(formals())), list(...)), class="sd_point")
}

#' @importFrom sf st_as_sf st_buffer
#' @importFrom ggplot2 aes theme scale_type geom_sf coord_sf
#' @importFrom spatialdataR transform element<- feature_key getTable
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom methods is
.plot <- \(x, y, key=NULL, n=NULL, assay=1, i=1, ...) {
    if (is(y, "SpatialDataPoint") && !is.null(key)) {
        stopifnot(is.character(key), nzchar(key))
        fk <- feature_key(y)
        y <- dplyr::filter(y, .data[[fk]] %in% key)
        if (!length(y)) stop("no instances of specified 'key'(s)")
    }
    if (!is.null(n)) {
        stopifnot(is.numeric(n), length(n) == 1, n > 0)
        n <- min(length(y), n)
        y <- y[sample(length(y), n)]
        element(x, i) <- y
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
                col <- match(arg, c("col", "color", "colour"))
                .arg <- ifelse(!is.na(col), "colour", arg)
                aes[[.arg]] <- aes(.data[[val]])[[1]]
                dot[[arg]] <- NULL
            }
        }
    }
    
    if ("radius" %in% names(df))
        df <- st_buffer(df, df$radius)
    list(
        do.call(geom_sf, c(list(data=df, mapping=aes), c(dot))),
        theme(legend.key.size=unit(0.5, "lines")),
        coord_sf(expand=FALSE, reverse="y"))
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR shapeNames shape transform CTname
#' @importFrom utils modifyList
ggplot_add.sd_shape <- function(object, plot, object_name) {
    if (is.null(object$x)) {
        x <- plot@meta$sd_args$sd
    } else {
        x <- object$x
    }
    if (is.numeric(object$i)) 
        object$i <- shapeNames(x)[object$i]
    y <- shape(x, object$i)
    if (is.null(object$j)) {
        j <- plot@meta$sd_args$ct_name
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    y <- transform(y, j)
    plot + do.call(.plot, modifyList(object, list(x=x, y=y, j=NULL, `...`=NULL)))
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR pointNames point transform CTname
#' @importFrom utils modifyList
ggplot_add.sd_point <- function(object, plot, object_name) {
    if (is.null(object$x)) {
        x <- plot@meta$sd_args$sd
    } else {
        x <- object$x
    }
    if (is.numeric(object$i)) 
        object$i <- pointNames(x)[object$i]
    y <- point(x, object$i)
    if (is.null(object$j)) {
        j <- plot@meta$sd_args$ct_name
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    y <- transform(y, j)
    plot + do.call(.plot, modifyList(object, list(x=x, y=y, j=NULL, `...`=NULL)))
}
