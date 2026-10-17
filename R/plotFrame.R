#' Add shape layer to SpatialData plot
#'
#' @param x \code{\link[spatialdataR]{SpatialData}} object. If \code{NULL},
#'   the object will be inherited from \code{plotSpatialData()}.
#' @param i Index or name of shape to plot.
#' @param j Index or name of coordinate system to use. 
#'   If \code{NULL}, will be inherited from \code{plotSpatialData()}.
#' @param assay Index or name of an assay; in case of \code{fill} or
#'   \code{colour} denoting a row name, specifies which \code{assay}
#'   data to use (see \code{\link[spatialdataR]{getTable}}).
#'   Ignored when \code{x} is a \code{SpatialDataPoint}.
#' @param n Positive integer scalar; 
#'   if not \code{NULL}, will randomly sample \code{n} points for plotting. 
#'   Use \code{set.seed()} before plotting to get a reproducible subset.
#' @param as Character scalar defining how to plot shapes. Must be one of
#'   \code{"border"} (just plot the outline, the default), \code{"solid"}
#'   (plot filled shapes) or \code{"centroid"} (just plot a point at the
#'   coordinates of the centroid of the shape).
#' @param ... Optional aesthetic arguments (for example, 
#'   \code{fill} or \code{color}) passed to \code{geom_sf}.
#'
#' @returns An object of type \code{sd_shape}, 
#' which can be added to an existing \code{ggplot}.
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
#'   plotShape("blobs_circles", fill="pink") +
#'   plotShape("blobs_polygons", colour="red")
#' patchwork::wrap_plots(a, b)
#'
#' @export
plotShape <- function(i=1, j=NULL, assay=1, x=NULL, n=NULL,
    as=c("border", "solid", "centroid"), ...) {
    args <- mget(names(formals()))
    args$as <- match.arg(as)
    structure(c(args, list(...)), class="sd_shape")
}

#' Add point layer to SpatialData plot
#'
#' @inheritParams plotShape
#' @param i Index or name of point to plot.
#' @param key Character vector; if not \code{NULL}, only plot points 
#'   where \code{feature_key(point(x, i))} is a value in \code{key}.
#'
#' @returns An object of type \code{sd_point}, 
#' which can be added to an existing \code{ggplot}.
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#'
#' i <- "blobs_points"
#' p <- plotSpatialData(x)
#' p + plotPoint(i=i)                     # simple
#' p + plotPoint(i=i, colour="genes")     # discrete
#' p + plotPoint(i, colour="instance_id") # continuous
#'
#' @export
plotPoint <- function(i=1, j=NULL, x=NULL, n=NULL, key=NULL, ...) {
    structure(c(mget(names(formals())), list(...)), class="sd_point")
}

#' @importFrom sf st_as_sf st_buffer st_geometry st_centroid st_geometry<-
#' @importFrom ggplot2 aes theme scale_type geom_sf coord_sf
#' @importFrom spatialdataR transform element<- feature_key getTable
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom methods is
.plot <- \(x, y, key=NULL, n=NULL, assay=1, i=1, as="border",  ...) {
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
        if (is.character(val) && length(val) == 1L) {
            z <- tryCatch(
                error=\(e) NULL,
                getTable(x, i, val, assay=assay))
            if (!is.null(z)) df[[val]] <- z
            if (val %in% names(df)) {
                if (scale_type(df[[val]]) == "discrete")
                    df[[val]] <- factor(df[[val]])
                col <- match(arg, c("col", "color", "colour"))
                .arg <- ifelse(!is.na(col), "colour", arg)
                aes[[.arg]] <- aes(.data[[val]])[[1]]
                dot[[arg]] <- NULL
            }
        }
    }
    if (as == "border") dot[["fill"]] <- NA
    if (as == "centroid") {
        st_geometry(df) <- st_centroid(st_geometry(df))
    } else if ("radius" %in% names(df) && as != "centroid") {
        df <- st_buffer(df, df$radius)
    }
    list(
        do.call(geom_sf, c(list(data=df, mapping=aes), c(dot))),
        theme(legend.key.size=unit(0.5, "lines")),
        coord_sf(expand=FALSE, reverse="y"))
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR shapeNames shape transform CTname CTunit
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
        if (is.null(j)) {
            # get CT from y
            j <- CTname(y)[1]
        }
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    if (is.null(plot@meta$sd_args$ct_name)) {
        plot@meta$sd_args$ct_name <- j
        message("Using coordinate system '", j, "'")
    }
    y <- transform(y, j)
    if (!is.null(CTunit(y)[[j]]$x)) {
        plot@meta$sd_args <- modifyList(
            plot@meta$sd_args, list(
                arrayLayerType="shape",
                arrayLayerName=object$i))
    }
    plot + do.call(.plot, modifyList(object, 
        list(x=x, y=y, j=NULL, `...`=NULL)))
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR pointNames point transform CTname CTunit
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
        if (is.null(j)) {
            # get CT from y
            j <- CTname(y)[1]
        }
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    if (is.null(plot@meta$sd_args$ct_name)) {
        plot@meta$sd_args$ct_name <- j
        message("Using coordinate system '", j, "'")
    }
    y <- transform(y, j)
    if (!is.null(CTunit(y)[[j]]$x)) {
        plot@meta$sd_args <- modifyList(
            plot@meta$sd_args, list(
                arrayLayerType="point",
                arrayLayerName=object$i))
    }
    plot + do.call(.plot, modifyList(object, 
        list(x=x, y=y, j=NULL, `...`=NULL)))
}
