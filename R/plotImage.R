#' Add image layer to SpatialData plot
#' 
#' @param x \code{\link[spatialdataR]{SpatialData}} object. If \code{NULL}, 
#'   the object will be inherited from \code{plotSpatialData()}.
#' @param i Index or name of image to plot.
#' @param j Index or name of coordinate transformation to use. If \code{NULL}, 
#'   the coordinate transformation will be inherited from 
#'   \code{plotSpatialData()}.
#' @param k Index of the scale to render; by default (NULL), will auto-select 
#'   scale in order to minimize memory-usage and blurring for a target size of 
#'   800 x 800px; use Inf to plot the lowest resolution available.
#' @param ch Image channel(s) to be used for plotting (defaults to 
#'   the first channel(s) available); use \code{channels()} to see 
#'   which channels are available for a given \code{SpatialDataImage}
#' @param c Character vector; colors to use for each channel. 
#' @param cl List of length-2 numeric vectors (non-negative, increasing); 
#'   specifies channel-wise contrast limits - defaults to [0, 1] for all 
#'   (ignored when \code{image(x, i)} is an RGB image; 
#'   for convenience, any NULL = [0, 1], and n = [0, n]).
#' @param t,z Integer scalar to indicate a specific time- or z-slice;
#'   if left unspecified (default NULL), will perform a max-projection.
#'
#' @returns An object of type \code{sd_image}, which can be added to an 
#' existing \code{ggplot}.
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' ms <- lapply(seq(3), \(.) 
#'   plotSpatialData(x) +
#'   plotImage(i=2, k=.))
#' patchwork::wrap_plots(ms)
#' 
#' # custom colors
#' cmy <- c("cyan", "magenta", "yellow")
#' plotSpatialData(x) + plotImage(c=cmy)
#' 
#' # contrast limits
#' cl <- rep(list(c(0, 1/3)), 3)
#' plotSpatialData(x, ct="global") + 
#'   plotImage(k=1, c=cmy, cl=cl) + 
#'   plotShape(i="blobs_circles", fill="pink") + 
#'   plotPoint(i="blobs_points", colour="instance_id")
#' 
#' @import spatialdataR
#' @export
plotImage <- function(x=NULL, i=1, j=NULL, k=NULL, ch=NULL, c=NULL, cl=NULL, t=NULL, z=NULL) {
    structure(mget(names(formals())), class = "sd_image")
}

#' @noRd
#' @keywords internal
.check_cl <- \(cl, d) {
    if (is.numeric(cl)) {
        stopifnot(length(cl) == 2, cl[2] > cl[1])
        cl <- rep(list(cl), d)
    } else {
        # should be a list with as many elements as channels
        if (!is.list(cl)) stop("'cl' should be a list")
        if (length(cl) != d) stop("'cl' should be of length ", d)
        for (. in seq_len(d)) {
            # replace NULL by [0, 1] & n by [0, n]
            # TODO: use the percentile approach here as well
            cl[[.]] <- cl[[.]] %||% c(0, 1)
            if (length(cl[[.]]) == 1) {
                if (cl[[.]] < 0) stop("scalar 'cl' can't be < 0")
                cl[[.]] <- c(0, cl[[.]])
            }
        }
        # elements should be length-2, numeric, non-negative, increasing
        .f <- \(.) length(.) == 2 && is.numeric(.) && all(. >= 0) && .[2] > .[1]
        if (!all(vapply(cl, .f, logical(1))))
            stop("elements of 'cl' should be length-2,",
                " non-negative, increasing numeric vectors")
    }
    cl <- do.call(rbind, cl)
    return(cl)
}

# merge/manage image channels
# if no colors and channels defined, return the first channel
#' @importFrom MatrixGenerics rowQuantiles
#' @importFrom grDevices col2rgb
#' @importFrom farver encode_colour
#' @noRd
#' @keywords internal
.prep_ia <- \(a, c=NULL, cl=NULL) {
    d <- dim(a)[1]
    if (is.null(c)) {
        if (d == 1) {
            c <- "white"
        } else {
            c <- .DEFAULT_COLORS
            n <- length(c)
            if (n < d) stop(
                "Only ", n, " default colors available, ",
                "but ", d, " are needed; please specify 'c'")
            c <- c[seq_len(d)]
        }
    }
    # linear_a is a reshaped to [d, H*W], where d is the number of channels.
    # FIXME: Ideally, we would make sure linear_a is a DelayedArray as well,
    # but it's not implemented yet AFAICT.
    # Keep an eye on https://github.com/Bioconductor/DelayedArray/issues/47.
    linear_a <- matrix(a, nrow=d)
    if (!is.null(cl)) {
        cl <- .check_cl(cl, d)
    } else {
        cl <- rowQuantiles(linear_a, probs=c(0.05, 0.95))
        cl <- matrix(cl, ncol=2)
    }
    colors_rgb <- col2rgb(c)
    normed_a <- (linear_a - cl[, 1]) / (cl[, 2] - cl[, 1])
    flat_img <- (colors_rgb %*% normed_a) / d
    flat_img |> 
        t() |> 
        encode_colour() |> 
        matrix(nrow=dim(a)[2], ncol=dim(a)[3])
}

# normalize the image data given its data type
#' @noRd
#' @keywords internal
.norm_ia <- \(a, dt) {
    d <- dim(a)[1]
    if (dt %in% names(.DTYPE_MAX_VALUES)) {
        a <- a / .DTYPE_MAX_VALUES[dt]
    } else if (max(a) > 1) {
        maxs <- apply(a, 1, max)
        a <- sweep(a, MARGIN = 1, STATS = maxs, FUN = "/")
    }
  return(a)
}

# check if an image is RGB or not
# (NOTE: some RGB channels are named 0, 1, 2)
#' @importFrom methods is
#' @noRd
#' @keywords internal
.is_rgb <- \(x) {
    if (is(x, "SpatialDataImage") &&
        !is.null(md <- meta(x)))
        x <- channels(x)
    if (!is.vector(x)) stop("invalid 'x'")
    is_len <- length(x) == 3
    is_012 <- setequal(x, seq(0, 2))
    is_rgb <- setequal(x, c("r", "g", "b"))
    return(is_len && (is_012 || is_rgb))
}
  
# check if channels are indices or channel names
#' @importFrom spatialdataR channels
#' @noRd
#' @keywords internal
.ch_idx <- \(x, ch) {
    if (is.null(ch)) return(1)
    lbs <- channels(x)
    if (all(ch %in% lbs)) {
        return(match(ch, lbs))
    } else if (!any(ch %in% lbs)) {
        warning("Couldn't find some channels; picking first one(s)!")
        return(1)
    } else {
        warning("Couldn't find channels; picking first one(s)!")
        return(1)
    }
    return(NULL)
}

#' @importFrom spatialdataR data_type axes
#' @noRd
#' @keywords internal
.df_i <- \(x, k=NULL, ch=NULL, t=NULL, c=NULL, cl=NULL, z=NULL) {
    a <- .get_ms_data(x, k)
    axisNames <- axes(x, "name")
    # 2D max-projection
    a <- .project(x, a, z)
    axisNames <- axisNames[axisNames != "z"]
    ti <- which(axisNames == "t")
    tn <- length(ti)
    # subset channels and timepoint of interest
    if (tn) {
        if (is.null(t)) {
            t <- 1
        } else if (length(t) > 1) {
            stop("Only a single timepoint can be selected")
        }
    }
    a <- .subset_array_by_axes(a=a, axisNames=axisNames, 
                               c=.ch_idx(x, ch), t=t, drop=FALSE)
    # remove time axis if it exists
    if (tn) {
        dim(a) <- dim(a)[axisNames != "t"]
        axisNames <- axisNames[-ti]
    }
    # if no channel axis, add dummy axis
    if (!("c" %in% axisNames)) {
        dim(a) <- c(1, dim(a))
        axisNames <- c("c", axisNames)
    }
    a <- .norm_ia(a, data_type(x))
    # color merging & contrasts
    a <- .prep_ia(a, c, cl)
}

#' @importFrom rlang .data
#' @importFrom ggplot2 guides geom_point geom_blank annotation_raster aes
#' @importFrom ggplot2 scale_color_identity guide_legend
#' @importFrom ggnewscale new_scale_color
#' @noRd
#' @keywords internal
.gg_i <- \(x, w, h, pal=NULL) {
    l <- if (!is.null(names(pal))) list(
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
        geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(l,
        geom_blank(aes(x=.data$x, y=.data$y), data.frame(x=w, y=h)),
        annotation_raster(x, w[1],w[2], h[2],h[1], interpolate=FALSE),
        scale_color_identity(NULL, guide="legend", breaks=pal, labels=names(pal)),
        new_scale_color())
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR imageNames CTname transform image channels
ggplot_add.sd_image <- function(object, plot, object_name) {
    if (is.null(object$x)) {
        x <- plot@meta$sd_args$sd
    } else {
        x <- object$x
    }

    if (is.numeric(object$i))
        object$i <- imageNames(x)[object$i]
    y <- image(x, object$i)
    if (is.null(object$j)) {
        j <- plot@meta$sd_args$ct_name
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    y <- transform(y, j)
    if (.is_rgb(y)) {
        # RGB: we plot everything by default and we don't normalize
        object$ch <- object$ch %||% channels(y)
        object$cl <- object$cl %||% c(0, 1/3)
    }
    df <- .df_i(y, object$k, object$ch, object$t, object$c, object$cl, object$z)
    pal <- object$c %||% .DEFAULT_COLORS
    if (dim(y)[1] > 1 && !.is_rgb(y)) {
        nms <- unlist(channels(y))[idx <- .ch_idx(y, object$ch)]
        pal <- pal[seq_along(idx)]; names(pal) <- nms
    }
    # physical space mapping
    wh <- .get_wh(y)
    plot + .gg_i(df, wh$w, wh$h, pal)
}

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
    p@meta$sd_args <- list(
        sd = x,
        ct_name = ct
    )
    p
}
# `annotation_raster` plots the array the same way it is printed, i.e., with the
# row 1 at the top, which means we need to flip the y-axis to have the correct axis labels.
# We tried flipping the image itself but it means everything gets out of alignment if
# the user sets `scale_y_reverse()` themselves.