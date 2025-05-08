#' @name plotImage
#' @title \code{SpatialData} image viz.
#' @aliases plotSpatialData
#' 
#' @description ...
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i element to use from a given layer.
#' @param j name of target coordinate system. 
#' @param k index of the scale of an image; by default (NULL), will auto-select 
#'   scale in order to minimize memory-usage and blurring for a target size of 
#'   800 x 800px; use Inf to plot the lowest resolution available.
#' @param ch image channel(s) to be used for plotting (defaults to 
#'   the first channel(s) available); use \code{channels()} to see 
#'   which channels are available for a given \code{ImageArray}
#' @param c character vector; colors to use for each channel. 
#' @param cl list of length-2 numeric vectors (non-negative, increasing); 
#'   specifies channel-wise contrast limits - defaults to [0, 1] for all 
#'   (ignored when \code{image(x, i)} is an RGB image; 
#'   for convenience, any NULL = [0, 1], and n = [0, n]).
#'
#' @return ggplot
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' ms <- lapply(seq(3), \(.) 
#'   plotSpatialData() +
#'   plotImage(x, i=2, k=.))
#' patchwork::wrap_plots(ms)
#' 
#' # custom colors
#' cmy <- c("cyan", "magenta", "yellow")
#' plotSpatialData() + plotImage(x, c=cmy)
#' 
#' # contrast limits
#' plotSpatialData() + plotImage(x, c=cmy, 
#'   cl=list(c(0.2,1), c(0,0.8), c(0,1)))
#' 
#' @import SpatialData
NULL

#' @rdname plotImage
#' @importFrom ggplot2 ggplot scale_y_reverse
#' @export
plotSpatialData <- \() ggplot() + scale_y_reverse() + .theme 

.check_cl <- \(cl, d) {
    if (is.null(cl)) {
        # default to [0, 1] for all channels
        cl <- replicate(d, c(0, 1), FALSE)
    } else {
        # should be a list with as many elements as channels
        if (!is.list(cl)) stop("'cl' should be a list")
        if (length(cl) != d) stop("'cl' should be of length ", d)
        for (. in seq_len(d)) {
            # replace NULL by [0, 1] & n by [0, n]
            if (is.null(cl[[.]])) cl[[.]] <- c(0, 1)
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
    return(cl)
}

# merge/manage image channels
# if no colors and channels defined, return the first channel
#' @importFrom grDevices col2rgb
#' @noRd
.chs2rgb <- \(a, ch, c=NULL, cl=NULL) {
    cl <- .check_cl(cl, d <- dim(a)[1])
    if (length(ch) > (n <- length(.DEFAULT_COLORS)) && is.null(c))
        stop("Only ", n, " default colors available, but",
            length(ch), " are needed; please specify 'c'")
    if (!is.null(c) || (is.null(c) && length(ch) > 1)) {
        if (is.null(c)) c <- .DEFAULT_COLORS[seq_along(ch)] 
        c <- col2rgb(c)/255
        b <- array(0, dim=c(3, dim(a)[-1]))
        for (i in seq_len(d)) {
            for (j in seq_len(3)) {
                rgb <- a[i,,,drop=FALSE]*c[j,i]
                # apply upper contrast lim.
                rgb <- rgb*(1/cl[[i]][2]) 
                b[j,,] <- b[j,,,drop=FALSE] + rgb
                # apply lower contrast lim.
                b[j,,][b[j,,] < cl[[i]][1]] <- 0
            }
        }
        a <- pmin(b, 1)
    } else {
        a <- a[rep(1, 3), , ]
    }
    return(a)
}

# check if an image is rgb or not
#' @importFrom SpatialData getZarrArrayPath
#' @importFrom Rarr zarr_overview
#' @noRd
.get_img_dt <- \(a) {
    pa <- getZarrArrayPath(a)
    df <- zarr_overview(pa, as_data_frame=TRUE)
    if (!is.null(dt <- df$data_type)) return(dt)
}

# normalize the image data given its data type
#' @noRd
.norm_ia <- \(a, dt) {
    d <- dim(a)[1]
    if (dt %in% names(.DTYPE_MAX_VALUES)) {
        a <- a / .DTYPE_MAX_VALUES[dt]
    } else if (max(a) > 1) {
        for (i in seq_len(d))
            a[i,,] <- a[i,,] / max(a[i,,])
    }
  return(a)
}

# check if an image is RGB or not
# (NOTE: some RGB channels are named 0, 1, 2)
#' @importFrom methods is
#' @noRd
.is_rgb <- \(x) {
    if (is(x, "ImageArray") &&
        !is.null(md <- meta(x)))
        x <- channels(x)
    if (!is.vector(x)) stop("invalid 'x'")
    is_len <- length(x) == 3
    is_012 <- setequal(x, seq(0, 2))
    is_rgb <- setequal(x, c("r", "g", "b"))
    return(is_len && (is_012 || is_rgb))
}
  
# check if channels are indices or channel names
#' @importFrom SpatialData channels
#' @noRd
.ch_idx <- \(x, ch) {
    if (is.null(ch))
        return(1)
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

#' @importFrom methods as
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
.df_i <- \(x, k=NULL, ch=NULL, c=NULL, cl=NULL) {
    a <- .get_multiscale_data(x, k)
    ch <- .ch_idx(x, ch)
    if (!.is_rgb(x))
        a <- a[ch, , , drop=FALSE]
    dt <- .get_img_dt(a)
    a <- as(a, "DelayedArray")
    a <- .norm_ia(realize(a), dt)
    # enter when image isn't RGB already, either
    # custom colors or contrasts are specified
    if (!.is_rgb(x) || !is.null(c) || !is.null(cl))
        a <- .chs2rgb(a, ch, c, cl)
    apply(a, c(2, 3), \(.) do.call(rgb, as.list(.))) 
}

.get_wh <- \(x, i, j) {
    ds <- dim(data(image(x, i), 1))
    ts <- CTpath(x, i, j)
    wh <- data.frame(x=c(0, ds[3]), y=c(0, ds[2]))
    wh <- .trans_xy(wh, ts)
    list(w=wh[, 1], h=wh[, 2])
}

#' @importFrom ggplot2 
#'   scale_color_identity
#'   scale_x_continuous
#'   scale_x_continuous
#'   annotation_raster
.gg_i <- \(x, w, h, pal=NULL) {
    lgd <- if (!is.null(pal)) list(
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
        scale_color_identity(NULL, guide="legend", labels=names(pal)),
        geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(lgd,
        scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
        annotation_raster(x, w[2],w[1], -h[1],-h[2], interpolate=FALSE))
}

#' @rdname plotImage
#' @export
setMethod("plotImage", "SpatialData", \(x, i=1, j=1, k=NULL, ch=NULL, c=NULL, cl=NULL) {
    if (is.numeric(i))
        i <- imageNames(x)[i]
    y <- image(x, i)
    if (is.numeric(j))
        j <- CTname(y)[j]
    df <- .df_i(y, k, ch, c, cl)
    wh <- .get_wh(x, i, j)
    pal <- if (!.is_rgb(y) && dim(y)[1] > 1) {
        nms <- channels(y)[idx <- .ch_idx(y, ch)]
        pal <- if (is.null(c)) .DEFAULT_COLORS else c
        pal <- pal[seq_along(idx)]; names(pal) <- nms; pal
    }
    .gg_i(df, wh$w, wh$h, pal)
})