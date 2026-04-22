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
plotSpatialData <- \() ggplot() + .theme 

.check_cl <- \(cl, d) {
    if (is.null(cl)) {
        # default to [0, 1] for all channels
        cl <- replicate(d, c(0, 1), FALSE)
    } else if (is.numeric(cl)) {
        stopifnot(length(cl) == 2, cl[2] > cl[1])
        cl <- rep(list(cl), d)
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

.calc_cl <- \(a) {
    # FIXME: It would be great if MatrixGenerics::rowQuantiles() supported 
    # DelayedArrays, so we can keep the lazy representation.
    . <- apply(simplify=FALSE, a, 1, \(.) 
    quantile(., c(0.05, 0.95)))
    if (dim(a)[1] == 1) rep(., 3) else .
}

# merge/manage image channels
# if no colors and channels defined, return the first channel
#' @importFrom grDevices col2rgb
#' @noRd
.prep_ia <- \(a, c=NULL, cl=NULL) {
    dims <- dim(a)[1]
    if (is.null(c)) {
        c <- .DEFAULT_COLORS
        n <- length(c)
        if (n < dims) stop(
            "Only ", n, " default colors available, ",
            "but", dims, " are needed; please specify 'c'")
    }
    cl <- if (is.null(cl)) .calc_cl(a) else .check_cl(cl, dims)
    cl_min <- vapply(cl, \(.) .[1], numeric(1))
    cl_max <- vapply(cl, \(.) .[2], numeric(1))
    colors_rgb <- col2rgb(c)
    rgb_array <- array(0, dim = c(dim(a)[1], 3, dim(a)[2]*dim(a)[3]))   # [d, 3, H*W]
    normed_a <- (a - cl_min) / (cl_max - cl_min)  # normalize by contrast limits
    for (i in seq_len(dims)) {
        rgb_array[i, , ] <- outer(colors_rgb[, i], normed_a[i, , ])
    }
    flat_img <- colMeans(rgb_array)                  # collapse channels -> [3, H*W]
    flat_img <- pmin(flat_img, 255)
    flat_img |> 
        t() |> 
        farver::encode_colour() |> 
        matrix(nrow=dim(a)[2], ncol=dim(a)[3])
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
    if (.is_rgb(x)) return(seq_len(3))
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

#' @importFrom methods as
#' @importFrom DelayedArray realize
#' @importFrom SpatialData data_type
.df_i <- \(x, k=NULL, ch=NULL, c=NULL, cl=NULL) {
    a <- .get_multiscale_data(x, k)
    a <- a[.ch_idx(x, ch),,,drop=FALSE]
    # if (is(a, "ZarrArray"))
    #     a <- as(a, "DelayedArray")
    # if (is(a, "DelayedArray"))
    #     a <- realize(a)
    a <- .norm_ia(a, data_type(x))
    a <- .prep_ia(a, c, cl)
}

.get_wh <- \(x, i, j) {
    ds <- dim(data(image(x, i), 1))
    ts <- CTpath(x, i, j)
    if (is.null(wh <- metadata(image(x, i))$wh)) {
        df <- data.frame(x=c(0, ds[3]), y=c(0, ds[2]))
    } else {
        df <- data.frame(x=wh[[1]], y=wh[[2]])
    }
    #wh <- .trans_xy(wh, ts)
    list(w=df[, 1], h=df[, 2])
}

#' @importFrom ggplot2 
#'   scale_color_identity
#'   scale_x_continuous
#'   scale_x_continuous
#'   annotation_raster
.gg_i <- \(x, w, h, pal=NULL) {
    l <- if (!is.null(names(pal))) list(
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
        geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(l,
        scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
        annotation_raster(x, w[2],w[1], h[2],h[1], interpolate=FALSE),
        scale_color_identity(NULL, guide="legend", breaks=pal, labels=names(pal)))
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
    pal <- if (is.null(c)) .DEFAULT_COLORS else c
    if (dim(y)[1] > 1) {
        nms <- unlist(channels(y))[idx <- .ch_idx(y, ch)]
        pal <- pal[seq_along(idx)]; names(pal) <- nms
    }
    .gg_i(df, wh$w, wh$h, pal)
})