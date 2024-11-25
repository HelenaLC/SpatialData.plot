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
#' @param ch the image channels to be used for plotting (default: first channel)
#' @param c plotting aesthetics; color 
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
#' @import SpatialData
NULL

#' @rdname plotImage
#' @importFrom ggplot2 ggplot scale_y_reverse
#' @export
plotSpatialData <- \() ggplot() + scale_y_reverse() + .theme 

# merge/manage image channels
# if no colors and channels defined, return the first channel
#' @noRd
.manage_channels <- \(a, ch, c=NULL){
  if(length(ch) > length(.DEFAULT_COLORS) && is.null(c))
    stop("You can only choose at most seven default colors!")
  if(!is.null(c) || (is.null(c) && length(ch) > 1)) {
    if(is.null(c))
      c <- default_colors[1:length(ch)] 
    c <- col2rgb(c)/255
    a_new <- array(0, dim = c(3,dim(a)[-1]))
    for(i in 1:dim(a)[1]){
      a_new[1,,] <-  a_new[1,,,drop = FALSE] + a[i,,,drop = FALSE]*c[1,i]
      a_new[2,,] <-  a_new[2,,,drop = FALSE] + a[i,,,drop = FALSE]*c[2,i]
      a_new[3,,] <-  a_new[3,,,drop = FALSE] + a[i,,,drop = FALSE]*c[3,i]
    }
    a <- pmin(a_new,1)
  } else {
    a <- a[rep(1,3),,]
  }
  a
}

# check if an image is rgb or not
#' @importFrom SpatialData getZarrArrayPath
#' @noRd
.get_image_dtype <- \(a){
  zarray_spec <- Rarr::zarr_overview(getZarrArrayPath(a), 
                                     as_data_frame = TRUE)
  if("data_type" %in% names(zarray_spec))
    return(zarray_spec$data_type)
  return(NULL)
}

# normalize the image data given its data type
#' @noRd
.normalize_image_array <- \(a, dt){
  if(dt %in% names(.DTYPE_MAX_VALUES)) a <- a/.DTYPE_MAX_VALUES[[dt]]
  else if(max(a) > 1){
    for(i in 1:dim(a)[1])
      a[i,,] <- a[i,,]/max(a[i,,])
  }
  a 
}

# check if an image is rgb or not
# NOTE: some rgb channels are named as 0:2
#' @noRd
.is.rgb <- \(x){
  if(!is.null(md <- x@meta))
    labels <- md[[2]]$channels$label
  if(length(labels) == 3)
    if(all(labels %in% c("r", "g", "b")) || all(labels %in% seq(0,2))) {
      return(TRUE)
    }
  return(FALSE)
}

#' get channel names
#' @export
channelNames <- function(x){
  if(!is.null(md <- attr(x, "meta")))
    return(md[[2]]$channels$label)
  return(NULL)
}
  
# check if channels are indices or channel names
#' @noRd
.ch_ind <- \(x, ch){
  if(is.null(ch))
    return(1)
  lbs <- channelNames(x)
  if(all(ch %in% lbs)){
    return(match(ch,lbs))
  } else if(!any(ch %in% lbs)){
    warning("Some channels are not found, picking first one!")
    return(1)
  } else {
    warning("Channels are not found, picking first one!")
    return(1)
  }
  return(NULL)
}

.guess_scale <- \(x, w, h) {
    n <- length(dim(x))
    i <- ifelse(n == 3, -1, TRUE)
    d <- vapply(x@data, dim, numeric(n))
    d <- apply(d, 2, \(.) sum(abs(.[i]-c(h, w))))
    which.min(d)
}

.get_plot_data <- \(x, k=NULL, w=800, h=800) {
    if (!is.null(k)) return(data(x, k))
    data(x, .guess_scale(x, w, h))
}

#' @importFrom methods as
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
.df_i <- \(x, k=NULL, ch=NULL, c=NULL) {
  a <- .get_plot_data(x, k)
  ch_i <- .ch_ind(x, ch)
  if(!.is.rgb(x))
    a <- a[ch_i,,,drop = FALSE]
  dt <- .get_image_dtype(a)
  a <- realize(as(a, "DelayedArray"))
  a <- .normalize_image_array(a, dt)
  if(!.is.rgb(x))
    a <- .manage_channels(a, ch_i, c)
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
#'   scale_x_continuous
#'   scale_x_continuous
#'   annotation_raster
.gg_i <- \(x, w, h, dpi) list(
    scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
    annotation_raster(x, w[2],w[1], -h[1],-h[2], interpolate=FALSE))

#' @rdname plotImage
#' @export
setMethod("plotImage", "SpatialData", \(x, i=1, j=1, k=NULL, ch=NULL, c=NULL) {
  if (is.numeric(i)) 
    i <- imageNames(x)[i]
  y <- image(x, i)
  if (is.numeric(j)) 
    j <- CTname(y)[j]
  df <- .df_i(y, k, ch, c)
  wh <- .get_wh(x, i, j)
  .gg_i(df, wh$w, wh$h)
})