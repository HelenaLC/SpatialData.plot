#' @name plotLabel
#' @title \code{SpatialData} label viz.
#' 
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param j name of target coordinate system. 
#' @param k index of the scale of an image; by default (NULL), will auto-select 
#'   scale in order to minimize memory-usage and blurring for a target size of 
#'   800 x 800px; use Inf to plot the lowest resolution available.
#' @param c the default, NULL, gives a binary image of whether or not 
#'   a given pixel is non-zero; alternatively, a character string specifying
#'   a \code{colData} column or row name in a \code{table} annotating \code{i}.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#' @param a scalar numeric in [0, 1]; alpha value passed to \code{geom_tile}.
#' @param pal character vector; color for discrete/continuous values
#'   (interpolated automatically when insufficient values are provided).
#' @param nan character string; color for missing values (hidden by default).
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=FALSE)
#' 
#' i <- "blobs_labels"
#' p <- plotSpatialData()
#' 
#' # simple binary image
#' p + plotLabel(x, i)
#' 
#' # mock up some extra data
#' t <- getTable(x, i)
#' t$id <- sample(letters, ncol(t))
#' table(x) <- t
#' 
#' # coloring by 'colData'
#' n <- length(unique(t$id))
#' # TODO: did a small fix to color scheme below, will fix example later
#' # pal <- hcl.colors(n, "Spectral")
#' pal_d <- hcl.colors(10, "Spectral")
#' p + plotLabel(x, i, c = "id", pal=pal_d)
#' 
#' # coloring by 'assay' data
#' p + plotLabel(x, i, c = "channel_1_sum")
NULL

#' @rdname plotLabel
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom S4Vectors metadata
#' @importFrom rlang .data
#' @importFrom methods as
#' @importFrom ggplot2 
#'   scale_fill_manual scale_fill_gradientn
#'   aes geom_raster theme unit guides guide_legend
#' @importFrom SingleCellExperiment colData
#' @export
setMethod("plotLabel", "SpatialData", \(x, i=1, j=1, k=NULL, c=NULL, 
    a=0.5, pal=c("red", "green"), nan=NA, assay=1) {
    if (is.numeric(i)) i <- labelNames(x)[i]
    i <- match.arg(i, labelNames(x))
    y <- label(x, i)
    ym <- as.matrix(.get_multiscale_data(label(x, i), k))
    df <- data.frame(x=c(col(ym)), y=c(row(ym)), z=c(ym))
    # transformation
    if (is.numeric(j))
      j <- CTname(y)[j]
    ts <- CTpath(x, i, j)
    df[,c("x", "y")] <- .trans_xy(df[,c("x", "y")], ts)
    aes <- aes(.data[["x"]], .data[["y"]])
    if (!is.null(c)) {
        stopifnot(length(c) == 1, is.character(c))
        t <- table(x, hasTable(x, i, name=TRUE))
        ik <- .instance_key(t)
        # TODO: search ik in both internal and regular colData for now
        # thus perhaps update, SpatialData::valTable instead
        # idx <- match(df$z, int_colData(t)[[ik]])
        if(ik %in% names(int_colData(t))){
          coldata <- int_colData(t)[[ik]]
        } else {
          coldata <- colData(t)[[ik]]
        }
        idx <- match(df$z, coldata)
        df$z <- valTable(x, i, c, assay=assay)[idx]
        if (c == ik) df$z <- factor(df$z)
        aes$fill <- aes(.data[["z"]])[[1]]
        switch(scale_type(df$z), 
            discrete={
                val <- sort(setdiff(unique(df$z), NA))
                pal <- colorRampPalette(pal)(length(val))
                thm <- list(
                    theme(legend.key.size=unit(0.5, "lines")),
                    guides(fill=guide_legend(override.aes=list(alpha=1))),
                    scale_fill_manual(c, values=pal, breaks=val, na.value=nan))
            },
            continuous=thm <- list(
                theme(legend.key.size=unit(0.5, "lines")),
                scale_fill_gradientn(c, colors=pal, na.value=nan)))
    } else {
        thm <- guides(fill="none")
        aes$fill <- aes(.data$z != 0)[[1]]
        thm <- list(
            theme(legend.position="none"),
            scale_fill_manual(NULL, values=pal))
    }
    list(thm, do.call(geom_raster, list(data=df, mapping=aes, alpha=a)))
})