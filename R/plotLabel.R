#' @name plotLabel
#' @title \code{SpatialData} label viz.
#' 
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param c determines label colors; 
#'   the default (NULL), gives a binary image of whether or not a
#'   pixel is non-zero; alternatively, a character string specifying
#'   a \code{colData} column or row name in an annotation \code{table}.
#' @param assay character string; 
#'   in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use 
#'   (see \code{\link[spatialdataR]{getTable}}).
#' @param a scalar numeric in [0, 1]; alpha value passed to \code{geom_tile}.
#' @param pal character vector; color for discrete/continuous values
#'   (interpolated automatically when insufficient values are provided).
#'   When left unspecified, color will be sampled at random.
#' @param nan character string; color for missing values (hidden by default).
#' @inheritParams plotImage
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
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
#' p + plotLabel(x, i, c="id")
#' 
#' # coloring by 'assay' data
#' p + plotLabel(x, i, 
#'   c="channel_1_sum", 
#'   pal=c("lavender", "blue"))
NULL

#' @export
#' @rdname plotLabel
#' @importFrom methods as
#' @importFrom rlang .data
#' @importFrom S4Vectors metadata
#' @importFrom SingleCellExperiment colData
#' @importFrom grDevices colors hcl.colors colorRampPalette
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn
#' @importFrom ggplot2 aes theme unit guides guide_legend geom_tile
setMethod("plotLabel", "SpatialData", \(x, i=1, j=1, k=NULL, c=NULL, 
    a=0.5, pal=NULL, nan=NA, assay=1, t=NULL, z=NULL) {

    if (!is.null(z)) {
        ok <- length(z) == 1 && is.numeric(z) && z == round(z) && z > 0
        if (!ok) stop("invalid 'z'; should be a scalar integer > 0")
    }

    if (is.numeric(i)) i <- labelNames(x)[i]
    i <- match.arg(i, labelNames(x))
    y <- label(x, i)
    
    # transformation
    if (is.numeric(j))
      j <- CTname(y)[j]
    y <- transform(y, j)

    # get array data
    ym <- .get_ms_data(y, k)
    axisNames <- axes(x=y, y="name")
    ym <- .project(y, ym, z)
    axisNames <- axisNames[axisNames != "z"]
    # subset to selected time
    tidx <- which(axisNames=="t") 
    if (length(tidx)>0) {
        if (is.null(t)) {
            t <- 1
        } 
        if (length(t)>1) {
            stop("Only a single timepoint can be selected")
        }
        ym <- .subset_array_by_axes(a=ym, axisNames=axisNames,
                                    t=t, drop=FALSE)
        dim(ym) <- dim(ym)[axisNames!="t"]
        axisNames <- axisNames[-tidx]
    }

    # keep only indices != 0 since labels might be sparse 
    # and thus save memory by not plotting all pixels
    idx <- BiocGenerics::which(ym != 0L, arr.ind=TRUE)
    
    # physical space mapping
    ds <- dim(ym)
    wh <- .get_wh(y)
    nx <- tail(ds, 1)
    ny <- tail(ds, 2)[1]
    sx <- diff(wh$w)/nx
    sy <- diff(wh$h)/ny
    df <- data.frame(
        x=wh$w[1]+idx[,2L]*sx, 
        y=wh$h[1]+idx[,1L]*sy, 
        z=ym[idx])
    
    aes <- aes(.data$x, .data$y)
    if (!is.null(c)) {
        stopifnot(length(c) == 1, is.character(c))
        if (is.null(pal)) pal <- hcl.colors(12, "Spectral")
        se <- getTable(x, i)
        is <- instances(se)
        ik <- instance_key(se)
        val <- getTable(x, i, c, assay=assay)
        df$z <- val[match(df$z, is)]
        if (c == ik) df$z <- factor(df$z)
        aes$fill <- aes(.data[["z"]])[[1]]
        thm <- switch(scale_type(df$z), 
            discrete={
                val <- sort(unique(df$z), na.last=NA)
                pal <- colorRampPalette(pal)(length(val))
                list(
                    theme(legend.key.size=unit(0.5, "lines")),
                    guides(fill=guide_legend(override.aes=list(alpha=1))),
                    scale_fill_manual(c, values=pal, breaks=val, na.value=nan))
            },
            continuous=list(
                theme(legend.key.size=unit(0.5, "lines")),
                scale_fill_gradientn(c, colors=pal, na.value=nan)))
    } else {
        if (is.null(pal)) {
            id <- instances(y)
            pal <- sample(colors(), length(id), TRUE)
            aes$fill <- aes(factor(.data$z))[[1]]
        } else {
            aes$fill <- aes(.data$z != 0)[[1]]
        }
        thm <- list(
            theme(legend.position="none"),
            scale_fill_manual(NULL, values=pal))
    }
    list(thm, do.call(geom_tile, list(data=df, mapping=aes, alpha=a)))
})
