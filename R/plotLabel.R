#' Add label layer to SpatialData plot
#' 
#' @param x \code{\link[spatialdataR]{SpatialData}} object. If \code{NULL}, 
#'   the object will be inherited from \code{plotSpatialData()}.
#' @param i Index or name of label to plot.
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
#' @returns An object of type \code{sd_label}, which can be added to an 
#' existing \code{ggplot}.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#' 
#' i <- "blobs_labels"
#' p <- plotSpatialData(x)
#' 
#' # simple binary image
#' p + plotLabel(i=i)
#' 
#' # mock up some extra data
#' t <- getTable(x, i)
#' t$id <- sample(letters, ncol(t))
#' table(x) <- t
#' 
#' # coloring by 'colData'
#' t <- getTable(x, i)
#' t$id <- sample(letters, ncol(t))
#' table(x) <- t
#' plotSpatialData(x) + plotLabel(i=i, c="id")
#' 
#' # coloring by 'assay' data
#' plotSpatialData(x) + plotLabel(i=i, 
#'   c="channel_1_sum", 
#'   pal=c("lavender", "blue"))
#' 
#' @export
plotLabel <- function(x=NULL, i=1, j=NULL, k=NULL, c=NULL, a=0.5, pal=NULL, nan=NA, assay=1, t=NULL, z=NULL) {
    structure(mget(names(formals())), class="sd_label")
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom rlang .data
#' @importFrom grDevices colors hcl.colors colorRampPalette
#' @importFrom ggplot2 scale_fill_manual scale_fill_gradientn scale_type
#' @importFrom ggplot2 aes theme unit guides guide_legend geom_tile
#' @importFrom spatialdataR labelNames label CTname transform axes getTable
#' @importFrom spatialdataR instances instance_key
#' @importFrom BiocGenerics which
ggplot_add.sd_label <- function(object, plot, object_name) {
    if (is.null(object$x)) {
        x <- plot@meta$sd_args$sd
    } else {
        x <- object$x
    }
    if (!is.null(object$z)) {
        ok <- length(object$z) == 1 && is.numeric(object$z) && 
            object$z == round(object$z) && object$z > 0
        if (!ok) stop("invalid 'z'; should be a scalar integer > 0")
    }

    if (is.numeric(object$i)) 
        object$i <- labelNames(x)[object$i]
    y <- label(x, object$i)
    if (is.null(object$j)) {
        j <- plot@meta$sd_args$ct_name
    } else {
        j <- object$j
        if (is.numeric(j))
            j <- CTname(y)[j]
    }
    y <- transform(y, j)

    # get array data
    ym <- .get_ms_data(y, object$k)
    axisNames <- axes(x=y, y="name")
    
    # z-slice or max-projection
    ym <- .project(y, ym, object$z)
    axisNames <- axisNames[axisNames != "z"]
    
    # subset to selected time
    tidx <- which(axisNames == "t") 
    if (length(tidx) > 0) {
        if (is.null(object$t)) {
            object$t <- 1
        } else if (length(object$t) > 1) {
            stop("Only a single timepoint can be selected")
        }
        ym <- .subset_array_by_axes(a=ym, axisNames=axisNames, t=object$t, drop=FALSE)
        dim(ym) <- dim(ym)[axisNames != "t"]
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
    if (!is.null(object$c)) {
        stopifnot(length(object$c) == 1, is.character(object$c))
        if (is.null(object$pal)) object$pal <- hcl.colors(12, "Spectral")
        se <- getTable(x, object$i)
        is <- instances(se)
        ik <- instance_key(se)
        val <- getTable(x, object$i, object$c, assay=object$assay)
        df$z <- val[match(df$z, is)]
        if (object$c == ik) df$z <- factor(df$z)
        aes$fill <- aes(.data[["z"]])[[1]]
        thm <- switch(scale_type(df$z), 
            discrete={
                val <- sort(unique(df$z), na.last=NA)
                pal <- colorRampPalette(object$pal)(length(val))
                list(
                    theme(legend.key.size=unit(0.5, "lines")),
                    guides(fill=guide_legend(override.aes=list(alpha=1))),
                    scale_fill_manual(c, values=pal, breaks=val, na.value=object$nan))
            },
            continuous=list(
                theme(legend.key.size=unit(0.5, "lines")),
                scale_fill_gradientn(c, colors=object$pal, na.value=object$nan)))
    } else {
        if (is.null(object$pal)) {
            id <- instances(y)
            object$pal <- sample(colors(), length(id), TRUE)
            aes$fill <- aes(factor(.data$z))[[1]]
        } else {
            aes$fill <- aes(.data$z != 0)[[1]]
        }
        thm <- list(
            theme(legend.position="none"),
            scale_fill_manual(NULL, values=object$pal))
    }
    
    if (is.list(axes(y)[[1]]) && !is.null(axes(y)[[1]]$name)) {
        xi <- which(axes(y, "name") == "x")
        if (length(xi) > 0 && !is.null(axes(y)[[xi]]$unit)) {
            plot@meta$sd_args <- modifyList(plot@meta$sd_args, 
                                            list(arrayLayerType = "label",
                                                 arrayLayerName = object$i))
        }
    }
    plot + list(thm, do.call(geom_tile, list(data=df, mapping=aes, alpha=object$a)))
}
