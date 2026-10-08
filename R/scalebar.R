#' Add scalebar to plot
#' 
#' \code{scalebar} will get the axis unit and scale information from the 
#' closest preceding layer where this information is present. Therefore, 
#' the placement of \code{scalebar} in the sequence of layers is important.
#' 
#' @param len scalar numeric giving the length of the scalebar 
#'   in physical coordinate space; the unit will be extracted
#'   from the data's Zarr specifications (see \code{axes(x)}).
#' @param col string indicating the color to use for the scalebar.
#' @param lwd scalar numeric indicating the linewidth to use for the scalebar.
#' @param xrel,yrel scalar numeric indicating relative position of the scalebar.
#'
#' @return 
#' length-two list of \code{ggplot2::annotate()} layers corresponding to
#' scalebar line (\code{geom="segment"}) and unit label (\code{geom="text"})
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' sd <- readSpatialData(zs, tables=FALSE)
#' 
#' # mock unit (data misses specification!)
#' md <- meta(image(sd, 2))
#' md$multiscales[[1]]$axes[[3]]$unit <- "micron"
#' sd$images[[2]]@meta <- md
#' 
#' plotSpatialData(sd) + 
#'   plotImage(i=2) + 
#'   scalebar(len=10)
#' 
#' @export
scalebar <- function(len=NULL, col="red", lwd=1, xrel=0.05, yrel=0.05) {
    structure(mget(names(formals())), class="sd_scalebar")
}

#' @exportS3Method ggplot2::ggplot_add
#' @importFrom spatialdataR axes image label extent
#' @importFrom ggplot2 annotate
ggplot_add.sd_scalebar <- function(object, plot, object_name) {
    x <- plot@meta$sd_args$sd
    arrayLayerType <- plot@meta$sd_args$arrayLayerType
    arrayLayerName <- plot@meta$sd_args$arrayLayerName
    
    if (!is.null(arrayLayerType) && !is.null(arrayLayerName)) {
        x <- get(arrayLayerType)(x, arrayLayerName)

        # validity
        ok <- \(x) is.numeric(x) && is.finite(x) && length(x) == 1
        if (!is.null(object$len)) stopifnot(ok(object$len), object$len > 0)
        stopifnot(ok(object$xrel), ok(object$yrel))
        
        xi <- which(axes(x, "name") == "x")
        unit <- axes(x)[[xi]]$unit
        if (unit %in% names(.unit_map))
            unit <- .unit_map[unit]

        wh <- extent(x)[c("x", "y")] |> setNames(c("w", "h"))
        if (is.null(object$len)) object$len <- 0.05*diff(wh$w)
        if (object$xrel <= 0.5) {
            xmin <- diff(wh$w) * object$xrel + wh$w[1]
            xmax <- diff(wh$w) * object$xrel + wh$w[1] + object$len
        } else {
            xmin <- wh$w[2] - diff(wh$w) * (1 - object$xrel) - object$len
            xmax <- wh$w[2] - diff(wh$w) * (1 - object$xrel)
        }
        y <- wh$h[2] - diff(wh$h) * object$yrel
        
        line <- annotate(
            geom="segment", 
            color=object$col, linewidth=object$lwd,
            x=xmin, xend=xmax, y=y, yend=y)
        text <- annotate(
            geom="text", 
            x=(xmin+xmax)/2, y=y, 
            vjust=ifelse(object$yrel > 0.5, 1.5, -0.5),
            color=object$col, label=paste0(round(object$len, 1), unit))
        plot <- plot + list(line, text)
    }
    plot
}
