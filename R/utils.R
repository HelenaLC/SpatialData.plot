# convenience functions until this is fixed/exported by 'SpatialData'

#' @importFrom methods is
#' @importFrom SingleCellExperiment int_metadata
.spatialdata_attrs <- \(x) {
    if (is(x, "SingleCellExperiment")) {
        int_metadata(x)$spatialdata_attrs
    } else if (is(x, "SpatialDataElement")) {
        meta(x)$spatialdata_attrs
    } else if (is(x, "Zattrs")) {
        x$spatialdata_attrs
    } else stop("invalid 'x'")
}

.instance_key <- \(x) .spatialdata_attrs(x)$instance_key
.region_key <- \(x) .spatialdata_attrs(x)$region_key
.region <- \(x) .spatialdata_attrs(x)$region

#' @importFrom SingleCellExperiment int_colData
.instance_ids <- \(x) int_colData(x)[[.instance_key(x)]]

#' @importFrom grDevices col2rgb
.str_is_col <- \(x) !inherits(tryCatch(error=\(e) e, col2rgb(x)), "error")

#' @importFrom ggplot2 theme_bw theme element_blank element_text element_line
.theme <- list(
    theme_bw(), theme(
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.size=unit(0, "lines"),
        legend.background=element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text=element_text(color="grey"),
        axis.ticks=element_line(color="grey"))
)

# default colors (from ImageJ/Fiji)
.DEFAULT_COLORS <- c("red", "green", "blue", "gray", "cyan", "magenta", "yellow")

# image data type factors (max values)
# TODO: add more cases from other data types
# https://doc.embedded-wizard.de/uint-type
.DTYPE_MAX_VALUES <- c("uint8" = 255,
                       "uint16" = 65535,
                       "uint32" = 4294967295,
                       "uint64" = 2^64 - 1)

# guess scale of image or label
.guess_scale <- \(x, w, h) {
    i <- match(c("y", "x"), axes(x=x, y="name"))
    d <- vapply(x@data, dim, numeric(length(dim(x))))
    d <- apply(d, 2, \(.) sum(abs(.[i]-c(h, w))))
    which.min(d)
}

# get multiscale
.get_multiscale_data <- \(x, k=NULL, w=800, h=800) {
    if (!is.null(k)) return(data(x, k))
    data(x, .guess_scale(x, w, h))
}

#' @importFrom spatialdataR meta
.get_multiscale_scale <- \(x) {
    ms <- spatialdataR:::multiscales(meta(x))[[1]]
    ds <- ms$datasets[[1]]
    ct <- ds$coordinateTransformations[[1]]
    return(unlist(ct$scale))
}

.subset_array_by_axes <- \(a, axisNames, ..., drop=FALSE) {
    if (length(dim(a)) != length(axisNames)) {
        stop("axisNames must have the same length as the number of dimensions of x")
    }
    specs <- list(...)
    idx <- lapply(axisNames, \(nm) {
        if (!is.null(specs[[nm]])) {
            specs[[nm]]
        } else {
            seq.int(dim(a)[match(nm, axisNames)])
        }
    })
    do.call("[", c(list(a), idx, list(drop=drop)))
}
