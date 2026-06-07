# internal helper for null-coalescing
`%||%` <- \(a, b) if (is.null(a)) b else a

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
.get_ms_data <- \(x, k=NULL, w=800, h=800) {
    if (!is.null(k)) return(data(x, k))
    data(x, .guess_scale(x, w, h))
}

# x = image or label
# y = high-dim. array
# z = (optional) index
.project <- \(x, y, z=NULL) {
    # max-projection over z-stacks
    axisNames <- axes(x, y="name")
    zidx <- which(axisNames == "z")
    if (length(zidx) > 0) {
        if (is.null(z)) {
            # max-projection across z-slices
            y <- apply(y, seq_along(dim(x))[-zidx], max)
        } else {
            if (length(z) > 1) stop("only a single z-plane can be selected")
            # subset target z-slice
            y <- .subset_array_by_axes(a=y, axisNames=axisNames, z=z, drop=FALSE)
            dim(y) <- dim(y)[axisNames != "z"]
        }
    }
    y
}

#' @importFrom utils tail
.raw_wh <- \(x) {
    wh <- metadata(x)$wh
    if (!is.null(wh)) {
        df <- data.frame(x=wh[[1]], y=wh[[2]])
    } else {
        ds <- dim(data(x, 1))
        df <- data.frame(
            x=c(0, tail(ds, 1)), 
            y=c(0, tail(ds, 2)[1]))
    }
    wh <- list(w=df$x, h=df$y)
    return(wh)
}
    
# map index to physical space
# through multi-scale adjustment
.get_wh <- \(x) {
    wh <- .raw_wh(x)
    if (wh$w[2] == tail(dim(x), 1) ||
        wh$h[2] == tail(dim(x), 2)[1]) {
        ts <- spatialdataR:::.get_ms_scale(x)
        tx <- tail(ts, 1)
        ty <- tail(ts, 2)[1]
    } else {
        tx <- ty <- 1
    }
    wh$w[2] <- wh$w[2]*tx
    wh$h[2] <- wh$h[2]*ty
    return(wh)
}

.subset_array_by_axes <- \(a, axisNames, ..., drop=FALSE) {
    # this should never be trigger as object validity should prevent it
    ok <- length(dim(a)) == length(axisNames)
    if (!ok) stop("'length(axes(x))' must equal 'length(dim(x))'")
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

.unit_map <- c(micrometer="\U03BCm", micron="\U03BCm")
