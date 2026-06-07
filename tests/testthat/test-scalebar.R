require(ggplot2, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

set_unit <- \(x, dim="x", val="micron") {
    y <- meta(x)
    i <- which(axes(x, "name") == dim)
    y$multiscales[[1]]$axes[[i]]$unit <- val
    x@meta <- y
    return(x)
}

test_that("scalebar()", {
    # not an image/label
    expect_error(scalebar(point(x)))
    expect_error(scalebar(shape(x)))
    
    # missing 'unit'
    expect_error(scalebar(image(x)))
    y <- set_unit(image(x), "y")
    expect_error(scalebar(image(y), 1))
    
    # valid specification
    y <- set_unit(image(x))
    l <- scalebar(y, 
        len=len <- 5.1234, 
        xrel=xrel <- 0.05, 
        yrel=yrel <- 0.11,
        col=col <- "pink", 
        lwd=lwd <- 7)
    expect_is(l, "list")
    expect_length(l, 2)
    
    # check data
    p <- ggplot() + l
    df <- layer_data(p, 1)
    expect_equal(df$colour, col)
    expect_equal(df$linewidth, lwd)
    
    expect_equal(df$x, dim(y)[3]*xrel)
    expect_equal(df$xend, dim(y)[3]*xrel+len)
    expect_equal(df$y, dim(y)[2]*(1-yrel))
    expect_equal(df$yend, df$y)
})
