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

test_that("invalid scalebar()", {
    # not an image/label
    expect_error(scalebar(point(x)))
    expect_error(scalebar(shape(x)))
    
    # missing 'unit'
    expect_error(scalebar(image(x)))
    y <- set_unit(image(x), "y")
    expect_error(scalebar(image(y), 1))
    
    # invalid arguments
    y <- set_unit(image(x), "x")
    v <- c(c(1,1), Inf, TRUE, "")
    for (. in v) {
        expect_error(scalebar(y, len=.))
        expect_error(scalebar(y, len=1, xrel=.))
        expect_error(scalebar(y, len=1, yrel=.))
    }
})

test_that("valid scalebar()", {
    # to make tests more challenging, crop image 
    # to be non-square & offset from the origin
    y <- list(xmin=dx <- 16, xmax=64, ymin=0, ymax=48)
    y <- set_unit(crop(image(x), y), "x")
    
    # default 'len'
    expect_silent(l <- scalebar(y, len=NULL))
    p <- ggplot() + l
    df <- layer_data(p, 1)
    expect_equal(df$xend-df$x, 0.05*dim(y)[3])
    
    # valid arguments
    l <- scalebar(y, 
        len=len <- 5.1234, 
        xrel=xrel <- 0.05, 
        yrel=yrel <- 0.11,
        col=col <- "pink", 
        lwd=lwd <- 7)
    expect_is(l, "list")
    expect_length(l, 2)
    
    # check placement
    p <- ggplot() + l
    df <- layer_data(p, 1)
    expect_equal(df$colour, col)
    expect_equal(df$linewidth, lwd)
    
    expect_equal(df$x, dx+dim(y)[3]*xrel)
    expect_equal(df$xend, dx+dim(y)[3]*xrel+len)
    expect_equal(df$y, dim(y)[2]*(1-yrel))
    expect_equal(df$yend, df$y)
    
    # flexible 'x/yrel'
    l <- scalebar(y, xrel=0, yrel=0)
    df <- layer_data(ggplot() + l, 1)
    expect_equal(df$x, dx)
    expect_equal(df$y, dim(y)[2])
    
    l <- scalebar(y, xrel=-1, yrel=-1)
    df <- layer_data(ggplot() + l, 1)
    expect_equal(df$x, dx-dim(y)[3])
    expect_equal(df$y, 2*dim(y)[2])
    
    l <- scalebar(y, xrel=a <- .9, yrel=b <- 1.2)
    df <- layer_data(ggplot() + l, 1)
    expect_equal(df$xend, dx+a*dim(y)[3])
    expect_equal(df$y, -(b-1)*dim(y)[2])
})
