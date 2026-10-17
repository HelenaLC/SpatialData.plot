require(ggplot2, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

test_that("invalid scalebar()", {
    # not an image/label
    expect_s3_class(scalebar(), "sd_scalebar")

    # missing 'unit'
    xtmp <- x
    image(xtmp)@meta@.Data[[1]][[1]]$coordinateTransformations[[1]]$output$axes[[3]]$unit <- NULL
    p_no_scalebar <- plotSpatialData(xtmp) + plotImage() + scalebar(len=3)
    expect_length(p_no_scalebar@layers, 2L)
    
    p_empty <- plotSpatialData(x) + scalebar(len=3)
    expect_length(p_empty@layers, 0L)
    
    # invalid arguments
    xtmp <- x
    v <- c(c(1,1), Inf, TRUE, "")
    p <- plotSpatialData(xtmp) + plotImage()
    for (. in v) {
        expect_error(p + scalebar(len=.))
        expect_error(p + scalebar(len=1, xrel=.))
        expect_error(p + scalebar(len=1, yrel=.))
    }
})

test_that("valid scalebar()", {
    # to make tests more challenging, crop image 
    # to be non-square & offset from the origin
    y <- list(xmin=dx <- 16, xmax=64, ymin=0, ymax=48)
    y <- crop(image(x), y)
    xtmp <- x
    image(xtmp) <- y
    
    p <- plotSpatialData(xtmp) + plotImage()
    
    # default 'len'
    expect_silent(l <- p + scalebar(len=NULL))
    expect_length(l@layers, 4L)
    df <- layer_data(l, 3)
    expect_equal(df$xend-df$x, 0.05*dim(y)[3])
    
    # valid arguments
    l <- p + scalebar( 
        len=len <- 5.1234, 
        xrel=xrel <- 0.05, 
        yrel=yrel <- 0.11,
        col=col <- "pink", 
        lwd=lwd <- 7)
    expect_s3_class(l, "ggplot")
    expect_length(l@layers, 4L)
    
    # check placement
    df <- layer_data(l, 3)
    expect_equal(df$colour, col)
    expect_equal(df$linewidth, lwd)
    
    expect_equal(df$x, dx+dim(y)[3]*xrel)
    expect_equal(df$xend, dx+dim(y)[3]*xrel+len)
    expect_equal(df$y, dim(y)[2]*(1-yrel))
    expect_equal(df$yend, df$y)
    
    # flexible 'x/yrel'
    l <- p + scalebar(xrel=0, yrel=0)
    df <- layer_data(l, 3)
    expect_equal(df$x, dx)
    expect_equal(df$y, dim(y)[2])
    
    l <- p + scalebar(xrel=-1, yrel=-1)
    df <- layer_data(l, 3)
    expect_equal(df$x, dx-dim(y)[3])
    expect_equal(df$y, 2*dim(y)[2])
    
    l <- p + scalebar(xrel=a <- .9, yrel=b <- 1.2)
    df <- layer_data(l, 3)
    expect_equal(df$xend, dx+a*dim(y)[3])
    expect_equal(df$y, -(b-1)*dim(y)[2])
})
