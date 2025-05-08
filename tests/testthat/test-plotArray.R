require(SpatialData, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("plotSpatialData()", {
    p <- plotSpatialData()
    expect_s3_class(p, "ggplot")
    expect_length(p$data, 0)
    expect_length(p$layers, 0)
})

.check_xy <- \(p, d) {
    xy <- p$scales$scales
    expect_equal(xy[[1]]$limits, c(0, d[2]))
    expect_equal(xy[[2]]$limits, c(-d[1], 0))
}

test_that(".guess_scale", {
    img <- ImageArray(
        lys <- lapply(
            dim <- lapply(c(6, 3), \(.) c(3, rep(., 2))), \(.) 
            array(sample(seq_len(255), prod(.), replace=TRUE), dim=.)))
    # manual scale
    expect_identical(.get_multiscale_data(img, k=1), lys[[1]]) 
    expect_identical(.get_multiscale_data(img, k=2), lys[[2]])
    # automatic scale
    expect_identical(.get_multiscale_data(img, k=NULL, w=5, h=7), lys[[1]]) 
    expect_identical(.get_multiscale_data(img, k=NULL, w=2, h=2), lys[[2]])
})

test_that("plotImage()", {
    p <- plotSpatialData()
    # simple
    y <- image(x, "blobs_image")
    y <- y[,,seq_len(32)] # subset to make things harder
    image(x, i <- ".") <- y
    q <- p + plotImage(x, i)
    expect_s3_class(q, "ggplot")
    expect_equal(q$coordinates$ratio, 1)
    .check_xy(q, dim(y)[-1])
    # multiscale
    y <- image(x, "blobs_multiscale_image")
    y <- y[,seq_len(32),] # same thing but different
    image(x, i <- ".") <- y
    q <- lapply(seq_along(y@data), \(.) p + plotImage(x, i, k=.))
    lapply(q, .check_xy, dim(y)[-1])
    lapply(seq_along(q), \(.) {
        l <- q[[.]]$layers[[1]]
        l <- l$geom_params$raster
        expect_equal(dim(l), dim(data(y, .))[-1])
    })
})

test_that("plotLabel()", {
    p <- plotSpatialData()
    # simple
    y <- label(x, i <- "blobs_labels")
    y <- y[,seq_len(32)] # subset to make things harder
    q <- p + plotLabel(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    expect_equal(q$coordinates$ratio, 1)
    expect_is(q$layers[[1]]$mapping$fill, "quosure")
    # alpha
    q <- p + plotLabel(x, i, a=a <- runif(1))
    expect_identical(q$layers[[1]]$aes_params$alpha, a)
    expect_error(show(plotSpatialData() + plotLabel(x, i, a=".....")))
    expect_error(show(plotSpatialData() + plotLabel(x, i, a=c(1, 2))))
    # TODO: use 'annotation_raster'
    # TODO: multiscale plotting
})
