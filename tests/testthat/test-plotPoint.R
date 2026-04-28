require(ggplot2, quietly=TRUE)
require(SpatialData, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("plotPoint(),SpatialData", {
    p <- plotSpatialData()
    y <- point(x, i <- "blobs_points")
    df <- dplyr::collect(data(y))
    # invalid
    expect_error(plotPoint(x, "."))
    expect_error(plotPoint(x, 100))
    expect_error(show(ggplot() + plotPoint(x, i, color=".")))
    # simple
    q <- p + plotPoint(x, i)
    expect_s3_class(q, "ggplot")
    expect_identical(q$layers[[1]]$data, df)
    expect_null(q$layers[[1]]$mapping$colour)
    # coloring by color
    q <- p + plotPoint(x, i, colour=. <- "red")
    expect_identical(q$layers[[1]]$data, df)
    expect_identical(q$layers[[1]]$aes_params$colour, .)
    # coloring by value
    q <- p + plotPoint(x, i, colour="genes")
    expect_s3_class(q, "ggplot")
})
