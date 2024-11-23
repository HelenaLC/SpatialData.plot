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
    expect_error(plotPoint(x, i, c="."))
    # simple
    q <- p + plotPoint(x, i)
    expect_s3_class(q, "ggplot")
    expect_identical(q$layers[[1]]$data, df)
    expect_null(q$layers[[1]]$mapping$colour)
    # coloring by color
    q <- p + plotPoint(x, i, c=. <- "red")
    expect_identical(q$layers[[1]]$data, df)
    expect_identical(q$layers[[1]]$aes_params$colour, .)
    # coloring by coord
    q <- p + plotPoint(x, i, c="x")
    expect_s3_class(q, "ggplot")
    expect_null(q$guides$guides)
    expect_identical(q$layers[[1]]$data, df)
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # coloring by other
    q <- p + plotPoint(x, i, c="genes")
    expect_s3_class(q, "ggplot")
    expect_is(q$guides$guides, "list")
    expect_identical(q$layers[[1]]$data, df)
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # coloring by 'table'
    f <- list(
        numbers=\(n) runif(n),
        letters=\(n) sample(letters, n, TRUE))
    t <- getTable(z <- setTable(x, i, f), i)
    .test <- \(p, t) {
        expect_s3_class(p, "ggplot")
        df <- p$layers[[1]]$data
        ik <- meta(t)$instance_key
        cs <- match(df[[ik]], t[[ik]])
        expect_identical(df[[.]], t[[.]][cs])
        expect_is(p$layers[[1]]$mapping$colour, "quosure")
    }
    # continuous
    q <- p + plotPoint(z, i, c=. <- "numbers")
    expect_null(q$guides$guides)
    .test(q, t)
    # discrete
    q <- p + plotPoint(z, i, c=. <- "letters")
    expect_is(q$guides$guides, "list")
    .test(q, t)
})

test_that("plotPoint(),PointFrame", {
    # simple
    y <- point(x, 1)
    p <- plotPoint(y)
    expect_s3_class(p, "ggplot")
    # coloring by color
    p <- plotPoint(y, c=. <- "red")
    expect_identical(p$layers[[1]]$aes_params$colour, .)
    # invalid
    expect_error(plotPoint(y, c="."))
})

