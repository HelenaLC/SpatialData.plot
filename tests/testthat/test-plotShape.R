require(SpatialData, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("plotShape(),circles", {
    p <- plotSpatialData()
    # invalid
    expect_error(plotShape(x, "."))
    expect_error(plotShape(x, 100))
    # simple
    y <- shape(x, i <- "blobs_circles")
    q <- p + plotShape(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    df <- st_coordinates(st_as_sf(data(y)))
    fd <- q$layers[[1]]$data[, c("x", "y")]
    expect_equivalent(as.matrix(df), as.matrix(fd))
    expect_null(q$layers[[1]]$mapping$colour)
    expect_s3_class(q$layers[[1]]$geom, "GeomCircle")
    # size
    q <- p + plotShape(x, i, s=s <- runif(1, 1, 10))
    expect_s3_class(q$layers[[1]]$geom, "GeomPoint")
    expect_identical(q$layers[[1]]$aes_params$size, s)
    expect_error(show(plotSpatialData() + plotShape(x, i, s=".")))
    # color
    expect_error(plotShape(x, i, c="."))
    q <- p + plotShape(x, i, c=NA) # none
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- 1) # numeric
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- "red") # string
    expect_identical(q$layers[[1]]$aes_params$col, c)
})

test_that("plotShape(),polygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_polygons")
    # simple
    q <- p + plotShape(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    df <- st_coordinates(st_as_sf(data(y)))[, c(1, 2)]
    fd <- q$layers[[1]]$data[, c("x", "y")]
    expect_equivalent(as.matrix(df), as.matrix(fd))
    expect_s3_class(q$layers[[1]]$geom, "GeomPolygon")
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # color
    expect_error(plotShape(x, i, c="."))
    q <- p + plotShape(x, i, c=NA) # none
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- 1) # numeric
    expect_identical(q$layers[[1]]$aes_params$col, c)
    q <- p + plotShape(x, i, c=c <- "red") # string
    expect_identical(q$layers[[1]]$aes_params$col, c)
    # coloring by 'table'
    f <- list(
        numbers=\(n) runif(n),
        letters=\(n) sample(letters, n, TRUE))
    t <- getTable(y <- setTable(x, i, f), i)
    q <- p + plotShape(y, i, c=. <- "numbers")
    expect_s3_class(q, "ggplot")
    expect_null(q$guides$guides)
    q <- p + plotShape(y, i, c=. <- "letters")
    expect_s3_class(q, "ggplot")
    df <- q$layers[[1]]$data
    expect_equal(base::table(t[[.]]), base::table(df[[.]])/4)
})

test_that("plotShape(),multipolygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_multipolygons")
    # simple
    q <- p + plotShape(x, i)
    expect_s3_class(q, "ggplot")
    df <- q$layers[[1]]$data
    # coloring by string
    q <- p + plotShape(x, i, c=. <- "red")
    expect_identical(q$layers[[1]]$aes_params$col, .)
    fd <- q$layers[[1]]$data
    expect_identical(df, fd)
})
