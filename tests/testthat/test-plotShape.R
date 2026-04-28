require(ggplot2, quietly=TRUE)
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
    q <- p + plotShape(x, i)
    expect_s3_class(q, "ggplot")
    df <- st_coordinates(st_as_sf(data(y)))
    geom <- layer_data(q, 1)$geometry
    fd <- st_coordinates(geom)
    expect_equivalent(as.matrix(df), as.matrix(fd))
    expect_null(q$layers[[1]]$mapping$colour)
    expect_s3_class(geom, "sfc_POINT")
    # size
    q <- p + plotShape(x, i, size=s <- runif(1, 1, 10))
    expect_all_equal(layer_data(q, 1)$size, s)
    # color
    expect_error(show(ggplot() + plotShape(x, i, colour=".")))
    q <- p + plotShape(x, i, colour=NA) # none
    expect_all_equal(layer_data(q, 1)$colour, NA)
    q <- p + plotShape(x, i, colour=c <- 1) # numeric
    expect_all_equal(layer_data(q, 1)$colour, c)
    q <- p + plotShape(x, i, colour=c <- "red") # string
    expect_all_equal(layer_data(q, 1)$colour, c)
})

test_that("plotShape(),polygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_polygons")
    # simple
    q <- p + plotShape(x, i)
    geom <- layer_data(q)$geometry
    expect_s3_class(q, "ggplot")
    df <- centroids(y)
    fd <- st_coordinates(geom)
    .f <- \(.) as.matrix(.[,c(1,2)])
    expect_equivalent(.f(df), .f(fd))
    expect_s3_class(geom, "sfc_POLYGON")
    # color
    expect_error(show(ggplot() + plotShape(x, i, colour=".")))
    q <- p + plotShape(x, i, colour=NA) # none
    expect_all_equal(layer_data(q, 1)$colour, NA)
    q <- p + plotShape(x, i, colour=c <- 1) # numeric
    expect_all_equal(layer_data(q, 1)$colour, c)
    q <- p + plotShape(x, i, colour=c <- "red") # string
    expect_all_equal(layer_data(q, 1)$colour, c)
    # TODO
    # # coloring by 'table'
    # f <- list(
    #     numbers=\(n) runif(n),
    #     letters=\(n) sample(letters, n, TRUE))
    # t <- getTable(y <- setTable(x, i, f), i)
    # q <- p + plotShape(y, i, colour=. <- "numbers")
    # expect_s3_class(q, "ggplot")
    # expect_null(q$guides$guides)
    # q <- p + plotShape(y, i, colour=. <- "letters")
    # expect_s3_class(q, "ggplot")
    # df <- layer_data(q)
    # expect_equal(base::table(t[[.]]), base::table(df[[.]])/4)
})

test_that("plotShape(),multipolygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_multipolygons")
    # simple
    q <- p + plotShape(x, i)
    expect_s3_class(q, "ggplot")
    # coloring by string
    df <- layer_data(q)
    q <- p + plotShape(x, i, colour="red")
    expect_all_equal(df$colour, hex <- "#595959FF")
    fd <- layer_data(q)
    fd$colour <- hex
    expect_identical(df, fd)
})
