require(sf, quietly=TRUE)
require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

test_that("plotShape(),circles", {
    expect_error(plotShape(x=x, as="missing"), "should be")
    p <- plotSpatialData(x)
    # invalid
    expect_error(p + plotShape(i="."))
    expect_error(p + plotShape(i=100))
    # simple
    y <- shape(x, i <- "blobs_circles")
    q <- p + plotShape(i)
    expect_s3_class(q, "ggplot")
    df <- sf::st_coordinates(sf::st_as_sf(data(y)))
    geom <- ggplot2::layer_data(q, 1)$geometry
    expect_s3_class(geom, "sfc_POLYGON")
    geom <- sf::st_centroid(geom)
    fd <- sf::st_coordinates(geom)
    expect_equivalent(as.matrix(df), as.matrix(fd))
    
    # size
    q <- p + plotShape(i=i, size=s <- runif(1, 1, 10))
    expect_all_equal(ggplot2::layer_data(q, 1)$size, s)
    # color
    expect_error(show(p + plotShape(i=i, colour=".")))
    q <- p + plotShape(i=i, colour=NA) # none
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, NA)
    q <- p + plotShape(i=i, colour=c <- 1) # numeric
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, c)
    q <- p + plotShape(i=i, colour=c <- "red") # string
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, c)
    
    y <- x
    shape(y)@data <- duckspatial::ddbs_is_simple(data(shape(y)))
    q <- plotSpatialData(y) + plotShape(col="is_simple")
    expect_all_equal(layer_data(q, 1)$colour, "#F8766D")
})

test_that("plotShape(),polygons", {
    p <- plotSpatialData(x)
    y <- shape(x, i <- "blobs_polygons")
    # simple
    q <- plotSpatialData() + plotShape(x=x, i=i)
    geom <- ggplot2::layer_data(q)$geometry
    expect_s3_class(q, "ggplot")
    df <- centroids(y)
    fd <- sf::st_coordinates(sf::st_centroid(geom))
    .f <- \(.) as.matrix(.[,c(1,2)])
    expect_equivalent(.f(df), .f(fd))
    expect_s3_class(geom, "sfc_POLYGON")
    # color
    expect_error(show(p + plotShape(i = i, colour=".")))
    q <- p + plotShape(i=i, colour=NA) # none
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, NA)
    q <- p + plotShape(i=i, colour=c <- 1) # numeric
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, c)
    q <- p + plotShape(i=i, colour=c <- "red") # string
    expect_all_equal(ggplot2::layer_data(q, 1)$colour, c)
    
    q <- p + plotShape(i=i, as="border") + plotShape(i=i, as="centroid")
    expect_length(q@layers, 2L)
    
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
    p <- plotSpatialData(x)
    y <- shape(x, i <- "blobs_multipolygons")
    # simple
    q <- p + plotShape(i = i)
    expect_s3_class(q, "ggplot")
    # coloring by string
    df <- ggplot2::layer_data(q)
    q <- p + plotShape(i = i, colour="red")
    expect_all_equal(df$colour, hex <- "#595959FF")
    fd <- ggplot2::layer_data(q)
    fd$colour <- hex
    expect_identical(sort(colnames(df)), sort(colnames(fd)))
    expect_identical(df, fd[, colnames(df)])
})

test_that("plotShape(..., as)", {
    p <- plotSpatialData(x)
    f <- "blue"; c <- "red"
    .aes <- \(.) .@layers[[1]]$aes_params
    .geom <- \(.) sf::st_geometry_type(.@layers[[1]]$data, by_geometry=FALSE)
    for (i in c("blobs_circles", "blobs_polygons")) {
        # border
        q <- p + plotShape(i=i, as="border", fill=f, colour=c)
        expect_true(is.na(.aes(q)$fill))
        expect_true(.aes(q)$colour == c)
        expect_true(.geom(q) == "POLYGON")
        # solid
        q <- p + plotShape(i=i, as="solid", fill=f, colour=c)
        expect_true(.aes(q)$fill == f)
        expect_true(.aes(q)$colour == c)
        expect_true(.geom(q) == "POLYGON")
        # centroid
        q <- p + plotShape(i=i, as="centroid", fill=f, colour=c)
        expect_true(is.na(.aes(q)$fill))
        expect_true(.aes(q)$colour == c)
        expect_true(.geom(q) == "POINT")
        # invalid
        expect_error(p + plotShape(i=i, as="x"))
        expect_error(p + plotShape(i=i, as=c("border", "solid")))
    }
})
