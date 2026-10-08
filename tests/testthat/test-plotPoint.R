require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

test_that("plotPoint(),SpatialData", {
    p <- plotSpatialData(x)
    y <- point(x, i <- "blobs_points")
    df <- dplyr::collect(data(y))
    # invalid
    expect_error(p + plotPoint(i = "."))
    expect_error(p + plotPoint(i = 100))
    expect_message(expect_error(show(p + plotPoint(i = i, color="."))),
                   "Coordinate system already present")
    # simple
    expect_message(q <- p + plotPoint(i), 
                   "Coordinate system already present")
    expect_s3_class(q, "ggplot")
    expect_identical(q$layers[[1]]$data, df)
    expect_null(q$layers[[1]]$mapping$colour)
    # coloring by color
    expect_message(q <- p + plotPoint(i = i, colour=. <- "red"),
                   "Coordinate system already present")
    expect_identical(q$layers[[1]]$data, df)
    expect_identical(q$layers[[1]]$aes_params$colour, .)
    # coloring by value
    q <- p + plotPoint(i = i, colour="genes")
    expect_s3_class(q, "ggplot")
})

test_that("point coloring", {
    fk <- feature_key(point(x))
    fs <- unique(point(x)[[fk]])
    expect_is(l <- plotPoint(col=fk), "sd_point")
    expect_s3_class(p <- plotSpatialData(x) + l, "ggplot")
    expect_is(ggplot2::get_layer_data(p)$colour, "character")
    g <- ggplot2::get_guide_data(p, "colour")
    expect_setequal(g[[2]], fs)
})

test_that("point feature", {
    # invalid
    p <- plotSpatialData(x)
    expect_error(p + plotPoint(key=""))
    expect_error(p + plotPoint(key="x"))
    expect_error(p + plotPoint(key=123))
    expect_error(p + plotPoint(key=character(0)))
    # single valid
    fk <- feature_key(point(x))
    fs <- unique(point(x)[[fk]])
    ks <- sample(fs, 1) 
    expect_is(l <- plotPoint(key=ks), "sd_point")
    df <- ggplot2::get_layer_data(p + l)
    expect_equal(nrow(df), sum(point(x)[[fk]] == ks))
    # multiple valid
    expect_is(l <- plotPoint(key=fs), "sd_point")
    df <- ggplot2::get_layer_data(p + l)
    expect_equal(nrow(df), length(point(x)))
})

test_that("point downsampling", {
    # valid
    p <- plotSpatialData(x)
    n <- length(point(x))
    m <- sample(seq(2, n/2), 1)
    expect_is(l <- plotPoint(n=m), "sd_point")
    df <- ggplot2::get_layer_data(p + l)
    expect_equal(nrow(df), m)
    # acceptable
    expect_no_error(p + plotPoint(n=n+1))
    expect_no_error(p + plotPoint(n=Inf))
    expect_no_error(p + plotPoint(n=NULL))
    # invalid
    expect_error(p + plotPoint(n=0))
    expect_error(p + plotPoint(n=-1))
    expect_error(p + plotPoint(n=-Inf))
})

test_that("shape annotation", {
    ni <- length(id <- instances(shape(x)))
    df <- S4Vectors::DataFrame(id, num=runif(ni), fac=gl(ni, 1))
    mx <- matrix(runif((ng <- 3)*ni), nr=ng)
    rownames(mx) <- letters[seq_len(ng)]
    se <- SingleCellExperiment::SingleCellExperiment(list(mx), colData=df)
    y <- setTable(x, shapeNames(x)[1], se)
    
    # continuous (colData)
    expect_is(l <- plotShape(fill="num"), "sd_shape")
    p <- plotSpatialData(y) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # continuous (assay)
    expect_is(l <- plotShape(fill="a"), "sd_shape")
    p <- plotSpatialData(y) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # discrete
    expect_is(l <- plotShape(fill="fac"), "sd_shape")
    p <- plotSpatialData(y) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
    
    # arbitrary aesthetics
    l <- plotShape(fill="fac", color="num", 
                   stroke="num", linetype="fac")
    df <- ggplot2::layer_data(plotSpatialData(y) + l)
    expect_equal(df$stroke, se$num)
    expect_is(df$fill, "character")
    expect_is(df$colour, "character")
    expect_is(df$linetype, "character")
    expect_true(df$linetype[1] == "solid")
})
