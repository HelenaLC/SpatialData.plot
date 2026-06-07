require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
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

test_that("point coloring", {
    fk <- feature_key(point(x))
    fs <- unique(point(x)[[fk]])
    expect_is(l <- plotPoint(x, col=fk), "list")
    expect_s3_class(p <- ggplot() + l, "ggplot")
    expect_is(get_layer_data(p)$colour, "character")
    g <- get_guide_data(p, "colour")
    expect_setequal(g[[2]], fs)
})

test_that("point feature", {
    # invalid
    expect_error(plotPoint(x, key=""))
    expect_error(plotPoint(x, key="x"))
    expect_error(plotPoint(x, key=123))
    expect_error(plotPoint(x, key=character(0)))
    # single valid
    fk <- feature_key(point(x))
    fs <- unique(point(x)[[fk]])
    ks <- sample(fs, 1) 
    expect_is(l <- plotPoint(x, key=ks), "list")
    df <- get_layer_data(ggplot() + l)
    expect_equal(nrow(df), sum(point(x)[[fk]] == ks))
    # multiple valid
    expect_is(l <- plotPoint(x, key=fs), "list")
    df <- get_layer_data(ggplot() + l)
    expect_equal(nrow(df), length(point(x)))
})

test_that("point downsampling", {
    # valid
    n <- length(point(x))
    m <- sample(seq(2, n/2), 1)
    expect_is(l <- plotPoint(x, n=m), "list")
    df <- get_layer_data(ggplot() + l)
    expect_equal(nrow(df), m)
    # acceptable
    expect_no_error(plotPoint(x, n=n+1))
    expect_no_error(plotPoint(x, n=Inf))
    expect_no_error(plotPoint(x, n=NULL))
    # invalid
    expect_error(plotPoint(x, n=0))
    expect_error(plotPoint(x, n=-1))
    expect_error(plotPoint(x, n=-Inf))
})

test_that("shape annotation", {
    ni <- length(id <- instances(shape(x)))
    df <- DataFrame(id, num=runif(ni), fac=gl(ni, 1))
    mx <- matrix(runif((ng <- 3)*ni), nr=ng)
    rownames(mx) <- letters[seq_len(ng)]
    se <- SingleCellExperiment(list(mx), colData=df)
    y <- setTable(x, shapeNames(x)[1], se)
    
    # continuous (colData)
    expect_is(l <- plotShape(y, fill="num"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # continuous (assay)
    expect_is(l <- plotShape(y, fill="a"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # discrete
    expect_is(l <- plotShape(y, fill="fac"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
    
    # arbitrary aesthetics
    l <- plotShape(y, 
        fill="fac", color="num", 
        stroke="num", linetype="fac")
    df <- layer_data(ggplot() + l)
    expect_equal(df$stroke, se$num)
    expect_is(df$fill, "character")
    expect_is(df$colour, "character")
    expect_is(df$linetype, "character")
    expect_true(df$linetype[1] == "solid")
})
