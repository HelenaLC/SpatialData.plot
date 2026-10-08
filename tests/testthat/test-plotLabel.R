require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

# mock high-dim. label
.mock <- \(t=0, z=0, y=80, x=120) {
    dim <- c(t, z, y, x); dim <- dim[dim != 0]
    arr <- drop(as(array(sample(prod(dim)), dim), "ZarrArray"))
    sda <- SpatialDataAttrs(type="label", dim=length(dim))
    SpatialData(labels=list(SpatialDataLabel(list(arr), sda)))
}

test_that("invalid plotLabel()", {
    # bad element
    expect_error(plotSpatialData(x) + plotLabel(i="x"))
    expect_error(plotSpatialData(x) + plotLabel(i=123))
    # bad coordinate space
    expect_error(plotSpatialData(x) + plotLabel(j="x"))
    expect_error(plotSpatialData(x) + plotLabel(j=123))
})

test_that("3/4D plotLabel()", {
    x <- .mock(t=2, z=3)
    # invalid
    expect_error(plotSpatialData(x) + plotLabel(z=4))
    expect_error(plotSpatialData(x) + plotLabel(t=3))
    expect_error(plotSpatialData(x) + plotLabel(t=c(1,2)))
    expect_error(plotSpatialData(x) + plotLabel(z=c(2,3)))
    # valid
    expect_is(plotLabel(x=x), "sd_label") # project both
    expect_is(plotLabel(x=x, t=1), "sd_label") # t-slice
    expect_is(plotLabel(x=x, z=1), "sd_label") # z-slice
    # check that t=1 will be chosen if not specified
    set.seed(782L)
    p1 <- plotSpatialData(x) + plotLabel(t=1)
    set.seed(782L)
    p2 <- plotSpatialData(x) + plotLabel()
    expect_identical(ggplot2::layer_data(p1, 1),
                     ggplot2::layer_data(p2, 1))
    # check data
    x <- .mock(t=2, z=3, y=h <- 44, x=w <- 55)
    expect_is(l <- plotLabel(z=1, t=1), "sd_label")
    df <- ggplot2::layer_data(plotSpatialData(x) + l)
    expect_equal(nrow(df), h*w)
    expect_is(df$fill, "character")
    expect_equal(range(df$x), c(1, w))
    expect_equal(range(df$y), c(1, h))
})

test_that("coloring plotLabel()", {
    # mock annotation
    ni <- length(id <- instances(label(x)))
    df <- S4Vectors::DataFrame(id, num=runif(ni), fac=gl(ni, 1))
    mx <- matrix(runif((ng <- 3)*ni), nr=ng)
    rownames(mx) <- letters[seq_len(ng)]
    se <- SingleCellExperiment::SingleCellExperiment(list(mx), colData=df)
    y <- setTable(x, labelNames(x)[1], se)
    
    # continuous (colData)
    expect_is(l <- plotLabel(1, c="num"), "sd_label")
    p <- plotSpatialData(y, ct=1) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # continuous (assay)
    expect_is(l <- plotLabel(c="a"), "sd_label")
    p <- plotSpatialData(y) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    
    # discrete
    expect_is(l <- plotLabel(c="fac"), "sd_label")
    p <- plotSpatialData(y) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
    # by instance (default)
    expect_is(l <- plotLabel(), "sd_label")
    p <- plotSpatialData(x) + l
    g <- ggplot2::get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
})
