require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

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
    expect_error(plotLabel(x, i="x"))
    expect_error(plotLabel(x, i=123))
    # bad coordinate space
    expect_error(plotLabel(x, j="x"))
    expect_error(plotLabel(x, j=123))
})

test_that("3/4D plotLabel()", {
    x <- .mock(t=2, z=3)
    # invalid
    expect_error(plotLabel(x, z=4))
    expect_error(plotLabel(x, t=3))
    expect_error(plotLabel(x, t=c(1,2)))
    expect_error(plotLabel(x, z=c(2,3)))
    # valid
    expect_is(plotLabel(x), "list") # project both
    expect_is(plotLabel(x, t=1), "list") # t-slice
    expect_is(plotLabel(x, z=1), "list") # z-slice
    # check data
    x <- .mock(t=2, z=3, y=h <- 44, x=w <- 55)
    expect_is(l <- plotLabel(x, z=1, t=1), "list")
    df <- layer_data(ggplot() + l)
    expect_equal(nrow(df), h*w)
    expect_is(df$fill, "character")
    expect_equal(range(df$x), c(1, w))
    expect_equal(range(df$y), c(1, h))
})

test_that("coloring plotLabel()", {
    # mock annotation
    ni <- length(id <- instances(label(x)))
    df <- DataFrame(id, num=runif(ni), fac=gl(ni, 1))
    mx <- matrix(runif((ng <- 3)*ni), nr=ng)
    rownames(mx) <- letters[seq_len(ng)]
    se <- SingleCellExperiment(list(mx), colData=df)
    y <- setTable(x, labelNames(x)[1], se)
    
    # continuous (colData)
    expect_is(l <- plotLabel(y, c="num"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    # continuous (assay)
    expect_is(l <- plotLabel(y, c="a"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "numeric")
    
    # discrete
    expect_is(l <- plotLabel(y, c="fac"), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
    # by instance (default)
    expect_is(l <- plotLabel(x), "list")
    p <- ggplot() + l
    g <- get_guide_data(p, "fill")
    expect_is(g[[2]], "character")
})
