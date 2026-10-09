require(ggplot2, quietly=TRUE)
require(spatialdataR, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

# mock high-dim. image
.mock <- \(c=3, t=0, z=0, y=80, x=120) {
    dim <- c(c, t, z, y, x); dim <- dim[dim != 0]
    arr <- drop(as(array(runif(prod(dim)), dim), "ZarrArray"))
    sda <- SpatialDataAttrs(dim=length(dim)-1, nch=c)
    SpatialDataImage(list(arr), sda)
}

test_that(".is_rgb()", {
    # valid integer vector
    expect_false(.is_rgb(c(0, 1, 1)))
    expect_true(.is_rgb(. <- seq(0, 2)))
    expect_true(.is_rgb(rev(.)))
    # valid character vector
    expect_false(.is_rgb(c("r", "g", "g")))
    expect_true(.is_rgb(. <- c("r", "g", "b")))
    expect_true(.is_rgb(rev(.)))
    # only works for 'SpatialDataImage'
    expect_true(.is_rgb(image(x, 1)))
    expect_error(.is_rgb(label(x, 1)))
})

test_that(".ch_idx()", {
    expect_identical(.ch_idx(image(x,1), NULL), 1)
    # get indices of channels
    expect_equal(.ch_idx(image(x,1), ch=c(2,0,1)), c(3,1,2))
    # return first if no matching channel
    expect_warning(expect_equal(.ch_idx(image(x,1), ch=99), 1)) 
})

test_that(".df_i()", {
    m <- .mock(c=3, t=2, z=1, y=5, x=3)
    expect_identical(.df_i(x=m, k=1, ch="a", t=1, c=NULL, cl=NULL, z=1),
                     .df_i(x=m, k=1, ch="a", t=NULL, c=NULL, cl=NULL, z=1))
    expect_error(.df_i(x=m, k=1, ch=1, t=c(1,2), c=NULL, cl=NULL, z=1),
                 "Only a single timepoint can be selected")
})

test_that(".check_cl", {
    # valid
    n <- sample(seq(3, 9), 1)
    v <- replicate(n, sort(runif(2)), FALSE)
    expect_identical(.check_cl(v, n), do.call(rbind, v))
    # one NULL, rest scalar
    n <- sample(seq(3, 9), 1)
    i <- sample(n, 1)
    . <- replicate(n, NULL, FALSE)
    .[[i]] <- v <- c(0.2, 0.8)
    l <- .check_cl(., n)
    expect_is(l, "matrix")
    expect_identical(l[i,], v)
    expect_identical(l[-i,], t(replicate(n-1, c(0, 1))))
    # invalid
    expect_error(.check_cl(c("0.2", "0.4"), 3)) # non-list
    expect_error(.check_cl(as.list(seq_len(4)), 3)) # wrong length
    expect_error(.check_cl(list(NULL, NULL, c(-1, 1)), 3)) # negative entry
    expect_error(.check_cl(as.list(letters[seq_len(3)]), 3)) # non-numeric
    expect_error(.check_cl(list(NULL, NULL, c(1, 0)), 3)) # decreasing
    expect_error(.check_cl(list(NULL, NULL, -1), 3)) # negative scalar
    expect_error(.check_cl(list(NULL, NULL, 0), 3)) # zero scalar
})

test_that(".norm_ia", {
    a <- data(.mock())
    nch <- dim(a)[1]
    # valid data type
    dt <- data_type(a)
    b <- .norm_ia(DelayedArray::realize(a), dt)
    expect_equal(
        tolerance=1e-3,
        apply(b, 1, range), 
        replicate(nch, c(0, 1)))
    # invalid data type
    b <- .norm_ia(DelayedArray::realize(a), "")
    expect_equal(
        tolerance=1e-3,
        apply(b, 1, range), 
        replicate(nch, c(0, 1)))
    # integer type
    b <- .norm_ia(a, dt = "uint8")
    expect_equal(range(b), range(a) / (2^8 - 1))
    expect_equal(.norm_ia(2 * as.array(a), dt = "missing"),
                 .norm_ia(3 * as.array(a), dt = "missing"))
})

test_that(".prep_ia", {
    # insufficient default colors
    a <- data(.mock(33))
    expect_error(.prep_ia(a), "default")
    # no colors, no contrasts
    a <- data(i <- .mock(c=c <- 7))
    b <- .prep_ia(a, seq_len(c))
    expect_is(b, "matrix")
    expect_length(dim(b), 2)
    expect_equal(dim(a)[-1], dim(b))
    expect_is(b[1,1], "character")
    # colors
    pal <- colors()[seq_len(c)]
    b <- .prep_ia(a, c=pal)
    expect_length(dim(b), 2)
    expect_equal(dim(a)[-1], dim(b))
    expect_is(b, "matrix")
    expect_is(b[1,1], "character")
    
    a <- data(.mock(1))
    b <- .prep_ia(a)
    expect_true(all(apply(col2rgb(c(b)), 2, \(x) length(unique(x))) == 1))
})

test_that("plotImage,3/4D", {
    f <- \(x, ...) plotImage(SpatialData(images=list(x)), ...)
    m <- .mock(c=5, t=3, z=4)
    # valid
    expect_is(f(m), "sd_image") # project both
    expect_is(f(m, t=1), "sd_image") # t-slice
    expect_is(f(m, z=1), "sd_image") # z-slice

    p <- plotSpatialData(SpatialData(images = list(m = m))) + 
        plotImage()
    expect_s3_class(p, "ggplot")
    
    p1 <- plotSpatialData() + plotImage(x=x)
    p2 <- plotSpatialData(x) + plotImage(j=1)
    expect_identical(ggplot2::layer_data(p1, 2), ggplot2::layer_data(p2, 2))
    
    # invalid
    #expect_error(f(x, t=4))
    #expect_error(f(x, z=5)) TODO: this is not throwing an error?
})
