require(spatialdataR, quietly=TRUE)
require(SpatialData.data, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

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
    # get indices of channels
    expect_equal(.ch_idx(image(x,1), ch=c(2,0,1)), c(3,1,2))
    # return first if no matching channel
    expect_warning(expect_equal(.ch_idx(image(x,1), ch=99), 1)) 
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
    expect_error(.check_cl(c(0.2, 0.4, 0.6), 3)) # non-list
    expect_error(.check_cl(as.list(seq_len(4)), 3)) # wrong length
    expect_error(.check_cl(list(NULL, NULL, c(-1, 1)), 3)) # negative entry
    expect_error(.check_cl(as.list(letters[seq_len(3)]), 3)) # non-numeric
    expect_error(.check_cl(list(NULL, NULL, c(1, 0)), 3)) # decreasing
    expect_error(.check_cl(list(NULL, NULL, -1), 3)) # negative scalar
    expect_error(.check_cl(list(NULL, NULL, 0), 3)) # zero scalar
})

# mock multiplex image
l <- 4; m <- 80; n <- 120
a <- as(array(runif(l*m*n), c(l,m,n)), "ZarrArray")
y <- SpatialDataImage(list(a), SpatialDataAttrs(type="image", dim=2, nch=l))
x <- SpatialData(list(y))

test_that(".norm_ia", {
    # valid data type
    dt <- data_type(a)
    b <- .norm_ia(realize(a), dt)
    expect_equal(
        tolerance=1e-3,
        apply(b, 1, range), 
        replicate(l, c(0, 1)))
    # invalid data type
    b <- .norm_ia(realize(a), "")
    expect_equal(
        tolerance=1e-3,
        apply(b, 1, range), 
        replicate(l, c(0, 1)))
})

test_that(".prep_ia", { testthat::skip()
    dt <- data_type(a)
    ch <- seq_len(d <- dim(a)[1])
    a <- .norm_ia(realize(a), dt)
    # no colors, no contrasts
    b <- .prep_ia(a, ch)
    expect_is(b, "matrix")
    expect_length(dim(b), 2)
    expect_is(b[1,1], "character")
    # colors
    pal <- colors()[seq_len(l)]
    b <- .prep_ia(a, c=pal)
    expect_equal(dim(a)[-1], dim(b))
    expect_is(b, "matrix")
    expect_is(c(b), "character")
})
