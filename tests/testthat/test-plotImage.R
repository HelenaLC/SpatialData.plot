require(SpatialData, quietly=TRUE)
require(SpatialData.data, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
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
    # only works for 'ImageArray'
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
    expect_identical(.check_cl(v, n), v)
    # single NULL
    n <- sample(seq(3, 9), 1)
    l <- .check_cl(NULL, n)
    expect_is(l, "list")
    expect_identical(l, replicate(n, c(0, 1), FALSE))
    # one NULL, rest scalar
    n <- sample(seq(3, 9), 1)
    i <- sample(n, 1)
    . <- replicate(n, NULL, FALSE)
    .[[i]] <- v <- c(0.2, 0.8)
    l <- .check_cl(., n)
    expect_is(l, "list")
    expect_identical(l[[i]], v)
    expect_identical(l[-i], replicate(n-1, c(0, 1), FALSE))
    # invalid
    expect_error(.check_cl(c(0.2, 0.4, 0.6), 3)) # non-list
    expect_error(.check_cl(as.list(seq_len(4)), 3)) # wrong length
    expect_error(.check_cl(list(NULL, NULL, c(-1, 1)), 3)) # negative entry
    expect_error(.check_cl(as.list(letters[seq_len(3)]), 3)) # non-numeric
    expect_error(.check_cl(list(NULL, NULL, c(1, 0)), 3)) # decreasing
    expect_error(.check_cl(list(NULL, NULL, -1), 3)) # negative scalar
    expect_error(.check_cl(list(NULL, NULL, 0), 3)) # zero scalar
})

dir.create(td <- tempfile())
(x <- SpatialData.data::MulticancerSteinbock())
a <- data(image(x)[seq_len(3), seq_len(100), seq_len(100)], 1)

test_that(".get_img_dt", {
    da <- (za <- data(image(x)))[1,,]
    dt_za <- .get_img_dt(za) # from 'ZarrArray'
    dt_da <- .get_img_dt(da) # from 'DelayedArray'
    expect_is(dt_za, "character")
    expect_identical(dt_za, dt_da)
})

test_that(".norm_ia", {
    # valid data type
    dt <- .get_img_dt(a)
    b <- .norm_ia(realize(a), dt)
    expect_equal(
        apply(b, 1, range), 
        replicate(3, c(0, 1)))
    # invalid data type
    b <- .norm_ia(realize(a), "")
    expect_equal(
        apply(b, 1, range), 
        replicate(3, c(0, 1)))
})

test_that(".chs2rgb", {
    dt <- .get_img_dt(a)
    ch <- seq_len(d <- dim(a)[1])
    a <- .norm_ia(realize(a), dt)
    # no colors, no contrasts
    b <- .chs2rgb(a, ch)
    expect_identical(a, b)
    # colors
    cmy <- c("cyan", "magenta", "yellow")
    b <- .chs2rgb(a, ch, c=cmy)
    expect_equal(dim(a), dim(b))
    expect_equal(
        apply(b, 1, range), 
        replicate(d, c(0, 1)))
    # lower contrast lim.
    lim <- list(c(0.5, 1), NULL, NULL)
    b <- .chs2rgb(a, ch, cl=lim)
    expect_identical(b[-1,,], a[-1,,])
    expect_true(sum(b[1,,] == 0) > sum(a[1,,] == 0))
    # upper contrast lim.
    lim <- list(c(0, 0.5), NULL, NULL)
    b <- .chs2rgb(a, ch, cl=lim)
    fac <- mean(b[1,,]/a[1,,], na.rm=TRUE)
    expect_equal(fac, 2, tolerance=0.05)
})