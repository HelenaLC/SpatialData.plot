require(SpatialData, quietly=TRUE)
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

# TODO: any tests for image array normalization ? 
test_that(".normalize_image_array", {
    skip()
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
