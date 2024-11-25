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
