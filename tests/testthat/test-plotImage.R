require(SpatialData, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("get/check channel names", {
  
  # get channel names
  expect_equal(channelNames(image(x,1)), c(0,1,2))

  # get indices of channels
  expect_equal(.ch_ind(image(x,1), ch = c(2,0,1)), c(3,1,2))
  expect_warning(expect_equal(.ch_ind(image(x,1), ch = 45), 1)) # return first if no matching channel
  
  # .is.rgb
  expect_true(.is.rgb(image(x,1)))
})

# TODO: any tests for image array normalization ? 
test_that(".normalize_image_array", {
  skip()
})
