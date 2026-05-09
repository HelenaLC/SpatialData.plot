test_that("regression test of overlays", {
  x <- system.file("extdata", "blobs.zarr", package="SpatialData") |> 
    readSpatialData()

  p <- plotSpatialData()
  # joint
  all <- p +
      plotImage(x) +
      plotLabel(x, a=1/3) +
      plotShape(x, 1) +
      plotShape(x, 3) +
      plotPoint(x, col="genes") +
      ggplot2::ggtitle("layered")
  # split
  one <- list(
      p + plotImage(x) + ggplot2::ggtitle("image"),
      p + plotLabel(x) + ggplot2::ggtitle("labels"),
      p + plotShape(x, 1) + ggplot2::ggtitle("circles"),
      p + plotShape(x, 3) + ggplot2::ggtitle("polygons"),
      p + plotPoint(x, col="genes") + ggplot2::ggtitle("points"))
  fig <- patchwork::wrap_plots(c(list(all), one), nrow=2)

  vdiffr::expect_doppelganger("overlays", fig)
})
