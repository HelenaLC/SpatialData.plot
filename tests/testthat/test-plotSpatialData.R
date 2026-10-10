test_that("regression test of overlays", {
    zs <- system.file("extdata", "blobs.zarr", package="spatialdataR")
    x <- readSpatialData(zs)
    
    p <- plotSpatialData(x)
    # joint
    all <- p +
        plotImage() +
        plotLabel(a=1/3) +
        plotShape(1) +
        plotShape(3) +
        plotPoint(col="genes") +
        ggplot2::ggtitle("layered")
    # split
    one <- list(
        p + plotImage() + ggplot2::ggtitle("image"),
        p + plotLabel() + ggplot2::ggtitle("labels"),
        p + plotShape(1) + ggplot2::ggtitle("circles"),
        p + plotShape(3) + ggplot2::ggtitle("polygons"),
        p + plotPoint(col="genes") + ggplot2::ggtitle("points"))
    fig <- patchwork::wrap_plots(c(list(all), one), nrow=2)
    
    vdiffr::expect_doppelganger("overlays", fig)
})
