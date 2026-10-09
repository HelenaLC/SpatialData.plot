x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

.mock <- \(c=3, t=0, z=0, y=80, x=120) {
    dim <- c(c, t, z, y, x); dim <- dim[dim != 0]
    arr <- drop(as(array(runif(prod(dim)), dim), "ZarrArray"))
    sda <- SpatialDataAttrs(dim=length(dim)-1, nch=c)
    SpatialDataImage(list(arr), sda)
}

test_that(".str_is_col works", {
    expect_true(.str_is_col("grey"))
    expect_false(.str_is_col("jfdkls"))
})

test_that(".project works", {
    m <- .mock(c=2, t=0, z=3, y=5, x=3)
    expect_error(.project(m, m@data[[1]], z=1:2), 
                 "only a single z-plane can be selected")
    # project
    p <- .project(m, m@data[[1]], z=NULL)
    expect_identical(p[1, , ], Reduce(pmax, lapply(1:3, function(i) as.array(m@data[[1]][1, i, , ]))))
    expect_identical(p[2, , ], Reduce(pmax, lapply(1:3, function(i) as.array(m@data[[1]][2, i, , ]))))
    
    # select
    p <- .project(m, m@data[[1]], z=1)
    expect_identical(p, m@data[[1]][, 1, , ])
})

test_that(".get_wh works", {
    m <- .mock(c=2, t=0, z=3, y=5, x=3)
    S4Vectors::metadata(m)$wh <- list(w=c(0,6), h=c(0,10))
    rwh <- .raw_wh(m)
    expect_identical(rwh, list(w=c(0,6), h=c(0,10)))
    
    wh <- .get_wh(m)
    expect_identical(wh, rwh)
})

test_that(".subset_array_by_axes works", {
    m <- .mock(c=2, t=0, z=3, y=5, x=3)
    expect_error(.subset_array_by_axes(m, axisNames = c("x", "y")), 
                 "must equal")
    expect_identical(dim(.subset_array_by_axes(m@data[[1]], axisNames=c("c", "z", "y", "x"), y=1:2, x=1)), c(2L, 3L, 2L, 1L))
    expect_identical(dim(.subset_array_by_axes(m@data[[1]], axisNames=c("c", "z", "y", "x"), y=1:2, x=1, drop=TRUE)), c(2L, 3L, 2L))
    
})