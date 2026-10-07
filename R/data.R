#' @name blobs
#' @rdname blobs
#' @title `SpatialData` .zarr toy datasets 
#' 
#' @description data were retrieved on Nov. 11th, 2024, from \href{https://github.com/scverse/spatialdata-notebooks/tree/main/notebooks/developers_resources/storage_format/multiple_elements.zarr}{here}.
#'
#' @returns
#' a \code{SpatialData} .zarr store of toy example data with all types of
#' elements represented: (multiscale) RGB \code{image}, (multiscale) \code{label}, 
#' \code{point}, circle and (multi)polygon \code{shape}, \code{table} annotation.
#' In addition, all types of coordinate transformations are represented: 
#' identity, scale, translation, affine, and sequence.
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' (x <- readSpatialData(x))
NULL