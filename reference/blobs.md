# \`SpatialData\` .zarr toy datasets

data were retrieved on Nov. 11th, 2024, from
[here](https://github.com/scverse/spatialdata-notebooks/tree/main/notebooks/developers_resources/storage_format/multiple_elements.zarr).

## Examples

``` r
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
(x <- readSpatialData(x))
#> duckdb keeps downloaded extensions and secrets in a temporary directory:
#> ℹ /tmp/RtmpDGjKPP/duckdb
#> This is removed when the R session ends.
#> • Extensions are re-downloaded each session.
#> • Secrets are lost.
#> ℹ Run duckdb(shared_home = TRUE) (or create ~/.duckdb) to keep them (suitable for most users).
#> ℹ Run duckdb(shared_home = FALSE) to accept the temporary directory (and silence this message).
#> ℹ See ?duckdb_storage for details and alternatives.
#> class: SpatialData
#> - images(2):
#>   - blobs_image (3,64,64)
#>   - blobs_multiscale_image (3,64,64)
#> - labels(2):
#>   - blobs_labels (64,64)
#>   - blobs_multiscale_labels (64,64)
#> - points(1):
#>   - blobs_points (200)
#> - shapes(3):
#>   - blobs_circles (5,circle)
#>   - blobs_multipolygons (2,polygon)
#>   - blobs_polygons (5,polygon)
#> - tables(1):
#>   - table (3,10) [blobs_labels]
#> coordinate systems(5):
#> - global(8): blobs_image blobs_multiscale_image ... blobs_polygons
#>   blobs_points
#> - scale(1): blobs_labels
#> - translation(1): blobs_labels
#> - affine(1): blobs_labels
#> - sequence(1): blobs_labels
```
