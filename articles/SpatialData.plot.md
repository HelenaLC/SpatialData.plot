# \`SpatialData.plot\`

``` r

library(ggplot2)
library(patchwork)
library(ggnewscale)
library(spatialdataR)
library(SpatialData.plot)
library(SingleCellExperiment)
```

## Introduction

The `SpatialData.plot` package contains a set of plotting functions for
spatial omics data stored as
[SpatialData](https://spatialdata.scverse.org/en/latest/index.html)
`.zarr` files that follow [OME-NGFF
specs](https://ngff.openmicroscopy.org/latest/#image-layout).

Each `SpatialData` object is composed of five layers: images, labels,
shapes, points, and tables. Each layer may contain an arbitrary number
of elements.

Images and labels are represented as `ZarrArray`s
(*[Rarr](https://bioconductor.org/packages/3.24/Rarr)*). Points and
shapes are represented as
*[arrow](https://CRAN.R-project.org/package=arrow)* objects linked to an
on-disk *.parquet* file. As such, all data are represented out of
memory.

Element annotation as well as cross-layer summarizations (e.g., count
matrices) are represented as
*[SingleCellExperiment](https://bioconductor.org/packages/3.24/SingleCellExperiment)*
as tables.

``` r

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
(x <- readSpatialData(x))
```

    ## class: SpatialData
    ## - images(2):
    ##   - blobs_image (3,64,64)
    ##   - blobs_multiscale_image (3,64,64)
    ## - labels(2):
    ##   - blobs_labels (64,64)
    ##   - blobs_multiscale_labels (64,64)
    ## - points(1):
    ##   - blobs_points (200)
    ## - shapes(3):
    ##   - blobs_circles (5,circle)
    ##   - blobs_multipolygons (2,polygon)
    ##   - blobs_polygons (5,polygon)
    ## - tables(1):
    ##   - table (3,10) [blobs_labels]
    ## coordinate systems(5):
    ## - global(8): blobs_image blobs_multiscale_image ... blobs_polygons
    ##   blobs_points
    ## - scale(1): blobs_labels
    ## - translation(1): blobs_labels
    ## - affine(1): blobs_labels
    ## - sequence(1): blobs_labels

## Visualization

#### Images

`Image/LabelArray`s are linked to potentially multiscale .zarr stores.
Their show method includes the scales available for a given element:

``` r

image(x, "blobs_image")
```

    ## class:  SpatialDataImage  
    ## Scales (1): (3,64,64)

``` r

image(x, "blobs_multiscale_image")
```

    ## class:  SpatialDataImage (MultiScale) 
    ## Scales (3): (3,64,64 3,32,32 3,16,16)

Internally, multiscale `ImageArray`s are stored as a list of
`ZarrArray`, e.g.:

``` r

i <- image(x, "blobs_multiscale_image")
vapply(data(i, k=NULL), dim, numeric(3))
```

    ##      [,1] [,2] [,3]
    ## [1,]    3    3    3
    ## [2,]   64   32   16
    ## [3,]   64   32   16

To retrieve a specific scale’s `ZarrArray`, we can use `data(., k)`,
where `k` specifies the target scale. This also works for plotting:

``` r

wrap_plots(nrow=1, lapply(seq(3), \(.) 
    plotSpatialData() + plotImage(x, i=2, k=.)))
```

![](SpatialData.plot_files/figure-html/ms-plot-1.png)

#### Labels

``` r

i <- "blobs_labels"
t <- getTable(x, i)
t$id <- sample(letters, ncol(t))
table(x) <- t

p <- plotSpatialData()
pal_d <- hcl.colors(10, "Spectral")
pal_c <- hcl.colors(9, "Inferno")[-9]

a <- p + plotLabel(x, i, pal="grey")                   # binary
b <- p + plotLabel(x, i, c="id", pal=pal_d)            # metadata
c <- p + plotLabel(x, i, c="channel_1_sum", pal=pal_c) # assay

(a | b | c) + 
    plot_layout(guides="collect") & 
    theme(legend.position="bottom")
```

![](SpatialData.plot_files/figure-html/plotLabel-1.png)

#### Points

``` r

i <- "blobs_points"
a <- p + plotPoint(x, i)
b <- p + plotPoint(x, i, col="genes")       # discrete
c <- p + plotPoint(x, i, col="instance_id") # continuous
(a | b | c) 
```

![](SpatialData.plot_files/figure-html/plotPoint-1.png)

#### Shapes

``` r

p <- plotSpatialData()
a <- p +
  ggtitle("polygons") +
  plotShape(x, "blobs_polygons")
b <- p +
  ggtitle("multipolygons") +
  plotShape(x, "blobs_multipolygons")
c <- p +
  ggtitle("circles") +
  plotShape(x, "blobs_circles")
(a | b | c)
```

![](SpatialData.plot_files/figure-html/plotShape-1.png)

#### Layering

``` r

p <- plotSpatialData()
# joint
all <- p +
    plotImage(x) +
    plotLabel(x, a=1/3) +
    plotShape(x, 1) +
    plotShape(x, 3) +
    new_scale_color() +
    plotPoint(x, col="genes") +
    ggtitle("layered")
# split
one <- list(
    p + plotImage(x) + ggtitle("image"),
    p + plotLabel(x) + ggtitle("labels"),
    p + plotShape(x, 1) + ggtitle("circles"),
    p + plotShape(x, 3) + ggtitle("polygons"),
    p + plotPoint(x, col="genes") + ggtitle("points"))
wrap_plots(c(list(all), one), nrow=2)
```

![](SpatialData.plot_files/figure-html/blobs-plot-1.png)

## Session info

    ## R Under development (unstable) (2026-10-05 r90641)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats4    stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] SingleCellExperiment_1.35.2 SummarizedExperiment_1.43.0
    ##  [3] Biobase_2.73.2              GenomicRanges_1.65.4       
    ##  [5] Seqinfo_1.3.2               IRanges_2.47.5             
    ##  [7] S4Vectors_0.51.10           BiocGenerics_0.59.12       
    ##  [9] generics_0.1.4              MatrixGenerics_1.25.0      
    ## [11] matrixStats_1.5.0           SpatialData.plot_0.99.7    
    ## [13] spatialdataR_0.99.44        ggnewscale_0.5.2           
    ## [15] patchwork_1.3.2             ggplot2_4.0.3              
    ## [17] BiocStyle_2.41.0           
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1    grumpy_0.1.1        blob_1.3.0         
    ##  [4] dplyr_1.2.1         farver_2.1.2        R.utils_2.13.0     
    ##  [7] S7_0.2.2            fastmap_1.2.0       duckdb_1.5.6       
    ## [10] tweenr_2.0.3        digest_0.6.39       lifecycle_1.0.5    
    ## [13] sf_1.1-3            paws.storage_0.10.0 magrittr_2.0.5     
    ## [16] compiler_4.7.0      rlang_1.3.0         sass_0.4.10        
    ## [19] tools_4.7.0         yaml_2.3.12         knitr_1.52         
    ## [22] labeling_0.4.3      S4Arrays_1.13.2     classInt_0.4-11    
    ## [25] curl_8.0.0          reticulate_1.47.0   DelayedArray_0.39.8
    ## [28] RColorBrewer_1.1-3  abind_1.4-8         KernSmooth_2.23-27 
    ## [31] withr_3.0.3         purrr_1.2.2         desc_1.4.3         
    ## [34] R.oo_1.27.1         polyclip_1.10-7     grid_4.7.0         
    ## [37] e1071_1.7-17        MASS_7.3-66         scales_1.4.0       
    ## [40] cli_3.6.6           rmarkdown_2.32      crayon_1.5.3       
    ## [43] ragg_1.5.2          otel_0.2.0          ggforce_0.5.0      
    ## [46] DBI_1.3.0           cachem_1.1.0        proxy_0.4-29       
    ## [49] BiocManager_1.30.27 XVector_0.53.0      vctrs_0.7.3        
    ## [52] Matrix_1.7-6        jsonlite_2.0.0      bookdown_0.48      
    ## [55] RBGL_1.89.0         systemfonts_1.3.2   jquerylib_0.1.4    
    ## [58] units_1.0-1         glue_1.8.1          pkgdown_2.2.1      
    ## [61] ZarrArray_1.1.7     gtable_0.3.6        Rarr_2.1.43        
    ## [64] tibble_3.3.1        pillar_1.11.1       htmltools_0.5.9    
    ## [67] graph_1.91.0        dbplyr_2.6.0        R6_2.6.1           
    ## [70] httr2_1.3.0         wk_0.9.5            textshaping_1.0.5  
    ## [73] evaluate_1.0.5      lattice_0.23-1      R.methodsS3_1.8.2  
    ## [76] png_0.1-9           duckspatial_1.2.1   paws.common_0.9.0  
    ## [79] bslib_0.12.0        class_7.3-24        uuid_1.2-2         
    ## [82] Rcpp_1.1.2          SparseArray_1.13.4  anndataR_1.3.2     
    ## [85] xfun_0.61           fs_2.1.0            pkgconfig_2.0.3
