# ARM macOS has rounding issues. See https://github.com/r-lib/vdiffr/issues/119
# All other platforms share the default snapshots.
snapshot_variant <- function() {
    if (Sys.info()[["sysname"]] == "Darwin" && R.version$arch == "aarch64") {
        "apple-silicon"
    } else {
        NULL
    }
}
