#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end

## mockable bindings: start
## mockable bindings: end
NULL

# This is already provided by base R in the version we require but the backport
# fixes r-universe checks in oldrel (R 4.5.0 at the time of writing)
`%notin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
