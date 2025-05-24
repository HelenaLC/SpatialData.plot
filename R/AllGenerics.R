# trs ----

setGeneric("scale", \(x, t, ...) standardGeneric("scale"))
setGeneric("rotate", \(x, t, ...) standardGeneric("rotate"))
setGeneric("transform", \(x, ...) standardGeneric("transform"))
setGeneric("translation", \(x, t, ...) standardGeneric("translation"))

# plt ----

setGeneric("plotImage", \(x, ...) standardGeneric("plotImage"))
setGeneric("plotLabel", \(x, ...) standardGeneric("plotLabel"))
setGeneric("plotPoint", \(x, ...) standardGeneric("plotPoint"))
setGeneric("plotShape", \(x, ...) standardGeneric("plotShape"))
