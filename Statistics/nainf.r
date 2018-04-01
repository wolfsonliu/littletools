is.pinfinite <- function(x) {
    x == Inf
}
is.ninfinite <- function(x) {
    x == -Inf
}

inf.omit <- function(object, ...) {
    UseMethod('inf.omit', object)
}

inf.omit.default <- function(object,
                             direction=c('all', '-', '+'),
                             ...) {
    direction <- match.arg(direction)
    is.infinitefunc <- list()
    is.infinitefunc[['all']] <- is.infinite
    is.infinitefunc[['+']] <- is.pinfinite
    is.infinitefunc[['-']] <- is.ninfinite

    return(object[!is.infinitefunc[[direction]](object)])
}

inf.omit.data.frame <- function(object,
                                direction=c('all', '-', '+'),
                                ...) {
    direction <- match.arg(direction)
    is.infinitefunc <- list()
    is.infinitefunc[['all']] <- is.infinite
    is.infinitefunc[['+']] <- is.pinfinite
    is.infinitefunc[['-']] <- is.ninfinite
    col.names <- colnames(object)
    slcts <- list()
    for (x in col.names) {
        slcts[[x]] <- !is.infinitefunc[[direction]](object[[x]])
    }
    slct <- Reduce( function(x, y) {x & y}, slcts)
    return(object[slct,])
}
