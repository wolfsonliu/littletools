# -*- coding: utf-8 -*-

#' @title Normalize data
#'
#' The total sequence number can affect the sgRNA counts.
#' The data should be normalized before investigation.
#'
#' @param x a vector or matrix, containing numeric, integer.
#' @param method method used in normalization.
#'     rpm, z, mean, median could be used for vector or matrix.
#'     UQS, quantile could be used for matrix.
#'
#' @examples
#' x <- seq(1:10)
#'
#' y <- cbind(seq(1,10), seq(2,20,2), 3)
#'
#' normalize(x, method='rpm')
#'
#' normalize(y, 'rpm', na.rm=TRUE, col.as.rep=FALSE)
#'
#' @export
normalize <- function(x,
                      method,
                      ...) {
    UseMethod('normalize', x)
}

#' @param sizefactor number, used in rpm normalization
#'
#' @details
#' rpm: reads per million, sizefactor usually set to 1 million
#' z: z score
#' mean: all counts divided by mean
#' median: all counts divided by median
#'
#' @describeIn normalize
#' @export
normalize.vector <- function(x,
                             method=c(
                                 'rpm', 'z', 'mean', 'median'
                             ),
                             ...,
                             na.rm=FALSE,
                             sizefactor=10^6) {
    method <- match.arg(method)
    if (length(x) == 1) {
        stop('The lenght of x should be larger than 1.')
    } else {}

    if (na.rm) {
        x <- x[!is.na(x)]
    } else {}

    if (method == 'rpm') {
        normx <- x / sum(x, na.rm=na.rm) * sizefactor
    } else if (method == 'z') {
        normx <- (x - mean(x, na.rm=na.rm)) / sqrt(var(x, na.rm=na.rm))
    } else if (method == 'mean') {
        normx <- x / mean(x, na.rm=na.rm)
    } else if (method == 'median') {
        normx <- x / median(x, na.rm=na.rm)
    }

    normx
}

#' @details
#' UQS: upper quantile scaling
#' quantile: quantile normalization
#'
#' @describeIn normalize
#' @export
normalize.matrix <- function(x,
                             method=c(
                                 'rpm', 'z', 'mean', 'median',
                                 'UQS', 'quantile'
                             ),
                             ...,
                             col.as.rep=TRUE,
                             na.fill=NA,
                             na.rm=TRUE,
                             sizefactor=10^6) {
    method <- match.arg(method)

    if (col.as.rep) {
        x <- t(x)

    } else {}
    ## x dim to replicant, item

    x[is.na(x)] <- na.fill

    if (method == 'rpm') {
        sums <- rowSums(x, na.rm=na.rm) # replicants num length
        normx <- x / sums * sizefactor  # broadcast sums
    } else if (method == 'z') {
        means <- rowMeans(x, na.rm=na.rm)
        vars <- rowVars(x, na.rm=na.rm)

        if (any(vars)) {
            stop('variance is zero')
        } else {}

        normx <- (x - means) / sqrt(vars)
    } else if (method == 'mean') {
        means <- rowMeans(x, na.rm=na.rm)

        normx <- x / means              # broadcast means
    } else if (method == 'median') {
        medians <- rowMedians(x, na.rm=na.rm)

        normx <- x / medians            # broadcast medians
    } else if (method == 'UQS') {
        ## upper quantile scale
        uq <- apply(y, MARGIN=1, fivenum)[4, ] # replicants num length
        uqgm <- geomean(uq)                    # geometric mean of uq
        normx <- x / uq * uqgm          # broadcast upper quantile
    } else if (method == 'quantile') {
        repmean <- sort(colMeans(x, na.rm=TRUE), decreasing=FALSE)
        repmeanr <- rankMatrix(
            x, byrow=TRUE, ties.method='first'
        )
        normx <- matrix(
            repmean[repmeanr],
            nrow=dim(repmeanr)[1],
            ncol=dim(repmeanr)[2]
        )
    }

    if (col.as.rep) {
        normx <- t(normx)
    } else {}

    return(normx)
}

## -----------------
