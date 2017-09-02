# -*- coding: utf-8 -*-

#' @title Calculate leverage statistic
#'
#' In linear model or other statistic tasks,
#' leverage statistic is useful in illustrate the donation of
#' each value to mean.
#'
#' @param x a vector, matrix or data.frame containing numeric,
#'     integer, or logical values.
#' @param na.rm logical. Should missing values (including 'NaN')
#'     be omitted from the calculations?
#' @param with.attr logical. Should the corresponding values be returned
#'     as attributes. Including mean, variance and number of items.
#'
#' @return A vector or matrix the same dimension like the input data
#'     with leverage calculated.
#'
#' @details The leverage is defined as
#'     \code{1/n + (x - mean)^2 / sum((x - mean)^2)}
#' If there is only one items, we set the leverage as 1.0,
#' and if there are two items, the leverage of each item is always 0.5.
#'
#' @examples
#' ## Calculate leverage of a vector:
#' x <- seq(1:10)
#'
#' leverage(x, with.attr=TRUE)
#'
#' ## Calculate leverage of a matrix:
#' y <- cbind(seq(1,10), seq(2,20,2), 3)
#'
#' leverage(y, byrow=TRUE)
#'
#' leverage(y, byrow=FALSE)
#'
#' @export
leverage <- function(x, ..., na.rm=FALSE, with.attr=FALSE) {
    UseMethod('leverage', x)
}

#' describeIn leverage
#' @export
leverage.default <- function(x, ..., na.rm=FALSE, with.attr=FALSE) {
    ## calculate the laverage statistic
    if (na.rm) x <- x[!is.na(x)]

    ## length of data
    n <- length(x)
    if (n == 0) {
        stop('length should not be 0')
    }

    if (n == 1) {
        l <- rep(1/n, n)
        m <- 1/n
        v <- 0
        return(rep(1/n, n))
    } else {
        m <- mean(x, na.rm=na.rm, ...)
        v <- var(x, na.rm=na.rm, ...)
        ## sum of square sum((x - mean(x)) ^ 2)
        ss <- v * (n - 1)
        l <- unlist(
            lapply(
                x,
                function(xi) {1 / n + (xi - m)^2 / ss}
            )
        )
    }

    if (with.attr) {
        attr(l, 'mean') <- m
        attr(l, 'var') <- v
        attr(l, 'n') <- n
    } else {}

    return(l)
}


#' @describeIn leverage
#' @param byrow logical. Should the leverage calculate
#'     in each row or in each columns
#' @export
leverage.matrix <- function(x, ...,
                            na.rm=FALSE,
                            with.attr=FALSE,
                            byrow=TRUE) {
    m <- NULL
    v <- NULL
    n <- NULL
    if (!byrow) {
        x <- t(x)
    } else {}

    m <- rowMeans(x, na.rm=na.rm)
    v <- rowVars(x, na.rm=na.rm)
    n <- rowSums(!is.na(x))
    ss <- v * (n - 1)

    lss <- (x - m)^2 / ss
    lss[is.nan(lss)] <- 0
    l <- (1 / n) + lss

    if (!byrow) {
        l <- t(l)
    } else {}

    if (with.attr) {
        attr(l, 'mean') <- m
        attr(l, 'var') <- v
        attr(l, 'n') <- n
    } else {}

    l
}

#' @describeIn leverage
#' @export
leverage.data.frame <- leverage.matrix

# ------------------
