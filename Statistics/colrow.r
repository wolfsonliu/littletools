# -*- coding: utf-8 -*-

#' @title Form Row and Column Vars, Medians
#'
#' Form row and column variances,
#' medians for numeric arrays (or data frames).
#'
#' @param x an array of two dimensions, containing numeric,
#'     integer or logical values, or a numric data frame.
#' @param na.rm logical. Should missing values (including 'NaN')
#'     be omitted from the calculations?
#'
#' @return A numeric vector with the variances.
#'
#' @seealso \code{\link{rowSums}} \code{\link{rowMeans}}
#'     \code{\link{colSums}} \code{\link{colMeans}}
#'
#' @author Wolfson Liu \email{wolfsonliu@@gmail.com}
#'
#' @examples
#' ## Compute row and column vars for a matrix:
#' x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
#' rowVars(x)
#' colVars(x)
#'
#' ## Compute row and column vars for a data.frame:
#' class(iris)
#'
#' colVars(
#'     iris[,
#'         c('Sepal.Length', 'Sepal.Width',
#'           'Petal.Length', 'Petal.Width')]
#' )
#' y <- cbind(seq(1,10), seq(2,20,2), 3)
#' colMedians(y)
#' @export
rowVars <- function(x, na.rm=FALSE) {
    if (na.rm) {
        nx <- rowSums(!is.na(x))
    } else {
        nx <- dim(x)[2]
    }
    rmean <- rowMeans(x, na.rm)
    sumsquare <- (x-rmean)^2
    rvar <- rowSums(
        sumsquare,
        na.rm
    ) / pmax(nx - 1, 1)
    return(rvar)
}

#' @describeIn rowVars
#' @export
colVars <- function(x, na.rm=FALSE) {
    ## calculate row variance
    if (na.rm) {
        nx <- colSums(!is.na(x))
    } else {
        nx <- dim(x)[1]
    }
    cmean <- colMeans(x, na.rm)
    sumsquare <- (x-cmean)^2
    cvar <- colSums(
        sumsquare,
        na.rm
    ) / pmax(nx - 1, 1)
    return(cvar)
}

#' @describeIn rowVars
#' @export
rowMedians <- function(x, na.rm=FALSE) {
    medians <- rep(NA, (dim(x)[1]))

    if (is.null(rownames(x))) {
        rows <- seq(dim(x)[1])
    } else {
        rows <- rownames(x)
    }

    for (i in rows) {
        medians[i] <- median(x[i,], na.rm=na.rm)
    }

    return(medians)
}

#' @describeIn rowVars
#' @export
colMedians <- function(x, na.rm=FALSE) {
    medians <- rep(NA, (dim(x)[2]))

    if (is.null(colnames(x))) {
        cols <- seq(dim(x)[2])
    } else {
        cols <- colnames(x)
    }

    for (i in cols) {
        medians[i] <- median(x[,i], na.rm=na.rm)
    }

    return(medians)
}

# ------------------
