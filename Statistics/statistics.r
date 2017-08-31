## Vars

rowVars <- function(x, na.rm=FALSE) {
    ## calculate row variance
    if (na.rm) {
        nx <- rowSums(!is.na(x))
    } else {
        nx <- dim(x)[2]
    }
    rmean <- rowMeans(x, na.rm)
    square <- (x-rmean)^2
    rvar <- rowSums(
        square,
        na.rm
    ) / pmax(nx - 1, 1)
    return(rvar)
}

colVars <- function(x, na.rm=FALSE) {
    ## calculate row variance
    if (na.rm) {
        nx <- colSums(!is.na(x))
    } else {
        nx <- dim(x)[1]
    }
    cmean <- colMeans(x, na.rm)
    square <- (x-cmean)^2
    cvar <- colSums(
        square,
        na.rm
    ) / pmax(nx - 1, 1)
    return(cvar)
}


## leverage

leverage <- function(x, ...) {
    UseMethod('leverge', x)
}

leverage.default <- function(x, na.rm=FALSE, with.attr, ...) {
    ## calculate the laverage statistic
    if (na.rm) x <- x[!is.na(x)]

    ## length of data
    n <- length(x)
    if (n==0) {
        stop('length should not be 0.'}
    } else {}

    if (ss == 0) {
        l <- rep(1/n, n)
        m <- 1/n
        v <- 0
    } else {
        m <- mean(x, na.rm=na.rm, ...)
        v <- var(x, na.rm=na.rm, ...)
        ## sum of square sum((x - mean(x)) ^ 2)
        ss <- v * (n - 1)
        l <- unlist(
            lapply(
                x,
                function(xi) { 1 / n + (xi - m)^2 / ss}
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


leverage.matrix <- function(x, ...,
                            na.rm=FALSE,
                            with.attr=FALSE,
                            byrow=TRUE) {
    ## matrix leverage
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
    l
}



leverage.data.frame <- leverage.matrix

## normalize

normalize <- function(x, method='rpm', sizefactor=10^6, ...) {
    ## normalize data
    if (method == 'rpm') {
        return(x / sum(x) * sizefactor)
    } else if (method == 'other') {
        stop('under develop')
    }
}


predict.regsubsets <- function(object, newdata, id, ...) {
    ## used for regsubsets prediction
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}


## flatten

flatten <- function(x, byrow=TRUE, ..., na.rm=TRUE) {
    UseMethod('flatten', x)
}

flatten.matrix <- function(x, byrow=TRUE, ..., na.rm=TRUE) {
    if (!byrow) {
        x <- t(x, ...)
    } else {}

    dim(x) <- length(x)

    if (na.rm) {
        x <- x[!is.na(x)]
    } else {}

    x
}




## stack

unstack.data.frame <- function(x, form=formula(x), ...) {
    variables <- match.call()[-1]
    col.mark <- as.character(form[3])
    row.mark <- as.character(form[2])
    ## convert factor to character vector
    for (i in c(col.mark, row.mark)) {
        if (class(x[[i]]) == 'factor') {
            x[[i]] <- as.character(x[[i]])
        } else {}
    }
    col.name <- as.character(unique(x[[col.mark]]))
    row.n <- max(table(x[[col.mark]]))
    result <- as.data.frame(
        lapply(
            col.name,
            function(it) {
                value <- x[x[[col.mark]] == it, row.mark]
                value <- c(value, rep(NA, row.n - length(value)))
                return(value)
            }
        ),
        col.names=col.name
    )
    return(result)
}
