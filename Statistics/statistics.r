rowVars <- function(x, na.rm=FALSE) {
    ## calculate row variance
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
    ) / max(nx - 1, 1)
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
    sumsquare <- (x-cmean)^2
    cvar <- colSums(
        sumsquare,
        na.rm
    ) / max(nx - 1, 1)
    return(cvar)
}

leverage <- function(data) {
    ## calculate the laverage statistic
    then <- length(data)
    thess <- var(data) * (then - 1)
    if (thess == 0) {
        return(rep(1/n, n))
    } else {}
    themean <- mean(data)
    l <- unlist(
        lapply(
            data,
            function(x) { 1 / then + (x - themean)^2 / thess}
        )
    )
    result <- list(
        leverage=l,
        mean=mean(l),
        var=var(l)
    )
    return(result)
}


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
