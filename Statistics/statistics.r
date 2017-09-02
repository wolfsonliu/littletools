

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
