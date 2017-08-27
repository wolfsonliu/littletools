rowVars <- function(x) {
    ## calculate row variables
    nx <- dim(x)[2]
    rvar <- rowSums((x - rowMeans(x)) ^ 2) / (nx - 1)
    return(rvar)
}


leverage <- function(data) {
    ## calculate the laverage statistic
    then <- length(data)
    thess <- var(data) * (then - 1)
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

predict.regsubsets <- function(object, newdata, id, ...) {
    ## used for regsubsets prediction
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}
