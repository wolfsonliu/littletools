leverage <- function(data) {
    ## calculate the laverage statistic
    then <- length(data)
    thess <- var(data) * (then - 1)
    themean <- mean(data)
    h <- unlist(
        lapply(
            data,
            function(x) { 1 / then + (x - themean)^2 / thess}
        )
    )
    return(h)
}
