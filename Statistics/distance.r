## cosine.similarity
cosine.similarity <- function(a, ...) {
    UseMethod('cosine.similarity')
}

cosine.similarity.numeric <- function(a, b, ...) {
    return(sum(a*b)/sqrt(sum(a^2)*sum(b^2)))
}

cosine.similarity.matrix <- function(a, byrow=TRUE, ...) {
    if (!byrow) {
        a <- t(a)
    }
    dimension <- dim(a)[1]
    thenames <- rownames(a)
    if (is.null(thenames)) {
        thenames <- seq(dimension)
    }
    result <- as.data.frame(
        lapply(
            seq(dimension),
            function(x) {
                simmx <- unlist(
                    lapply(
                        seq(dimension),
                        function(y) {cosine.similarity(a[x,], a[y,])}
                    )
                )
                simmx
            }
        )
    )
    colnames(result) <- thenames
    rownames(result) <- thenames
    return(as.matrix(result))
}

## similarity
similarity <- function(a, ...) {
    UseMethod('similarity')
}

similarity.numeric <- function(a, b, method='cosine') {
    if (method == 'cosine') {
        return(cosine.similarity(a, b))
    }
}
