`sqrtm` <-
function (x) 
{
    xe <- eigen(x)
    xe1 <- xe$values
    if (all(xe1 >= 0)) {
        xev1 <- diag(sqrt(xe1), nrow = length(xe1))
    }
    else {
        i = 1
        while (i < (length(xe1) + 1)) {
            if (xe1[i] < 0) {
                xe1[i] = 0
            }
            i = i + 1
        }
        xev1 <- diag(sqrt(xe1), nrow = length(xe1))
    }
    xval1 <- cbind(xe$vectors)
    xval1i <- solve(xval1)
    y <- xval1 %*% xev1 %*% xval1i
}

