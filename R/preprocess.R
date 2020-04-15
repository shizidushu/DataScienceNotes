#' Skwness
#'
#' @param x a numeric vector containing the values whose skewness is to be computed.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @details
#' An un-skewed distribution is one that is roughly symmetric. This means that the probability of falling on either side of the distribution's mean is roughly equal.
#'
#' A right-skewed distribution has a large number of points on the left side of the distribution (smaller values) than on the right side (larger values).
#'
#' If the predictor distribution is roughly symmetric, the skewness values will be close to zero. As the distribution becomes more right skewed, the skewness statistic becomes larger. Similarly, as the distribution becomes more left skewed, the value becomes negative.
#'
#' @references 447 Applied Predictive Modeling (page 31)
#' @return The estimated skewness of \code{x}.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' skewness(x)
skewness <- function(x, na.rm = FALSE) {
    if (na.rm) {
        is_na <- purrr::map_lgl(x, is.na)
        x <- x[!is_na]
    }
    n <- length(x)
    x_mean <- mean(x)
    v <- sum((x - x_mean) ^ 2) / (n - 1)
    y <- sum((x - x_mean) ^ 3) / ((n - 1) * (v ^ (3 / 2)))

    y
}



#' Box-Cox transformation of variable
#'
#' @param x
#' @param lambda
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#'
#' @references \url{http://onlinestatbook.com/2/transformations/box-cox.html}
#' @return
#' @export
#' x <- seq(0.5, 2, by = 0.01)
#' boxcox(x)
#' @examples
#'
boxcox <- function(x, lambda, na.rm = FALSE) {
    if (na.rm) {
        is_na <- purrr::map_lgl(x, is.na)
        x <- x[!is_na]
    }
    if (lambda == 0) {
        log(x)
    } else {
        (x^lambda - 1) / lambda
    }
}
