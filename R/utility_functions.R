# Utility functions

#' Normalize impacts to all positive by replacing negative influences
#' with their negatives.
#'
#' @param decision A numeric matrix with m rows for m alternatives and n columns
#'   for n criterions.
#' @param impacts A character vector of '+' and '-' signs for the way that each
#'   criterion influences on the alternatives.
#' @return A copy of \code{decision} matrix with normalized impacts.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' i <- c('+', '-', '+')
#' normalize_impacts(d, i)
#' @export
normalize_impacts <- function(decision, impacts) {
    if (length(impacts) != ncol(decision))
        warning("length of 'impacts' is not equal to number of columns, treating every next column as positively influencing")
    decision[, impacts == "-"] <- -decision[, impacts == "-"]
    return(decision)
}

#' Calculate Euclidean distance between two vectors.
#'
#' @param x,y Numeric vectors.
#' @return The distance between the vectors.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @examples
#' x <- c(2, 5)
#' y <- c(5, 7)
#' calculate_distance(x, y)
calculate_distance <- function(x, y) {
    sqrt(sum((x - y)^2))
}

#' Normalization for TOPSIS. Intended to be used with \code{apply} function
#' on a target matrix.
#'
#' @param decision A numeric matrix with m rows for m alternatives and n columns
#'   for n criterions.
#' @param weights A numeric vector with length equal to number of columns in
#'   decision matrix for weights of criterions.
#' @return A copy of \code{decision} matrix with TOPSIS-normalized values.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @references Hwang, C.L.; Yoon, K. (1981). Multiple Attribute Decision
#' Making: Methods and Applications. New York: Springer-Verlag.
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' d <- sapply(d, topsis_normalize, w)
topsis_normalize <- function(decision, weights) {
    weights * decision / sqrt(sum(decision^2))
}
