# Linear ordering methods

#' Sum of ranks method.
#'
#' @param decision A numeric matrix with m rows for m alternatives and n columns
#'   for n criterions.
#' @param weights A numeric vector with length equal to number of columns in
#'   decision matrix for weights of criterions.
#' @param impacts A character vector of '+' and '-' signs for the way that each
#'   criterion influences on the alternatives.
#' @return A data frame including elements:
#' \describe{
#'   \item{\code{alt.row}}{Row number of alternatives in decision matrix.}
#'   \item{\code{score}}{Score of alternatives.}
#'   \item{\code{rank}}{Rank of alternatives based on score.}
#' }
#' Should \code{decision} be a data frame, the row names will be carried over
#' to the return value.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' i <- c('+', '-', '+')
#' sum_of_ranks(d, w, i)
#' @export
sum_of_ranks <- function(decision, weights, impacts) {
    if (length(weights) != ncol(decision))
        stop("length of 'weights' is not equal to number of columns")
    matrix <- normalize_impacts(decision, impacts)
    matrix <- apply(matrix, 2, rank)
    score <- as.vector(apply(matrix, 1, weighted.mean, weights))
    df <- data.frame(alt.row = 1:length(vector), score = score, rank = rank(-score), row.names = rownames(decision))
    return(df)
}

#' Standarized sums method.
#'
#' @inheritParams sum_of_ranks
#' @return A data frame including elements:
#' \describe{
#'   \item{\code{alt.row}}{Row number of alternatives in decision matrix.}
#'   \item{\code{score}}{Score of alternatives.}
#'   \item{\code{rank}}{Rank of alternatives based on score.}
#' }
#' Should \code{decision} be a data frame, the row names will be carried over
#' to the return value.
#' Should \code{decision} be a data frame, the row names will be carried over
#' to the return value.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' i <- c('+', '-', '+')
#' standarized_sums(d, w, i)
#' @export
standarized_sums <- function(decision, weights, impacts) {
    if (length(weights) != ncol(decision))
        stop("length of 'weights' is not equal to number of columns")
    matrix <- normalize_impacts(decision, impacts)
    matrix <- scale(matrix)
    matrix <- matrix %*% diag(weights)
    matrix <- apply(matrix, 1, mean)
    score <- as.vector(matrix - min(matrix)) / max(matrix - min(matrix))

    df <- data.frame(alt.row = 1:length(vector), score = score, rank = rank(-score), row.names = rownames(decision))
    return(df)
}

#' Hellwig's method.
#'
#' @inheritParams sum_of_ranks
#' @return A data frame including elements:
#' \describe{
#'   \item{\code{alt.row}}{Row number of alternatives in decision matrix.}
#'   \item{\code{score}}{Score of alternatives.}
#'   \item{\code{rank}}{Rank of alternatives based on score.}
#' }
#' Should \code{decision} be a data frame, the row names will be carried over
#' to the return value.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @references Hellwig, Z. (1968). On the optimal choice of predictors.
#' In: Gostkowski, Z. (ed.) Toward a System of Quantitative Indicators of
#' Components of Human Resources Development, Study VI. Paris: UNESCO.
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' i <- c('+', '-', '+')
#' hellwig(d, w, i)
#' @export
hellwig <- function(decision, weights, impacts) {
    if (length(weights) != ncol(decision))
        stop("length of 'weights' is not equal to number of columns")
    matrix <- normalize_impacts(decision, impacts)
    matrix <- scale(matrix)
    matrix <- matrix %*% diag(weights)
    matrix_max <- apply(matrix, 2, max)
    matrix_distance <- apply(matrix, 1, calculate_distance, matrix_max)
    reasonable_distance <- mean(matrix_distance) + 2 * sd(matrix_distance)
    score <- as.vector(1 - matrix_distance / reasonable_distance)

    df <- data.frame(alt.row = 1:length(vector), score = score, rank = rank(-score), row.names = rownames(decision))
    return(df)
}

#' TOPSIS - the Technique for Order of Preference by Similarity to Ideal
#' Solution.
#'
#' @inheritParams sum_of_ranks
#' @return A data frame including elements:
#' \describe{
#'   \item{\code{alt.row}}{Row number of alternatives in decision matrix.}
#'   \item{\code{score}}{Score of alternatives.}
#'   \item{\code{rank}}{Rank of alternatives based on score.}
#' }
#' Should \code{decision} be a data frame, the row names will be carried over
#' to the return value.
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @references Hwang, C.L.; Yoon, K. (1981). Multiple Attribute Decision
#' Making: Methods and Applications. New York: Springer-Verlag.
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' i <- c('+', '-', '+')
#' topsis(d, w, i)
#' @export
topsis <- function(decision, weights, impacts) {
    if (length(weights) != ncol(decision))
        stop("length of 'weights' is not equal to number of columns")
    matrix <- normalize_impacts(decision, impacts)
    matrix <- sapply(matrix, topsis_normalize, weights)
    matrix_max <- apply(matrix, 2, max)
    matrix_min <- apply(matrix, 2, min)
    matrix_max_distance <- apply(matrix, 1, calculate_distance, matrix_max)
    matrix_min_distance <- apply(matrix, 1, calculate_distance, matrix_min)
    score <- as.vector(matrix_min_distance / (matrix_min_distance + matrix_max_distance))

    df <- data.frame(alt.row = 1:length(vector), score = score, rank = rank(-score), row.names = rownames(decision))
    return(df)
}
