# Grouping methods.

#' Group linearly ordered alternatives by average.
#'
#' @param ordered_df A data frame with elements \code{alt.row}, \code{score}
#'   and \code{rank} (returned from any linear ordering method from this library).
#' @return A list including elements:
#' \describe{
#'   \item{\code{highest}}{group of highest values}
#'   \item{\code{above_average}}{group of above average values}
#'   \item{\code{below_average}}{group of below average values}
#'   \item{\code{lowest}}{group of lowest values}
#' }
#' @author Antoni Baum \email{antoni.baum@protonmail.com}
#' @examples
#' d <- matrix(rpois(12, 5), nrow = 3, ncol = 3)
#' w <- c(1, 1, 2)
#' i <- c('+', '-', '+')
#' group_by_average(sum_of_ranks(d, w, i))
#' @export
group_by_average <- function(ordered_df) {
  mean_score = mean(ordered_df$score)
  sd_score = sd(ordered_df$score)
  groups <-
    list(
      "highest" = c(),
      "above_average" = c(),
      "below_average" = c(),
      "lowest" = c()
    )
  for (i in 1:nrow(ordered_df))
  {
    if (ordered_df$score[i] >= mean_score + sd_score) {
      groups$highest <- c(groups$highest, ordered_df$alt.row[i])
    }
    else if (mean_score <= ordered_df$score[i] &&
        ordered_df$score[i] < mean_score + sd_score) {
      groups$above_average <-
        c(groups$above_average, ordered_df$alt.row[i])
    }
    else if (mean_score - sd_score <= ordered_df$score[i] &&
        ordered_df$score[i] < mean_score) {
      groups$below_average <-
        c(groups$below_average, ordered_df$alt.row[i])
    }
    else {
      groups$lowest <- c(groups$lowest, ordered_df$alt.row[i])
    }
  }
  return (groups)
}
