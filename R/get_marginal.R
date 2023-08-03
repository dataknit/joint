#' Get marginals from a joint distribution
#'
#' @param joint
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_marginal <- function(joint, ...) {
  variables_to_marginalise <- rlang::enquos(...)

  marginal <- joint |>
    dplyr::group_by(!!!variables_to_marginalise) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  return(marginal)
}
