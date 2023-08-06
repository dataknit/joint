#' Fit
#'
#' @description
#' Create a joint distribution from a list of target distributions
#'
#'
#' @param targets
#' @param empty_joint
#' @param iterations
#' @param remove_zeroes If cells with zero people are not required for your analysis,
#' removing zeroes will speed up the fit
#'
#' @return
#' @export
#'
#' @examples
fit <- function(targets, empty_joint = NULL, iterations = 10,
                remove_zeroes = TRUE) {

  check_for_duplicate_strata(targets)

  if (is.null(empty_joint)) {
    message(("No tibble for empty_joint supplied so creating one automatically."))
    empty_joint <- create_empty_joint(targets)
  }

  summarise_fit_process(empty_joint, targets)

  # set up initial frame
  joint <- empty_joint |>
    dplyr::mutate(n = 1)

  total_iterations <- iterations * length(targets)

  for (i in 1:total_iterations) {
    target_marginal <- targets[[(i%%length(targets))+1]]
    joint <- single_fit_iteration(joint, target_marginal)
    if(remove_zeroes) { joint <- joint |> dplyr::filter(n > 0) }
  }

  return(joint)
}

check_for_duplicate_strata <- function(targets) {
  for (i in seq_along(targets)) {

    # Remove the 'n' column before checking for duplicates
    tibble_without_n <- targets[[i]] |>
      dplyr::select(-n)

    if (any(duplicated(tibble_without_n))) {
      stop(glue::glue("Error: There are repeated stratas in tibble {i}."))
    }

  }
}

summarise_fit_process <- function(targets, empty_joint) {
  message(glue::glue("You have provided {length(targets)} target distributions.
                     These are to be aligned to {ncol(empty_joint)} variables."))
}

single_fit_iteration <- function(joint, target_marginal) {
  marginal <- create_marginal(joint, target_marginal)
  difference <- check_difference(marginal)
  print(difference)
  joint <- update_joint_to_marginal(joint, marginal)

  return(joint)
}

create_marginal <- function(joint, target_marginal) {
  variables_to_align <- target_marginal |>
    dplyr::select(-n) |>
    names()
  current_marginal <- joint |>
    dplyr::group_by(!!!rlang::syms(variables_to_align)) |>
    dplyr::summarise(current_n = sum(n)) |>
    dplyr::ungroup()
  marginal <- current_marginal |>
    dplyr::left_join(target_marginal |>
                       dplyr::rename(target_n = n),
                     by = dplyr::join_by(!!!rlang::syms(variables_to_align)))
}

update_joint_to_marginal <- function(joint, marginal) {
  marginal <- marginal |>
    dplyr::mutate(ratio = dplyr::case_when(
      target_n == 0 ~ 0,
      is.na(target_n) ~ 1,
      TRUE ~ target_n/current_n))
  joint <- joint |>
    dplyr::left_join(marginal) |>
    dplyr::mutate(n = n*ratio) |>
    dplyr::select(-current_n, -target_n, -ratio)
  return(joint)
}

check_difference <- function(marginal) {
  difference <-  marginal |>
    dplyr::summarise((target_n - current_n) |>
                       abs() |>
                       mean(na.rm = TRUE)) |>
    as.numeric()
  return(difference)
}
