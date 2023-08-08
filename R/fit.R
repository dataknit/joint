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
#' @importFrom data.table `:=`
#' @export
#'
#' @examples
fit <- function(targets, empty_joint = NULL, iterations = 10,
                remove_zeroes = TRUE, backend = "base") {

  check_for_duplicate_strata(targets)

  if (is.null(empty_joint)) {
    message(("No tibble for empty_joint supplied so creating one automatically."))
    empty_joint <- create_empty_joint(targets, backend = backend)
  }

  if (backend %in% c("datatable", "data.table")) {
    empty_joint <- dtplyr::lazy_dt(empty_joint)
    targets <- purrr::map(targets, ~ dtplyr::lazy_dt(.))
  }

  summarise_process(empty_joint, targets)

  # set up initial frame
  joint <- empty_joint |>
    dplyr::mutate(n = 1)

  total_iterations <- iterations * length(targets)

  for (i in 1:total_iterations) {
    target_marginal <- targets[[(i%%length(targets))+1]]
    joint <- single_fit_iteration(joint, target_marginal, backend = backend)
    if(remove_zeroes) {
      joint <- joint |>
        dplyr::filter(n > 0)
    }
  }

  if (backend %in% c("datatable", "data.table")) {
    joint <- joint |>
      tibble::as_tibble()
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

summarise_process <- function(targets, empty_joint) {
  message(glue::glue("You have provided {length(targets)} target distributions.
                     These are to be aligned to {ncol(empty_joint)} variables."))
}

single_fit_iteration <- function(joint, target_marginal,
                                 backend = "base",
                                 show_difference = dplyr::if_else(backend %in% c("base", "dplyr"), TRUE, FALSE)) {
  marginal <- create_marginal(joint, target_marginal, backend = backend)
  if (show_difference) {
    difference <- check_difference(marginal)
    print(difference)
  }
  joint <- update_joint_to_marginal(joint, marginal)

  return(joint)
}

create_marginal <- function(joint, target_marginal, backend = "base") {
  if (backend %in% c("datatable", "data.table")) {
    variables_to_align <- target_marginal$parent |>
      dplyr::select(-n) |>
      names()
  } else {
    variables_to_align <- target_marginal |>
      dplyr::select(-n) |>
      names()
  }
  # variables_to_align <- variables_to_align |>
  #   dplyr::select(-n) |>
  #   names()
  current_marginal <- joint |>
    dplyr::group_by(!!!rlang::syms(variables_to_align)) |>
    dplyr::summarise(current_n = sum(n), .groups = "drop")
  marginal <- current_marginal |>
    dplyr::left_join(target_marginal |>
                       dplyr::rename(target_n = n),
                     by = variables_to_align)
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
