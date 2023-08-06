#' Create empty joint from targets
#'
#' @param targets
#' @param columns_to_exclude
#' @param backend "base", "dplyr" or "datatable"
#'
#' @return
#' @export
#'
#' @examples
create_empty_joint <- function(targets, columns_to_exclude = NULL,
                               backend = "base") {
  flat_unique_values <- targets |>
    purrr::map(~ .x[, !names(.x) %in% "n"]) |>
    purrr::map(~ purrr::map(.x, function(vec) {
      sort(unique(vec)) # Sort the elements to make permutations the same
    })) |>
    unlist(recursive = FALSE)

  if (!is.null(columns_to_exclude)) {
    flat_unique_values <- remove_vectors_by_names(flat_unique_values, columns_to_exclude)
  }

  unique_indices <- !duplicated(flat_unique_values)
  unique_flat_unique_values <- flat_unique_values[unique_indices]

  if (backend %in% c("dplyr", "base")) {
  combinations_tibble <- unique_flat_unique_values |>
    expand.grid(stringsAsFactors = FALSE)
  } else if (backend == "datatable") {
    combinations_tibble <- unique_flat_unique_values |>
      get_permutations_data_table()
  }

  combinations_tibble <- combinations_tibble |>
    tibble::as_tibble()

  return(combinations_tibble)
}

# Function to remove vectors by names from the targets list
remove_vectors_by_names <- function(char_list, names_to_remove) {
  if (!is.list(char_list) || !all(sapply(char_list, function(x) is.character(x) || is.integer(x)))) {
    stop("Error: Input must be a list of character or integer vectors.")
  }

  if (!is.character(names_to_remove)) {
    stop("Error: 'names_to_remove' must be a character vector.")
  }

  char_list_filtered <- char_list[!names(char_list) %in% names_to_remove]

  return(char_list_filtered)
}


get_permutations_data_table <- function(input_list) {
  list_names <- names(input_list)

  result <- do.call(data.table::CJ, input_list)

  data.table::setnames(result, list_names)

  return(result)
}
