#' Create empty joint from targets
#'
#' @param targets
#' @param columns_to_exclude
#'
#' @return
#' @export
#'
#' @examples
create_empty_joint <- function(targets, columns_to_exclude) {
  flat_unique_values <- targets |>
    map(~ .x[, !names(.x) %in% "n"]) |>
    map(~ map(.x, function(vec) {
      sort(unique(vec)) # Sort the elements to make permutations the same
    })) |>
    unlist(recursive = FALSE)

  flat_unique_values <- remove_vectors_by_names(flat_unique_values, columns_to_exclude)

  unique_indices <- !duplicated(flat_unique_values)
  unique_flat_unique_values <- flat_unique_values[unique_indices]

  combinations_tibble <- unique_flat_unique_values |>
    expand.grid(stringsAsFactors = FALSE) |>
    tibble::as_tibble()

  return(combinations_tibble)
}


# Function to remove vectors by names from the targets list
remove_vectors_by_names <- function(char_list, names_to_remove) {
  if (!is.list(char_list) || !all(sapply(char_list, is.character))) {
    stop("Error: Input must be a list of character vectors.")
  }

  if (!is.character(names_to_remove)) {
    stop("Error: 'names_to_remove' must be a character vector.")
  }

  char_list_filtered <- char_list[!names(char_list) %in% names_to_remove]

  return(char_list_filtered)
}

