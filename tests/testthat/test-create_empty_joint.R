test_that("create_empty_joint returns a tibble", {
  targets <- list(tibble::tibble(a = 1:5, b = 5:9),
                  tibble::tibble(b = 5:9),
                  tibble::tibble(c = 2:6, d = 3:7))
  columns_to_exclude <- c("a")

  result <- create_empty_joint(targets, columns_to_exclude)

  expect_s3_class(result, "tbl_df")
})

test_that("create_empty_joint contains correct number of rows and columns", {
  targets <- list(tibble::tibble(a = 1:5, b = 5:9),
                  tibble::tibble(b = 5:9),
                  tibble::tibble(c = 2:6, d = 3:7))
  columns_to_exclude <- c("a")

  result <- create_empty_joint(targets, columns_to_exclude)

  expect_equal(nrow(result), 5*5*5)
  expect_equal(ncol(result), 3)
})

test_that("create_empty_joint excludes columns", {
  targets <- list(tibble::tibble(a = 1:5, b = 5:9),
                  tibble::tibble(b = 5:9),
                  tibble::tibble(c = 2:6, d = 3:7))
  columns_to_exclude <- c("a", "c")

  result <- create_empty_joint(targets, columns_to_exclude)

  expect_false("a" %in% names(result))
  expect_false("c" %in% names(result))
  expect_true("b" %in% names(result))
  expect_true("d" %in% names(result))
})

test_that("create_empty_joint datatable backend gives the same result as dplyr
backend (ignoring permutations and attributes differences)", {
  targets <- list(tibble::tibble(a = 1:5, b = 5:9),
                  tibble::tibble(b = 5:9),
                  tibble::tibble(c = 2:6, d = 3:7))
  columns_to_exclude <- c("a", "c")

  result_dplyr <- create_empty_joint(targets, columns_to_exclude,
                                     backend = "dplyr")
  result_datatable <- create_empty_joint(targets, columns_to_exclude,
                                         backend = "datatable")

  result_dplyr <- result_dplyr |>
    arrange(b, d)

  result_dplyr <- data.frame(result_dplyr)
  result_datatable <- data.frame(result_datatable)

  expect_equal(result_dplyr, result_datatable)
})
