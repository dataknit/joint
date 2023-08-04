test_that("fit executes without errors and produces a tibble", {
  joint <- tidyr::crossing(a = c("A", "B", "C", "D"),
                          b = c("X", "Y", "Z"),
                          c = c("J", "O")) |>
    dplyr::mutate(n = rep(c(1:6), 4))

  target1 <- joint |>
    dplyr::group_by(a, b) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  target2 <- joint |>
    dplyr::group_by(a, c) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  target3 <- joint |>
    dplyr::group_by(b, c) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  targets <- list(target1, target2, target3)

  result <- fit(targets, iterations = 2)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("a", "b", "c", "n"))

  expect_equal(joint |> dplyr::arrange(n) |> dplyr::select(n) |> as.character(),
               result |> dplyr::arrange(n) |> dplyr::select(n) |> as.character())
})

test_that("check_for_duplicate_strata detects duplicate stratas", {
  targets <- list(
    tibble::tibble(age = c(1, 2, 3), gender = c("female", "male", "other"), n = c(10, 20, 30)),
    tibble::tibble(age = c(1, 1, 1), gender = c("female", "male", "other"), n = c(10, 20, 30)),
    tibble::tibble(age = c(1, 1, 3), gender = c("female", "female", "other"), n = c(10, 20, 30))
  )

  expect_error(check_for_duplicate_strata(targets), "Error: There are repeated stratas in tibble 3.")
})

test_that("single_fit_iteration produces expected output", {
  joint <- tibble::tibble(a = c("A", "B", "C", "D"),
                          b = c("X", "Y", "Z", "Z"),
                          c = 1:4,
                          n = c(9, 9, 9, 9))

  target_marginal <- tibble::tibble(a = c("A", "B", "C"),
                                    b = c("X", "Y", "Z"),
                                    n = c(15, 35, 55))

  result <- single_fit_iteration(joint, target_marginal)

  expect_s3_class(result, "tbl_df")
  expect_identical(names(result), c("a", "b", "c", "n"))

  expect_equal(result$n, c(target_marginal$n, 9))
})

test_that("create_marginal computes marginal correctly", {
  joint <- tibble::tibble(a = c("A", "B", "C"),
                          b = c("X", "Y", "Z"),
                          n = c(5, 15, 25))

  target_marginal <- tibble::tibble(a = c("A", "B", "C"),
                                    b = c("X", "Y", "Z"),
                                    n = c(15, 35, 55))

  result <- create_marginal(joint, target_marginal)

  expect_equal(result$current_n, c(5, 15, 25))
  expect_equal(result$target_n, c(15, 35, 55))
})

test_that("create_marginal handles missing target_marginal values", {
  joint <- tibble::tibble(a = c("A", "B", "C"),
                          b = c("X", "Y", "Z"),
                          n = c(5, 15, 25))

  target_marginal <- tibble::tibble(a = c("A", "B"),
                                    b = c("X", "Y"),
                                    n = c(15, 35))

  result <- create_marginal(joint, target_marginal)

  expect_equal(result$current_n, c(5, 15, 25))
  expect_equal(result$target_n, c(15, 35, NA))
})

test_that("update_joint_to_marginal calculates the ratio correctly", {
  marginal <- tibble::tibble(id = 1:4,
                             target_n = c(10, 20, 30, 40),
                             current_n = c(5, 15, 35, 45))
  joint <- tibble::tibble(id = 1:4, n = c(1, 1, 1, 1))

  result <- update_joint_to_marginal(joint, marginal)

  expect_identical(result$n, c(2, 4/3, 30/35, 40/45))
})

test_that("update_joint_to_marginal deals with NA", {
  marginal <- tibble::tibble(id = 1:4,
                             target_n = c(10, NA, 30, NA),
                             current_n = c(5, 15, 35, 45))
  joint <- tibble::tibble(id = 1:4, n = c(1, 1, 1, 1))

  result <- update_joint_to_marginal(joint, marginal)

  expect_identical(result$n, c(2, 1, 30/35, 1))
})

test_that("update_joint_to_marginal deals with zeroes", {
  marginal <- tibble::tibble(id = 1:4,
                             target_n = c(10, 0, 30, 0),
                             current_n = c(5, 15, 35, 45))
  joint <- tibble::tibble(id = 1:4, n = c(1, 1, 1, 1))

  result <- update_joint_to_marginal(joint, marginal)

  expect_identical(result$n, c(2, 0, 30/35, 0))
})


test_that("check_difference calculates the difference correctly", {
  marginal <- tibble::tibble(target_n = c(10, 0, 30, 40),
                             current_n = c(5, 15, 35, 40))

  result <- check_difference(marginal)

  expect_equal(result, 25/4)
})

test_that("check_difference handles missing values correctly", {
  marginal <- tibble::tibble(target_n = c(10, 0, 30, 40),
                             current_n = c(5, 15, NA, 40))

  result <- check_difference(marginal)

  expect_equal(result, 20/3)
})
