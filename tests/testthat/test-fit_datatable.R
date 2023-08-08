test_that("fit executes without errors and produces a tibble", {
  joint_actual <- tidyr::crossing(a = c("A", "B", "C", "D"),
                                  b = c("X", "Y", "Z"),
                                  c = c("J", "O")) |>
    dplyr::mutate(n = rep(c(1:6), 4))

  empty_joint <- joint_actual |> dplyr::select(-n)

  target1 <- joint_actual |>
    dplyr::group_by(a, b) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  target2 <- joint_actual |>
    dplyr::group_by(a, c) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  target3 <- joint_actual |>
    dplyr::group_by(b, c) |>
    dplyr::summarise(n = sum(n), .groups = "drop")

  targets <- list(target1, target2, target3)

  joint <- fit(targets, empty_joint, backend = "datatable", iterations = 3)

  joint_base <- fit(targets, empty_joint, backend = "base", iterations = 3)

  expect_s3_class(joint, "tbl_df")
  expect_identical(names(joint), c("a", "b", "c", "n"))

  expect_equal(joint |> dplyr::arrange(n) |> dplyr::select(n) |> as.character(),
               joint_actual |> dplyr::arrange(n) |> dplyr::select(n) |> as.character())

  expect_equal(joint, joint_base)
})
