test_that("calc_enrichment excludes missing grouping keys from references", {
  data <- data.frame(
    type = c("reference", NA, "sample"),
    d13C = c(-20, 999, -18)
  )

  result <- calc_enrichment(data, delta = "d13C")

  expect_equal(result$e13C, c(0, 1019, 2))
})

test_that("calc_enrichment validates source and output names", {
  data <- data.frame(
    type = c("reference", "sample"),
    d13C = c(-20, -18),
    e13C = c(100, 100)
  )

  expect_error(
    calc_enrichment(data, delta = "d13C"),
    class = "ggbothbar_name_collision"
  )
  expect_error(
    calc_enrichment(
      data,
      delta = c("d13C", "d13C"),
      epsilon = c("e1", "e2")
    ),
    class = "ggbothbar_input_error"
  )
  expect_error(
    calc_enrichment(
      data,
      delta = c("d13C", "e13C"),
      epsilon = c("enrichment", "enrichment")
    ),
    class = "ggbothbar_input_error"
  )
})

test_that("calc_enrichment overwrites only when explicitly requested", {
  data <- data.frame(
    type = c("reference", "sample"),
    d13C = c(-20, -18),
    e13C = c(100, 100)
  )

  result <- calc_enrichment(data, delta = "d13C", overwrite = TRUE)

  expect_equal(result$e13C, c(0, 2))
  expect_equal(names(result), names(data))
})

test_that("calc_enrichment preserves row order and data-frame class", {
  data <- dplyr::tibble(
    row_id = c(3, 1, 2),
    type = c("sample", "reference", "sample"),
    d13C = c(-18, -20, -19)
  )

  result <- calc_enrichment(data, delta = "d13C")

  expect_s3_class(result, "tbl_df")
  expect_identical(result$row_id, data$row_id)
})

test_that("calc_enrichment warns and isolates undefined reference means", {
  data <- data.frame(
    type = c("reference", "sample"),
    d13C = c(NA_real_, -18),
    d15N = c(7, 9)
  )

  expect_warning(
    result <- calc_enrichment(
      data,
      delta = c("d13C", "d15N"),
      na.rm = TRUE
    ),
    class = "ggbothbar_reference_mean_warning"
  )

  expect_true(all(is.na(result$e13C)))
  expect_equal(result$e15N, c(0, 2))
})

test_that("calc_enrichment requires numeric isotope columns", {
  data <- data.frame(
    type = c("reference", "sample"),
    d13C = c("-20", "-18")
  )

  expect_error(
    calc_enrichment(data, delta = "d13C"),
    class = "ggbothbar_input_error"
  )
})

test_that("custom calc_error functions receive preprocessed data", {
  observed <- new.env(parent = emptyenv())
  custom_error <- function(x) {
    observed$x <- x
    mean(x)
  }

  result <- calc_error(
    c(1, NA, 3),
    fun.errorbar = custom_error,
    na.rm = TRUE
  )

  expect_equal(result, 2)
  expect_equal(observed$x, c(1, 3))
})

test_that("custom calc_error return values follow a scalar contract", {
  expect_identical(
    calc_error(1, fun.errorbar = function(x) NA_real_),
    NA_real_
  )
  expect_error(
    calc_error(1:3, fun.errorbar = function(x) c(1, 2)),
    class = "ggbothbar_input_error"
  )
  expect_error(
    calc_error(1:3, fun.errorbar = function(x) "invalid"),
    class = "ggbothbar_input_error"
  )
  expect_error(
    calc_error(1:3, fun.errorbar = function(x) NaN),
    class = "ggbothbar_input_error"
  )
  expect_error(
    calc_error(1:3, fun.errorbar = function(x) Inf),
    class = "ggbothbar_input_error"
  )
})

test_that("built-in calc_error methods reject non-finite results", {
  expect_error(
    calc_error(c(1, Inf), fun.errorbar = "sd"),
    class = "ggbothbar_input_error"
  )
  expect_identical(calc_error(1, fun.errorbar = "ci"), NA_real_)
})
