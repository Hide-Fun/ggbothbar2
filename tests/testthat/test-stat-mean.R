test_that("stat_mean_point computes group means", {
  data <- data.frame(
    x = c(1, 3, NA),
    y = c(2, 6, NA)
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    stat_mean_point(na.rm = TRUE)

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$x, 2)
  expect_equal(layer_data$y, 4)
})

test_that("stat_mean_label keeps supplied group labels", {
  data <- data.frame(
    x = c(1, 3),
    y = c(2, 6),
    label = c("sample", "sample")
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, label = label)) +
    stat_mean_label()

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$x, 2)
  expect_equal(layer_data$y, 4)
  expect_identical(layer_data$label, "sample")
})

test_that("stat_mean_label formats a default coordinate label", {
  data <- data.frame(x = c(1, 3), y = c(2, 6))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    stat_mean_label(digits = 2)

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_identical(layer_data$label, "(2, 4)")
})

test_that("stat_mean_label warns before choosing among group labels", {
  data <- data.frame(
    x = c(1, 3),
    y = c(2, 6),
    label = c("first", "second")
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, label = label)) +
    stat_mean_label()

  expect_warning(
    layer_data <- ggplot2::ggplot_build(plot)$data[[1]],
    "Multiple labels found",
    fixed = TRUE
  )
  expect_identical(layer_data$label, "first")
})
