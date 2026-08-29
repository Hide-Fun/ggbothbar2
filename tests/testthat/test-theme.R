test_that("theme_aca preserves mapped legends", {
  plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(wt, mpg, colour = factor(cyl))
  ) +
    ggplot2::geom_point() +
    theme_aca(base_family = "")

  built <- ggplot2::ggplot_build(plot)

  expect_identical(built$plot$theme$legend.position, "right")
  expect_length(built$plot$guides$guides, 1L)
})

test_that("theme_isotope remains equivalent to theme_aca", {
  expect_identical(
    theme_isotope(base_size = 12, base_family = ""),
    theme_aca(base_size = 12, base_family = "")
  )
})

test_that("theme_aca mapped legend is visually stable", {
  skip_if_not_installed("vdiffr")
  skip_if(
    utils::packageVersion("ggplot2") < "4.0.0",
    "The snapshot is recorded with ggplot2 4.x."
  )

  plot <- ggplot2::ggplot(
    mtcars,
    ggplot2::aes(wt, mpg, colour = factor(cyl))
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(colour = "Cylinders") +
    theme_aca(base_size = 12, base_family = "")

  vdiffr::expect_doppelganger("theme aca mapped legend", plot)
})
