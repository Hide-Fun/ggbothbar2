test_that("fix_aspect_ratio preserves its coordinate contract", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  adjusted <- fix_aspect_ratio(plot, .ratio = 2, .clip = "on")

  expect_s3_class(adjusted$coordinates, "CoordCartesian")
  expect_equal(adjusted$coordinates$ratio, 0.332851063829787)
  expect_identical(adjusted$coordinates$clip, "on")
})

test_that("fix_limit remains an exact compatibility alias", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  preferred <- fix_aspect_ratio(plot, .ratio = 1.5, .clip = "off")
  compatibility <- fix_limit(plot, .ratio = 1.5, .clip = "off")

  expect_identical(compatibility$coordinates, preferred$coordinates)
})

test_that("the ggplot2 adapter exposes trained panel ranges", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  ranges <- ggbothbar:::plot_panel_ranges(plot)

  expect_equal(ranges$x, range(mtcars$wt))
  expect_equal(ranges$y, range(mtcars$mpg))
})

test_that("fix_aspect_ratio rejects invalid ratios and axis ranges", {
  continuous <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  discrete <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
    ggplot2::geom_point()

  expect_error(
    fix_aspect_ratio(continuous, .ratio = 0),
    "single positive finite numeric value",
    fixed = TRUE
  )
  expect_error(
    fix_aspect_ratio(discrete, .ratio = 1),
    "x axis range is not a finite numeric range",
    fixed = TRUE
  )
})
