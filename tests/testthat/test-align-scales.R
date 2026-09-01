test_that("adjust_axis_scales preserves log transformations", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10()

  adjusted <- adjust_axis_scales(plot, x_break_step = 0.1)
  scale <- ggplot2::ggplot_build(adjusted)$layout$panel_scales_x[[1]]

  expect_identical(scale$trans$name, "log-10")
})

test_that("adjust_axis_scales preserves flipped coordinates", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::coord_flip()

  adjusted <- adjust_axis_scales(plot)

  expect_true(inherits(adjusted$coordinates, "CoordFlip"))
})

test_that("align_axis_scales preserves scale and coordinate semantics", {
  first <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    ggplot2::coord_flip()
  second <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    ggplot2::coord_flip()

  aligned <- align_axis_scales(
    list(first, second),
    axes = "x",
    x_break_step = 0.1
  )

  expect_true(all(vapply(
    aligned,
    function(plot) inherits(plot$coordinates, "CoordFlip"),
    logical(1)
  )))
  expect_true(all(vapply(
    aligned,
    function(plot) {
      scale <- ggplot2::ggplot_build(plot)$layout$panel_scales_x[[1]]
      identical(scale$trans$name, "log-10")
    },
    logical(1)
  )))
})

test_that("log-scale limits and breaks stay in original data space", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_log10() +
    ggplot2::coord_flip()

  adjusted <- adjust_axis_scales(
    plot,
    x_break_step = 1,
    x_limits = c(2, 6)
  )
  scale <- adjusted$scales$get_scales("x")

  expect_identical(scale$trans$name, "log-10")
  expect_equal(scale$breaks, 2:6)
  expect_equal(adjusted$coordinates$limits$x, c(2, 6))
})

test_that("an omitted expansion preserves an explicit scale expansion", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.2))
  original_expand <- plot$scales$get_scales("x")$expand

  adjusted <- adjust_axis_scales(plot)

  expect_identical(
    adjusted$scales$get_scales("x")$expand,
    original_expand
  )
})

test_that("free facets produce a structured fallback warning", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(ggplot2::vars(cyl), scales = "free_x")

  expect_warning(
    adjusted <- adjust_axis_scales(plot),
    regexp = "x facet scale",
    class = "ggbothbar_axis_fallback_warning"
  )
  expect_s3_class(adjusted, "ggplot")
})

test_that("multi-plot fallback warnings identify the affected plot", {
  fixed <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()
  free <- fixed +
    ggplot2::facet_wrap(ggplot2::vars(cyl), scales = "free_x")

  expect_warning(
    aligned <- align_axis_scales(list(fixed, free), axes = "x"),
    regexp = "Plot 2 requires",
    class = "ggbothbar_axis_fallback_warning"
  )

  expect_length(aligned, 2)
})

test_that("unsupported coordinates warn before Cartesian fallback", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +
    ggplot2::coord_polar()

  expect_warning(
    adjusted <- adjust_axis_scales(plot),
    regexp = "coordinate class",
    class = "ggbothbar_axis_fallback_warning"
  )

  expect_true(inherits(adjusted$coordinates, "CoordCartesian"))
  expect_false(inherits(adjusted$coordinates, "CoordPolar"))
})

test_that("axis helper inputs are validated before plot changes", {
  plot <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point()

  expect_error(
    adjust_axis_scales(plot, x_break_step = 0),
    class = "ggbothbar_input_error"
  )
  expect_error(
    adjust_axis_scales(plot, clip = "invalid"),
    class = "ggbothbar_input_error"
  )
  expect_error(
    align_axis_scales(list(plot, plot), aspect_ratio = -1),
    class = "ggbothbar_input_error"
  )
})
