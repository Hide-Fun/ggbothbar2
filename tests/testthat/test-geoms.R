with_temp_pdf_device <- function(code, width = 7, height = 7) {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width, height = height)
  device <- grDevices::dev.cur()
  on.exit(
    {
      if (device %in% grDevices::dev.list()) {
        grDevices::dev.off(device)
      }
      unlink(path)
    },
    add = TRUE
  )
  force(code)
}

test_that("geom_errorbarb uses its summary stat by default", {
  layer <- geom_errorbarb()

  expect_true(inherits(layer$stat, "StatErrorbarb"))
  expect_equal(layer$geom_params$errorbar_tip_size, 0.2)
})

test_that("geom_errorbarb computes uncertainty in original data space", {
  data <- data.frame(
    x = c(10, 20, 30),
    y = c(2, 4, 6),
    group = 1
  )
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, group = group)) +
    geom_errorbarb(fun.errorbar = "sd") +
    ggplot2::scale_x_log10()

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$x, log10(20), tolerance = 1e-8)
  expect_equal(layer_data$xmin, log10(10), tolerance = 1e-8)
  expect_equal(layer_data$xmax, log10(30), tolerance = 1e-8)
  expect_equal(layer_data$y, 4, tolerance = 1e-8)
  expect_equal(layer_data$ymin, 2, tolerance = 1e-8)
  expect_equal(layer_data$ymax, 6, tolerance = 1e-8)
})

test_that("geom_errorbarb identity stat accepts precomputed endpoints", {
  data <- data.frame(
    x = 2,
    y = 3,
    xmin = 1,
    xmax = 4,
    ymin = 2,
    ymax = 5
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = x,
      y = y,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  ) +
    geom_errorbarb(stat = "identity")

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$xmin, 1)
  expect_equal(layer_data$xmax, 4)
  expect_equal(layer_data$ymin, 2)
  expect_equal(layer_data$ymax, 5)
})

test_that("geom_errorbarb sends lineend to final grid parameters", {
  data <- data.frame(
    x = 0.5,
    y = 0.5,
    xmin = 0.4,
    xmax = 0.6,
    ymin = 0.4,
    ymax = 0.6,
    colour = "black",
    alpha = NA_real_,
    linewidth = 0.5,
    linetype = 1
  )

  grob <- ggbothbar:::draw_errorbarb_row(data, 0.2, "square")
  lineends <- vapply(
    grob$children,
    function(child) child$gp$lineend,
    character(1)
  )

  expect_true(all(lineends == "square"))
  expect_error(
    ggplot2::ggplotGrob(
      ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
        geom_errorbarb(lineend = NA_character_)
    ),
    class = "ggbothbar_input_error"
  )
})

test_that("geom_errorbox honors the requested stat", {
  data <- data.frame(
    x = 2,
    y = 3,
    xmin = 1,
    xmax = 4,
    ymin = 2,
    ymax = 5
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = x,
      y = y,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  ) +
    geom_errorbox(stat = "identity")
  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$xmin, 1)
  expect_equal(layer_data$xmax, 4)
  expect_equal(layer_data$ymin, 2)
  expect_equal(layer_data$ymax, 5)
})

test_that("geom_errorbox computes transformed summaries in data space", {
  data <- data.frame(x = c(10, 20, 30), y = c(2, 4, 6), group = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, group = group)) +
    geom_errorbox(fun.errorbar = "sd") +
    ggplot2::scale_x_log10()

  layer_data <- ggplot2::ggplot_build(plot)$data[[1]]

  expect_equal(layer_data$xmin, log10(10), tolerance = 1e-8)
  expect_equal(layer_data$xmax, log10(30), tolerance = 1e-8)
})

test_that("errorbar tips preserve their physical centimeter size", {
  with_temp_pdf_device({
    grob <- errorbarbGrob(
      x = grid::unit(c(0.4, 0.6), "npc"),
      y = grid::unit(c(0.4, 0.6), "npc"),
      errorbar_tip_size = grid::unit(0.18, "cm")
    )
    grob <- grid::makeContent(grob)
    segments <- grob$children[[1]]
    cap_width <- abs(grid::convertX(
      segments$x1[[1]] - segments$x0[[1]],
      "mm",
      valueOnly = TRUE
    ))

    expect_equal(cap_width, 1.8, tolerance = 1e-6)
  })
})

test_that("the geom default cap is two millimeters", {
  with_temp_pdf_device({
    data <- data.frame(
      x = 0.5,
      y = 0.5,
      xmin = 0.4,
      xmax = 0.6,
      ymin = 0.4,
      ymax = 0.6,
      colour = "black",
      alpha = NA_real_,
      linewidth = 0.5,
      linetype = 1
    )
    grob <- ggbothbar:::draw_errorbarb_row(data, 0.2, "butt")
    lower_cap <- grob$children[[2]]
    cap_width <- abs(grid::convertX(
      lower_cap$x1 - lower_cap$x0,
      "mm",
      valueOnly = TRUE
    ))

    expect_equal(cap_width, 2, tolerance = 1e-6)
  })
})

test_that("either zero-variance axis leaves the other interval visible", {
  aesthetics <- data.frame(
    x = 0.5,
    y = 0.5,
    colour = "black",
    alpha = NA_real_,
    linewidth = 0.5,
    linetype = 1
  )
  zero_x <- cbind(
    aesthetics,
    xmin = 0.5,
    xmax = 0.5,
    ymin = 0.4,
    ymax = 0.6
  )
  zero_y <- cbind(
    aesthetics,
    xmin = 0.4,
    xmax = 0.6,
    ymin = 0.5,
    ymax = 0.5
  )

  vertical <- ggbothbar:::draw_errorbarb_row(zero_x, 0.2, "butt")
  horizontal <- ggbothbar:::draw_errorbarb_row(zero_y, 0.2, "butt")

  expect_length(vertical$children, 3)
  expect_length(horizontal$children, 3)
})

test_that("summary coordinates do not depend on viewport dimensions", {
  data <- data.frame(x = c(1, 2, 4), y = c(2, 5, 8), group = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, group = group)) +
    geom_errorbarb(fun.errorbar = "se")
  summary_at_width <- function(width) {
    with_temp_pdf_device(
      {
        print(plot)
        ggplot2::ggplot_build(plot)$data[[1]][
          c("x", "y", "xmin", "xmax", "ymin", "ymax")
        ]
      },
      width = width,
      height = 5
    )
  }

  expect_equal(summary_at_width(4), summary_at_width(10))
})

test_that("non-linear coordinates emit the geom-specific warning", {
  data <- data.frame(x = c(1, 2, 3), y = c(2, 4, 3), group = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x, y, group = group)) +
    geom_errorbarb() +
    ggplot2::coord_polar()

  with_temp_pdf_device({
    expect_warning(
      print(plot),
      regexp = "geom_errorbarb",
      class = "ggbothbar_coordinate_warning"
    )
  })
})

test_that("representative two-axis uncertainty plot is stable", {
  skip_if_not_installed("vdiffr")
  skip_if(
    utils::packageVersion("ggplot2") < "4.0.0",
    "The snapshot is recorded with ggplot2 4.x."
  )
  data <- data.frame(
    group = rep(c("a", "b"), each = 3),
    x = c(1, 2, 3, 4, 5, 6),
    y = c(2, 4, 3, 5, 7, 6)
  )
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x, y, colour = group, group = group)
  ) +
    geom_errorbarb(fun.errorbar = "se") +
    ggplot2::theme_test()

  vdiffr::expect_doppelganger("two-axis standard errors", plot)
})
