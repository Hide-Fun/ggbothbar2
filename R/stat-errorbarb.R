# Internal helpers for summary statistics that must be calculated in the
# original data space, before scale transformations.

inverse_scale_position <- function(values, scale) {
  if (is.null(scale) || is.null(scale$trans) || is.null(scale$trans$inverse)) {
    return(values)
  }
  scale$trans$inverse(values)
}

transform_scale_position <- function(values, scale) {
  if (is.null(scale) || is.null(scale$trans) || is.null(scale$trans$transform)) {
    return(values)
  }
  scale$trans$transform(values)
}

compute_error_summary <- function(data, scales, fun.errorbar, na.rm) {
  raw_x <- inverse_scale_position(data$x, scales$x)
  raw_y <- inverse_scale_position(data$y, scales$y)

  center_x <- mean(raw_x, na.rm = na.rm)
  center_y <- mean(raw_y, na.rm = na.rm)
  error_x <- calc_error(raw_x, fun.errorbar = fun.errorbar, na.rm = na.rm)
  error_y <- calc_error(raw_y, fun.errorbar = fun.errorbar, na.rm = na.rm)

  data.frame(
    x = transform_scale_position(center_x, scales$x),
    y = transform_scale_position(center_y, scales$y),
    xmin = transform_scale_position(center_x - error_x, scales$x),
    xmax = transform_scale_position(center_x + error_x, scales$x),
    ymin = transform_scale_position(center_y - error_y, scales$y),
    ymax = transform_scale_position(center_y + error_y, scales$y)
  )
}

StatErrorbarb <- ggplot2::ggproto(
  "StatErrorbarb",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, fun.errorbar = "sd", na.rm = FALSE) {
    if (
      !is.character(fun.errorbar) ||
        length(fun.errorbar) != 1L ||
        is.na(fun.errorbar) ||
        !fun.errorbar %in% c("sd", "se")
    ) {
      rlang::abort(
        "`fun.errorbar` must be either \"sd\" or \"se\".",
        class = "ggbothbar_input_error"
      )
    }

    compute_error_summary(
      data,
      scales,
      fun.errorbar = fun.errorbar,
      na.rm = na.rm
    )
  }
)
