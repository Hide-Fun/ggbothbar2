#' Fix Aspect Ratio of ggplot Based on Plot Limits
#'
#' This function adjusts the aspect ratio of a ggplot object by calculating the ratio
#' based on the current plot limits and a desired ratio modifier.
#'
#' @param .plot A ggplot object
#' @param .ratio Numeric value to modify the calculated aspect ratio
#' @param .clip Character string specifying the clipping behavior ("off" by default)
#'   See \code{\link[ggplot2]{coord_fixed}} for more details
#'
#' @return A modified ggplot object with adjusted aspect ratio
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Adjust the aspect ratio
#' fix_aspect_ratio(p, .ratio = 1)
#'
#' @importFrom ggplot2 coord_fixed
#' @export
fix_aspect_ratio <- function(.plot, .ratio, .clip = "off") {
  if (!is.numeric(.ratio) || length(.ratio) != 1L || !is.finite(.ratio) || .ratio <= 0) {
    stop("'.ratio' must be a single positive finite numeric value", call. = FALSE)
  }

  panel_ranges <- plot_panel_ranges(.plot)
  x_range_raw <- panel_ranges$x
  y_range_raw <- panel_ranges$y

  validate_axis_range <- function(range, axis) {
    if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
      stop(
        "Cannot fix aspect ratio because the ",
        axis,
        " axis range is not a finite numeric range.",
        call. = FALSE
      )
    }

    axis_range <- abs(range[[1]] - range[[2]])
    if (axis_range <= 0) {
      stop(
        "Cannot fix aspect ratio because the ",
        axis,
        " axis range has zero width.",
        call. = FALSE
      )
    }

    axis_range
  }

  # calculate diff
  x_range <- validate_axis_range(x_range_raw, "x")
  y_range <- validate_axis_range(y_range_raw, "y")
  stund <- x_range / y_range
  rlt <- .plot + coord_fixed(ratio = stund * .ratio, clip = .clip)
  return(rlt)
}

#' Fix Aspect Ratio of ggplot Based on Plot Limits
#'
#' `fix_limit()` is kept for backward compatibility. Use
#' `fix_aspect_ratio()` for new code.
#'
#' @inheritParams fix_aspect_ratio
#'
#' @return A modified ggplot object with adjusted aspect ratio
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # Backward-compatible API
#' fix_limit(p, .ratio = 1)
#'
#' @export
fix_limit <- function(.plot, .ratio, .clip = "off") {
  fix_aspect_ratio(.plot = .plot, .ratio = .ratio, .clip = .clip)
}
