#' ggproto object for error box statistics
#'
#' @format ggproto class
#' @importFrom ggplot2 ggproto
#' @keywords internal
StatErrorbox <- ggplot2::ggproto(
  "StatErrorbox",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales, fun.errorbar = "sd", na.rm = FALSE) {
    compute_error_summary(
      data,
      scales,
      fun.errorbar = fun.errorbar,
      na.rm = na.rm
    )
  }
)
