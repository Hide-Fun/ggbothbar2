#' Geom for two-axis error bars with fixed physical tips
#'
#' `geom_errorbarb()` summarizes each group in the original data space and
#' draws uncertainty intervals along both x and y. Use `stat = "identity"`
#' with precomputed `xmin`, `xmax`, `ymin`, and `ymax` aesthetics.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#' @param data The data to be displayed in this layer.
#' @param stat Statistical transformation. The default, `"errorbarb"`, computes
#'   group means and uncertainty intervals. Use `"identity"` for precomputed
#'   endpoints.
#' @param position Position adjustment.
#' @param ... Other arguments passed to [ggplot2::layer()].
#' @param fun.errorbar Error calculation method, either `"sd"` or `"se"`.
#' @param na.rm If `FALSE`, missing values are removed with a warning. If
#'   `TRUE`, missing values are silently removed.
#' @param errorbar_tip_size Error-bar tip width in millimeters. Defaults to
#'   `2`, which produces an approximately 2 mm cap.
#' @param lineend Line-end style: `"round"`, `"butt"`, or `"square"`.
#' @param linewidth Width of lines.
#' @param show.legend Logical. Should this layer be included in legends?
#' @param inherit.aes If `FALSE`, override rather than combine plot aesthetics.
#'
#' @return A ggplot2 layer.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   geom_errorbarb()
#' @export
geom_errorbarb <- function(
  mapping = NULL,
  data = NULL,
  stat = "errorbarb",
  position = "identity",
  ...,
  fun.errorbar = "sd",
  na.rm = FALSE,
  errorbar_tip_size = 2,
  lineend = "butt",
  linewidth = 0.5,
  show.legend = NA,
  inherit.aes = TRUE
) {
  params <- list(
    na.rm = na.rm,
    errorbar_tip_size = errorbar_tip_size,
    lineend = lineend,
    linewidth = linewidth,
    ...
  )
  if (!identical(stat, "identity") && !inherits(stat, "StatIdentity")) {
    params$fun.errorbar <- fun.errorbar
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = Geomerrorbarb,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

draw_errorbarb_row <- function(data, errorbar_tip_size, lineend) {
  gp <- grid::gpar(
    col = ggplot2::alpha(data$colour, data$alpha),
    lwd = data$linewidth * ggplot2::.pt,
    lty = data$linetype,
    lineend = lineend
  )
  tip <- grid::unit(errorbar_tip_size, "mm")
  half_tip <- tip / 2
  children <- list()

  vertical_ok <- all(is.finite(c(data$x, data$ymin, data$ymax))) &&
    data$ymax > data$ymin
  if (vertical_ok) {
    x <- grid::unit(data$x, "native")
    ymin <- grid::unit(data$ymin, "native")
    ymax <- grid::unit(data$ymax, "native")
    children <- c(
      children,
      list(
        grid::segmentsGrob(x, ymin, x, ymax, gp = gp),
        grid::segmentsGrob(x - half_tip, ymin, x + half_tip, ymin, gp = gp),
        grid::segmentsGrob(x - half_tip, ymax, x + half_tip, ymax, gp = gp)
      )
    )
  }

  horizontal_ok <- all(is.finite(c(data$y, data$xmin, data$xmax))) &&
    data$xmax > data$xmin
  if (horizontal_ok) {
    y <- grid::unit(data$y, "native")
    xmin <- grid::unit(data$xmin, "native")
    xmax <- grid::unit(data$xmax, "native")
    children <- c(
      children,
      list(
        grid::segmentsGrob(xmin, y, xmax, y, gp = gp),
        grid::segmentsGrob(xmin, y - half_tip, xmin, y + half_tip, gp = gp),
        grid::segmentsGrob(xmax, y - half_tip, xmax, y + half_tip, gp = gp)
      )
    )
  }

  if (length(children) == 0L) {
    return(grid::nullGrob())
  }
  do.call(grid::grobTree, children)
}

#' Geomerrorbarb ggproto object
#'
#' Internal ggproto backing [geom_errorbarb()]. Most users should call
#' `geom_errorbarb()` directly.
#'
#' @format NULL
#' @usage NULL
#' @family ggplot2 geoms
#' @noRd
Geomerrorbarb <- ggplot2::ggproto(
  "Geomerrorbarb",
  ggplot2::Geom,
  required_aes = c("x", "y", "xmin", "xmax", "ymin", "ymax"),
  draw_group = function(
    data,
    panel_params,
    coord,
    na.rm = FALSE,
    errorbar_tip_size = 2,
    lineend = "butt"
  ) {
    if (
      !is.numeric(errorbar_tip_size) ||
        length(errorbar_tip_size) != 1L ||
        !is.finite(errorbar_tip_size) ||
        errorbar_tip_size <= 0
    ) {
      rlang::abort(
        "`errorbar_tip_size` must be one positive finite number in millimeters.",
        class = "ggbothbar_input_error"
      )
    }
    if (
      !is.character(lineend) ||
        length(lineend) != 1L ||
        is.na(lineend) ||
        !lineend %in% c("round", "butt", "square")
    ) {
      rlang::abort(
        "`lineend` must be one of \"round\", \"butt\", or \"square\".",
        class = "ggbothbar_input_error"
      )
    }
    if (!coord$is_linear()) {
      rlang::warn(
        "`geom_errorbarb()` is only guaranteed on linear coordinate systems.",
        class = "ggbothbar_coordinate_warning"
      )
    }

    data <- coord$transform(data, panel_params)
    grobs <- lapply(
      seq_len(nrow(data)),
      function(index) {
        draw_errorbarb_row(
          data[index, , drop = FALSE],
          errorbar_tip_size = errorbar_tip_size,
          lineend = lineend
        )
      }
    )
    do.call(grid::grobTree, grobs)
  },
  draw_key = ggplot2::draw_key_path,
  default_aes = ggplot2::aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1L,
    alpha = NA
  )
)
