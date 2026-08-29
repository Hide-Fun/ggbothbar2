#' Align axis scales across ggplot objects
#'
#' `align_axis_scales()` gives multiple plots common visible ranges and break
#' positions while retaining each plot's supported scale transformation and
#' coordinate semantics. Continuous log scales and [ggplot2::coord_flip()] are
#' preserved.
#'
#' Free-scale facets and unsupported coordinate systems use a documented
#' best-effort fallback. The function emits a structured
#' `ggbothbar_axis_fallback_warning` before returning those plots because a
#' fallback may change facet or coordinate meaning.
#'
#' @param plots A list containing at least two ggplot objects.
#' @param axes Axes to align: `"x"`, `"y"`, or `c("x", "y")`.
#' @param x_break_step,y_break_step Positive finite break spacing in the
#'   original data space.
#' @param x_limits,y_limits Optional visible limits. Supply one finite number or
#'   `c(min, max)` in the original data space.
#' @param aspect_ratio Optional positive finite aspect-ratio multiplier.
#' @param clip Clipping mode passed to the coordinate system: `"on"`, `"off"`,
#'   or `"inherit"`.
#' @param expand_breaks If `TRUE`, extend each upper limit until its span is an
#'   integer multiple of the corresponding break step.
#' @param expand Expansion specification applied to adjusted continuous scales.
#'
#' @return A list of adjusted ggplot objects in input order.
#' @examples
#' library(ggplot2)
#' first <- ggplot(subset(mtcars, cyl == 4), aes(wt, mpg)) + geom_point()
#' second <- ggplot(subset(mtcars, cyl == 6), aes(wt, mpg)) + geom_point()
#'
#' aligned <- align_axis_scales(
#'   list(first, second),
#'   axes = c("x", "y"),
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#' @export
align_axis_scales <- function(
  plots,
  axes = c("x", "y"),
  x_break_step = 3,
  y_break_step = 3,
  x_limits = NULL,
  y_limits = NULL,
  aspect_ratio = NULL,
  clip = "off",
  expand_breaks = FALSE,
  expand = ggplot2::waiver()
) {
  axes <- match.arg(axes, choices = c("x", "y"), several.ok = TRUE)
  validate_axis_options(
    x_break_step = x_break_step,
    y_break_step = y_break_step,
    x_limits = x_limits,
    y_limits = y_limits,
    aspect_ratio = aspect_ratio,
    clip = clip,
    expand_breaks = expand_breaks
  )
  if (!is.list(plots) || length(plots) < 2L) {
    rlang::abort(
      "`plots` must be a list containing at least two ggplot objects.",
      class = "ggbothbar_input_error"
    )
  }
  if (!all(vapply(plots, inherits, logical(1), "gg"))) {
    rlang::abort(
      "Every element of `plots` must be a ggplot object.",
      class = "ggbothbar_input_error"
    )
  }

  builds <- lapply(plots, ggplot2::ggplot_build)
  x_info <- Map(axis_info, plots, builds, MoreArgs = list(axis = "x"))
  y_info <- Map(axis_info, plots, builds, MoreArgs = list(axis = "y"))
  adjust_x <- "x" %in% axes
  adjust_y <- "y" %in% axes

  if (adjust_x && !all(vapply(x_info, `[[`, logical(1), "continuous"))) {
    rlang::abort(
      "All x scales must be continuous when `axes` includes \"x\".",
      class = "ggbothbar_input_error"
    )
  }
  if (adjust_y && !all(vapply(y_info, `[[`, logical(1), "continuous"))) {
    rlang::abort(
      "All y scales must be continuous when `axes` includes \"y\".",
      class = "ggbothbar_input_error"
    )
  }

  for (index in seq_along(plots)) {
    reasons <- axis_fallback_reasons(
      plots[[index]],
      axes = axes,
      aspect_ratio = aspect_ratio
    )
    warn_axis_fallback(reasons, plot_index = index)
  }
  warn_mixed_transformations(x_info, adjust_x, "x")
  warn_mixed_transformations(y_info, adjust_y, "y")

  x_range <- if (adjust_x) {
    compute_axis_range(
      unlist(lapply(x_info, `[[`, "limits"), use.names = FALSE),
      user_limits = x_limits,
      break_step = x_break_step,
      expand_breaks = expand_breaks,
      axis = "x"
    )
  } else {
    NULL
  }
  y_range <- if (adjust_y) {
    compute_axis_range(
      unlist(lapply(y_info, `[[`, "limits"), use.names = FALSE),
      user_limits = y_limits,
      break_step = y_break_step,
      expand_breaks = expand_breaks,
      axis = "y"
    )
  } else {
    NULL
  }

  ratio <- if (is.null(aspect_ratio)) {
    NULL
  } else {
    x_span <- if (adjust_x) {
      diff(x_range$limits)
    } else {
      axis_span(x_info[[1]])
    }
    y_span <- if (adjust_y) {
      diff(y_range$limits)
    } else {
      axis_span(y_info[[1]])
    }
    (x_span / y_span) * aspect_ratio
  }

  Map(
    function(plot, x_axis, y_axis) {
      apply_axis_adjustment(
        plot,
        x_info = x_axis,
        y_info = y_axis,
        x_range = x_range,
        y_range = y_range,
        ratio = ratio,
        clip = clip,
        expand = expand
      )
    },
    plots,
    x_info,
    y_info
  )
}

#' Adjust axis scales on one ggplot object
#'
#' `adjust_axis_scales()` changes visible ranges, break spacing, expansion, and
#' an optional aspect ratio while preserving supported scale transformations
#' and coordinate systems. Continuous log scales and [ggplot2::coord_flip()]
#' are preserved.
#'
#' Free-scale facets and unsupported coordinates return a warned best-effort
#' fallback. The warning has class `ggbothbar_axis_fallback_warning`.
#'
#' @param plot A ggplot object.
#' @inheritParams align_axis_scales
#'
#' @return An adjusted ggplot object.
#' @examples
#' library(ggplot2)
#' plot <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' adjust_axis_scales(
#'   plot,
#'   x_break_step = 0.5,
#'   y_break_step = 5
#' )
#' @export
adjust_axis_scales <- function(
  plot,
  x_break_step = 3,
  y_break_step = 3,
  x_limits = NULL,
  y_limits = NULL,
  aspect_ratio = NULL,
  clip = "off",
  expand_breaks = FALSE,
  expand = ggplot2::waiver()
) {
  if (!inherits(plot, "gg")) {
    rlang::abort("`plot` must be a ggplot object.", class = "ggbothbar_input_error")
  }
  validate_axis_options(
    x_break_step = x_break_step,
    y_break_step = y_break_step,
    x_limits = x_limits,
    y_limits = y_limits,
    aspect_ratio = aspect_ratio,
    clip = clip,
    expand_breaks = expand_breaks
  )

  build <- ggplot2::ggplot_build(plot)
  x_info <- axis_info(plot, build, "x")
  y_info <- axis_info(plot, build, "y")
  adjusted_axes <- character()
  if (x_info$continuous) {
    adjusted_axes <- c(adjusted_axes, "x")
  } else if (!is.null(x_limits)) {
    rlang::warn(
      "The x scale is discrete; `x_limits` will be ignored.",
      class = "ggbothbar_axis_fallback_warning"
    )
  }
  if (y_info$continuous) {
    adjusted_axes <- c(adjusted_axes, "y")
  } else if (!is.null(y_limits)) {
    rlang::warn(
      "The y scale is discrete; `y_limits` will be ignored.",
      class = "ggbothbar_axis_fallback_warning"
    )
  }

  reasons <- axis_fallback_reasons(
    plot,
    axes = adjusted_axes,
    aspect_ratio = aspect_ratio
  )
  warn_axis_fallback(reasons)

  x_range <- if (x_info$continuous) {
    compute_axis_range(
      x_info$limits,
      user_limits = x_limits,
      break_step = x_break_step,
      expand_breaks = expand_breaks,
      axis = "x"
    )
  } else {
    NULL
  }
  y_range <- if (y_info$continuous) {
    compute_axis_range(
      y_info$limits,
      user_limits = y_limits,
      break_step = y_break_step,
      expand_breaks = expand_breaks,
      axis = "y"
    )
  } else {
    NULL
  }

  ratio <- if (is.null(aspect_ratio)) {
    NULL
  } else {
    x_span <- if (is.null(x_range)) axis_span(x_info) else diff(x_range$limits)
    y_span <- if (is.null(y_range)) axis_span(y_info) else diff(y_range$limits)
    (x_span / y_span) * aspect_ratio
  }

  apply_axis_adjustment(
    plot,
    x_info = x_info,
    y_info = y_info,
    x_range = x_range,
    y_range = y_range,
    ratio = ratio,
    clip = clip,
    expand = expand
  )
}

validate_axis_options <- function(
  x_break_step,
  y_break_step,
  x_limits,
  y_limits,
  aspect_ratio,
  clip,
  expand_breaks
) {
  validate_break_step(x_break_step, "x_break_step")
  validate_break_step(y_break_step, "y_break_step")
  validate_user_limits(x_limits, "x_limits")
  validate_user_limits(y_limits, "y_limits")
  if (
    !is.null(aspect_ratio) &&
      (!is.numeric(aspect_ratio) ||
        length(aspect_ratio) != 1L ||
        !is.finite(aspect_ratio) ||
        aspect_ratio <= 0)
  ) {
    rlang::abort(
      "`aspect_ratio` must be NULL or one positive finite number.",
      class = "ggbothbar_input_error"
    )
  }
  if (
    !is.character(clip) ||
      length(clip) != 1L ||
      is.na(clip) ||
      !clip %in% c("on", "off", "inherit")
  ) {
    rlang::abort(
      "`clip` must be one of \"on\", \"off\", or \"inherit\".",
      class = "ggbothbar_input_error"
    )
  }
  if (
    !is.logical(expand_breaks) ||
      length(expand_breaks) != 1L ||
      is.na(expand_breaks)
  ) {
    rlang::abort(
      "`expand_breaks` must be a single non-missing logical value.",
      class = "ggbothbar_input_error"
    )
  }
}

validate_break_step <- function(value, argument) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value <= 0
  ) {
    rlang::abort(
      paste0("`", argument, "` must be one positive finite number."),
      class = "ggbothbar_input_error"
    )
  }
}

validate_user_limits <- function(value, argument) {
  if (is.null(value)) {
    return(invisible(NULL))
  }
  if (
    !is.numeric(value) ||
      !length(value) %in% c(1L, 2L) ||
      any(!is.finite(value))
  ) {
    rlang::abort(
      paste0("`", argument, "` must contain one or two finite numbers."),
      class = "ggbothbar_input_error"
    )
  }
  if (length(value) == 2L && value[[1]] >= value[[2]]) {
    rlang::abort(
      paste0("`", argument, "` must be ordered as c(min, max)."),
      class = "ggbothbar_input_error"
    )
  }
}

axis_info <- function(plot, build, axis) {
  transformed_limits <- get_lim(build, axis)
  scale <- plot$scales$get_scales(axis)
  trained_scales <- if (axis == "x") {
    build$layout$panel_scales_x
  } else {
    build$layout$panel_scales_y
  }
  trained_scale <- trained_scales[[1]]
  transform <- trained_scale$trans

  if (is.numeric(transformed_limits)) {
    limits <- if (is.null(transform) || is.null(transform$inverse)) {
      transformed_limits
    } else {
      transform$inverse(transformed_limits)
    }
    continuous <- is.numeric(limits)
  } else {
    limits <- transformed_limits
    continuous <- FALSE
  }

  list(
    axis = axis,
    limits = limits,
    continuous = continuous,
    scale = scale,
    trained_scale = trained_scale,
    transform_name = if (is.null(transform$name)) "identity" else transform$name
  )
}

compute_axis_range <- function(
  values,
  user_limits,
  break_step,
  expand_breaks,
  axis
) {
  values <- values[is.finite(values)]
  if (length(values) == 0L) {
    rlang::abort(
      paste0("The ", axis, " scale has no finite values to align."),
      class = "ggbothbar_input_error"
    )
  }

  lower <- floor(min(values))
  upper <- ceiling(max(values))
  limits <- merge_lim(lower, upper, user_limits)
  lower <- limits[[1]]
  upper <- limits[[2]]
  if (upper == lower) {
    upper <- lower + break_step
  }

  if (expand_breaks) {
    remainder <- (upper - lower) %% break_step
    if (!isTRUE(all.equal(remainder, 0))) {
      upper <- upper + break_step - remainder
    }
  }

  list(
    limits = c(lower, upper),
    breaks = seq(lower, upper, by = break_step)
  )
}

apply_axis_adjustment <- function(
  plot,
  x_info,
  y_info,
  x_range,
  y_range,
  ratio,
  clip,
  expand
) {
  adjusted <- plot
  if (!is.null(x_range)) {
    adjusted <- adjusted + clone_axis_scale(
      x_info,
      breaks = x_range$breaks,
      expand = expand
    )
  }
  if (!is.null(y_range)) {
    adjusted <- adjusted + clone_axis_scale(
      y_info,
      breaks = y_range$breaks,
      expand = expand
    )
  }

  adjusted$coordinates <- adjusted_coordinate(
    plot$coordinates,
    x_limits = if (is.null(x_range)) NULL else x_range$limits,
    y_limits = if (is.null(y_range)) NULL else y_range$limits,
    ratio = ratio,
    clip = clip
  )
  adjusted
}

clone_axis_scale <- function(info, breaks, expand) {
  if (is.null(info$scale)) {
    if (info$axis == "x") {
      return(ggplot2::scale_x_continuous(breaks = breaks, expand = expand))
    }
    return(ggplot2::scale_y_continuous(breaks = breaks, expand = expand))
  }

  scale <- info$scale$clone()
  scale$breaks <- breaks
  if (!inherits(expand, "waiver")) {
    scale$expand <- expand
  }
  scale
}

adjusted_coordinate <- function(coord, x_limits, y_limits, ratio, clip) {
  supported <- inherits(coord, "CoordCartesian") ||
    inherits(coord, "CoordFixed") ||
    inherits(coord, "CoordFlip")

  if (!supported) {
    args <- list(xlim = x_limits, ylim = y_limits, clip = clip)
    if (is.null(ratio)) {
      return(do.call(ggplot2::coord_cartesian, args))
    }
    return(do.call(ggplot2::coord_fixed, c(args, list(ratio = ratio))))
  }

  if (!is.null(ratio) && !inherits(coord, "CoordFlip")) {
    if (inherits(coord, "CoordFixed")) {
      result <- ggplot2::ggproto(NULL, coord)
      result$ratio <- ratio
    } else {
      result <- ggplot2::coord_fixed(
        ratio = ratio,
        xlim = x_limits,
        ylim = y_limits,
        expand = coord$expand,
        clip = clip
      )
      return(result)
    }
  } else {
    result <- ggplot2::ggproto(NULL, coord)
  }

  if (is.null(result$limits)) {
    result$limits <- list(x = NULL, y = NULL)
  }
  if (!is.null(x_limits)) {
    result$limits$x <- x_limits
  }
  if (!is.null(y_limits)) {
    result$limits$y <- y_limits
  }
  result$clip <- clip
  result
}

axis_fallback_reasons <- function(plot, axes, aspect_ratio) {
  reasons <- character()
  free <- plot$facet$params$free
  if (!is.null(free)) {
    if ("x" %in% axes && isTRUE(free$x)) {
      reasons <- c(
        reasons,
        "the x facet scale is free and will receive a shared best-effort range"
      )
    }
    if ("y" %in% axes && isTRUE(free$y)) {
      reasons <- c(
        reasons,
        "the y facet scale is free and will receive a shared best-effort range"
      )
    }
  }

  coord <- plot$coordinates
  supported <- inherits(coord, "CoordCartesian") ||
    inherits(coord, "CoordFixed") ||
    inherits(coord, "CoordFlip")
  if (!supported) {
    reasons <- c(
      reasons,
      paste0(
        "coordinate class ",
        class(coord)[[1]],
        " is unsupported and will fall back to Cartesian coordinates"
      )
    )
  }
  if (!is.null(aspect_ratio) && inherits(coord, "CoordFlip")) {
    reasons <- c(
      reasons,
      "`aspect_ratio` cannot be imposed without replacing `coord_flip()` and will be ignored"
    )
  }
  reasons
}

warn_axis_fallback <- function(reasons, plot_index = NULL) {
  if (length(reasons) == 0L) {
    return(invisible(NULL))
  }
  target <- if (is.null(plot_index)) "The plot" else paste0("Plot ", plot_index)
  details <- stats::setNames(reasons, rep("i", length(reasons)))
  rlang::warn(
    c(
      paste0(target, " requires a best-effort axis fallback."),
      details
    ),
    class = "ggbothbar_axis_fallback_warning"
  )
}

warn_mixed_transformations <- function(info, adjusted, axis) {
  if (!adjusted) {
    return(invisible(NULL))
  }
  transformations <- unique(vapply(info, `[[`, character(1), "transform_name"))
  if (length(transformations) > 1L) {
    rlang::warn(
      c(
        paste0("The ", axis, " scales use different transformations."),
        i = "Each transformation is preserved, so visual spacing may not align."
      ),
      class = "ggbothbar_axis_fallback_warning"
    )
  }
}

axis_span <- function(info) {
  if (info$continuous) {
    span <- diff(range(info$limits, na.rm = TRUE))
  } else {
    span <- length(unique(info$limits)) - 1
  }
  if (!is.finite(span) || span <= 0) 1 else span
}

#' @keywords internal
merge_lim <- function(auto_min, auto_max, user_lim) {
  if (is.null(user_lim)) {
    return(c(auto_min, auto_max))
  }
  if (length(user_lim) == 2L) {
    return(user_lim)
  }
  if (user_lim <= auto_min) {
    return(c(user_lim, auto_max))
  }
  if (user_lim >= auto_max) {
    return(c(auto_min, user_lim))
  }
  c(user_lim, auto_max)
}

#' @keywords internal
get_lim <- function(build, axis) {
  panel_scales <- if (axis == "x") {
    build$layout$panel_scales_x
  } else {
    build$layout$panel_scales_y
  }
  unlist(
    lapply(
      panel_scales,
      function(scale) scale$get_limits()
    ),
    use.names = FALSE
  )
}
