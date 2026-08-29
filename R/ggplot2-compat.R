# Internal ggplot2 compatibility adapter
#
# Keep panel-scale and coordinate internals behind this boundary so public
# helpers do not depend on ggplot2 implementation details directly.

plot_panel_ranges <- function(plot) {
  build <- ggplot2::ggplot_build(plot)
  list(
    x = build$layout$panel_scales_x[[1]]$range$range,
    y = build$layout$panel_scales_y[[1]]$range$range
  )
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
