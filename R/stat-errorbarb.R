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
