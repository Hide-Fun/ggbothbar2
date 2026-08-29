#' Calculate Standard Error
#'
#' This function calculates the standard error of a numeric vector.
#'
#' @param x A numeric vector
#' @param na.rm A logical value indicating whether NA values should be stripped before the computation proceeds. Defaults to FALSE
#' @return The standard error of the input vector
#' @examples
#' se(c(1, 2, 3, 4, 5))
#' se(c(1, 2, 3, 4, 5, NA), na.rm = TRUE)
#' @export
se <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- stats::na.omit(x)
  }
  stats::sd(x) / sqrt(length(x))
}


#' Calculate Error for Errorbars
#'
#' This function calculates an error value (standard deviation, standard error,
#' 95% confidence interval, or a user-supplied function) for a numeric vector.
#'
#' @param x A numeric vector
#' @param fun.errorbar Either a character string specifying the method to
#'   calculate the error ("sd", "se", or "ci"), or a function that accepts
#'   a numeric vector and returns a single numeric value.
#' @param na.rm A logical value indicating whether \code{NA} values should be
#'   removed before computation. Defaults to \code{FALSE}.
#' @return The calculated error value of the input vector
#' @examples
#' calc_error(c(1, 2, 3, 4, 5))
#' calc_error(c(1, 2, 3, 4, 5, NA), fun.errorbar = "se", na.rm = TRUE)
#' calc_error(c(1, 2, 3, 4, 5), fun.errorbar = "ci")
#' calc_error(c(1, 2, 3, 4, 5), fun.errorbar = function(x) max(x) - min(x))
#' @export
calc_error <- function(x, fun.errorbar = "sd", na.rm = FALSE) {
  if (!is.numeric(x)) {
    rlang::abort("`x` must be numeric.", class = "ggbothbar_input_error")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    rlang::abort(
      "`na.rm` must be a single non-missing logical value.",
      class = "ggbothbar_input_error"
    )
  }

  if (is.character(fun.errorbar)) {
    if (length(fun.errorbar) != 1L || is.na(fun.errorbar)) {
      rlang::abort(
        "`fun.errorbar` must be one of \"sd\", \"se\", or \"ci\".",
        class = "ggbothbar_input_error"
      )
    }
    if (fun.errorbar == "sd") {
      result <- stats::sd(x, na.rm = na.rm)
    } else if (fun.errorbar == "se") {
      result <- se(x, na.rm = na.rm)
    } else if (fun.errorbar == "ci") {
      n <- if (na.rm) sum(!is.na(x)) else length(x)
      if (n <= 1L) {
        result <- NA_real_
      } else {
        result <- se(x, na.rm = na.rm) * stats::qt(0.975, df = n - 1)
      }
    } else {
      rlang::abort(
        paste0("Unsupported `fun.errorbar`: ", fun.errorbar, "."),
        class = "ggbothbar_input_error"
      )
    }
  } else if (is.function(fun.errorbar)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    result <- fun.errorbar(x)
  } else {
    rlang::abort(
      "`fun.errorbar` must be a character string or function.",
      class = "ggbothbar_input_error"
    )
  }

  validate_error_result(result)
}

validate_error_result <- function(result) {
  valid_na <- is.numeric(result) &&
    length(result) == 1L &&
    is.na(result) &&
    !is.nan(result)
  valid_finite <- is.numeric(result) &&
    length(result) == 1L &&
    is.finite(result)
  if (!valid_na && !valid_finite) {
    rlang::abort(
      paste0(
        "`fun.errorbar` must return one finite numeric value ",
        "or `NA_real_`."
      ),
      class = "ggbothbar_input_error"
    )
  }
  as.numeric(result)
}

#' Calculate Isotopic Enrichment
#'
#' Calculate isotopic enrichment factors by subtracting mean reference values
#' from each supplied isotope column.
#'
#' @param data A data frame containing isotopic data
#' @param var Character string specifying the column name that distinguishes between reference and sample groups
#' @param delta Character vector specifying the column names that store \eqn{\delta} values (e.g., d13C, d15N, d34S)
#' @param epsilon Optional character vector specifying the names of the enrichment columns to append; defaults to swapping the leading
#'   \code{"d"} in each entry of \code{delta} for \code{"e"} when \code{delta} matches the pattern \code{^d\\d+[A-Za-z]+$}
#' @param reference Character string specifying the reference group value in the 'var' column
#' @param na.rm Logical; if TRUE, removes NA values when calculating mean reference values
#' @param overwrite Logical; if `TRUE`, existing columns named by `epsilon` may
#'   be replaced. Defaults to `FALSE`.
#'
#' @return A data frame with additional enrichment columns, one for each element of \code{delta}
#'
#' @examples
#' # Example data
#' df <- data.frame(
#'   type = c("reference", "sample", "sample", "reference"),
#'   d13C = c(-20.0, -21.5, -19.0, -20.5),
#'   d15N = c(7.0, 8.2, 6.5, 7.4),
#'   d34S = c(12.0, 11.5, 13.2, 12.4)
#' )
#'
#' # 1) Default output names are inferred as "e13C" and "e15N"
#' out1 <- calc_enrichment(df)
#' head(out1)
#'
#' # 2) User supplied output names
#' out2 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N"),
#'   epsilon = c("e13c", "e15n")
#' )
#' head(out2)
#'
#' # 3) Multiple delta columns with inferred epsilon names ("e13C", "e15N", "e34S")
#' out3 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N", "d34S")
#' )
#' head(out3)
#'
#' # 4) Multiple delta columns with explicit epsilon names
#' out4 <- calc_enrichment(
#'   df,
#'   delta = c("d13C", "d15N", "d34S"),
#'   epsilon = c("E13C_enr", "E15N_enr", "E34S_enr")
#' )
#' head(out4)
#'
#' @export
calc_enrichment <- function(
  data,
  var = "type",
  delta = c("d13C", "d15N"),
  epsilon = NULL,
  reference = "reference",
  na.rm = FALSE,
  overwrite = FALSE
) {
  # -- Validation -------------------------------------------------------------
  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.", class = "ggbothbar_input_error")
  }
  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm)) {
    rlang::abort(
      "`na.rm` must be a single non-missing logical value.",
      class = "ggbothbar_input_error"
    )
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    rlang::abort(
      "`overwrite` must be a single non-missing logical value.",
      class = "ggbothbar_input_error"
    )
  }

  # Check that `var` column exists
  if (!is.character(var) || length(var) != 1L || !(var %in% names(data))) {
    rlang::abort(
      "`var` must be the name of a column in `data`.",
      class = "ggbothbar_input_error"
    )
  }

  # Check that `reference` level exists
  if (!is.character(reference) || length(reference) != 1L) {
    rlang::abort(
      "`reference` must be a single character value.",
      class = "ggbothbar_input_error"
    )
  }
  reference_rows <- !is.na(data[[var]]) & data[[var]] == reference
  if (!any(reference_rows)) {
    rlang::abort(
      "No rows match `reference` in the `var` column.",
      class = "ggbothbar_input_error"
    )
  }

  # Check `delta`
  if (!is.character(delta) || length(delta) < 1L) {
    rlang::abort(
      "`delta` must be a character vector of length >= 1.",
      class = "ggbothbar_input_error"
    )
  }
  if (anyNA(delta) || any(!nzchar(delta)) || anyDuplicated(delta)) {
    rlang::abort(
      "`delta` must contain unique, non-missing column names.",
      class = "ggbothbar_input_error"
    )
  }
  missing_delta <- setdiff(delta, names(data))
  if (length(missing_delta) > 0L) {
    rlang::abort(
      sprintf(
        "These `delta` columns are missing in `data`: %s",
        paste(missing_delta, collapse = ", ")
      ),
      class = "ggbothbar_input_error"
    )
  }
  non_numeric_delta <- delta[!vapply(data[delta], is.numeric, logical(1))]
  if (length(non_numeric_delta) > 0L) {
    rlang::abort(
      sprintf(
        "These `delta` columns must be numeric: %s",
        paste(non_numeric_delta, collapse = ", ")
      ),
      class = "ggbothbar_input_error"
    )
  }

  # Derive or validate `epsilon`
  if (is.null(epsilon)) {
    # When epsilon is NULL, validate delta patterns and derive names
    # Pattern: start with 'd', followed by digits, then one or more letters.
    is_valid <- grepl("^d\\d+[A-Za-z]+$", delta, perl = TRUE)
    if (!all(is_valid)) {
      bad <- delta[!is_valid]
      rlang::abort(
        sprintf(
          "When `epsilon` is NULL, each `delta` must match pattern ^d\\d+[A-Za-z]+$. Invalid: %s",
          paste(bad, collapse = ", ")
        ),
        class = "ggbothbar_input_error"
      )
    }
    epsilon <- sub("^d", "e", delta)
  } else {
    if (!is.character(epsilon) || length(epsilon) != length(delta)) {
      rlang::abort(
        "`epsilon` must be a character vector the same length as `delta`.",
        class = "ggbothbar_input_error"
      )
    }
  }
  if (anyNA(epsilon) || any(!nzchar(epsilon)) || anyDuplicated(epsilon)) {
    rlang::abort(
      "`epsilon` must contain unique, non-missing output names.",
      class = "ggbothbar_input_error"
    )
  }

  collisions <- intersect(epsilon, names(data))
  if (length(collisions) > 0L && !overwrite) {
    rlang::abort(
      sprintf(
        paste0(
          "These `epsilon` columns already exist: %s. ",
          "Use `overwrite = TRUE` to replace them."
        ),
        paste(collisions, collapse = ", ")
      ),
      class = "ggbothbar_name_collision"
    )
  }

  # -- Computation ------------------------------------------------------------
  reference_data <- data[reference_rows, , drop = FALSE]

  # Compute reference means for each delta
  ref_means <- vapply(
    delta,
    function(col) mean(reference_data[[col]], na.rm = na.rm),
    numeric(1)
  )
  invalid_means <- !is.finite(ref_means)
  if (any(invalid_means)) {
    rlang::warn(
      c(
        paste0(
          "Reference means are not finite for: ",
          paste(delta[invalid_means], collapse = ", "),
          "."
        ),
        i = paste0(
          "Affected enrichment columns will be filled with `NA`: ",
          paste(epsilon[invalid_means], collapse = ", "),
          "."
        )
      ),
      class = "ggbothbar_reference_mean_warning"
    )
  }

  # Prepare result and append enrichment columns
  result <- data
  for (i in seq_along(delta)) {
    # Enrichment = sample value - reference mean
    result[[epsilon[i]]] <- if (invalid_means[i]) {
      rep(NA_real_, nrow(data))
    } else {
      data[[delta[i]]] - ref_means[i]
    }
  }

  return(result)
}
