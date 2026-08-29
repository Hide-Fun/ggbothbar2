#' Create formatted axis labels for isotope data
#'
#' @param mass_number Numeric. The isotope number (e.g., 13 for carbon-13)
#' @param element Character. The element symbol (e.g., "C" for carbon)
#' @param notation Character. Either "delta" or "epsilon" (default: "delta")
#' @param units Character. The units to display (default: "\\u2030")
#' @param italic_iso_symbol Logical. Should the delta/epsilon symbol be set in italics? (default: FALSE)
#' @param is_markdown Logical. If TRUE, output in markdown format compatible with the marquee package
#'        (uses `{.sup ...}` for superscript). If FALSE (default), output as expression for ggplot2.
#'
#' @return If is_markdown = FALSE, returns an expression object for ggplot2 axis labels.
#'         If is_markdown = TRUE, returns a character string using marquee-style markdown,
#'         e.g., `\\u03b4{.sup13}C (\\u2030)` or `*\\u03b5*{.sup15}N (\\u2030)` depending on italic_iso_symbol.
#' @export
#'
#' @examples
#' # For delta 13C (expression)
#' label_isotope(13, "C")
#' # For epsilon 15N (markdown, symbol italic)
#' label_isotope(15, "N", notation = "epsilon", italic_iso_symbol = TRUE, is_markdown = TRUE)
label_isotope <- function(
  mass_number,
  element,
  notation = "delta",
  units = "\u2030",
  italic_iso_symbol = FALSE,
  is_markdown = FALSE
) {
  # Validate inputs
  if (!is.numeric(mass_number) || length(mass_number) != 1) {
    stop("mass_number must be a single numeric value")
  }
  if (!is.character(element) || length(element) != 1 || nchar(element) == 0) {
    stop("element must be a non-empty single character string")
  }
  if (!(notation %in% c("delta", "epsilon"))) {
    stop('notation must be either "delta" or "epsilon"')
  }
  if (!is.logical(italic_iso_symbol) || length(italic_iso_symbol) != 1) {
    stop("italic_iso_symbol must be a single logical value")
  }
  if (!is.logical(is_markdown) || length(is_markdown) != 1) {
    stop("is_markdown must be a single logical value")
  }

  # Define the notation symbol
  symbol <- switch(notation, "delta" = "\u03b4", "epsilon" = "\u03b5")

  if (is_markdown) {
    # Markdown output using marquee style custom span for superscript
    # Format: optionally italic symbol, then symbol{.sup mass_number}element (units)
    symbol_md <- if (italic_iso_symbol) {
      paste0("*", symbol, "*")
    } else {
      symbol
    }
    label_md <- paste0(
      symbol_md,
      "{.sup ",
      mass_number,
      "}",
      element,
      " (",
      units,
      ")"
    )
    return(label_md)
  } else {
    # Expression output for ggplot2
    symbol_expr <- if (italic_iso_symbol) {
      bquote(italic(.(symbol)))
    } else {
      bquote(.(symbol))
    }
    result <- bquote(
      expression(
        paste(
          .(symbol_expr)^.(mass_number),
          .(element),
          " (",
          .(units),
          ")"
        )
      )
    )
    return(eval(result))
  }
}
