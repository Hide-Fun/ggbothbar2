#' Assert that required packages are available
#'
#' @param local Whether local Excel saving is requested
#' @param download Whether downloading is requested
#' @keywords internal
assert_dependencies <- function(local, download) {
  required_pkgs <- required_sheet_packages(local, download)

  missing_packages <- required_pkgs[
    !vapply(
      required_pkgs,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

  if (length(missing_packages) > 0L) {
    abort_missing_dependencies(missing_packages)
  }
}

required_sheet_packages <- function(local, download) {
  if (local) {
    return("openxlsx")
  }

  packages <- "googlesheets4"
  if (download) {
    packages <- c(packages, "googledrive")
  }
  packages
}

abort_missing_dependencies <- function(packages) {
  package_names <- paste(packages, collapse = ", ")
  package_literals <- paste0('"', packages, '"', collapse = ", ")
  install_command <- paste0(
    "install.packages(c(",
    package_literals,
    "))"
  )

  rlang::abort(
    c(
      "Required optional packages are not installed.",
      i = paste0("Missing packages: ", package_names, "."),
      i = paste0("Install them with `", install_command, "`.")
    ),
    class = "ggbothbar_missing_dependency",
    missing_packages = packages
  )
}

#' Validate function parameters
#'
#' @param .data List of dataframes
#' @param sheet_names Vector of sheet names
#' @param name Spreadsheet/file name
#' @param local Local Excel option
#' @param download Download option
#' @param overwrite Overwrite option; a single logical value
#' @param filter Filter option
#' @param freeze_first_row Freeze first row option
#' @param auto_width Auto-width option
#' @keywords internal
assert_parameters <- function(
  .data,
  sheet_names,
  name,
  local,
  download,
  overwrite,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  # Check data input
  if (!is.list(.data)) {
    stop("'.data' must be a list of dataframes", call. = FALSE)
  }

  if (length(.data) < 1L) {
    stop("'.data' must contain at least one dataframe", call. = FALSE)
  }

  # Check each element is a data frame
  non_df <- purrr::map_lgl(.data, ~ !is.data.frame(.x))
  if (any(non_df)) {
    stop("All elements in '.data' must be dataframes", call. = FALSE)
  }

  # Check sheet names
  if (!is.character(sheet_names)) {
    stop("'sheet_names' must be a character vector", call. = FALSE)
  }

  if (anyNA(sheet_names) || any(!nzchar(sheet_names))) {
    stop(
      "'sheet_names' must not contain missing or empty values",
      call. = FALSE
    )
  }

  if (length(sheet_names) != length(.data)) {
    stop(
      "'sheet_names' must have the same length as '.data' (",
      length(.data),
      " vs ",
      length(sheet_names),
      ")",
      call. = FALSE
    )
  }

  if (anyDuplicated(tolower(sheet_names))) {
    stop("'sheet_names' must be unique, ignoring case", call. = FALSE)
  }

  too_long <- nchar(sheet_names) > 31L
  if (any(too_long)) {
    stop(
      "'sheet_names' must be 31 characters or fewer for Excel compatibility: ",
      paste(sheet_names[too_long], collapse = ", "),
      call. = FALSE
    )
  }

  invalid_chars <- c(":", "\\", "/", "?", "*", "[", "]")
  invalid_names <- vapply(
    strsplit(sheet_names, "", fixed = TRUE),
    function(chars) any(chars %in% invalid_chars),
    logical(1)
  )
  if (any(invalid_names)) {
    stop(
      "'sheet_names' must not contain Excel-invalid characters (: \\ / ? * [ ]): ",
      paste(sheet_names[invalid_names], collapse = ", "),
      call. = FALSE
    )
  }

  # Check name
  if (!is.character(name) || length(name) != 1) {
    stop("'name' must be a single string", call. = FALSE)
  }

  if (nchar(name) == 0) {
    stop("'name' cannot be an empty string", call. = FALSE)
  }

  # Check boolean parameters
  if (!is.logical(local) || length(local) != 1 || is.na(local)) {
    stop("'local' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(download) || length(download) != 1 || is.na(download)) {
    stop("'download' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    rlang::abort(
      "'overwrite' must be a single logical value (TRUE or FALSE)",
      class = "ggbothbar_input_error"
    )
  }

  if (!is.logical(filter) || length(filter) != 1 || is.na(filter)) {
    stop("'filter' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  if (
    !is.logical(freeze_first_row) ||
      length(freeze_first_row) != 1 ||
      is.na(freeze_first_row)
  ) {
    stop(
      "'freeze_first_row' must be a logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  if (!is.logical(auto_width) || length(auto_width) != 1 || is.na(auto_width)) {
    stop("'auto_width' must be a logical value (TRUE or FALSE)", call. = FALSE)
  }

  # Special case handling
  if (local && download) {
    warning(
      "Both 'local' and 'download' are TRUE. ",
      "Will save locally without using Google Sheets.",
      call. = FALSE
    )
  }
}

abort_existing_file <- function(file_path) {
  rlang::abort(
    paste0(
      "The destination already exists: ",
      file_path,
      ". Set `overwrite = TRUE` to replace it."
    ),
    class = "ggbothbar_file_exists"
  )
}
