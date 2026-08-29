#' Write Multiple Dataframes to Google Sheets or Local Excel File
#'
#' Takes multiple dataframes and writes them to separate sheets in either:
#' 1. A new Google Spreadsheet (default)
#' 2. A local Excel file (when local = TRUE)
#'
#' When using Google Sheets (option 1), you can optionally download
#' the spreadsheet as an Excel file (when download = TRUE)
#'
#' @param .data A list of dataframes to save
#' @param sheet_names A character vector of sheet names (must match length of `.data`)
#' @param name A string specifying the name of the spreadsheet/file to create
#' @param local Logical. Whether to save directly to a local Excel file. Default is FALSE.
#' @param download Logical. Whether to download the Google spreadsheet as an Excel file. Default is FALSE.
#' @param path Optional. Path where the Excel file will be saved.
#'            If NULL (default), saves to working directory with `name`.
#' @param filter Logical. Whether to enable column filters on the header row.
#' @param freeze_first_row Logical. Whether to freeze the first row.
#' @param auto_width Logical. Whether to automatically fit column widths.
#' @param overwrite Logical. Set `FALSE` to protect an existing local xlsx file,
#'   or `TRUE` to replace it. Defaults to `FALSE`.
#'
#' @return A tibble with:
#' \itemize{
#'   \item spreadsheet_id: The ID of the created Google spreadsheet (if local=FALSE)
#'   \item spreadsheet_url: The URL of the created Google spreadsheet (if local=FALSE)
#'   \item file_path: The local path where the Excel file was saved (if local=TRUE or download=TRUE)
#' }
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Create sample dataframes
#' student_data <- tibble(
#'   id = 1:5,
#'   name = c("John", "Emma", "David", "Sarah", "Michael"),
#'   score = c(85, 92, 78, 95, 88)
#' )
#'
#' product_data <- tibble(
#'   product_id = 101:105,
#'   product_name = c("Laptop", "Smartphone", "Tablet", "Earphones", "Mouse"),
#'   price = c(850, 950, 600, 150, 50)
#' )
#'
#' # Basic usage - write to Google Sheets
#' list(student_data, product_data) |>
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data"
#'   )
#'
#' # Write to local Excel file
#' list(student_data, product_data) |>
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data",
#'     local = TRUE
#'   )
#'
#' # Write to Google Sheets and download
#' result <- list(student_data, product_data) |>
#'   write_sheets(
#'     sheet_names = c("Students", "Products"),
#'     name = "Sample_Data",
#'     download = TRUE
#'   )
#'
#' # Access the spreadsheet URL
#' result$spreadsheet_url
#' }
#'
#' @export
write_sheets <- function(
  .data, # List of dataframes to save
  sheet_names, # List of sheet names
  name, # Name of the spreadsheet or file
  local = FALSE, # Whether to save directly to local Excel
  download = FALSE, # Whether to download the Google spreadsheet
  path = NULL, # File path for local save or download
  filter = TRUE, # Whether to add filters to the header row
  freeze_first_row = TRUE, # Whether to freeze the first row
  auto_width = TRUE, # Whether to auto-fit column widths
  overwrite = FALSE # Whether an existing local file may be replaced
) {
  # Validate parameters
  assert_parameters(
    .data,
    sheet_names,
    name,
    local,
    download,
    overwrite = overwrite,
    filter = filter,
    freeze_first_row = freeze_first_row,
    auto_width = auto_width
  )

  # Validate dependencies
  assert_dependencies(local, download)

  # Set file path if not provided
  if (is.null(path)) {
    path <- file.path(getwd(), paste0(name, ".xlsx"))
  }
  if ((local || download) && file.exists(path) && !overwrite) {
    abort_existing_file(path)
  }

  # Choose the appropriate method based on parameters
  if (local) {
    return(write_local_excel(
      .data,
      sheet_names,
      path,
      overwrite = overwrite,
      filter = filter,
      freeze_first_row = freeze_first_row,
      auto_width = auto_width
    ))
  } else {
    result <- write_google_sheets(
      .data,
      sheet_names,
      name,
      filter = filter,
      freeze_first_row = freeze_first_row,
      auto_width = auto_width
    )

    # Download if requested
    if (download) {
      file_path <- download_google_sheet(
        result$spreadsheet_id[[1]],
        path,
        overwrite = overwrite
      )
      if (!is.null(file_path)) {
        result$file_path <- file_path
      }
    }

    return(result)
  }
}
