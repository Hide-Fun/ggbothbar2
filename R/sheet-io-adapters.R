#' Write dataframes to a local Excel file
#'
#' @param data_list List of dataframes
#' @param sheet_names Vector of sheet names
#' @param file_path Path to save the Excel file
#' @param overwrite Whether an existing file may be replaced
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return A tibble with the file path
#' @keywords internal
write_local_excel <- function(
  data_list,
  sheet_names,
  file_path,
  overwrite = FALSE,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  message("Saving directly to local Excel file: ", file_path)

  if (file.exists(file_path) && !overwrite) {
    abort_existing_file(file_path)
  }

  # Create a new workbook
  wb <- openxlsx::createWorkbook()

  # Add sheets and write data
  purrr::walk2(
    data_list,
    sheet_names,
    ~ {
      message("Adding sheet: ", .y)
      openxlsx::addWorksheet(wb, .y)
      openxlsx::writeData(wb, sheet = .y, .x)

      if (filter && ncol(.x) > 0) {
        openxlsx::addFilter(wb, sheet = .y, rows = 1, cols = seq_len(ncol(.x)))
      }

      if (freeze_first_row) {
        openxlsx::freezePane(wb, sheet = .y, firstRow = TRUE)
      }

      if (auto_width && ncol(.x) > 0) {
        openxlsx::setColWidths(
          wb,
          sheet = .y,
          cols = seq_len(ncol(.x)),
          widths = "auto"
        )
      }
    }
  )

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  # Save in the destination directory, then prefer an atomic rename.
  temporary_path <- tempfile(
    pattern = ".ggbothbar-",
    tmpdir = dir_path,
    fileext = ".xlsx"
  )
  on.exit(unlink(temporary_path), add = TRUE)
  openxlsx::saveWorkbook(wb, temporary_path, overwrite = TRUE)

  moved <- file.rename(temporary_path, file_path)
  if (!moved) {
    copied <- file.copy(temporary_path, file_path, overwrite = overwrite)
    if (!copied) {
      rlang::abort(
        paste0("Could not write the workbook to: ", file_path),
        class = "ggbothbar_file_write_error"
      )
    }
    unlink(temporary_path)
  }

  message("Local Excel file saved successfully")
  return(dplyr::tibble(file_path = file_path))
}

#' Write dataframes to Google Sheets
#'
#' @param data_list List of dataframes
#' @param sheet_names Vector of sheet names
#' @param spreadsheet_name Name of the spreadsheet
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return A tibble with spreadsheet ID and URL
#' @keywords internal
write_google_sheets <- function(
  data_list,
  sheet_names,
  spreadsheet_name,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  # Check authentication
  if (!googlesheets4::gs4_has_token()) {
    message("Authenticating with Google Sheets...")
    googlesheets4::gs4_auth()
  }

  # Create a new Google spreadsheet
  message("Creating new spreadsheet: ", spreadsheet_name)
  ss <- googlesheets4::gs4_create(spreadsheet_name)

  # Get spreadsheet URL
  ss_id <- as.character(ss)
  ss_url <- paste0("https://docs.google.com/spreadsheets/d/", ss_id)
  message("Spreadsheet URL: ", ss_url)

  # Get current sheet names and rename the first sheet
  current_sheets <- googlesheets4::sheet_names(ss)
  googlesheets4::sheet_rename(
    ss,
    sheet = current_sheets[1],
    new_name = sheet_names[1]
  )

  # Write the first dataframe to the first sheet
  message("Writing data to sheet: ", sheet_names[1])
  googlesheets4::sheet_write(data_list[[1]], ss, sheet = sheet_names[1])
  apply_google_sheet_format(
    ss,
    sheet_names[1],
    data_list[[1]],
    filter = filter,
    freeze_first_row = freeze_first_row,
    auto_width = auto_width
  )

  # Add remaining sheets and write dataframes
  if (length(data_list) > 1) {
    purrr::walk2(
      data_list[-1],
      sheet_names[-1],
      ~ {
        message("Adding sheet: ", .y)
        googlesheets4::sheet_add(ss, .y)

        message("Writing data to sheet: ", .y)
        googlesheets4::sheet_write(.x, ss, sheet = .y)
        apply_google_sheet_format(
          ss,
          .y,
          .x,
          filter = filter,
          freeze_first_row = freeze_first_row,
          auto_width = auto_width
        )
      }
    )
  }

  message("Google Sheets spreadsheet created successfully")
  return(dplyr::tibble(
    spreadsheet_id = ss_id,
    spreadsheet_url = ss_url
  ))
}

#' Apply spreadsheet formatting to a Google Sheet worksheet
#'
#' @param ss Google Sheet identifier
#' @param sheet_name Worksheet name
#' @param data Dataframe written to the worksheet
#' @param filter Logical. Whether to enable column filters on the header row
#' @param freeze_first_row Logical. Whether to freeze the first row
#' @param auto_width Logical. Whether to automatically fit column widths
#' @return The input Google Sheet identifier, invisibly
#' @keywords internal
apply_google_sheet_format <- function(
  ss,
  sheet_name,
  data,
  filter = TRUE,
  freeze_first_row = TRUE,
  auto_width = TRUE
) {
  if (ncol(data) == 0) {
    return(invisible(ss))
  }

  ss_id <- googlesheets4::as_sheets_id(ss)
  ss_meta <- googlesheets4::gs4_get(ss_id)
  sheet_info <- ss_meta$sheets[
    ss_meta$sheets$name == sheet_name,
    ,
    drop = FALSE
  ]

  if (nrow(sheet_info) != 1) {
    stop(
      "Could not identify Google Sheet worksheet: ",
      sheet_name,
      call. = FALSE
    )
  }

  sheet_id <- sheet_info$id[[1]]
  requests <- list()

  if (filter) {
    requests <- append(
      requests,
      list(list(
        setBasicFilter = list(
          filter = list(
            range = list(
              sheetId = sheet_id,
              startRowIndex = 0,
              endRowIndex = nrow(data) + 1,
              startColumnIndex = 0,
              endColumnIndex = ncol(data)
            )
          )
        )
      ))
    )
  }

  if (freeze_first_row) {
    requests <- append(
      requests,
      list(list(
        updateSheetProperties = list(
          properties = list(
            sheetId = sheet_id,
            gridProperties = list(frozenRowCount = 1)
          ),
          fields = "gridProperties.frozenRowCount"
        )
      ))
    )
  }

  if (length(requests) > 0) {
    req <- googlesheets4::request_generate(
      "sheets.spreadsheets.batchUpdate",
      params = list(spreadsheetId = ss_id, requests = requests)
    )
    googlesheets4::request_make(req)
  }

  if (auto_width) {
    googlesheets4::range_autofit(
      ss_id,
      sheet = sheet_name,
      dimension = "columns"
    )
  }

  invisible(ss_id)
}

#' Download a Google Spreadsheet as Excel
#'
#' @param spreadsheet_id ID of the spreadsheet to download
#' @param file_path Path to save the Excel file
#' @param overwrite Whether an existing file may be replaced
#' @return The file path if successful, NULL otherwise
#' @keywords internal
download_google_sheet <- function(
  spreadsheet_id,
  file_path,
  overwrite = FALSE
) {
  if (file.exists(file_path) && !overwrite) {
    abort_existing_file(file_path)
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path) && dir_path != ".") {
    dir.create(dir_path, recursive = TRUE)
  }

  if (!googledrive::drive_has_token()) {
    message("Authenticating with Google Drive...")
    googledrive::drive_auth()
  }

  # Download the file
  message("Downloading spreadsheet to: ", file_path)
  googledrive::drive_download(
    file = googledrive::as_id(spreadsheet_id),
    path = file_path,
    type = "xlsx",
    overwrite = overwrite
  )

  message("Download completed successfully")
  return(file_path)
}
