sheet_fixture <- function() {
  list(data.frame(value = 1:2))
}

test_that("write_sheets forwards explicit local overwrite policy", {
  local_mocked_bindings(
    assert_dependencies = function(local, download) NULL,
    write_local_excel = function(..., overwrite) overwrite,
    .package = "ggbothbar"
  )

  result <- write_sheets(
    sheet_fixture(),
    sheet_names = "data",
    name = "example",
    local = TRUE,
    overwrite = FALSE
  )

  expect_false(result)
})

test_that("write_sheets warns while overwrite remains implicit", {
  local_mocked_bindings(
    assert_dependencies = function(local, download) NULL,
    write_local_excel = function(..., overwrite) overwrite,
    .package = "ggbothbar"
  )

  expect_warning(
    result <- write_sheets(
      sheet_fixture(),
      sheet_names = "data",
      name = "example",
      local = TRUE
    ),
    class = "ggbothbar_overwrite_warning"
  )

  expect_true(result)
})

test_that("local xlsx output refuses an existing file when requested", {
  skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  writeLines("existing content", path)
  original <- readBin(path, what = "raw", n = file.info(path)$size)

  expect_error(
    write_sheets(
      sheet_fixture(),
      sheet_names = "data",
      name = "example",
      local = TRUE,
      path = path,
      overwrite = FALSE
    ),
    class = "ggbothbar_file_exists"
  )

  expect_identical(
    readBin(path, what = "raw", n = file.info(path)$size),
    original
  )
})

test_that("explicit overwrite replaces a local workbook without temp debris", {
  skip_if_not_installed("openxlsx")
  directory <- tempfile("ggbothbar-write-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  path <- file.path(directory, "result.xlsx")
  writeLines("existing content", path)

  result <- write_sheets(
    sheet_fixture(),
    sheet_names = "data",
    name = "example",
    local = TRUE,
    path = path,
    overwrite = TRUE
  )

  expect_equal(openxlsx::read.xlsx(path)$value, 1:2)
  expect_identical(result$file_path[[1]], path)
  expect_identical(list.files(directory), "result.xlsx")
})

test_that("download overwrite policy is forwarded without Google writes", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    assert_dependencies = function(local, download) NULL,
    write_google_sheets = function(...) {
      data.frame(
        spreadsheet_id = "sheet-id",
        spreadsheet_url = "https://example.invalid/sheet-id"
      )
    },
    download_google_sheet = function(spreadsheet_id, file_path, overwrite) {
      observed$spreadsheet_id <- spreadsheet_id
      observed$file_path <- file_path
      observed$overwrite <- overwrite
      file_path
    },
    .package = "ggbothbar"
  )
  path <- tempfile(fileext = ".xlsx")

  result <- write_sheets(
    sheet_fixture(),
    sheet_names = "data",
    name = "example",
    download = TRUE,
    path = path,
    overwrite = FALSE
  )

  expect_identical(observed$spreadsheet_id, "sheet-id")
  expect_identical(observed$file_path, path)
  expect_false(observed$overwrite)
  expect_identical(result$file_path[[1]], path)
  expect_false(file.exists(path))
})

test_that("download collision fails before creating a Google spreadsheet", {
  observed <- new.env(parent = emptyenv())
  observed$created <- FALSE
  local_mocked_bindings(
    assert_dependencies = function(local, download) NULL,
    write_google_sheets = function(...) {
      observed$created <- TRUE
      data.frame(
        spreadsheet_id = "sheet-id",
        spreadsheet_url = "https://example.invalid/sheet-id"
      )
    },
    .package = "ggbothbar"
  )
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  writeLines("existing content", path)

  expect_error(
    write_sheets(
      sheet_fixture(),
      sheet_names = "data",
      name = "example",
      download = TRUE,
      path = path,
      overwrite = FALSE
    ),
    class = "ggbothbar_file_exists"
  )

  expect_false(observed$created)
})
