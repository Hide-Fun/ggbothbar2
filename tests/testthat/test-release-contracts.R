test_that("1.2.0 declares its runtime and optional Google dependencies", {
  description <- utils::packageDescription("ggbothbar")

  expect_identical(description$Version, "1.2.0")
  expect_match(description$Depends, "R \\(>= 4\\.1\\.0\\)")
  expect_false(grepl("googlesheets4|googledrive", description$Imports))
  expect_match(description$Suggests, "googlesheets4")
  expect_match(description$Suggests, "googledrive")

  namespace_imports <- names(getNamespaceImports("ggbothbar"))
  expect_false("googlesheets4" %in% namespace_imports)
  expect_false("googledrive" %in% namespace_imports)
})

test_that("rendering backends are internal while public aliases remain", {
  exports <- getNamespaceExports("ggbothbar")
  removed_backends <- c(
    "Geomerrorbarb",
    "errorbarbGrob",
    "create_errorbarb",
    "makeContent.errorbarb"
  )

  expect_false(any(removed_backends %in% exports))
  expect_true(all(
    c(
      "fix_limit",
      "fix_aspect_ratio",
      "theme_aca",
      "theme_isotope"
    ) %in%
      exports
  ))

  namespace <- asNamespace("ggbothbar")
  expect_true(exists(
    "makeContent.errorbarb",
    envir = namespace,
    inherits = FALSE
  ))
  expect_true(is.function(utils::getS3method(
    "makeContent",
    "errorbarb",
    optional = TRUE,
    envir = namespace
  )))
})
