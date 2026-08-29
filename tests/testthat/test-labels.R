test_that("label_isotope preserves expression labels", {
  expect_identical(
    label_isotope(13, "C"),
    expression(paste("δ"^13, "C", " (", "‰", ")"))
  )
})

test_that("label_isotope produces marquee-compatible markdown", {
  expect_identical(
    label_isotope(
      15,
      "N",
      notation = "epsilon",
      italic_iso_symbol = TRUE,
      is_markdown = TRUE
    ),
    "*ε*{.sup 15}N (‰)"
  )
})

test_that("label_isotope validates scalar inputs", {
  expect_error(
    label_isotope(c(13, 15), "C"),
    "mass_number must be a single numeric value",
    fixed = TRUE
  )
  expect_error(
    label_isotope(13, ""),
    "element must be a non-empty single character string",
    fixed = TRUE
  )
  expect_error(
    label_isotope(13, "C", notation = "invalid"),
    "notation must be either",
    fixed = TRUE
  )
})
