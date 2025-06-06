% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{calc_enrichment}
\alias{calc_enrichment}
\title{Calculate Isotopic Enrichment}
\usage{
calc_enrichment(
  data,
  var = "type",
  delta = c("d13C", "d15N"),
  reference = "reference",
  na.rm = FALSE
)
}
\arguments{
\item{data}{A data frame containing isotopic data}

\item{var}{Character string specifying the column name that distinguishes between reference and sample groups}

\item{delta}{Character vector of length 2 specifying the column names for d13C and d15N values}

\item{reference}{Character string specifying the reference group value in the 'var' column}

\item{na.rm}{Logical; if TRUE, removes NA values when calculating mean reference values}
}
\value{
A data frame with additional columns:
\item{e13c}{Enrichment factor for carbon isotopes (d13C sample - mean d13C reference)}
\item{e15n}{Enrichment factor for nitrogen isotopes (d15N sample - mean d15N reference)}
}
\description{
This function calculates the isotopic enrichment factors (e13c and e15n) by comparing
sample values to a reference group.
}
\examples{
# Create example data
df <- data.frame(
  type = c("reference", "reference", "sample", "sample"),
  d13c = c(-20, -21, -18, -19),
  d15n = c(5, 6, 8, 9)
)

# Calculate enrichment
enriched_data <- calc_enrichment(df, delta = c("d13c", "d15n"))

}
