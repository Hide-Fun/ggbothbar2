% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_isotope.R
\name{theme_isotope}
\alias{theme_isotope}
\title{Isotope theme for ggplot2}
\usage{
theme_isotope(
  base_size = 20,
  base_family = "Arial",
  base_face = "bold",
  text_color = "black"
)
}
\arguments{
\item{base_size}{Base font size}

\item{base_family}{Base font family ("Arial" by default)}

\item{base_face}{Base font face ("bold" by default)}

\item{text_color}{Text color ("black" by default)}
}
\value{
A ggplot2 theme
}
\description{
A clean theme with bold labels based on theme_classic
}
\examples{
\dontrun{
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  theme_isotope()
}
}
