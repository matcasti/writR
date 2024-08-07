% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_normal}
\alias{is_normal}
\title{Normality check}
\usage{
is_normal(x, alpha = 0.05, test)
}
\arguments{
\item{x}{Numeric vector with or without NA's.}

\item{alpha}{Threshold for rejection of the null hipotesis (of normality).}

\item{test}{A function that returns a single numeric value (p.value) to be tested.}
}
\description{
Normality check
}
