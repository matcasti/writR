% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_test.R
\name{pairwise_test}
\alias{pairwise_test}
\title{Pairwise Comparisons}
\usage{
pairwise_test(
  data,
  x,
  y,
  rowid = NULL,
  type = "auto",
  paired = FALSE,
  var.equal = FALSE,
  trim = 0.2,
  nboot = 100L,
  p.adjust.method = "none",
  alternative = "two.sided",
  conf.level = 0.95,
  internal = FALSE,
  ...
)
}
\arguments{
\item{data}{Data frame from which \code{x} and \code{y} (and possibly \code{rowid} if provided) will be searched.}

\item{x}{Character for the grouping factor. Must be present in data}

\item{y}{Character for the response variable. Must be present in data.}

\item{rowid}{Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.}

\item{type}{Set \code{"auto"} (default) for checking the normality and homogeneity of variances for test selection. Other options are \code{"p"} for parametric, \code{"np"} for non-parametric and \code{"r"} for robust tests.}

\item{paired}{Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is \code{FALSE.}}

\item{var.equal}{Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.}

\item{trim}{Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.}

\item{nboot}{Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).}

\item{p.adjust.method}{Adjustment method for p-values for multiple comparisons. Possible methods are: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" (default).}

\item{alternative}{A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}

\item{conf.level}{Confidence/Credible Interval (CI) level. Default to 0.95 (95\%).}

\item{internal}{Logical to whether this function is being used inside of other functions.}

\item{...}{Currently ignored.}
}
\description{
Pairwise comparisons for Post-hoc testing.
}
