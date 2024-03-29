% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autest.R
\name{autest}
\alias{autest}
\title{Hypothesis testing for group differences with assumption checking for test selection}
\usage{
autest(
  data,
  x,
  y = NULL,
  rowid = NULL,
  type = "auto",
  paired = FALSE,
  var.equal = FALSE,
  posthoc = FALSE,
  sphericity = "GG",
  test.value = 0,
  trim = 0.2,
  nboot = 100L,
  effsize.type = "unbiased",
  alternative = "two.sided",
  conf.level = 0.95,
  ss_type = 3,
  p.adjust.method = "none",
  lbl = if (is.null(markdown)) FALSE else TRUE,
  markdown = NULL,
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

\item{posthoc}{Logical indicating if post-hoc tests should be returned additionally to regular output}

\item{sphericity}{Character. Which sphericity correction of the degrees of freedom should be reported for the within-subject factors. Possible values are "GG" corresponding to the Greenhouse-Geisser correction, "HF" (i.e., Hyunh-Feldt correction), and "none" (i.e., no correction).}

\item{test.value}{A number indicating the true value of the mean (Default: 0) to be tested. Only for one sample test.}

\item{trim}{Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.}

\item{nboot}{Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).}

\item{effsize.type}{Options are \code{"unbiased"} or \code{"omega"} for partial omega squared (k-samples) or \code{"g"} for Hedges g (two-samples) and \code{"biased"} or \code{"eta"} for partial eta squared (k-samples) or \code{"d"} for Cohen's d as a measure of effect size. For non-parametric analysis, Kendalls' W is used for paired designs, where rank epsilon squared is used for independent groups designs in \code{k_sample()}, whereas rank-biserial correlation is used in \code{two_sample()}.}

\item{alternative}{A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}

\item{conf.level}{Confidence/Credible Interval (CI) level. Default to 0.95 (95\%).}

\item{ss_type}{Type of sum of squares for repeated measures ANOVA (defaults to 3). Possible values are "II", "III", 2, or 3.}

\item{p.adjust.method}{Adjustment method for p-values for multiple comparisons. Possible methods are: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" (default).}

\item{lbl}{Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.}

\item{markdown}{Logical (default FALSE). If \code{lbl} is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).}

\item{...}{Currently ignored.}
}
\description{
A list containing results from a multi-sample test.
}
