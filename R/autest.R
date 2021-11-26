#' @title Hypothesis testing for group differences with assumption checking for test selection
#' @name autest
#' @description A list containing results from a multi-sample test.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character for the grouping factor. Must be present in data
#' @param y Character for the response variable. Must be present in data.
#' @param rowid Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Set `"auto"` (default) for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param posthoc Logical indicating if post-hoc tests should be returned additionally to regular output
#' @param sphericity Character. Which sphericity correction of the degrees of freedom should be reported for the within-subject factors. Possible values are "GG" corresponding to the Greenhouse-Geisser correction, "HF" (i.e., Hyunh-Feldt correction), and "none" (i.e., no correction).
#' @param test.value A number indicating the true value of the mean (Default: 0) to be tested. Only for one sample test.
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param effsize.type Options are `"unbiased"` or `"omega"` for partial omega squared (k-samples) or `"g"` for Hedges g (two-samples) and `"biased"` or `"eta"` for partial eta squared (k-samples) or `"d"` for Cohen's d as a measure of effect size. For non-parametric analysis, Kendalls' W is used for paired designs, where rank epsilon squared is used for independent groups designs in `k_sample()`, whereas rank-biserial correlation is used in `two_sample()`.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param ss_type Type of sum of squares for repeated measures ANOVA (defaults to 3). Possible values are "II", "III", 2, or 3.
#' @param p.adjust.method Adjustment method for p-values for multiple comparisons. Possible methods are: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" (default).
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param ... Currently ignored.
#' @export

autest <- function(data, x, y = NULL,
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
                   character.only = FALSE,
                   ...) {

  if (!is.null(y)) {


    # Data cleaning
    data <- clean_data(data, x = x, y = y, rowid = rowid, paired = paired, wide = FALSE, character.only = character.only)

  } else {
  # One sample test
    test <- one_sample(
      data = data,
      y = x,
      type = type,
      test.value = test.value,
      trim = trim,
      nboot = nboot,
      effsize.type = effsize.type,
      alternative = alternative,
      conf.level = conf.level,
      lbl = lbl,
      markdown = markdown,
    )

    return(test)
  }
  # K-samples test
    x_lvl <- levels(data[[x]])
    if (length(x_lvl) > 2) {
      test <- k_sample(
        data = data,
        x = x,
        y = y,
        rowid = rowid,
        type = type,
        paired = paired,
        var.equal = var.equal,
        sphericity = sphericity,
        trim = trim, nboot = nboot,
        effsize.type = effsize.type,
        alternative = alternative,
        conf.level = conf.level,
        ss_type = ss_type,
        lbl = lbl,
        markdown = markdown,
        internal = TRUE
      )

      # With post-hoc if required
      if (posthoc) {
        posthoc <- pairwise_test(
          data = data,
          x = x,
          y = y,
          rowid = rowid,
          type = type,
          paired = paired,
          var.equal = var.equal,
          trim = trim,
          nboot = nboot,
          p.adjust.method = p.adjust.method,
          alternative = alternative,
          conf.level = conf.level,
          internal = TRUE
        )

        output <- list(test = test, pwc = posthoc)
        return(output)
      } else {
        return(test)
      }
    } else {
      # Two sample test
      test <- two_sample(
        data = data,
        x = x,
        y = y,
        rowid = rowid,
        type = type,
        paired = paired,
        var.equal = var.equal,
        trim = trim,
        nboot = nboot,
        effsize.type = effsize.type,
        alternative = alternative,
        conf.level = conf.level,
        lbl = lbl,
        markdown = markdown,
        internal = TRUE
      )

      return(test)
    }
}
