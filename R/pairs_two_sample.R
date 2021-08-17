#' @title Pairwise Two Sample test
#' @name pairs_two_sample
#' @description A wrapper around `two_sample()` for pairwise comparisons. For posthoc testing we recomend using `pairwise_test()` accordingly.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character for the grouping factor. Must be present in data
#' @param y Character for the response variable. Must be present in data.
#' @param rowid Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Set `"auto"` (default) for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param effsize.type Options are `"unbiased"` or `"g"` for Hedges g and `"biased"` or `"d"` for Cohen's d as a measure of effect size for parametric test. The rank-biserial correlation is used for non-parametric analysis.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param ... Currently ignored.
#' @importFrom data.table rbindlist
#' @importFrom utils combn
#' @export

pairs_two_sample <- function(data, x, y,
                             rowid = NULL,
                             type = "auto",
                             paired = FALSE,
                             var.equal = FALSE,
                             trim = 0.2,
                             nboot = 100L,
                             effsize.type = "unbiased",
                             alternative = "two.sided",
                             conf.level = 0.95,
                             lbl = if(is.null(markdown)) FALSE else TRUE,
                             markdown = NULL,
                             ...) {

  # Data cleaning
  data <- clean_data(data, x = x, y = y, rowid = rowid, paired = paired, wide = FALSE)
  rowid <- if(is.null(rowid)) NULL else "rowid"

  # Levels of 'x'
  x_var <- data[[x]]
  x_lvl <- levels(x_var)

  # Checking assumptions
  if (type == "auto") {
    # Create vectors of variables
    y_var <- data[[y]]

    # Check normality
    normal <- vapply(x_lvl, function(i) {
      is_normal(y_var[x_var == i])
    }, NA, USE.NAMES = FALSE)
    type <- if (all(normal)) "check" else "np"

    # Check homogeneity ov variances
    if (!paired && type == "check") {
      var.equal <- is_var.equal(y_var, x_var)
    }
  }

  .lab <- if(lbl) function(i) lablr(i, markdown = markdown) else `(`
  # Rowbind list-wise every item of...
  test <- data.table::rbindlist(
    # lists of ...
    l = lapply(
      # Every possible pairwise combinations of 'x'
      X = utils::combn(
        x = x_lvl,
        m = 2,
        simplify = F
      ),
      FUN = function(i) {
        append(
          x = list(
            group1 = i[1L],
            group2 = i[2L]
          ),
          # After applying a two_sample test
          values = .lab(
            i = two_sample(
              data = data[x_var %in% i],
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
              internal = FALSE
            )
          )
        )
      }
    )
  )
  return(test)
}
