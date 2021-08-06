#' @title Two Sample test
#' @name two_sample
#' @description A list containing results from a two-sample test and effect size plus confidence intervals.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character name for the grouping factor. Must be present in data
#' @param y Character name for the response variable. Must be present in data.
#' @param rowid Character name for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Set `"auto"` (default) for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param effsize.type Options are `"unbiased"` or `"g"` for Hedges g and `"biased"` or `"d"` for Cohen's d as a measure of effect size. rank-biserial correlation is used for non-parametric analysis.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param internal Logical to whether this function is being used inside of internal functions.
#' @param ... Currently ignored.
#' @importFrom data.table %chin%
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#' @importFrom stats t.test na.omit wilcox.test
#' @importFrom WRS2 yuend dep.effect yuen yuen.effect.ci
#' @export

two_sample <- function(data, x, y,
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
                       internal = FALSE,
                       ...) {

  # Avoid unnecesary computation if called internally inside another function
  if (!internal) {
    data <- clean_data(data, x = x, y = y, rowid = rowid, paired = paired, wide = FALSE)
  }

  # Create vectors of variables
  y_var <- data[[y]]
  x_var <- data[[x]]

  # Levels of 'x'
  x_lvl <- levels(x_var)

  # Check if x has more than two levels
  !internal && length(x_lvl) > 2 && stop(x, " has more than two levels. Try witk k_sample()", call. = F)

  # Create vectors of 'y' for each group
  y_var_1 <- y_var[x_var == x_lvl[[1L]]]
  y_var_2 <- y_var[x_var == x_lvl[[2L]]]

  # Check normality
  if (type == "auto") {
    type <- if (is_normal(y_var_1) && is_normal(y_var_2)) "check" else "np"
  }

  # Parametric statistics
  if (type %chin% c("p", "check")) {

    # Assign an effect size
    .f <- if (effsize.type %chin% c("biased", "d")) {
      effectsize::cohens_d
    } else if (effsize.type %chin% c("unbiased", "g")) {
      effectsize::hedges_g
    }

    # Check homogeneity of variances
    if (!paired && type == "check") {
      var.equal <- is_var.equal(y_var, x_var)
    }

    # Student t-test
    test <- stats::t.test(
      x = y_var_1,
      y = y_var_2,
      var.equal = var.equal,
      paired = paired,
      alternative = alternative,
      conf.level = conf.level
    )

    es <- .f(
      x = y_var_1,
      y = y_var_2,
      pooled_sd = var.equal,
      paired = paired,
      ci = conf.level,
      verbose = FALSE
    )

    test <- list(
      "y" = y,
      "x" = x,
      statistic = test$statistic,
      df = as.numeric(test$parameter),
      df.error = NA_real_,
      p.value = test$p.value,
      method = test$method,
      alternative = alternative,
      estimate = es[[1L]],
      conf.level = es[["CI"]],
      conf.low = es[["CI_low"]],
      conf.high = es[["CI_high"]],
      effectsize = names(es)[1L],
      n_obs = if(paired) length(y_var_1) else length(y_var)
    )

    if(lbl) {
      test <- lablr(test, markdown)
    }

    class(test) <- c("writR", "list")

    return(test)
  }
  # non-parametric statistics
  if (type == "np") {

    # Wilcoxon test
    test <- stats::wilcox.test(
      x = y_var_1,
      y = y_var_2,
      alternative = alternative,
      paired = paired
    )

    es <- effectsize::rank_biserial(
      x = y_var_1,
      y = y_var_2,
      ci = conf.level,
      paired = paired,
      verbose = FALSE
    )

    test <- list(
      "y" = y,
      "x" = x,
      statistic = log(test$statistic),
      df = NA_real_,
      df.error = NA_real_,
      p.value = test$p.value,
      method = test$method,
      alternative = if(is.null(test$alternative)) NA_character_ else test$alternative,
      estimate = es[[1L]],
      conf.level = es[["CI"]],
      conf.low = es[["CI_low"]],
      conf.high = es[["CI_high"]],
      effectsize = "rank-biserial correlation",
      n_obs = if(paired) length(y_var_1) else length(y_var)
    )

    if(lbl) {
      test <- lablr(test, markdown)
    }

    class(test) <- c("writR", "list")

    return(test)
  }
  # Robust statistics
  if (type == "r") {
    if (paired) {

      # Yuen test - paired samples
      test <- WRS2::yuend(
        x = y_var_1,
        y = y_var_2,
        tr = trim
      )

      es <- WRS2::dep.effect(
        x = y_var_1,
        y = y_var_2,
        nboot = nboot,
        tr = trim
      )["AKP", ]

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$test,
        df = as.numeric(test$df),
        df.error = NA_real_,
        p.value = test$p.value,
        method = "Paired Yuen's test on trimmed means",
        alternative = "two.sided",
        estimate = es[["Est"]],
        conf.level = 0.95,
        conf.low = es[["ci.low"]],
        conf.high = es[["ci.up"]],
        effectsize = "Algina-Keselman-Penfield robust standardized difference",
        n_obs = length(y_var_1)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    } else {

      # Yuen test - independent samples
      test <- WRS2::yuen(
        formula = y_var ~ x_var,
        tr = trim
      )

      es <- WRS2::yuen.effect.ci(
        formula = y_var ~ x_var,
        tr = trim,
        nboot = nboot
      )

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$test,
        df = as.numeric(test$df),
        df.error = NA_real_,
        p.value = test$p.value,
        method = "Two sample Yuen's test on trimmed means",
        alternative = "two.sided",
        estimate = es[["effsize"]],
        conf.level = 0.95,
        conf.low = es[["CI"]][[1L]],
        conf.high = es[["CI"]][[2L]],
        effectsize = "Algina-Keselman-Penfield robust standardized difference",
        n_obs = length(y_var)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    }
  }
}
