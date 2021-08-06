#' @title One Sample test
#' @name one_sample
#' @description A list containing results from a one-sample test.
#'
#' @param data data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param y Character for the response variable Must be present in data
#' @param type set `"auto"` (default) for checking the normality for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param test.value A number indicating the true value of the mean (Default: 0) to be tested.
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param effsize.type options are `"unbiased"` or `"g"` for Hedges g and `"biased"` or `"d"` for Cohen's d as a measure of effect size. rank-biserial correlation is used for non-parametric analysis.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param ... Currently ignored.
#' @importFrom effectsize cohens_d hedges_g rank_biserial
#' @importFrom stats t.test wilcox.test
#' @export

one_sample <- function(data, y,
                       type = "auto",
                       test.value = 0,
                       trim = 0.2,
                       nboot = 100L,
                       effsize.type = "unbiased",
                       alternative = "two.sided",
                       conf.level = 0.95,
                       lbl = if(is.null(markdown)) FALSE else TRUE,
                       markdown = NULL,
                       ...) {

  # Create vectors of variables
  y_var <- data[[y]]
  y_var <- y_var[!is.na(y_var)]

  # Check normality
  if(type == "auto") {
    type <- if(is_normal(y_var)) "p" else "np"
  }

  # Parametric statistics
  if(type == "p") {

    # Assign an effect size
    .f <- if (effsize.type %chin% c("biased", "d")) {
      effectsize::cohens_d
    } else if (effsize.type %chin% c("unbiased", "g")) {
      effectsize::hedges_g
    }

    # Student t test
    test <- stats::t.test(
      x = y_var,
      mu = test.value,
      alternative = alternative
    )

    es <- .f(
      x = y_var,
      mu = test.value,
      ci = conf.level,
      verbose = FALSE
    )

    test <- list(
      "y" = y,
      "x" = NA_character_,
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
      n_obs = length(y_var)
    )

    if(lbl) {
      test <- lablr(test, markdown)
    }

    class(test) <- c("writR", "list")

    return(test)
  }
  # Non-parametric statistics
  if(type == "np") {
    test <- stats::wilcox.test(
      x = y_var,
      mu = test.value,
      alternative = alternative
      )

    es <- effectsize::rank_biserial(
      x = y_var,
      mu = test.value,
      ci = conf.level,
      verbose = FALSE
    )

    test <- list(
      "y" = y,
      "x" = NA_character_,
      statistic = log(test$statistic),
      df = NA_real_,
      df.error = NA_real_,
      p.value = test$p.value,
      method = test$method,
      alternative = alternative,
      estimate = es[[1L]],
      conf.level = es[["CI"]],
      conf.low = es[["CI_low"]],
      conf.high = es[["CI_high"]],
      effectsize = names(es)[1L],
      n_obs = length(y_var)
    )

    if(lbl) {
      test <- lablr(test, markdown)
    }

    class(test) <- c("writR", "list")

    return(test)
  }
  # Robust statistics
  if(type == "r") {

    test <- trimcibt(
      x = y_var,
      nv = test.value,
      tr = trim,
      nboot = nboot,
      ci = conf.level
    )

    test <- list(
      "y" = y,
      "x" = NA_character_,
      statistic = test$statistic,
      df = NA_real_,
      df.error = NA_real_,
      p.value = test$p.value,
      method = test$method,
      alternative = alternative,
      estimate = test$estimate,
      conf.level = test[["conf.level"]],
      conf.low = test$conf.low,
      conf.high = test$conf.high,
      effectsize = test$effectsize,
      n_obs = length(y_var)
    )

    if(lbl) {
      test <- lablr(test, markdown)
    }

    class(test) <- c("writR", "list")

    return(test)
  }
}
