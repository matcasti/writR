#' @title One Sample test
#' @name one_sample
#' @description A list containing results from a one-sample test.
#'
#' @param data data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param y Character for the response variable Must be present in data
#' @param type set `"auto"` (default) for checking the normality for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param test.value A number indicating the true value of the mean (Default: 0) to be tested.
#' @param effsize.type options are `"unbiased"` or `"g"` for Hedges g and `"biased"` or `"d"` for Cohen's d as a measure of effect size. rank-biserial correlation is used for non-parametric analysis.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param character.only whether to treat `x` as a character. Default is FALSE.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param ... Currently ignored.
#' @export

one_sample <- function(data, y,
                       type,
                       test.value = 0,
                       effsize.type = "unbiased",
                       alternative = "two.sided",
                       conf.level = 0.95,
                       character.only = FALSE,
                       markdown,
                       ...) {
  y <- deparser(y, character.only)

  y_var <- data[[y]]
  y_var <- y_var[!is.na(y_var)]

  if (missing(type)) {
    type <- if (is_normal(y_var)) "p" else "np"
  }

  if (type == "p") {
    .f <- stats::t.test
    if (effsize.type %chin% c("biased", "d")) .f.es <- effectsize::cohens_d
    if (effsize.type %chin% c("unbiased", "g")) .f.es <- effectsize::hedges_g
  }
  if (type == "np") {
    .f <- stats::wilcox.test
    .f.es <- effectsize::rank_biserial
  }

  arg.f <- alist(
    x = y_var,
    mu = test.value,
    alternative = alternative,
    conf.level = conf.level
  )
  arg.f.es <- alist(
    x = y_var,
    mu = test.value,
    ci = conf.level,
    verbose = FALSE
  )

  .f    <- do.call(.f, arg.f)
  .f.es <- do.call(.f.es, arg.f.es)

  test  <- get_one_expr(.f, .f.es, type, n_obs = length(y_var), y, alternative)

  if (!missing(markdown) && isTRUE(markdown)) {
    test <- lablr(test, markdown)
  }

  class(test) <- c("writR", "list")

  return(test)
}


get_one_expr <- function(.f, .f.es, type, n_obs, ylab, alternative) {

  if (type == "p") {
    statistic  <- .f$statistic
    df         <- as.numeric(.f$parameter)
    method     <- .f$method
    effectsize <- names(.f.es)[1L]
    if (effectsize %like% "Hedges") effectsize <- "Hedges' g"
    if (effectsize %like% "Cohen")  effectsize <- "Cohen's d"
  }

  if (type == "np") {
    statistic  <- log(.f$statistic)
    df         <- NA_real_
    method     <- trimws(.f$method)
    effectsize <- "rank-biserial correlation"
  }

  list(
    "y"         = ylab,
    "x"         = NA_character_,
    statistic   = statistic,
    df          = df,
    df.error    = NA_real_,
    p.value     = .f$p.value,
    method      = method,
    alternative = alternative,
    estimate    = .f.es[[1L]],
    conf.level  = .f.es[["CI"]],
    conf.low    = .f.es[["CI_low"]],
    conf.high   = .f.es[["CI_high"]],
    effectsize  = effectsize,
    n_obs       = n_obs
  )
}
