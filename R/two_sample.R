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
#' @param effsize.type Options are `"unbiased"` or `"g"` for Hedges g and `"biased"` or `"d"` for Cohen's d as a measure of effect size. rank-biserial correlation is used for non-parametric analysis.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param character.only whether to treat `x` as a character. Default is FALSE.
#' @param ... Currently ignored.
#' @export

two_sample <- function(data, x, y,
                       rowid = NULL,
                       type,
                       paired = FALSE,
                       var.equal = FALSE,
                       effsize.type = "unbiased",
                       alternative = "two.sided",
                       conf.level = 0.95,
                       character.only = FALSE,
                       ...) {

  x     <- deparser(x, character.only)
  y     <- deparser(y, character.only)
  rowid <- deparser(rowid, character.only)

  data <- clean_data(data, x, y, rowid, paired, character.only = TRUE)

  # Create vectors of variables
  y_var <- data[[y]]
  x_var <- data[[x]]

  # Levels of 'x'
  x_lvl <- levels(x_var)

  # Check if x has more than two levels
  if (length(x_lvl) > 2) stop(x, " has more than two levels. Try witk k_sample()", call. = F)

  # Create vectors of 'y' for each group
  y_var_1 <- y_var[x_var == x_lvl[[1L]]]
  y_var_2 <- y_var[x_var == x_lvl[[2L]]]

  # Number of observations
  n_obs <-  if (paired) length(y_var_1) else length(y_var)

  # Check normality
  if (missing(type)) {
    normal <- is_normal(y_var_1) && is_normal(y_var_2)
    type <- if (normal) "check" else "np"
  }

  # Parametric statistics
  if (type %chin% c("p", "check")) {
    .f <- stats::t.test
    if (effsize.type %chin% c("biased", "d"))   .f.es <- effectsize::cohens_d
    if (effsize.type %chin% c("unbiased", "g")) .f.es <- effectsize::hedges_g
    if (!paired && type == "check") var.equal <- is_var.equal(y_var, x_var)
  }

  # Non-parametric statistics
  if (type == "np") {
    .f    <- stats::wilcox.test
    .f.es <- effectsize::rank_biserial
  }

  arg.f <- alist(
    x = y_var_1,
    y = y_var_2,
    var.equal = var.equal,
    paired = paired,
    alternative = alternative,
    conf.level = conf.level
  )

  arg.f.es <- alist(
    x = y_var_1,
    y = y_var_2,
    pooled_sd = var.equal,
    paired = paired,
    ci = conf.level,
    verbose = FALSE
  )

  .f    <- do.call(.f, arg.f)
  .f.es <- do.call(.f.es, arg.f.es)

  res  <- get_two_expr(.f, .f.es, type, n_obs, x, y)

  return(res)
}

get_two_expr <- function(.f, .f.es, type, n_obs, xlab, ylab) {

  if (is.null(.f$alternative)) .f$alternative <- NA_character_

  if (type %in% c("p", "check")) {
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

  res <- list(
    "y"         = ylab,
    "x"         = xlab,
    statistic   = statistic,
    df          = df,
    df.error    = NA_real_,
    p.value     = .f$p.value,
    method      = method,
    alternative = .f$alternative,
    estimate    = .f.es[[1L]],
    conf.level  = .f.es[["CI"]],
    conf.low    = .f.es[["CI_low"]],
    conf.high   = .f.es[["CI_high"]],
    effectsize  = effectsize,
    n_obs       = n_obs
  )

  class(res) <- c("writR", "writR.two", class(res))
  return(res)
}
