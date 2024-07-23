#' @title K Sample test
#' @name k_sample
#' @description A list containing results from a multi-sample test.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character for the grouping factor. Must be present in data
#' @param y Character for the response variable. Must be present in data.
#' @param rowid Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Missing (default) or `NULL` for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param is_spherical Logical. checks whether to assume that the sphericity assumptions holds or not, if `NULL` (the default) it will be tested using mauchly's test with a threshold of 0.05.
#' @param adjust Character. correction for sphericity to be applied, it can be any character of length one starting with 'g' (indicating Greenhouse–Geisser correction) or 'h' (indicating Huynh–Feldt correction).
#' @param effsize.type Options are `"unbiased"` or `"omega"` for partial omega squared and `"biased"` or `"eta"` for partial eta squared as a measure of effect size. For non-parametric analysis, Kendalls' W is used for paired designs, where rank epsilon squared is used for independent groups designs.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param character.only Logical. checks whether to use the unevaluated expression or its
#' content (when TRUE), asumming is a character vector. Defaults to `FALSE`.
#' @param ... Currently ignored.
#'
#' @export

k_sample <- function(data, x, y,
                     rowid = NULL,
                     type,
                     paired = FALSE,
                     var.equal = FALSE,
                     is_spherical = NULL,
                     adjust = NULL,
                     effsize.type = "unbiased",
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

  # Levels of 'x' variable
  x_lvl <- levels(x_var)

  # Check normality
  if (missing(type) || is.null(type)) {
    normal <- vapply(x_lvl, function(i) is_normal(y_var[x_var == i]), NA)
    type <- if (all(normal)) "check" else "np"
  }

  n_obs <- if (paired) length(y_var) / length(x_lvl) else length(y_var)

  # Parametric statistics
  if (type %chin% c("p", "check")) {

    # Assign an effect size
    if (effsize.type %in% c("biased", "eta")) .f.es <- effectsize::F_to_eta2
    if (effsize.type %in% c("unbiased", "omega")) .f.es <- effectsize::F_to_omega2

    if (paired) {
      .f <- rm_anova(data, x, y, is_spherical, adjust, character.only = TRUE)
      .f.es <- NULL
      method <- .f$method
    } else {
      if (type == "check") var.equal <- is_var.equal(y_var, x_var)
      .f <- stats::oneway.test(formula = y_var ~ x_var, var.equal = var.equal)
      .f.es <- .f.es(f = .f$statistic, df = .f$parameter[[1L]], df_error = .f$parameter[[2L]], ci = conf.level)
      method <- if (var.equal) "Fisher's ANOVA" else "Welch's ANOVA"
    }
  }
  # Non-parametric statistics
  if (type == "np") {
    # Friedman rank-sum test for dependent samples
    if (paired) {
      .f <- stats::friedman.test(y = y_var, groups = x_var, blocks = data$rowid)
      .f.es <- effectsize::kendalls_w(x = y_var, groups = x_var, blocks = data$rowid, ci = conf.level, verbose = FALSE)
      method <- "Friedman Rank Sum Test"

    # Kruskal-Wallis rank-sum test for independent samples
    } else {
      .f <- stats::kruskal.test(x = y_var, g = x_var)
      .f.es <- effectsize::rank_epsilon_squared(x = y_var, groups = x_var, ci = conf.level)
      method <- "Kruskal-Wallis Rank Sum Test"
    }
  }

  res <- get_k_expr(.f, .f.es, type, paired, x, y, n_obs, effsize.type, method)

  return(res)
}

get_k_expr <- function(.f, .f.es, type, paired, x, y, n_obs, effsize.type, method) {
  if (type %in% c("p", "check")) {
    p.value   <- .f$p.value
    statistic <- .f$statistic

    if (effsize.type %in% c("biased", "eta")) effectsize <- "Eta-squared (partial)"
    if (effsize.type %in% c("unbiased", "omega")) effectsize <- "Omega-squared (partial)"

    if (paired) {
      .f.es <- list(.f$estimate, CI = .f$conf.level, CI_low = .f$conf.low, CI_high = .f$conf.high)
      df        <- .f$df_num
      df.error  <- .f$df_den
    } else {
      df        <- .f$parameter[[1L]]
      df.error  <- .f$parameter[[2L]]
    }
  } else {
    effectsize <- if (paired) "Kendall's W" else "rank-biserial correlation"
    statistic <- .f$statistic
    df <- .f$parameter
    df.error <- NA_real_
    p.value <- .f$p.value
  }

  res <- list(
    "y"         = y,
    "x"         = x,
    statistic   = statistic,
    df          = df,
    df.error    = df.error,
    p.value     = p.value,
    method      = method,
    alternative = NA_character_,
    estimate    = .f.es[[1L]],
    conf.level  = .f.es[["CI"]],
    conf.low    = .f.es[["CI_low"]],
    conf.high   = .f.es[["CI_high"]],
    effectsize  = effectsize,
    n_obs       = n_obs
  )

  class(res) <- c("writR", "writR.k", class(res))
  return(res)
}
