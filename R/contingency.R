#' Report categorical analyses
#'
#' Perform nominal/ordinal analysis on 1 dimensional table for goodnes-of-fit chi-squared, and two dimensional data for Pearson chi-squared, Fisher exact test or (if paired), McNemar test reporting their corresponding stats and effect sizes in APA Style.
#' @param data Data frame containing the variables `x` and `y`.
#' @param x Factor variable, quoted or unquoted.
#' @param y Factor. If `NULL`, a goodness-of-fit is carried, otherwise a two-way analysis is performed.
#' @param paired Logical. If `TRUE` McNemar's Chi-squared test is carried on.
#' @param exact Logical. If `TRUE` then Fisher's Exact Test is carried on, but only when `paired = FALSE` (default). If is a 2 x 2 design, Odds Ratio (OR) is returned as effect size, otherwise it will only return the formated p-value.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Whether you want the output formated for inline R Markdown or as plain text.
#' @param ... Currently not used.
#' @keywords contingency
#' @importFrom stats chisq.test fisher.test mcnemar.test
#' @importFrom effectsize cramers_v oddsratio cohens_g
#' @export

contingency <- function(data
                   , x
                   , y = NULL
                   , paired = FALSE
                   , exact = FALSE
                   , conf.level = 0.95
                   , lbl = if(is.null(markdown)) FALSE else TRUE
                   , markdown = NULL
                   , ...) {

  x_var <- data[[x]]
  if(is.null(y)) {
    tab <- table(x_var)
  }  else {
    y_var <- data[[y]]
    tab <- table(x_var, y_var)
  }

  if(paired) {
    # Mcnemar test
    .f <- stats::mcnemar.test(tab)
    .es <- effectsize::cohens_g(tab, ci = conf.level)
  } else if(exact) {
    # Exact test
    .f <- stats::fisher.test(tab)
    .es <- try(expr = effectsize::oddsratio(tab, ci = conf.level), silent = TRUE)
    if("try-error" %chin% class(.es)) {
      .es <- rep(NA_real_, 4)
      .es <- `names<-`(.es, rep(NA, 4))
      .f$method <- paste(.f$method, "without OR")
    }
  } else {
    # Chi-square
    .f <- stats::chisq.test(tab)
    .es <- effectsize::cramers_v(tab, ci = conf.level)
  }

  res <- list(
    y = if (is.null(y)) NA_character_ else y,
    x = x,
    statistic = if (is.null(.f$statistic)) NA_real_ else .f$statistic,
    df = if (is.null(.f$parameter)) NA_real_ else .f$parameter,
    df.error = NA_real_,
    p.value = .f$p.value,
    method = .f$method,
    alternative = NA_character_,
    estimate = as.numeric(.es[[1L]]),
    conf.level = as.numeric(.es[[2L]]),
    conf.low = as.numeric(.es[[3L]]),
    conf.high = as.numeric(.es[[4L]]),
    effectsize = names(.es)[[1L]],
    n_obs = sum(tab)
  )

  if(lbl) {
    res <- lablr(res, markdown)
  }

  class(res) <- c("writR", "list")

  return(res)
}

