#' Report categorical analyses
#'
#' Perform nominal/ordinal analysis on 1 dimensional table for goodnes-of-fit chi-squared, and two dimensional data for Pearson chi-squared, Fisher exact test or (if paired), McNemar test reporting their corresponding stats and effect sizes in APA Style.
#' @param data Data frame containing the variables `x` and `y`.
#' @param x Factor variable, quoted or unquoted.
#' @param y Factor. If `NULL`, a goodness-of-fit is carried, otherwise a two-way analysis is performed.
#' @param paired Logical. If `TRUE` McNemar's Chi-squared test is carried on.
#' @param ratio A vector of probabilities of the same length of x. An error is given if any entry of p is negative.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param character.only Logical. checks whether to use the unevaluated expression or its
#' content (when TRUE), asumming is a character vector. Defaults to `FALSE`.
#' @param ... Currently not used.
#' @keywords contingency
#'
#' @export

contingency <- function(data
                        , x
                        , y = NULL
                        , paired = FALSE
                        , ratio = NULL
                        , conf.level = 0.95
                        , character.only = FALSE
                        , ...) {

  x <- deparser(x, character.only)
  y <- deparser(y, character.only)

  x_var <- data[[x]]

  if (is.null(y)) {
    x_tab <- table(x_var)
    if (is.null(ratio)) ratio <- rep(1 / length(x_tab), length(x_tab))
    .f <- stats::chisq.test
    .f.es <- effectsize::pearsons_c
    .f.arg <- alist(x = x_tab, p = ratio, correct = FALSE)
  } else {
    xy_tab <- table(x_var, data[[y]])
    if (paired) {
      .f <- stats::mcnemar.test
      .f.es <- effectsize::cohens_g
    } else {
      .f <- stats::chisq.test
      .f.es <- effectsize::cramers_v
    }
    .f.arg <- alist(x = xy_tab, correct = FALSE)
  }

  .f <- do.call(.f, .f.arg)

  .f.arg <- append(.f.arg, list(adjust = TRUE, ci = conf.level))
  .f.es <- do.call(.f.es, .f.arg)

  res <- get_contingency_expr(.f, .f.es, x, y)

  return(res)
}

get_contingency_expr <- function(.f, .f.es, x, y) {
  if (is.null(y)) {
    y <- NA_character_
    effectsize <- "Pearson's C"
  } else {
    es_name <- tolower(x = names(.f.es)[[1L]])
    if (grepl("cramer", es_name)) effectsize <- "Cramer's V"
    if (grepl("cohen", es_name)) effectsize <- "Cohen's g"
  }

  if (is.null(.f$statistic)) .f$statistic <- NA_real_
  if (is.null(.f$parameter)) .f$parameter <- NA_real_

  res <- list(
    y = y,
    x = x,
    statistic = .f$statistic,
    df = .f$parameter,
    df.error = NA_real_,
    p.value = .f$p.value,
    method = .f$method,
    alternative = NA_character_,
    estimate = .f.es[[1L]],
    conf.level = .f.es[[2L]],
    conf.low = .f.es[[3L]],
    conf.high = .f.es[[4L]],
    effectsize = effectsize,
    n_obs = NA_integer_
  )

  class(res) <- c("writR", "writR.contingency", class(res))
  return(res)
}
