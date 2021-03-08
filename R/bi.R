#' Two samples testing (Independent or Paired)
#'
#' This is function let you perform automated inferential testing based on certain assumptions, some of which are tested automatically, then the propper test is perform, giving you an APA formated output with your statistical results.
#' @param data Your dataset in long format, can have some missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param paired A logical indicating whether you want a paired test (default is FALSE).
#' @param type Whether you want to manually specify a parametric test (type = 'p'), a non-parametric test (type = 'np') or a robust test (type = 'r').
#' @param var.equal If `TRUE`, then Welch correction is applied to the degrees of freedom, only when `paired = FALSE` and `type = 'p'`.
#' @param trim Trim level for the mean (available only for robust test).
#' @param nboot Number of bootstrap samples.
#' @param markdown Whether you want the `$report` output formated for inline RMarkdown or as plain text.
#' @param ... Currently not used.
#' @keywords bi
#' @return A list of length 2 with `$report` of statistical test and `$method` used.

bi <- function(data
                , variable
                , by
                , paired = FALSE
                , type = 'auto'
                , var.equal = FALSE
                , trim = 0.1
                , nboot = 100
                , markdown = TRUE
                , ...) {

  if(paired) {
    # Muestras relaciondas ----
    bipair(data = data
            , variable = {{variable}}
            , by = {{by}}
            , type = type
            , trim = trim
            , nboot = nboot
            , markdown = markdown
            , ...)
  } else {
    # Muestras independientes ----
    bitwo(data = data
           , variable = {{variable}}
           , by = {{by}}
           , type = type
           , var.equal = var.equal
           , trim = trim
           , nboot = nboot
           , markdown = markdown
           , ...)
  }
}
