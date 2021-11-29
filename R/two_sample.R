#' Two sample test
#'
#' @export

two <- function(data, x, y, rowid,
                paired = FALSE,
                type,
                var.equal,
                effsize.type = "unbiased",
                alternative = "two.sided",
                conf.level = 0.95,
                character.only = FALSE,
                ...) {

  # Argumet checks
  if (missing(data)) stop("`data` must be specified!", call. = FALSE)
  if (missing(x) || missing(y)) stop("`x` and `y` must be specified!", call. = FALSE)
  if (missing(rowid)) {
    rowid <- NULL
  }
  if (!character.only) {
    x <- deparse(substitute(x)); y <- deparse(substitute(y))
    rowid <- substitute(rowid)
    if (!is.null(rowid)) {
      rowid <- deparse(rowid)
    } else {
      rowid <- NULL
    }
  }

  # Get cleaned data
  data <- clean_data(data, x, y, rowid, paired, character.only = TRUE)

  # Create vectors of variables
  x_var <- data[[x]]
  y_var <- data[[y]]

  # Levels of 'x'
  x_lvl <- levels(x_var)

  # Check if x has more than two levels
  if (length(x_lvl) > 2) stop(x, " has more than two levels. Try witk k_sample()", call. = F)

  # Create vectors of 'y' for each group
  y_var_1 <- y_var[x_var == x_lvl[[1L]]]
  y_var_2 <- y_var[x_var == x_lvl[[2L]]]

  # If type is missing, is estimated
  if (missing(type)) {
    type <- if (is_normal(y_var_1) && is_normal(y_var_2)) "p" else "np"
  } else if (!type %in% c("p", "np")) stop("`type` must be \"p\" or \"np\"")

  if (paired) {
    var.equal <- TRUE
  }

  if (type == "p" && !paired && missing(var.equal)) {

  }

  # Get appropriate functions
  func <- two_func(type)
  func.args <- list(x = y_var_1, y = y_var_2, alternative = alternative, paired = paired,
                    var.equal = var.equal, conf.level = conf.level)

  f.es <- two_f.es(type, effsize.type)

  do.call(func)

}

#' Auxliary functions for `two()`

two_func <- function(type) {
  if (type == "p") {
    stats::t.test
  } else if (type == "np") {
    stats::wilcox.test
  }
}

two_f.es <- function(type, effsize.type) {
  if (type == "p") {
    if (effsize.type %in% c("unbiased", "g")) {
      effectsize::hedges_g
    } else if (effsize.type %in% c("biased", "d")) {
      effectsize::cohens_d
    }
  } else if (type == "np") {
    effectsize::rank_biserial
  }
}

is_var.equal <- function(y, x, alpha = 0.05, center = stats::median) {
  valid <- stats::complete.cases(y, x)
  meds <- tapply(y[valid], x[valid], center)
  resp <- abs(y - meds[x])
  stats::anova(stats::lm(resp ~ x))[["Pr(>F)"]][[1]] > alpha
}
