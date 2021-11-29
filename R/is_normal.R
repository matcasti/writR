#' Is a numeric vector normally distribuited
#'
#' Through analytical testing, check if a numeric vector is
#' normally distribuited

is_normal <- function(x, alpha = 0.05, test) {
  stopifnot(
    is.numeric(x),
    is.numeric(alpha) & alpha < 1 & alpha > 0,
    missing(test) || is.function(test)
  )
  x <- x[!is.na(x)]
  if (missing(test)) {
    test <- function(i) stats::shapiro.test(i)$p.value
  }
  test(x) > alpha
}
