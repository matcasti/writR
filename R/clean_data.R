#' Remove NA's from long to wide/long data.table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function allows removing NA's from long format data into wide (or long) format
#' data, even suporting repeated measures designs (i.e., with more than one subject per
#' factor level).
#'
#' @param data Data from which `x` and `y` (and possibly `rowid` if provided) will
#' be searched.
#' @param x Name for the grouping factor. Must be present in data
#' @param y Name for the response variable. Must be present in data.
#' @param rowid Name for the subject-id column. If null, then is assumed that
#' data is sorted for paired designs, creating one. So if your data is not sorted and you
#' leave this argument unspecified, the results can be inaccurate when there are more than
#' two levels in x and there are NAs present. Ignored if `paired` is `FALSE`.
#' @param paired Logical that decides whether the experimental design is repeated
#' measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param wide Logical to whether return a data.frame in wide format (`TRUE`, i.e. one
#' columns per group/time) or in long format (`FALSE`).
#' @param character.only Logical. checks whether to use the unevaluated expression or its
#' content (when TRUE), asumming is a character vector. Defaults to `FALSE`.
#' @param ... Currently ignored.
#'
#'
#' @export

clean_data <- function(data, x, y, rowid,
                       paired = FALSE,
                       wide = FALSE,
                       character.only = FALSE,
                       ...) {

  # Argument checking ---------------------------------------------------------------------------

  # Check if `data` is null
  if (missing(data)) stop("`data` can't be null", call. = FALSE)

  if (!"data.table" %in% class(data))
    data <- data.table::as.data.table(data)

  # Check if `x` or `y` is null
  if (missing(x) || missing(y)) stop("`x` and `y` can't be null", call. = FALSE)

  if (character.only) {
    x <- as.name(x); y <- as.name(y)
    if (!missing(rowid)) {
      rowid <- as.name(rowid)
    }
  }

  expr <- if (missing(rowid) || !paired) {
    substitute(data[, .("rowid" = factor(seq_len(.N)), y), .(x <- factor(x))])
  } else {
    substitute(data[, y, .(x <- factor(x), "rowid" = factor(rowid))])
  }

  data <- eval(expr)

  y_label <- deparse(substitute(y)); x_label <- deparse(substitute(x))

  if (paired || wide) {
    expr <- stats::as.formula(paste("rowid ~", x_label))
    data <- data.table::dcast(data = data, formula = expr, value.var = y_label, drop = c(F, T))
    data <- data[, .SD, .SDcols = function(i) !all(is.na(i))]
  }

  if (paired) {
    data <- stats::na.omit(data)[, droplevels(.SD)]
  }

  if (!wide && paired) {
    data <- data.table::melt(data, id.vars = 1, variable.name = x_label, value.name = y_label)
  }

  # Formato largo y no pareado
  return(data)
}
