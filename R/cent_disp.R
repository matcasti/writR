#' @title Inline summary descriptive
#'
#' @description This function returns a character vector with mean and sd or median and iqr as a inline summary statistics for reports or articles in R markdown or plain text.
#' @param x Numeric vector.
#' @param type To choose a parametric (`type = 'p'`) estimate (i.e. mean and sd) or a non-parametric one (`type = 'np'`, median and iqr). Can be choosen automatically with `type = 'auto`. It can be set to `'custom'` so you can insert your own expresions using glue syntax in `str.a` argument.
#' @param str.a Works only when `type = 'custom'`. Allows you to set your own expresions using glue syntax, e.g. `str.a = "{median} (*MAD* = {mad}, *IQR* = {IQR})`
#' @param k Number of decimals.
#' @param markdown Whether you want the output formated for inline R markdown (TRUE) or as plain text (FALSE).
#' @keywords cent_disp
#' @return A character vector of length one.
#' @export

cent_disp <- function(x,
                      type = if (is.null(str.a)) "auto" else "custom",
                      str.a = NULL,
                      k = 1,
                      markdown = TRUE) {

  if (!is.numeric(x)) stop(deparse(substitute(x)), " is not numeric.")

  x <- x[!is.na(x)]
  if (type != "custom") {
    if (type == "auto") {
      type <- if (is_normal(x)) "p" else "np"
    }
    if (type == "p")  .f <- list(cent = base::mean,   disp = stats::sd,  m = "M",   i = "SD")
    if (type == "np") .f <- list(cent = stats::median, disp = stats::IQR, m = "Mdn", i = "IQR")

    m <- round(.f$cent(x), k)
    i <- round(.f$disp(x), k)
    if (markdown) {
      paste0("*", .f$m, "* = ", m, ", *", .f$i, "* = ", i)
    } else {
      paste0(.f$m, " = ", m, ", ", .f$i, " = ", i)
    }
  } else {
    m <- gregexpr(pattern = "\\{(.*?)\\}", text = str.a)
    m <- regmatches(str.a, m)[[1]]

    str.b <- gsub("\\{|\\}", "", m)
    str_eval <- function(i) {
      .f <- eval(expr = as.name(x = i))
      round(x = .f(x), digits = k)
    }

    res <- vapply(str.b, str_eval, FUN.VALUE = NA_real_)

    glue::glue(str.a, .envir = as.list(res))
  }
}
