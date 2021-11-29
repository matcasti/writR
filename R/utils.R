#' Check if argument is quoted
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Internal functions implemented for checking quotes in arguments.
#'
#' @param x object to be tested. If is of length > 1 it will always return false.
#' @param eval Logical indicating whether to eval `x` and its content or just the unevaluated
#' symbol.
#' @param call_env environment in which `x` is captured. Defaults to calling environment.
#'
#' @returns
#' For is_quoted it will return TRUE if the argument is quoted, e.g. \code{"x"}, whereas if
#' is a vector of length > 1 or an unquoted object it will return FALSE.
#'
#' @note It is worth note that `x` will never be evaluated unless `eval = TRUE`, so if you pass
#' an object that represents a character vector it will eval the symbol rather than the content.
#'
#' @examples
#' # This will always return true
#'
#' is_quoted("hello")
#'
#' # While this will return false
#'
#' is_quoted(hello)        # length == 1
#'
#' # Those of length > 1 will always return false
#'
#' is_quoted(c("hello", "you"))    # length == 2
#' is_quoted(c(hello, you))        #
#'
#' # This will eval the symbol rather than the content, so
#' # it will return false for `is_quoted`
#'
#' my_vector <- "Hello"
#'
#' is_quoted(my_vector)     # This return false
#'
#' # But if you specify eval = TRUE, it will eval `x` and its content
#' # rather than the symbol
#'
#' is_quoted(my_vector, eval = TRUE)     # Now this return true
#'
#' @export

is_quoted <- function(x, eval = FALSE, call_env = parent.frame()) {
  missing(x) && stop("argument is missing", call. = FALSE)
  if (isFALSE(eval)) {
    x <- if (identical(call_env, .GlobalEnv)) {
      substitute(x)
    } else {
      substitute(x, env = call_env)
    }
  }
  is.character(x) && length(x) == 1
}

#' Check if argument is unquoted
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Internal functions implemented for checking quotes in arguments.
#'
#' @inheritParams is_quoted
#'
#' @returns
#' For is_unquoted it will return TRUE if the argument is unquoted, e.g. \code{x}, whereas if
#' is a vector of length > 1 or a quoted object it will return FALSE.
#'
#' @note It is worth note that `x` will never be evaluated unless `eval = TRUE`, so if you pass
#' an object that represents a character vector it will eval the symbol rather than the content.
#'
#' @examples
#' # This will always return true
#'
#' is_unquoted(hello)
#'
#' # While this will return false
#'
#' is_unquoted("hello")    # length == 1
#'
#' # Those of length > 1 will always return false
#'
#' is_unquoted(c("hello", "you"))  # length == 2
#' is_unquoted(c(hello, you))      #
#'
#' # This will eval the symbol rather than the content, so
#' # it will return false for `is_quoted`
#'
#' my_vector <- "Hello"
#'
#' is_unquoted(my_vector)   # This return true
#'
#' # But if you specify eval = TRUE, it will eval `x` and its content
#' # rather than the symbol
#'
#' is_unquoted(my_vector, eval = TRUE)   # Now this return false
#'
#' @export

is_unquoted <- function(x, eval = FALSE, call_env = parent.frame()) {
  missing(x) && stop("argument is missing", call. = FALSE)
  if (isFALSE(eval)) {
    x <- if (identical(call_env, .GlobalEnv)) {
      substitute(x)
    } else {
      substitute(x, env = call_env)
    }
  }
  is.name(x) && length(x) == 1
}

(function(i) {
  env = parent.frame()
  message("is quoted:", is_quoted(i, call_env = env))
  message("is unquoted:", is_unquoted(i, call_env = env))
})("hola")
