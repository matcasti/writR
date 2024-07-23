#' Always returns the outermost expression
#'
#' @param x An expression from which to capture the outermost expression.
#' @param env The last environment in which to search `x`. Default is missing.
#' @param ... Currently not used.
#'
#' @source <https://stackoverflow.com/a/26558733>

subs <- function(x, env, ...) {
  .call <- quote(substitute(x))
  .name <- eval(.call)

  .envs <- rev(x = sys.frames())
  if (!missing(env) && is.environment(env)) {
    to_enclos <- vapply(.envs, identical, env, FUN.VALUE = NA)
    to_enclos <- 1:which(to_enclos)
    .envs <- .envs[to_enclos]
  }

  for (i in .envs) {
    .call[[2]] <- .name
    .name <- eval(.call, i)
  }

  return(.name)
}

#' Get the deparsed value of the outermost expression
#'
#' @param x An expression from which to capture the outermost expression.
#' @param character.only whether to treat `x` as a character. Default is FALSE.
#' @param ... Currently not used.

deparser <- function(x, character.only = FALSE, ...) {
  if (missing(x)) return(NULL)

  if (isTRUE(character.only)) {
    x <- as.character(x)
    return(x)
  }

  x <- subs(x, env = parent.frame())

  .f <- function(i) {
    if (is.null(i)) return(NULL)
    if (!is.character(i)) i <- deparse(i)
    return(i)
  }

  if (length(x) > 1) {
    x <- as.list(x)[-1L]
    x <- lapply(x, .f)
    unlist(x)
  } else {
    .f(x)
  }
}
