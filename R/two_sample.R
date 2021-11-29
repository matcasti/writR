#' Two sample test
#'
#' @export

two <- function(data, x, y, rowid,
                paired = FALSE,
                type,
                var.equal = FALSE,
                effsize.type = "unbiased",
                alternative = "two.sided",
                conf.level = 0.95,
                character.only,
                ...) {

  # Argumet checks
  if (missing(data)) stop("`data` must be specified!", call. = FALSE)
  if (missing(x) || missing(y)) stop("`x` and `y` must be specified!", call. = FALSE)
  if (missing(rowid)) rowid <- NULL

  # If character.only is missing, then check the unevaluated expression
  if (missing(character.only)) {
    if (is.name(x = substitute(x)))     x     <- deparse(expr = substitute(x))
    if (is.name(x = substitute(y)))     y     <- deparse(expr = substitute(y))
    if (is.name(x = substitute(rowid))) rowid <- deparse(expr = substitute(rowid))
  } else if (!character.only) {
    x <- deparse(expr = substitute(x))
    y <- deparse(expr = substitute(y))
    if (!is.null(x = substitute(rowid))) rowid <- deparse(expr = substitute(rowid))
  }

  # Get cleaned data
  data <- clean_data(data, x, y, rowid, paired, character.only = TRUE)
  vars <- get_vars(data, x, y)

  # If type is missing, is estimated
  if (missing(type)) {
    type <- if (is_normal(vars$y1) && is_normal(vars$y2)) "check" else "np"
  }

  # Get appropriate functions
  if (type %in% c("p", "check")) {
    .f <- stats::t.test
    if (type == "check") var.equal  <- is_var.equal(vars$y, vars$x)
    if (effsize.type %in% c("biased", "d"))   .f.es <- effectsize::cohens_d
    if (effsize.type %in% c("unbiased", "g")) .f.es <- effectsize::hedges_g
  } else if (type == "np") {
    .f    <- stats::wilcox.test
    .f.es <- effectsize::rank_biserial
  }

  # Set test arguments
  f.args <- alist(
    x           = vars$y1,
    y           = vars$y2,
    alternative = alternative,
    paired      = paired,
    var.equal   = var.equal,
    conf.level  = conf.level
  )

  # Set effectsize test arguments
  f.es.args <- alist(
    x           = vars$y1,
    y           = vars$y2,
    alternative = alternative,
    paired      = paired,
    pooled_sd   = var.equal,
    ci          = conf.level,
    verbose     = FALSE
  )

  # Run test
  .f    <- do.call(.f, f.args)
  .f.es <- do.call(.f.es, f.es.args)

  list(.f, .f.es)
}

writR:::two(
  data = youngSwimmers::youngswimmers,
  x = sex,
  y = weight,
  paired = FALSE
)

#' Utils for two()

get_vars <- function(data, x, y) {

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

  list(x = x_var,
       y = y_var,
       y1 = y_var_1,
       y2 = y_var_2)
}
