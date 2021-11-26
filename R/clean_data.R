#' Remove NA's from long to wide/long data.table
#'
#' This function allows removing NA's from long format data into wide (or long) format
#' data, even suporting repeated measures designs (i.e., with more than one subject per
#' factor level).
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Unquoted name for the grouping factor. Must be present in data
#' @param y Unquoted name for the response variable. Must be present in data.
#' @param rowid Unquoted name for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param wide Logical to whether return a data.frame in wide format (`TRUE`, i.e. one columns per group/time) or in long format (`FALSE`).
#' @param character.only Logical to whether treat variables as names (with Non Standard Evaluation)
#' @param ... Currently ignored.
#'
#' @importFrom stats na.omit as.formula
#' @importFrom data.table rowidv setnames data.table dcast := melt
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

  # Check if `x` or `y` is null
  if (missing(x) || missing(y)) stop("`x` and `y` can't be null", call. = FALSE)


# If `character.only` == FALSE, then NSE is used ----------------------------------------------

  x <- substitute(x); y <- substitute(y)

  if (isFALSE(character.only)) {
    if (is.character(x) || is.character(y)) stop("When `character.only` is set to FALSE, both `x` and `y` must be unquoted variables of length one", call. = FALSE)
    x <- deparse(x); y <- deparse(y)
  } else {
    if ((is.name(x) | length(x) != 1) || (is.name(y) | length(y) != 1)) stop("When `character.only` is set to TRUE, both `x` and `y` must be quoted variables of length one", call. = FALSE)
  }

# Check if variables are present in data ------------------------------------------------------

  # Get names
  colum_names <- names(data)

  # Otherwise, check if is in data
  if (all(x != colum_names) || all(y != colum_names)) stop("`x` and `y` must be within your data. Check your arguments", call. = FALSE)

  # For latter use
  x_var <- data[[x]]

  # Check if rowid is present in the data if is paired and not null
  if (isTRUE(paired)) {
    if (missing(rowid) || is.null(rowid)) {
      rowid_var <- data.table::rowidv(x_var)
    } else {
      if (all(rowid != colum_names)) stop("`rowid` not found in data", call. = FALSE)
      rowid_var <- data[[rowid]]
    }
  } else if (isFALSE(paired)) {
    rowid_var <- NULL
  } else stop("`paired` must be TRUE or FALSE")

  # Add rowid if paired is TRUE and drop unused levels
  dt <- droplevels(
    x = stats::na.omit(
      object = data.table::setnames(
        x = data.table::data.table(
          x_col = x_var
          , y_col = data[[y]]
          , rowid = rowid_var
        )
        , old = c("x_col", "y_col")
        , new = c(x, y)
      )
    )
  )

  # Get levels for input data and output data
  data_x_levels <- levels(data[[x]])
  dt_x_levels <- levels(dt[[x]])

  # Check for empty levels
  if (length(data_x_levels) > length(dt_x_levels)) {
    drlv <- data_x_levels[!data_x_levels %in% dt_x_levels]
    # Display a warning if any
    warning(
      "Unused levels (i.e. ",
      paste0(drlv, collapse = ", "),
      ") were dropped",
      call. = FALSE
    )
  }

  if (isTRUE(paired)) {
    # Check if they're more than 1 observation per subject on each factor level
    if (any(table(dt[["rowid"]], dt[[x]]) > 1)) {
      stop("More than 1 observation per subject for at least one factor level, check your data with table()", call. = FALSE)
    }

    # Wide format - one row per subject and remove missing values
    dt <- stats::na.omit(
      object = data.table::dcast(
        data = dt,
        formula = stats::as.formula(
          object = paste("rowid ~", x)
        ),
        value.var = y
      )
    )[j = rowid := factor(x = rowid)][]

    # Si el output solicitado es formato ancho devuelve el resultado intermedio
    if (isTRUE(wide)) {
      # Formato ancho
      return(dt)
    } else if (isFALSE(wide)) {
      # De lo contrario en formato largo
      dt <- data.table::melt(
        data = dt,
        id.vars = "rowid",
        variable.name = x,
        value.name = y
      )
      return(dt)
    } else stop("`wide` must be TRUE or FALSE")
  }

  if (isTRUE(wide)) {

    p <- function(i, lng) { i[!is.na(i)][seq_len(lng)] }

    lng <- max(table(dt[[x]]), na.rm = TRUE)

    # Formato ancho y no pareado
    dt <- `attr<-`(
      x = tapply(dt[[y]], dt[[x]], p, lng, simplify = FALSE),
      which = "class",
      value = c("data.table", "data.frame")
    )
  }

  # Formato largo y no pareado
  return(dt)
}
