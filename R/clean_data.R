#' @title Data table without NA's and with rowid (paired designs only).
#' @name clean_data
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Unquoted name for the grouping factor. Must be present in data
#' @param y Unquoted name for the response variable. Must be present in data.
#' @param rowid Unquoted name for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param wide Logical to whether return a data.frame in wide format (`TRUE`, i.e. one columns per group/time) or in long format (`FALSE`).
#' @param ... Currently ignored.
#' @importFrom stats na.omit as.formula
#' @importFrom data.table rowidv setnames data.table dcast := melt
#' @export

clean_data <- function(data, x, y,
                       rowid = NULL,
                       paired = FALSE,
                       wide = FALSE,
                       ...) {

  arg <- match.call()

  # Check if data is null
  is.null(arg$data) && stop("'data' can't be null", call. = F)

  # Get names
  n_dt <- names(data)

  # Check if x or y is null
  is.null(arg$x) || is.null(arg$y) && stop("'x' and 'y' can't be null", call. = F)

  # For latter use
  x_var <- data[[x]]

  # Otherwise, check if is in data
  all(x != n_dt) || all(y != n_dt) && stop("'x' and 'y' must be within your data. Check your arguments", call. = F)

  # Check if rowid is present in the data if is paired and not null
  if(paired) {
    if(is.null(rowid)) {
      rowid_var <- data.table::rowidv(x_var)
    } else {
      all(rowid != n_dt) && stop(rowid, " not found in data", call. = F)
      rowid_var <- data[[rowid]]
    }
  } else {
    rowid_var <- NULL
  }

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
  data_x_levels <- levels(data[[arg$x]])
  dt_x_levels <- levels(dt[[arg$x]])

  # Check for empty levels
  if(length(data_x_levels) > length(dt_x_levels)) {
    drlv <- data_x_levels[!data_x_levels %in% dt_x_levels]
    # Display a warning if any
    warning(
      "Unused levels (i.e. ",
      paste0(drlv, collapse = ", "),
      ") were dropped",
      call. = FALSE
    )
  }

  if (paired) {
    # Check if they're more than 1 observation per subject on each factor level
    if (any(table(dt[["rowid"]], dt[[x]]) > 1)) {
      stop("More than 1 observation per subject for at least one factor level, check your data with table()", call. = F)
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
    if (wide) {
      # Formato ancho
      return(dt)
    } else {
      # De lo contrario en formato largo
      dt <- data.table::melt(
        data = dt,
        id.vars = "rowid",
        variable.name = x,
        value.name = y
      )
      return(dt)
    }
  }

  if (wide) {
    p <- function(i, lng) {
      i[!is.na(i)][seq_len(lng)]
    }
    lng <- max(table(dt[[x]]))
    # Formato ancho y no pareado
    dt <- `attr<-`(
      x = tapply(dt[[y]], dt[[x]], p, lng, simplify = FALSE),
      which = "class",
      value = c("data.table", "data.frame")
    )

    return(dt)
  } else {
    # Formato largo y no pareado
    return(dt)
  }
}
