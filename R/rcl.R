#' Reshape data from long format
#'
#' This function allows you to handle missing values.
#' @param data Your dataset in long format, can have missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param paired If the design is for paired groups, eliminates missing values at subject level (default FALSE).
#' @keywords rcl
#' @return A dataframe with NAs removed.

rcl <- function(data,
                variable, # character vector
                by,       # idem
                paired = TRUE) {

  # Data preparation ----
  dt <- data.table::as.data.table(data)

  # Creating ID if paired data----
  if(paired) { dt[, rowid := factor(seq_len(.N)), by]

  # Core computation ----
  data.table::dcast( # Wide format
    data = dt,
    formula = stats::as.formula(
      object = paste("rowid ~", by)),
    value.var = variable
    )[, stats::na.omit(.SD) # Drop NA's
      ][, data.table::melt( # Stack it back together in long format
        data = .SD,
        id.vars = "rowid",
        value.name = variable,
        variable.name = by)]
  } else {
    # Drop NA's
    dt[, c(..by, ..variable)][, stats::na.omit(.SD)]
  }
}
