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
  dt <- data.table::data.table(variable = data[[variable]], by = data[[by]])
  data.table::setnames(dt, old = c("variable","by"), c(variable, by))

  # Creating ID ----
  dt[, rowid := factor(seq_len(.N)), by]

  # Drop NA's if paired = TRUE ----
  .f <- if(paired) stats::na.omit else function(x) x

  # Core computation ----
  data.table::dcast( # Wide format
    data = dt,
    formula = stats::as.formula(
      object = paste("rowid ~", by)),
    value.var = variable
    )[, .f(.SD) # Drop rows with NA's if paired
      ][, data.table::melt( # Stack it back together in long format
        data = .SD,
        id.vars = "rowid",
        value.name = variable,
        variable.name = by)]
}
