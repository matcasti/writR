#' Reshape data from long format
#'
#' This function allows you to handle missing values.
#' @param data Your dataset in long format, can have missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param paired If the design is for paired groups, eliminates missing values at subject level (default FALSE).
#' @keywords rcl
#' @return A tibble with NAs removed.

rcl <- function(data,
                variable, # character vector
                by,       # idem
                paired = TRUE) {

  # Data preparation ----
  dt <- data.table::as.data.table(data
          )[,c(..variable, ..by)]

  # Creating ID if paired data----
  dt <- if(paired) {

  dt <- dt[, rowid := factor(seq_len(.N)), by
     ][, na.omit(.SD)][,(by) := droplevels(get(by))]

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
        variable.name = by)
        ][, rowid := droplevels(rowid)
          ][, dplyr::as_tibble(.SD)]
  } else {
    # Drop NA's
    dt[, c(..by, ..variable)
       ][, stats::na.omit(.SD)
         ][, dplyr::as_tibble(.SD)]
  }

  if(nlevels(data[[by]]) > nlevels(dt[[by]])) {
    drlv <- levels(data[[by]])[!levels(data[[by]]) %in% levels(dt[[by]])]
    warning("Levels ", paste0(drlv, collapse = ", ")," were removed", immediate. = F)
  }

  return(dt)
}
