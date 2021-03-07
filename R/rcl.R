#' Reshape data from long format
#'
#' This function allows you to handle missing values and is a wrapper around `ipmisc::long_to_wide_converter()`.
#' @param data Your dataset in long format, can have missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param paired If the design is for paired groups, eliminates missing values at subject level (default FALSE).
#' @param spread TRUE will give you wide format output, otherwise long format (default FALSE).
#' @keywords rcl
#' @return A dataframe with NAs removed.

rcl <- function(data
               , variable
               , by
               , paired = FALSE
               , spread = FALSE) {

    ipmisc::long_to_wide_converter(
        data = data,
        x = {{by}},
        y = {{variable}},
        paired = paired,
        spread = spread)
  }
