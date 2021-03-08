#' Reshape data from long format
#'
#' This function allows you to handle missing values and is a wrapper around `ipmisc::long_to_wide_converter()`.
#' @param data Your dataset in long format, can have missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param subject.id Ignored.
#' @param paired If the design is for paired groups, eliminates missing values at subject level (default FALSE).
#' @param spread TRUE will give you wide format output, otherwise long format (default FALSE).
#' @keywords rcl
#' @return A dataframe with NAs removed.

rcl <- function (
  data
  , variable
  , by
  , subject.id = NULL
  , paired = TRUE
  , spread = FALSE) {

  by <- rlang::ensym(by); variable <- rlang::ensym(variable)

  if (isFALSE(paired))
    subject.id <- NULL
    data <- dplyr::arrange(
      .data = dplyr::mutate(
        .data = dplyr::select(
          .data = data, {{by}}, {{variable}}, rowid = {{subject.id}}),
        `:=`({{by}}, droplevels(as.factor({{by}})))), {{by}})

    if (!"rowid" %in% names(data)) {
        if (isTRUE(paired))
            data <- dplyr::group_by(.data = data, {{by}})
        data <- dplyr::mutate(.data = data, rowid = dplyr::row_number())
    }
    data <- tidyr::unnest(
      data = dplyr::filter(
        .data = dplyr::nest_by(
          .data = dplyr::ungroup(data), rowid, .key = "df"),
        sum(is.na(df)) == 0),
      cols = c(df))

    if (spread && paired)
        data <- tidyr::pivot_wider(data = data, names_from = {{by}}, values_from = {{variable}})

      dplyr::arrange(
        .data = dplyr::as_tibble(dplyr::select(
          .data = data, rowid, dplyr::everything()), rowid))

}
