#' @title ANOVA for factorial designs
#' @description A wrapper around `afex::aov_ez` wich allows mix effects model.
#' @keywords aov_r
#'
#' @param data Your dataset in long format, can have some missing values.
#' @param response Response variable, numeric.
#' @param between Character indicating the between-subject(s) factor(s)/column(s) in data. Default is NULL indicating no between-subjects factors. Must be character vector if more than one between-subject(s) factor(s)/column(s) is specified.
#' @param within Character indicating the within-subject(s) factor(s)/column(s) in data. Default is NULL indicating no between-subjects factors. Must be character vector if more than one within-subject(s) factor(s)/column(s) is specified.
#' @param rowid Character (of length 1) indicating the subject identifier column in data.
#' @param ss_type Type of sum of squares for repeated measures ANOVA (defaults to 3). Possible values are "II", "III", 2, or 3.
#' @param effsize.type The effect size used to estimate the effects of the factors on the response variable. Possible values are "eta" ("biased") or "omega" ("unbiased", the default).
#' @param sphericity If "none", then sphericity assumption is assumed to be met for within-subject(s) factor(s). "GG": applies Greenhouse-Geisser correction. "HF": applies Hyunh-Feldt correction. 'auto' (Default) choose the appropiate correction based on Mauchly test of sphericity (p-value > 0.05)
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param character.only Logical. checks whether to use the unevaluated expression or its
#' content (when TRUE), asumming is a character vector. Defaults to `FALSE`.
#' @return A list with statistical results.
#'
#' @export

aov_r <- function(data,
                  response,
                  between = NULL,
                  within = NULL,
                  rowid = NULL,
                  ss_type = 3,
                  effsize.type = "unbiased",
                  sphericity = "auto",
                  conf.level = 0.95,
                  character.only = FALSE,
                  markdown) {

  `Pr(>F)` <- rn <- `num Df` <- `den Df` <- NULL

  # Argument checking - Response
  if (missing(response)) stop("response can't be NULL", call. = FALSE)

  # Transform data to data.table
  if (!"data.table" %chin% class(data)) data <- data.table::as.data.table(data)

  # Transform arguments to character
  response <- deparser(response, character.only)
  between  <- deparser(between, character.only)
  within   <- deparser(within, character.only)
  rowid    <- deparser(rowid, character.only)

  # Argument checking - arguments
  if (is.null(between) && is.null(within)) stop("between and within can't both be NULL", call. = FALSE)
  if (!is.null(within) && is.null(rowid)) stop("if within is provided, rowid can't be NULL", call. = FALSE)

  # Get only variables of interest
  data <- droplevels(data[, .SD, .SDcols = c(rowid, response, between, within)])

  if (length(within) == 0 && is.null(rowid) && length(between) > 0) {
    rowid <- "rowid"
    data[, rowid := seq_len(.N)]
  }

  # Run model
  model <- suppressMessages(
    suppressWarnings(
      afex::aov_ez(
        id = rowid,
        dv = response,
        data = data,
        between = between,
        within = within,
        type = ss_type
      )
    )
  )

  # Get number of observations
  n_obs <- nrow(model$data$wide)

  # Get sphericity correction
  if (length(within) == 0) {
    sphericity <- NULL
  } else {
    sphericity <- sphericity_check(model)
  }

  # Get effectsize
  efs <- if (effsize.type %chin% c("eta", "biased")) {
    effectsize::eta_squared(model, ci = conf.level, verbose = FALSE)
  } else if (effsize.type %chin% c("omega", "unbiased")) {
    effectsize::omega_squared(model, ci = conf.level, verbose = FALSE)
  } else {
    stop('You have to choose between "eta" ("biased") or "omega" ("unbiased")', call. = FALSE)
  }

  # Get ANOVA table
  model <- data.table::as.data.table(
    stats::anova(
      object = model
      , correction = sphericity
    ), keep.rownames = TRUE
  )

  # Set sphericity correction (if available)
  within_method <- NA_character_
  if (!is.null(within)) {
    within_method <- data.table::fcase(
      sphericity == "none", "Fisher's rmANOVA",
      sphericity == "GG", "Greenhouse-Geisser's rmANOVA",
      sphericity == "HF", "Huynh-Feldt's rmANOVA"
    )
  }

  model <- model[j = list(
    y = response,
    x = rn,
    statistic = F,
    df = `num Df`,
    df.error = `den Df`,
    p.value = `Pr(>F)`,
    method = data.table::fcase(rn %chin% between, "Fisher's ANOVA",
                               rn %chin% within, within_method,
                               utils::combn(within, 1, grepl, T, rn), within_method),
    estimate = efs[[2L]],
    conf.level = efs$CI,
    conf.low = efs$CI_low,
    conf.high = efs$CI_high,
    effectsize = names(efs)[[2L]],
    n_obs = n_obs
  )]

  if (!missing(markdown) && isTRUE(markdown)) {
    model <- model[j = lablr(.SD, markdown = markdown), by = "x"]
  }

  return(model)
}

