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
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @importFrom data.table %chin% as.data.table .SD fcase
#' @importFrom utils combn
#' @importFrom stats anova
#' @importFrom effectsize eta_squared omega_squared
#' @return A list with statistical results.
#' @export

aov_r <- function(data,
                  response = NULL,
                  between = NULL,
                  within = NULL,
                  rowid = NULL,
                  ss_type = 3,
                  effsize.type = "unbiased",
                  sphericity = "auto",
                  conf.level = 0.95,
                  lbl = if(is.null(markdown)) FALSE else TRUE,
                  markdown = NULL) {

  if(!"data.table" %chin% class(data)) {
    data <- data.table::as.data.table(data)
  }

  is.empty <- function(i) length(i) == 0

  data <- droplevels(data[j = .SD, .SDcols = c(rowid, response, between, within)])
  is.null(response) && stop("'response' can't be null", call. = FALSE)
  is.empty(between) && is.empty(within) && stop("Need to specify one of between or within factors", call. = FALSE)
  is.null(rowid) && stop("'rowid' can't be null", call. = FALSE)

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

  n_obs <- nrow(model$data$wide)

  if (is.empty(within)) {
    sphericity <- NULL
  } else {
    sphericity <- sphericity_check(model)
  }

  efs <- if (effsize.type %chin% c("eta", "biased")) {
    effectsize::eta_squared(model, ci = conf.level, verbose = FALSE)
  } else if (effsize.type %chin% c("omega", "unbiased")) {
    effectsize::omega_squared(model, ci = conf.level, verbose = FALSE)
  } else {
    stop('You have to choose between "eta" ("biased") or "omega" ("unbiased")', call. = FALSE)
  }

  model <- data.table::as.data.table(
    stats::anova(
      object = model
      , correction = sphericity
    )
  , keep.rownames = TRUE
  )

  within_method <- NULL
  if (!is.null(within)) {
    within_method <- data.table::fcase(
      sphericity == "none", "Fisher's repeated measures ANOVA",
      sphericity == "GG", "Repeated measures ANOVA with GG correction",
      sphericity == "HF", "Repeated measures ANOVA with HG correction"
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
    alternative = NA,
    estimate = efs[[2L]],
    conf.level = efs$CI,
    conf.low = efs$CI_low,
    conf.high = efs$CI_high,
    effectsize = names(efs)[[2L]],
    n_obs = n_obs
  )]

  if(lbl) {
    model <- model[j = lablr(.SD, markdown = markdown), by = "x"]
  }
  return(model)
}
