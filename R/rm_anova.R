#' Repeated measures ANOVA
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This functions allows you to perform a one way repeated measures ANOVA,
#' making adjustments for violations of sphericity, with no dependencies
#' related to `{afex}` or `{car}` packages.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character name for the grouping factor. Must be present in data
#' @param y Character name for the response variable. Must be present in data.
#' @param is_spherical Logical. checks whether to assume that the sphericity assumptions holds or not, if missing (the default) it will be tested using mauchly test with a threshold of 0.05.
#' @param adjust Character. correction for sphericity to be applied, it can be any character of length one starting with 'g' (indicating Greenhouse–Geisser correction) or 'h' (indicating Huynh–Feldt correction).
#' @param effsize.type Options are `"unbiased"` or `"omega"` for partial omega squared and `"biased"` or `"eta"` for partial eta squared as a measure of effect size. For non-parametric analysis, Kendalls' W is used for paired designs, where rank epsilon squared is used for independent groups designs.
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param character.only Logical. checks whether to use the unevaluated expression or its
#' content (when TRUE), asumming is a character vector. Defaults to `FALSE`.
#' @param ... Currently not used.
#'
#' @export

rm_anova <- function(data, x, y,
                     is_spherical,
                     adjust,
                     effsize.type = "omega",
                     conf.level = 0.95,
                     character.only = FALSE,
                     ...) {

  # data = clean_data(swimmers, "period", "bmi", "subject", paired = TRUE)
  # x = "period"
  # y = "bmi"
  # effsize.type = "omega"
  # conf.level = 0.95
  # character.only = TRUE

# Escape variables ----------------------------------------------------------------------------

  x <- deparser(x, character.only)
  y <- deparser(y, character.only)

# Clean and transform data --------------------------------------------------------------------

  wide <- do.call(
    what = data.frame,
    args = tapply(data[[y]], data[[x]], c)
  )

  wide <- as.matrix(wide)

# Run and specify model -----------------------------------------------------------------------

  mlm <- stats::lm(wide ~ 1)
  .SSD <- stats::SSD(mlm)

  x_lvl <- unique(x = data[[x]])
  idata <- `names<-`(as.data.frame(x_lvl), x)

  # ANOVA computation
  n <- nrow(wide)
  k <- ncol(wide)
  df1 <- k - 1
  df2 <- df1 * (n - 1)
  y_t <- mean(wide)
  # Systematic variance due to conditions
  y_j <- colMeans(wide)
  ss_condition <- n * sum((y_j - y_t)^2)
  ms_condition <- ss_condition / df1
  # Subject variance
  y_s <- rowMeans(wide)
  ss_subjects <- k * sum((y_s - y_t)^2)
  ms_subjects <- ss_subjects / (n - 1)
  # Unsystematic within-groups variance
  s_s <- apply(wide, 2, function(i) i - mean(i))
  ss_within <- sum(s_s ^ 2)
  ms_within <- ss_within / (k * (n - 1))
  # Unsystematic variance for the repeated measures
  ss_resid <- ss_within - ss_subjects
  ms_resid <- ss_resid / df2
  # F-ratio calculation
  f_ratio <- ms_condition / ms_resid

  # Effect size calculation
  if (effsize.type %in% c("biased", "eta")) {
    es <- ss_condition / (ss_resid + ss_condition)
    effectsize <- "Eta squared (partial)"
  } else if (effsize.type %in% c("unbiased", "omega")) {
    es <- (ss_condition - df1 * ms_resid) / (ss_condition + ss_resid + ss_subjects + ms_subjects)
    effectsize <- "Omega squared (partial)"
  }
  # Confidence interval
  es <- pmax(0, es)
  f <- (es / df1) / ((1 - es) / df2)
  ci <- effectsize::F_to_eta2(f, df1, df2, ci = conf.level)[-1]

# Corrections and diagnostics -----------------------------------------------------------------

  if (is.nan(f_ratio)) {
    warning(y, " is essentially constant", call. = FALSE)
    is_spherical <- TRUE
  }

  if (missing(is_spherical) || is.null(is_spherical)) {
    mch <- stats::mauchly.test(.SSD, idata = idata, X = ~ 1)
    is_spherical <- mch$p.value > 0.05
    is_spherical <- is.na(is_spherical) || is_spherical
  }

  if (!is_spherical) {
    correction <- sphericity(.SSD, idata = idata, X = ~ 1)[1:2]
    names(correction) <- c("Greenhouse-Geisser", "Huynh-Feldt")
    correction[which(correction > 1)] <- 1

    if (missing(adjust) || is.null(adjust)) {
      method <- if (all(correction < 0.75)) "Greenhouse-Geisser" else "Huynh-Feldt"
    } else {
      adjust <- tolower(adjust)
      if (grepl("^g", adjust)) method <- "Greenhouse-Geisser"
      if (grepl("^h", adjust)) method <- "Huynh-Feldt"
    }

    # Correction for degrees of freedom in demanded
    df1 <- df1 * correction[[method]]
    df2 <- df2 * correction[[method]]
  } else {
    method <- "Fisher"
  }

  # p-value calculation
  p <- stats::pf(f_ratio, df1, df2, lower.tail = FALSE)


# Pretty printing -----------------------------------------------------------------------------

  res <- list(
    statistic = f_ratio,
    p.value = p,
    method = paste0(method, "'s rmANOVA"),
    df_num = df1,
    df_den = df2,
    estimate = es,
    effectsize = effectsize,
    conf.level = conf.level,
    conf.low = ci[[2L]],
    conf.high = ci[[3L]]
  )

  return(res)
}
