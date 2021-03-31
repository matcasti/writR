#' Independent two samples testing
#'
#' This is function let you perform automated inferential testing based on certain assumptions, some of which are tested automatically, then the propper test is perform, giving you an APA formated output with your statistical results.
#' @param data Your dataset in long format, can have some missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param type Whether you want to manually specify a parametric test (type = 'p'), a non-parametric test (type = 'np') or a robust test (type = 'r').
#' @param var.equal If `TRUE`, then Welch correction is applied to the degrees of freedom, only when `type = 'p'`.
#' @param trim Trim level for the mean (available only for robust test).
#' @param nboot Number of bootstrap samples used for robust methods.
#' @param markdown Whether you want the `$report` output formated for inline RMarkdown or as plain text.
#' @param ... Currently not used.
#' @keywords bitwo
#' @return A list of length 2 with `$report` of statistical test and `$method` used.

bitwo <- function(data
                   , variable
                   , by
                   , type = 'auto'
                   , var.equal = FALSE
                   , trim = 0.1
                   , nboot = 100
                   , markdown = TRUE
                   , ...) {

  data <- rcl(data = data
              , variable = {{variable}}
              , by = {{by}}
              , paired = FALSE
              , spread = FALSE)

  result <- list()

    if(type == 'auto') {
      # Prueba de normalidad ----
      n.test <- all( tapply(
        X = data[[variable]],
        INDEX = list(data[[by]]),
        FUN = function(x) if(length(x) < 50) {
          stats::shapiro.test(x)$p.value } else {
          nortest::lillie.test(x)$p.value } ) > 0.05 )
      # Tipo de test
      type <- if(n.test) { c('check') } else { 'np' }
    }
    if(type %in% c('p','check')) {

      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        'M = '
        , round(mean(data[data[[by]] == j,][[variable]], na.rm = T),2), ', SD = '
        , round(sd(data[data[[by]] == j,][[variable]], na.rm = T),2) ) ) }

      if(type == 'check') {
        # Prueba de Levene ----
        hvar.test <- car::leveneTest(data[[variable]], data[[by]] )[1,3] > 0.05
        var.equal <- if(hvar.test) { TRUE } else { FALSE }
      }
      if(var.equal) {
        # T-Student, muestras independientes ----
        test <- stats::t.test(stats::as.formula(paste(variable, by, sep = "~")), data, var.equal = TRUE)
        d <- effectsize::effectsize(test, verbose = F)

        if(markdown) {

        result[['full']] <- paste0(
          stats <- paste0('*t* ~Student~ (', round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', *p* ',  ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0("*d* ~Cohen's~ = ", round(d$d,2)
          , ', CI~95%~[', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
        } else {
        result[['full']] <- paste0(
          stats <- paste0('t(', round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001' , paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0("d = ", round(d$d,2)
          , ', CI95% [', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
        }

        result[['method']] <- "Student's t-test for independent samples";
        result

      } else {
        # T-Welch, muestras independientes ----
        test <- stats::t.test(stats::as.formula(paste(variable, by, sep = '~')), data, var.equal = FALSE)
        d <- effectsize::effectsize(test, verbose = F)

        if(markdown) {
        result[['full']] <- paste0(
          stats <- paste0('*t* ~Welch~ (', round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0("*d* ~Cohen's~ = ",round(d$d,2)
          , ', CI~95%~[', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('t(', round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0("d = ",round(d$d,2)
          , ', CI95% [', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }

        result[['method']] <- "Welch's t-test for independent samples"
        result

      }
    } else if(type == 'r') {
      # Prueba de Yuen de medias recortadas, muestras independientes ----
      test <- WRS2::yuen(
        formula = stats::as.formula(paste0(variable,' ~ ',by))
        , data = data
        , tr = trim)
      r <- WRS2::yuen.effect.ci(
        formula = stats::as.formula(paste0(variable,' ~ ',by))
        , data = data
        , tr = trim
        , nboot = nboot)

      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        'M = '
        , round(mean(data[data[[by]] == j,][[variable]], na.rm = T, trim = trim),2), ', SD = '
        , round(sd(data[data[[by]] == j,][[variable]], na.rm = T),2) ) ) }

      if(markdown) {
        result[['full']] <- paste0(
          stats <- paste0('*t* ~Yuen~ (', round(test$df, 2)
          , ') = ', round(test$test,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('$\\xi$ = ', round(r$effsize,2)
          , ', CI~95%~[', round(r$CI[1L],2)
          , ', ', round(r$CI[2L], 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('t(', round(test$df, 2)
          , ') = ', round(test$test,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('xi = ', round(r$effsize,2)
          , ', CI95% [', round(r$CI[1L],2)
          , ', ', round(r$CI[2L], 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }

      result[['method']] <- "Yuen's test for trimmed means for independent samples"
      result
    } else {
      # U de Mann-Whitney ----
      test <- stats::wilcox.test(stats::as.formula(paste(variable, by, sep = "~")), data, correct = T, exact = F, paired = F)
      r <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data)

      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        'Mdn = '
        , round(median(data[data[[by]] == j,][[variable]], na.rm = T),2), ', IQR = '
        , round(IQR(data[data[[by]] == j,][[variable]], na.rm = T),2) ) ) }

      if(markdown) {
        result[['full']] <-  paste0(
          stats <- paste0('*W* = ', round(test$statistic,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('*r* ~biserial~ = ', round(r$r_rank_biserial,2)
          , ', CI~95%~[', round(r$CI_low,2)
          , ', ', round(r$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('W = ', round(test$statistic,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('r = ', round(r$r_rank_biserial,2)
          , ', CI95% [', round(r$CI_low,2)
          , ', ', round(r$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }
      result[['method']] <- 'Mann Whitney U test for independent samples'
      result
    }
}
