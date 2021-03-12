#' Paired two samples testing
#'
#' This is function let you perform automated inferential testing based on certain assumptions, some of which are tested automatically, then the propper test is perform, giving you an APA formated output with your statistical results.
#' @param data Your dataset in long format, can have some missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param type Whether you want to manually specify a parametric test (type = 'p'), a non-parametric test (type = 'np') or a robust test (type = 'r').
#' @param trim Trim level for the mean (available only for robust test).
#' @param nboot Number of bootstrap samples.
#' @param markdown Whether you want the `$report` output formated for inline RMarkdown or as plain text.
#' @param ... Currently not used.
#' @keywords bipair
#' @return A list of length 2 with `$report` of statistical test and `$method` used.

bipair <- function(data
                   , variable
                   , by
                   , type = 'auto'
                   , trim = 0.1
                   , nboot = 100
                   , markdown = TRUE
                   , ...) {

  data <- rcl(data = data
              , variable = {{variable}}
              , by = {{by}}
              , paired = TRUE
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
      type <- if(n.test) { 'p' } else { 'np' }
    }
    if(type == 'p') {
      # T-Student, muestras relacionadas ----
      test <- stats::t.test(stats::as.formula(paste(variable, by, sep = '~')), data, paired = T)
      d <- effectsize::effectsize(test, verbose = F)

      if(markdown) {
        result[['full']] <- paste0(
          stats <- paste0('*t* ~Student~ (',round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=',round(test$p.value,3) ) ) ),', '
          , es <- paste0("*d* ~Cohen's~ = ", round(d$d,2)
          , ', CI~95%~[', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('t(', round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0("d = ",round(d$d,2)
          , ', CI95% [', round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }

      result[['method']] <- "Student's t-test for dependent samples"
      result
    } else if(type == 'r') {
      # Prueba de Yuen de medias recortadas, muestras dependientes ----
      test <- WRS2::yuend(
        x = data[[variable]][data[[by]] == levels(data[[by]])[1L]]
        , y = data[[variable]][data[[by]] == levels(data[[by]])[2L]]
        , tr = trim)
      r <- WRS2::dep.effect( # robust standardized difference
        x = data[[variable]][data[[by]] == levels(data[[by]])[1L]]
        , y = data[[variable]][data[[by]] == levels(data[[by]])[2L]]
        , tr = trim
        , nboot = nboot)['AKP',]

      if(markdown) {
        result[['full']] <- paste0(
          stats <- paste0('*t* ~Yuen~ (', round(test$df, 1)
          , ') = ', round(test$test,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('$\\delta_R^{AKP}$ = ', round(r[['Est']],2)
          , ', CI~95%~[', round(r[['ci.low']],2)
          , ', ', round(r[['ci.up']], 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('t(', round(test$df, 1)
          , ') = ', round(test$test,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('delta = ', round(r[['Est']],2)
          , ', CI95% [', round(r[['ci.low']],2)
          , ', ', round(r[['ci.up']], 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }
      result[['method']] <- "Yuen's test for trimmed means for dependent samples"
      result
    } else {
      # Prueba de rangos con signo de Wilcoxon ----
      test <- stats::wilcox.test(stats::as.formula(paste(variable, by, sep = '~')), data, correct = T, exact = F, paired = T)
      r <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data,
                                     paired = TRUE, verbose = FALSE)

      if(markdown) {
        result[['full']] <- paste0(
          stats <- paste0('*V* = ', round(test$statistic,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('*r* ~biserial~ = ', round(r$r_rank_biserial,2)
          , ', CI~95%~[', round(r$CI_low,2)
          , ', ', round(r$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      } else {
        result[['full']] <- paste0(
          stats <- paste0('V = ', round(test$statistic,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0('r = ', round(r$r_rank_biserial, 2)
          , ', CI95% [', round(r$CI_low,2)
          , ', ', round(r$CI_high, 2), ']') )

        result[["stats"]] <- stats
        result[["es"]] <- es
      }

      result[['method']] <- 'Wilcoxon signed-rank test for dependent samples'
      result
    }
}
