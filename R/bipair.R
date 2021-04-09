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

      desc <- if(markdown) {
        list(m = '$M$ = ', i = ', $SD$ = ', t = '$t_{~Student}$ (', p = ', $p$ '
             , d = "$d_{~Cohen}$ = ", ci = ', CI~95%~[') } else {
          list(m = 'M = ', i = ', SD = ', t = 't(', p = ', p '
               , d = "d = ", ci = ', CI95% [') }


      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        desc$m
        , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
        , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

        result[['full']] <- paste0(
          stats <- paste0(desc$t,round(test$parameter,1)
          , ') = ', round(test$statistic,2)
          , desc$p, ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=',round(test$p.value,3) ) ) ),', '
          , es <- paste0(desc$d, round(d$d,2)
          , desc$ci, round(d$CI_low,2)
          , ', ', round(d$CI_high, 2), ']') )
        result[["stats"]] <- stats
        result[["es"]] <- es

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

      desc <- if(markdown) {
        list(m = '$M$ = ', i = ', $SD$ = ', t = '$t_{~Yuen}$ (', p = ', $p$ '
             , d = '$\\delta_R^{AKP}$ = ', ci = ', CI~95%~[') } else {
          list(m = 'M = ', i = ', SD = ', t = 't(', p = ', p '
               , d = "delta = ", ci = ', CI95% [') }

      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        desc$m
        , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T, trim = trim), 2), desc$i
        , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

        result[['full']] <- paste0(
          stats <- paste0(desc$t, round(test$df, 1)
          , ') = ', round(test$test,2)
          , desc$p, ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0(desc$d, round(r[['Est']],2)
          , desc$ci, round(r[['ci.low']],2)
          , ', ', round(r[['ci.up']], 2), ']') )
        result[["stats"]] <- stats
        result[["es"]] <- es

      result[['method']] <- "Yuen's test for trimmed means for dependent samples"
      result
    } else {
      # Prueba de rangos con signo de Wilcoxon ----
      test <- stats::wilcox.test(stats::as.formula(paste(variable, by, sep = '~')), data, correct = T, exact = F, paired = T)
      r <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data,
                                     paired = TRUE, verbose = FALSE)

      desc <- if(markdown) {
        list(m = '$Mdn$ = ', i = ', $IQR$ = ', v = '$V$ = ', p = ', $p$ '
             , r = '$r_{~biserial}$ = ', ci = ', CI~95%~[') } else {
          list(m = 'Mdn = ', i = ', IQR = ', v = 'V = ', p = ', p '
               , r = 'r = ', ci = ', CI95% [') }

      for(j in levels(data[[by]])) {
      result[['desc']][j] <- list(paste0(
        desc$m
        , round(stats::median(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
        , round(stats::IQR(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

        result[['full']] <- paste0(
          stats <- paste0(desc$v, round(test$statistic,2)
          , desc$p, ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', round(test$p.value,3) ) ) ),', '
          , es <- paste0(desc$r, round(r$r_rank_biserial,2)
          , desc$ci, round(r$CI_low,2)
          , ', ', round(r$CI_high, 2), ']') )
        result[["stats"]] <- stats
        result[["es"]] <- es


      result[['method']] <- 'Wilcoxon signed-rank test for dependent samples'
      result
    }
}
