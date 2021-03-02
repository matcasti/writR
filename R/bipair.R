# Función: Pruebas de dos grupos relacionados
bipair <- function(data
                   , variable
                   , by
                   , type = 'auto'
                   , trim = 0.1
                   , nboot = 100
                   , markdown = TRUE
                   , ...) {

  data <- rcl(data, {{variable}}, {{by}}, paired = TRUE); result <- list()

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
      test <- stats::t.test(data[[variable]] ~ data[[by]], paired = T)
      d <- effectsize::effectsize(test, verbose = F)

      if(markdown) {
        result[['report']] <- paste0(
          '*t* ~Student~ (',janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=',janitor::round_half_up(test$p.value,3) ) )
          , ", *Cohen's d* = ", janitor::round_half_up(d$d,2)
          , ', IC~95%~[', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
      } else {
        result[['report']] <- paste0(
          't(', janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ", d = ",janitor::round_half_up(d$d,2)
          , ', IC95% [', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
      }

      result[['method']] <- 't de Student para muestras relacionadas'
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
        result[['report']] <- paste0(
          '*t* ~Yuen~ (', janitor::round_half_up(test$df, 3)
          , ') = ', janitor::round_half_up(test$test,3)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', $\\delta_R^{AKP}$ = ', janitor::round_half_up(r[['Est']],2)
          , ', IC~95%~[', janitor::round_half_up(r[['ci.low']],2)
          , ', ', janitor::round_half_up(r[['ci.up']], 2), ']')
      } else {
        result[['report']] <- paste0(
          't(', janitor::round_half_up(test$df, 3)
          , ') = ', janitor::round_half_up(test$test,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', delta = ', janitor::round_half_up(r[['Est']],2)
          , ', IC95% [', janitor::round_half_up(r[['ci.low']],2)
          , ', ', janitor::round_half_up(r[['ci.up']], 2), ']')
      }

      result[['method']] <- 'Prueba de Yuen de medias recortadas para muestras relacionadas'
      result
    } else {
      # Prueba de rangos con signo de Wilcoxon ----
      test <- stats::wilcox.test(data[[variable]] ~ data[[by]], correct = T, exact = F, paired = T)
      r <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data,
                                     paired = TRUE, verbose = FALSE)

      if(markdown) {
        result[['report']] <- paste0(
          '*V* = ', janitor::round_half_up(test$statistic,3)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', *r* ~biserial~ = ', janitor::round_half_up(r$r_rank_biserial,2)
          , ', IC~95%~[', janitor::round_half_up(r$CI_low,2)
          , ', ', janitor::round_half_up(r$CI_high, 2), ']')
      } else {
        result[['report']] <- paste0(
          'V = ', janitor::round_half_up(test$statistic,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', r = ', janitor::round_half_up(r$r_rank_biserial, 2)
          , ', IC95% [', janitor::round_half_up(r$CI_low,2)
          , ', ', janitor::round_half_up(r$CI_high, 2), ']')
      }

      result[['method']] <- 'Rangos con signo de Wilcoxon para muestras relacionadas'
      result
    }
}
