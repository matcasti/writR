# Función: Pruebas de dos grupos independientes
bitwo <- function(data
                   , variable
                   , by
                   , type = 'auto'
                   , var.equal = FALSE
                   , trim = 0.1
                   , nboot = 100
                   , markdown = TRUE
                   , ...) {
  data <- rcl(data, {{variable}}, {{by}}, paired = FALSE); result <- list()

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
      if(type == 'check') {
        # Prueba de Levene ----
        hvar.test <- car::leveneTest(data[[variable]], data[[by]] )[1,3] > 0.05
        var.equal <- if(hvar.test) { TRUE } else { FALSE }
      }
      if(var.equal) {
        # T-Student, muestras independientes ----
        test <- stats::t.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
        d <- effectsize::effectsize(test, verbose = F)

        if(markdown) {

        result[['report']] <- paste0(
          '*t* ~Student~ (', janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', *p* ',  ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ", *Cohen's d* = ", janitor::round_half_up(d$d,2)
          , ', IC~95%~[', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
        } else {
        result[['report']] <- paste0(
          't(', janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001' , paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ", d = ", janitor::round_half_up(d$d,2)
          , ', IC95% [', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
        }

        result[['method']] <- 't de Student para muestras independientes';
        result

      } else {
        # T-Welch, muestras independientes ----
        test <- stats::t.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
        d <- effectsize::effectsize(test, verbose = F)

        if(markdown) {

        result[['report']] <- paste0(
          '*t* ~Welch~ (', janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ", *Cohen's d* = ",janitor::round_half_up(d$d,2)
          , ', IC~95%~[', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
      } else {
        result[['report']] <- paste0(
          't(', janitor::round_half_up(test$parameter,1)
          , ') = ', janitor::round_half_up(test$statistic,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ", d = ",janitor::round_half_up(d$d,2)
          , ', IC95% [', janitor::round_half_up(d$CI_low,2)
          , ', ', janitor::round_half_up(d$CI_high, 2), ']')
      }

        result[['method']] <- 't de Welch para muestras independientes'
        result

      }
    } else if(type == 'r') {
      # Prueba de Yuen de medias recortadas, muestras independientes ----
      test <- WRS2::yuen(
        formula = as.formula(paste0(variable,' ~ ',by))
        , data = data
        , tr = trim)
      r <- WRS2::yuen.effect.ci(
        formula = as.formula(paste0(variable,' ~ ',by))
        , data = data
        , tr = trim
        , nboot = nboot)

      if(markdown) {
        result[['report']] <- paste0(
          '*t* ~Yuen~ (', janitor::round_half_up(test$df, 2)
          , ') = ', janitor::round_half_up(test$test,2)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', $\\xi$ = ', janitor::round_half_up(r$effsize,2)
          , ', IC~95%~[', janitor::round_half_up(r$CI[1L],2)
          , ', ', janitor::round_half_up(r$CI[2L], 2), ']')
      } else {
        result[['report']] <- paste0(
          't(', janitor::round_half_up(test$df, 2)
          , ') = ', janitor::round_half_up(test$test,2)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001',paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', xi = ', janitor::round_half_up(r$effsize,2)
          , ', IC95% [', janitor::round_half_up(r$CI[1L],2)
          , ', ', janitor::round_half_up(r$CI[2L], 2), ']')
      }

      result[['method']] <- 'Prueba de Yuen de medias recortadas para muestras independientes'
      result
    } else {
      # U de Mann-Whitney ----
      test <- stats::wilcox.test(data[[variable]] ~ data[[by]], correct = T, exact = F, paired = F)
      r <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data)

      if(markdown) {

        result[['report']] <-  paste0(
          '*W* = ', janitor::round_half_up(test$statistic,3)
          , ', *p* ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', *r* ~biserial~ = ', janitor::round_half_up(r$r_rank_biserial,2)
          , ', IC~95%~[', janitor::round_half_up(r$CI_low,2)
          , ', ', janitor::round_half_up(r$CI_high, 2), ']')
      } else {
        result[['report']] <- paste0(
          'W = ', janitor::round_half_up(test$statistic,3)
          , ', p ', ifelse(test$p.value < 0.001, '< 0.001', paste(
            '=', janitor::round_half_up(test$p.value,3) ) )
          , ', r = ', janitor::round_half_up(r$r_rank_biserial,2)
          , ', IC95% [', janitor::round_half_up(r$CI_low,2)
          , ', ', janitor::round_half_up(r$CI_high, 2), ']')
      }

      result[['method']] <- 'Suma de rangos de Wilcoxon para muestras independientes'
      result

    }
}
