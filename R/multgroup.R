# Función: Pruebas de tres o más grupos independientes
multgroup <- function(data
                       , variable
                       , by
                       , type = 'auto'
                       , var.equal = FALSE
                       , trim = 0.1
                       , pairwise.comp = FALSE
                       , p.adjust = 'none'
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
      type <- if(n.test) { c('check') } else { 'np' }
    }
    if(type %in% c('p','check')) {
      if(type == 'check') {
        # Prueba de Levene ----
        hvar.test <- car::leveneTest(data[[variable]], data[[by]] )[1,3] > 0.05
        var.equal <- if(hvar.test) { TRUE } else { FALSE }
      }
      if(var.equal) {
        # ANOVA de Fisher, muestras independientes ----
        test <- stats::oneway.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
        eta <- effectsize::effectsize(test, verbose = FALSE, ci = 0.95)

        if(pairwise.comp) {
          # Post-Hoc: T-Student ----
          result[['post-hoc']] <- suppressWarnings(expr = { stats::pairwise.t.test(
            x = data[[variable]]
            , g = data[[by]]
            , p.adjust.method = p.adjust
            , paired = F) })
        }
        if(markdown) {
          result[['report']] <- paste0(
            '*F* ~Fisher~ (', janitor::round_half_up(test$parameter[1],1)
            ,', ', janitor::round_half_up(test$parameter[2],1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', *p* ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', $\\eta$^2^ = ', janitor::round_half_up(eta$Eta2,2)
            , ', IC~95%~[', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
        } else {
          result[['report']] <- paste0(
            'F(', janitor::round_half_up(test$parameter[1],1)
            , ', ', janitor::round_half_up(test$parameter[2],1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', p ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', eta^2 = ', janitor::round_half_up(eta$Eta2,2)
            , ', IC95% [', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
        }

        result[['method']] <- 'ANOVA de un factor de Fisher'
        result
      } else {
        # ANOVA de Welch, muestras independientes ----
        test <- stats::oneway.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
        eta <- effectsize::effectsize(test, verbose = FALSE, ci = 0.95)

        if(pairwise.comp) {
          # Post-Hoc: Games Howell ----
          result[['post-hoc']] <- suppressWarnings(
            expr = { PMCMRplus::gamesHowellTest(
              x = data[[variable]]
              , g = data[[by]]) })
        }
        if(markdown)  {
          result[['report']] <- paste0(
            '*F* ~Welch~ (', janitor::round_half_up(test$parameter[1],1)
            , ', ', janitor::round_half_up(test$parameter[2],1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', *p* ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', $\\eta$^2^ = ', janitor::round_half_up(eta$Eta2,2)
            , ', IC~95%~[', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
        } else {
          result[['report']] <- paste0(
            'F(',janitor::round_half_up(test$parameter[1],1)
            , ', ', janitor::round_half_up(test$parameter[2],1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', p ', ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', eta^2 = ', janitor::round_half_up(eta$Eta2,2)
            , ', IC95% [', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
        }

        result[['method']] <- 'ANOVA de un factor de Welch'
        result
      }
    } else if(type == 'r') {
      # ANOVA de medias recortadas, muestras independientes ----
      test <- WRS2::t1way(
        formula = as.formula(paste0(variable,' ~ ',by))
        , data = data
        , tr = trim)

      if(pairwise.comp) {
        # Post-Hoc ----
          result[['post-hoc']] <- suppressWarnings(
            expr = { WRS2::lincon(
              formula = as.formula(paste0(variable,' ~ ',by))
              , data = data
              , tr = trim) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '*F* ~Medias-recortadas~ (', janitor::round_half_up(test$df1,1)
            ,', ', janitor::round_half_up(test$df2,1)
            , ') = ', janitor::round_half_up(test$test,3)
            , ', *p* ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', $\\xi$ = ', janitor::round_half_up(test$effsize,2)
            , ', IC~95%~[', janitor::round_half_up(test$effsize_ci[1],2)
            , ', ', janitor::round_half_up(test$effsize_ci[2],2), ']')
        } else {
          result[['report']] <- paste0(
            'F(', janitor::round_half_up(test$df1,1)
            ,', ', janitor::round_half_up(test$df2,1)
            , ') = ', janitor::round_half_up(test$test,3)
            , ', p ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', xi = ', janitor::round_half_up(test$effsize,2)
            , ', IC95% [', janitor::round_half_up(test$effsize_ci[1],2)
            , ', ', janitor::round_half_up(test$effsize_ci[2],2), ']')
        }

      result[['method']] <- 'ANOVA de medias recortadas de un factor para muestras independientes'
      result
    } else {
      # Suma de rangos de Kruskal-Wallis, muestras independientes ----
      test <- stats::kruskal.test(data[[variable]] ~ data[[by]])
      epsilon <- effectsize::rank_epsilon_squared(data[[variable]] ~ data[[by]], data)

      if(pairwise.comp) {
        # Post-Hoc: Dunn test ----
          result[['post-hoc']] <- suppressWarnings(
            expr = { PMCMRplus::kwAllPairsDunnTest(
              x = data[[variable]]
              , g = data[[by]]
              , p.adjust.method = p.adjust) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '$\\chi$^2^ ~Kruskal-Wallis ~(', janitor::round_half_up(test$parameter,1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', *p* ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', $\\epsilon$^2^ = ', janitor::round_half_up(epsilon$rank_epsilon_squared,2)
            , ', IC~95%~[', janitor::round_half_up(epsilon$CI_low,2)
            , ', ', janitor::round_half_up(epsilon$CI_high,2), ']')
        } else {
          result[['report']] <- paste0(
            'X^2(', janitor::round_half_up(test$parameter,1)
            , ') = ', janitor::round_half_up(test$statistic,3)
            , ', p ',ifelse(test$p.value < 0.001, '< 0.001', paste(
              '=', janitor::round_half_up(test$p.value, 3) ) )
            , ', epsilon^2 = ', janitor::round_half_up(epsilon$rank_epsilon_squared,2)
            , ', IC95% [', janitor::round_half_up(epsilon$CI_low,2)
            , ', ', janitor::round_half_up(epsilon$CI_high,2), ']')
        }

      result[['method']] <- 'Suma de rangos de Kruskal-Wallis para muestras independientes'
      result
    }
}
