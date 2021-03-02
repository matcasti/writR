# Paquetes requeridos para el funcionamiento de esta función
  # require(ipmisc)
  # require(nortest)
  # require(stats)
  # require(effectsize)
  # require(janitor)
  # require(car)
  # require(afex)
  # require(purrr)
  # require(PMCMRplus)
  # require(WRS2)



# Función: Pruebas de dos grupos independientes
.bitwo <- function(data
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

# Función: Pruebas de dos grupos  ========================================
.bi <- function(data
                , variable
                , by
                , paired = FALSE
                , type = 'auto'
                , var.equal = FALSE
                , trim = 0.1
                , nboot = 100
                , markdown = TRUE
                , ...) {

  if(paired) {
    # Muestras relaciondas ----
    .bipair(data = data
            , variable = {{variable}}
            , by = {{by}}
            , type = type
            , trim = trim
            , nboot = nboot
            , markdown = markdown
            , ...)
  } else {
    # Muestras independientes ----
    .bitwo(data = data
           , variable = {{variable}}
           , by = {{by}}
           , type = type
           , var.equal = var.equal
           , trim = trim
           , nboot = nboot
           , markdown = markdown
           , ...)
  }
}


# Función: Pruebas de tres o más grupos relacionados
.multpair <- function(data
                      , variable
                      , by
                      , type = 'auto'
                      , trim = 0.1
                      , sphericity = 'GG'
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
      type <- if(n.test) { 'check' } else { 'np' }
    }
    if(type %in% c('p','check')) {
      # ANOVA, medidas repetidas ----
      model <- afex::aov_ez(id = 'rowid', {{variable}}, data, within = {{by}})
      eta <- effectsize::eta_squared(model, ci = 0.95)
      spher.test <- suppressWarnings(expr = { afex::test_sphericity(model) })

      if(pairwise.comp) {
        # Post-Hoc: T-Student ----
          result[['post-hoc']] <- suppressWarnings(expr = { stats::pairwise.t.test(
            x = data[[variable]]
            , g = data[[by]]
            , p.adjust.method = p.adjust
            , paired = T) })
      }

      sphericity <- if(type == 'check') {
        # Comprobación de esfericidad ----
        if( purrr::is_empty(spher.test) || spher.test[[2]] > 0.05 ) {
          TRUE } else { ges <- model$anova_table$ges <= 0.75
          if(ges) { 'GG' } else { 'HF' } }
        } else if(purrr::is_empty(spher.test)) { TRUE } else { sphericity }

      if(isTRUE(sphericity)) {
        # ANOVA de Fisher, medidas repetidas ----
        suppressWarnings(expr = {output <- anova(model, correction = 'none')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Fisher~ (', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC~95%~[', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC95% [', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          }

        result[['method']] <- 'ANOVA de medidas repetidas de Fisher'
        result
      } else if(sphericity == 'GG') {
        # ANOVA de Greenhouse-Geisser, medidas repetidas ----
        suppressWarnings(expr = {output <- anova(model, correction = 'GG')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Greenhouse-Geisser~ (', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC~95%~[', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC95% [', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          }

        result[['method']] <- 'ANOVA de medidas repetidas de Greenhouse-Geisser'
        result
      } else {
        # ANOVA de Huynh-Feldt, medidas repetidas ----
        suppressWarnings(expr = {output <- anova(model, correction = 'HF')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Huynh-Feldt~ (', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC~95%~[', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', janitor::round_half_up(output[["num Df"]],1)
              , ', ', janitor::round_half_up(output[["den Df"]],1)
              , ') = ', janitor::round_half_up(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', janitor::round_half_up(eta$Eta2_partial,3)
            , ', IC95% [', janitor::round_half_up(eta$CI_low,2)
            , ', ', janitor::round_half_up(eta$CI_high,2), ']')
          }

        result[['method']] <- 'ANOVA de medidas repetidas de Huynh-Feldt'
        result
      }
    } else if(type == 'r') {
      # ANOVA de medias recortadas, medidas repetidas ----
      output <- WRS2::rmanova(y = data[[variable]],
                              groups = data[[by]],
                              blocks = data[['rowid']],
                              tr = trim)

      # Tamaño de efecto no disponible para RMANOVA
      # r <- effectsize::kendalls_w({{variable}}, {{by}}, 'rowid', data)

      if(pairwise.comp) {
        # Post-Hoc ----
        result[['post-hoc']] <- suppressWarnings(
          expr = { WRS2::rmmcp(y = data[[variable]],
            groups = data[[by]],
            blocks = data[['rowid']],
            tr = trim) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '*F* ~Medias-recortadas~ (', janitor::round_half_up(output$df1,1)
            ,', ', janitor::round_half_up(output$df2,1)
            , ') = ',janitor::round_half_up(output$test,3)
            , ', *p* ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  janitor::round_half_up(output$p.value, 3) ) ) )
          } else {
            result[['report']] <- paste0(
            'F(', janitor::round_half_up(output$df1,1)
            ,', ', janitor::round_half_up(output$df2,1)
            , ') = ',janitor::round_half_up(output$test,3)
            , ', p ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  janitor::round_half_up(output$p.value, 3) ) ) )
          }

      result[['method']] <- 'ANOVA de medias recortadas de un factor para medidas repetidas'
      result
    } else {
      # Suma de rangos de Friedman, medidas repetidas ----
      output <- stats::friedman.test(y = data[[variable]], groups = data[[by]], blocks = data[['rowid']])
      kendall <- effectsize::kendalls_w({{variable}}, {{by}}, 'rowid', data)

      if(pairwise.comp) {
        # Post-Hoc: Durbin test ----
        result[['post-hoc']] <- suppressWarnings(
          expr = { PMCMRplus::durbinAllPairsTest(
          y = data[[variable]]
          , groups = data[[by]]
          , blocks = data[['rowid']]
          , p.adjust.method = p.adjust) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '$\\chi$^2^ ~Friedman~ (', janitor::round_half_up(output$parameter,1)
            , ') = ',janitor::round_half_up(output$statistic,3)
            , ', *p* ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  janitor::round_half_up(output$p.value, 3) ) )
            , ', *W* ~Kendall~ = ', janitor::round_half_up(kendall$Kendalls_W, 2)
            , ", IC~95%~[", janitor::round_half_up(kendall$CI_low,2)
            , ', ', janitor::round_half_up(kendall$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'X^2(',janitor::round_half_up(output$parameter,1)
              , ') = ', janitor::round_half_up(output$statistic,3)
              , ', p ', ifelse(output$p.value < 0.001, '< 0.001', paste(
                '=', janitor::round_half_up(output$p.value, 3) ) )
              , ', W = ', janitor::round_half_up(kendall$Kendalls_W, 2)
            , ", IC95% [", janitor::round_half_up(kendall$CI_low,2)
            , ', ', janitor::round_half_up(kendall$CI_high,2), ']')
          }

      result[['method']] <- 'Suma de rangos de Friedman para muestras relacionadas'
      result
    }
}

# Función: Pruebas de tres o más grupos independientes
.multgroup <- function(data
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

# Función: Pruebas de tres o más grupos  ========================================
.mult <- function(data
                , variable
                , by
                , paired = FALSE
                , type = 'auto'
                , var.equal = FALSE
                , trim = 0.1
                , sphericity = 'GG'
                , pairwise.comp = FALSE
                , p.adjust = 'none'
                , markdown = TRUE
                , ...) {

  if(paired) {
    # Medidas repetidas ----
    .multpair(data = data
              , variable = {{variable}}
              , by = {{by}}
              , type = type
              , trim = trim
              , sphericity = sphericity
              , pairwise.comp = pairwise.comp
              , p.adjust = p.adjust
              , markdown = markdown
              , ...)
  } else {
    # Muestras independientes ----
    .multgroup(data = data
               , variable = {{variable}}
               , by = {{by}}
               , type = type
               , var.equal = var.equal
               , trim = trim
               , pairwise.comp = pairwise.comp
               , p.adjust = p.adjust
               , markdown = markdown
               , ...)
  }
}

#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#------------------------- Función principal --------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#

# Test para muestras independientes o relacionadas
report <- function(data
                    , variable
                    , by
                    , paired = FALSE
                    , type = 'auto'
                    , var.equal = FALSE
                    , trim = 0.1
                    , nboot = 100
                    , sphericity = 'GG'
                    , pairwise.comp = FALSE
                    , p.adjust = 'none'
                    , markdown = TRUE
                    , ...) {

  if(nlevels(data[[by]]) >= 3) {
    .mult(
      data = data
      , variable = {{variable}}
      , by = {{by}}
      , paired = paired
      , type = type
      , var.equal = var.equal
      , sphericity = sphericity
      , pairwise.comp = pairwise.comp
      , p.adjust = p.adjust
      , markdown = markdown
      , ... )
  } else {
    .bi(
      data = data
      , variable = {{variable}}
      , by = {{by}}
      , paired = paired
      , type = type
      , var.equal = var.equal
      , markdown = markdown)
  }
}
