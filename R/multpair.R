#' Dependent K samples testing
#'
#' This is function let you perform automated inferential testing based on certain assumptions, some of which are tested automatically, then the propper test is perform, giving you an APA formated output with your statistical results.
#' @param data Your dataset in long format, can have some missing values.
#' @param variable Response variable, numeric.
#' @param by Grouping variable, a factor. It can have more than two levels.
#' @param type Whether you want to manually specify a parametric test (type = 'p'), a non-parametric test (type = 'np') or a robust test (type = 'r').
#' @param trim Trim level for the mean (available only for robust test).
#' @param sphericity If `TRUE`, then sphericity assumption is assumed to be met in paired designs, only when `type = 'p'`. "GG": applies Greenhouse-Geisser correction. "HF": applies Hyunh-Feldt correction.
#' @param pairwise.comp Logical. For pairwise comparisons (i.e. post-hoc; default is FALSE).
#' @param p.adjust see `p.adjust.methods`.
#' @param markdown Whether you want the `$report` output formated for inline RMarkdown or as plain text.
#' @param ... Currently not used.
#' @keywords multpair
#' @return A list of length 2 with `$report` of statistical test and `$method` used, or length 3 if `pairwise.comp = TRUE`.

multpair <- function(data
                      , variable
                      , by
                      , type = 'auto'
                      , trim = 0.1
                      , sphericity = 'GG'
                      , pairwise.comp = FALSE
                      , p.adjust = 'none'
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
      type <- if(n.test) { 'check' } else { 'np' }
    }
    if(type %in% c('p','check')) {
      # ANOVA, medidas repetidas ----
      model <- afex::aov_ez(id = 'rowid',
                            dv = as.character(variable),
                            data = data,
                            within = as.character(by))
      eta <- effectsize::eta_squared(model, ci = 0.95)
      spher.test <- suppressWarnings(expr = { afex::test_sphericity(model) })

      if(pairwise.comp) {
        # Post-Hoc: T-Student ----
          result[['post-hoc']] <- suppressWarnings(expr = { parameters::parameters(
            stats::pairwise.t.test(
            x = data[[variable]]
            , g = data[[by]]
            , p.adjust.method = p.adjust
            , paired = T)) })
      }

      sphericity <- if(type == 'check') {
        # Comprobación de esfericidad ----
        if( purrr::is_empty(spher.test) || spher.test[[2]] > 0.05 ) {
          TRUE } else { ges <- model$anova_table$ges <= 0.75
          if(ges) { 'GG' } else { 'HF' } }
        } else if(purrr::is_empty(spher.test)) { TRUE } else { sphericity }

      if(isTRUE(sphericity)) {
        # ANOVA de Fisher, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'none')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Fisher~ (', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', round(eta$Eta2_partial,3)
            , ', IC~95%~[', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', round(eta$Eta2_partial,3)
            , ', IC95% [', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          }

        result[['method']] <- 'One-way repeated measures ANOVA'
        result
      } else if(sphericity == 'GG') {
        # ANOVA de Greenhouse-Geisser, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'GG')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Greenhouse-Geisser~ (', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', round(eta$Eta2_partial,3)
            , ', IC~95%~[', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', round(eta$Eta2_partial,3)
            , ', IC95% [', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          }

        result[['method']] <- 'One-way repeated measures ANOVA with Greenhouse-Geisser correction'
        result
      } else {
        # ANOVA de Huynh-Feldt, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'HF')})

        if(markdown) {
            result[['report']] <- paste0(
              '*F* ~Huynh-Feldt~ (', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', *p* ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', $\\eta$^2^ = ', round(eta$Eta2_partial,3)
            , ', IC~95%~[', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'F(', round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],3)
              , ', p ',ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) )
              , ', eta^ = ', round(eta$Eta2_partial,3)
            , ', IC95% [', round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']')
          }

        result[['method']] <- 'One-way repeated measures ANOVA with Huynh-Feldt correction'
        result
      }
    } else if(type == 'r') {
      # ANOVA de medias recortadas, medidas repetidas ----
      output <- WRS2::rmanova(y = data[[variable]],
                              groups = data[[by]],
                              blocks = data[['rowid']],
                              tr = trim)

      # Tamaño de efecto no disponible para RMANOVA

      if(pairwise.comp) {
        # Post-Hoc ----
        result[['post-hoc']] <- suppressWarnings(
          expr = { parameters::parameters(
            WRS2::rmmcp(y = data[[variable]],
            groups = data[[by]],
            blocks = data[['rowid']],
            tr = trim)) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '*F* ~Medias-recortadas~ (', round(output$df1,1)
            ,', ', round(output$df2,1)
            , ') = ',round(output$test,3)
            , ', *p* ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  round(output$p.value, 3) ) ) )
          } else {
            result[['report']] <- paste0(
            'F(', round(output$df1,1)
            ,', ', round(output$df2,1)
            , ') = ',round(output$test,3)
            , ', p ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  round(output$p.value, 3) ) ) )
          }

      result[['method']] <- 'Heteroscedastic one-way repeated measures ANOVA for trimmed means'
      result
    } else {
      # Suma de rangos de Friedman, medidas repetidas ----
      output <- stats::friedman.test(stats::as.formula(paste(paste(variable, by, sep = '~'),'| rowid')), data)
      kendall <- effectsize::kendalls_w(as.character(variable), as.character(by), 'rowid', data)

      if(pairwise.comp) {
        # Post-Hoc: Durbin test ----
        result[['post-hoc']] <- suppressWarnings(
          expr = { parameters::parameters(PMCMRplus::durbinAllPairsTest(
          y = data[[variable]]
          , groups = data[[by]]
          , blocks = data[['rowid']]
          , p.adjust.method = p.adjust)) })
      }
      if(markdown) {
          result[['report']] <- paste0(
            '$\\chi$^2^ ~Friedman~ (', round(output$parameter,1)
            , ') = ',round(output$statistic,3)
            , ', *p* ',ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  round(output$p.value, 3) ) )
            , ', *W* ~Kendall~ = ', round(kendall$Kendalls_W, 2)
            , ", IC~95%~[", round(kendall$CI_low,2)
            , ', ', round(kendall$CI_high,2), ']')
          } else {
            result[['report']] <- paste0(
              'X^2(',round(output$parameter,1)
              , ') = ', round(output$statistic,3)
              , ', p ', ifelse(output$p.value < 0.001, '< 0.001', paste(
                '=', round(output$p.value, 3) ) )
              , ', W = ', round(kendall$Kendalls_W, 2)
            , ", IC95% [", round(kendall$CI_low,2)
            , ', ', round(kendall$CI_high,2), ']')
          }

      result[['method']] <- 'Friedman rank sum test'
      result
    }
}
