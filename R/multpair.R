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
          result[['pwc.method']] <- "Student's t-test for dependent samples"
          result[['pwc.table']] <- suppressWarnings(expr = {
            dplyr::as_tibble(
            parameters::parameters(
            stats::pairwise.t.test(
            x = data[[variable]]
            , g = data[[by]]
            , p.adjust.method = p.adjust
            , paired = TRUE))) })
      }

      sphericity <- if(type == 'check') {
        # Comprobación de esfericidad ----
        if( purrr::is_empty(spher.test) || spher.test[[2]] > 0.05 ) {
          TRUE } else { ges <- all(summary(model)[['pval.adjustments']][,c('GG eps','HF eps')] > 0.75)
          if(ges) { 'HF' } else { 'GG' } }
        } else if(purrr::is_empty(spher.test)) { TRUE } else { sphericity }

      if(isTRUE(sphericity)) {
        # ANOVA de Fisher, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'none')})

        desc <- if(markdown) {
        list(m = '*M* = ', i = ', *SD* = ', f = '$F_{~Fisher}$ (', p = ', *p* '
             , eta = '$\\eta^2$ = ', ci = ', CI~95%~[') } else {
          list(m = 'M = ', i = ', SD = ', f = 'F(', p = ', p '
               , eta = "eta^2 = ", ci = ', CI95% [') }

        for(j in levels(data[[by]])) {
          result[['desc']][j] <- list(paste0(
          desc$m
          , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
          , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

            result[['full']] <- paste0(
              stats <- paste0(desc$f, round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],2)
              , desc$p,ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) ) ),', '
              , es <- paste0(desc$eta, round(eta$Eta2_partial,2)
            , desc$ci, round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']') )
            result[['stats']] <- stats
            result[['es']] <- es

        result[['method']] <- 'One-way repeated measures ANOVA'
        result
      } else if(sphericity == 'GG') {
        # ANOVA de Greenhouse-Geisser, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'GG')})

        desc <- if(markdown) {
        list(m = '*M* = ', i = ', *SD* = ', f = '$F_{~GG}$ (', p = ', *p* '
             , eta = '$\\eta^2$ = ', ci = ', CI~95%~[') } else {
          list(m = 'M = ', i = ', SD = ', f = 'F(', p = ', p '
               , eta = "eta^2 = ", ci = ', CI95% [') }

        for(j in levels(data[[by]])) {
          result[['desc']][j] <- list(paste0(
          desc$m
          , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
          , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

            result[['full']] <- paste0(
              stats <- paste0(desc$f, round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],2)
              , desc$p,ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) ) ),', '
              , es <- paste0(desc$eta, round(eta$Eta2_partial,2)
            , desc$ci, round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']') )
            result[["stats"]] <- stats
            result[["es"]] <- es

        result[['method']] <- 'One-way repeated measures ANOVA with Greenhouse-Geisser correction'
        result
      } else {
        # ANOVA de Huynh-Feldt, medidas repetidas ----
        suppressWarnings(expr = {output <- stats::anova(model, correction = 'HF')})

        desc <- if(markdown) {
        list(m = '*M* = ', i = ', *SD* = ', f = '$F_{~HF}$ (', p = ', *p* '
             , eta = '$\\eta^2$ = ', ci = ', CI~95%~[') } else {
          list(m = 'M = ', i = ', SD = ', f = 'F(', p = ', p '
               , eta = "eta^2 = ", ci = ', CI95% [') }

        for(j in levels(data[[by]])) {
          result[['desc']][j] <- list(paste0(
          desc$m
          , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
          , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

            result[['full']] <- paste0(
              stats <- paste0(desc$f, round(output[["num Df"]],1)
              , ', ', round(output[["den Df"]],1)
              , ') = ', round(output[["F"]],2)
              , desc$p,ifelse(output[["Pr(>F)"]] < 0.001, '< 0.001', paste(
                '=', round(output[["Pr(>F)"]], 3) ) ) ),', '
              , es <- paste0(desc$eta, round(eta$Eta2_partial,2)
            , desc$ci, round(eta$CI_low,2)
            , ', ', round(eta$CI_high,2), ']') )
            result[["stats"]] <- stats
            result[["es"]] <- es

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
          result[['pwc.method']] <- "Yuen's test on trimmed means"
          result[['pwc.table']] <- suppressWarnings(
          expr = { dplyr::as_tibble(
            parameters::parameters(
            WRS2::rmmcp(y = data[[variable]],
            groups = data[[by]],
            blocks = data[['rowid']],
            tr = trim)))[,c(1,2,7)] })
      }

      desc <- if(markdown) {
        list(m = '*M* = ', i = ', *SD* = ', f = '$F_{~trimmed-means}$ (', p = ', *p* ') } else {
          list(m = 'M = ', i = ', SD = ', f = 'F(', p = ', p ') }

        for(j in levels(data[[by]])) {
          result[['desc']][j] <- list(paste0(
          desc$m
          , round(base::mean(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
          , round(stats::sd(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

          result[['full']] <- paste0(
            stats <- paste0(desc$f, round(output$df1,1)
            ,', ', round(output$df2,1)
            , ') = ',round(output$test,2)
            , desc$p,ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  round(output$p.value, 3) ) ) ) )
          result[["stats"]] <- stats
          result[["es"]] <- 'Not available'

      result[['method']] <- 'Heteroscedastic one-way repeated measures ANOVA for trimmed means'
      result
    } else {
      # Suma de rangos de Friedman, medidas repetidas ----
      output <- stats::friedman.test(stats::as.formula(paste(paste(variable, by, sep = '~'),'| rowid')), data)
      kendall <- effectsize::kendalls_w(as.character(variable), as.character(by), 'rowid', data)

      if(pairwise.comp) {
        # Post-Hoc: Durbin test ----
          result[['pwc.method']] <- "Durbin-Conover test"
          result[['pwc.table']] <- suppressWarnings(
          expr = { dplyr::as_tibble(
            parameters::model_parameters(
              PMCMRplus::durbinAllPairsTest(
                y = data[[variable]]
                , groups = data[[by]]
                , blocks = data[['rowid']]
                , p.adjust.method = p.adjust)))[,c(1,2,4)] })
      }

      desc <- if(markdown) {
        list(m = '*Mdn* = ', i = ', *IQR* = ', chi = '$\\chi^2_{~Friedman}$ (', p = ', *p* '
             , w = '$W_{~Kendall}$ = ', ci = ', CI~95%~[') } else {
          list(m = 'Mdn = ', i = ', IQR = ', chi = 'X^2(', p = ', p '
               , w = 'W = ', ci = ', CI95% [') }

      for(j in levels(data[[by]])) {
        result[['desc']][j] <- list(paste0(
        desc$m
        , round(stats::median(data[data[[by]] == j,][[variable]], na.rm = T), 2), desc$i
        , round(stats::IQR(data[data[[by]] == j,][[variable]], na.rm = T), 2) ) ) }

          result[['full']] <- paste0(
            stats <- paste0(desc$chi, round(output$parameter,1)
            , ') = ',round(output$statistic,2)
            , desc$p,ifelse(output$p.value < 0.001, '< 0.001', paste(
              '=',  round(output$p.value, 3) ) ) ),', '
            , es <- paste0(desc$w, round(kendall$Kendalls_W, 2)
            , desc$ci, round(kendall$CI_low,2)
            , ', ', round(kendall$CI_high,2), ']') )
          result[["stats"]] <- stats
          result[["es"]] <- es

      result[['method']] <- 'Friedman rank sum test'
      result
    }
}
