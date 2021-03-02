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

.c <- function(data                       # Función para la preparación de los datos
               , variable
               , by
               , paired = FALSE) {
  
    ipmisc::long_to_wide_converter(
        data = data,
        x = {{by}},
        y = {{variable}},
        paired = paired,
        spread = F)
  }

.tbi <- function(data                      # Análisis de dos muestras
                , variable
                , by
                , paired = FALSE
                , type = 'auto'
                , var.equal = FALSE, ...) {
  
  data <- .c(data, {{variable}}, {{by}}, paired); result <- list()
  
  if(paired) { 
    # Muestras relaciondas ----
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
      result[['test']] <- stats::t.test(data[[variable]] ~ data[[by]], paired = T)
      result[['es']] <- effectsize::effectsize(test, verbose = F)
      result[['method']] <- 't de Student para muestras relacionadas'
      result
    } else {
      # Prueba de rangos con signo de Wilcoxon ----
      result[['test']] <- stats::wilcox.test(
        data[[variable]] ~ data[[by]], correct = T, exact = F, paired = T)
      result[['es']] <- effectsize::rank_biserial(
        data[[variable]] ~ data[[by]], data = data, paired = TRUE, verbose = FALSE)
      result[['method']] <- 'Rangos con signo de Wilcoxon para muestras relacionadas'
      result
    }
  } else { 
    # Muestras independientes ----
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
        result[['test']] <- stats::t.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
        result[['es']] <- effectsize::effectsize(test, verbose = F)
        result[['method']] <- 't de Student para muestras independientes'; 
        result
      } else {
        # T-Welch, muestras independientes ----
        result[['test']] <- stats::t.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
        result[['es']] <- effectsize::effectsize(test, verbose = F)
        result[['method']] <- 't de Welch para muestras independientes'
        result 
      }
    } else {
      # U de Mann-Whitney ----
      result[['test']] <- stats::wilcox.test(
        data[[variable]] ~ data[[by]], correct = T, exact = F, paired = F)
      result[['es']] <- effectsize::rank_biserial(data[[variable]] ~ data[[by]], data = data)
      result[['method']] <- 'Suma de rangos de Wilcoxon para muestras independientes'
      result
      
    }
  }
}

.tmult <- function(data                    # Análisis de tres o más muestras
                , variable
                , by
                , paired = FALSE
                , type = 'auto'
                , var.equal = FALSE
                , sphericity = 'GG'
                , pairwise.comp = FALSE
                , p.adjust = 'none', ...) {
  
  data <- .c(data, {{variable}}, {{by}}, paired); result <- list()
  
  if(paired) { 
    # Medidas repetidas ----
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
        result[['test']] <- output
        result[['method']] <- 'ANOVA de medidas repetidas de Fisher'
        result
      } else if(sphericity == 'GG') {
        # ANOVA de Greenhouse-Geisser, medidas repetidas ----
        suppressWarnings(expr = {output <- anova(model, correction = 'GG')})
        result[['test']] <- output          
        result[['method']] <- 'ANOVA de medidas repetidas de Greenhouse-Geisser'
        result
      } else {
        # ANOVA de Huynh-Feldt, medidas repetidas ----
        suppressWarnings(expr = {output <- anova(model, correction = 'HF')})
        result[['test']] <- output          
        result[['method']] <- 'ANOVA de medidas repetidas de Huynh-Feldt'
        result
      }
    } else {
      # Suma de rangos de Friedman, medidas repetidas ----
      result[['test']] <- stats::friedman.test(
        y = data[[variable]], groups = data[[by]], blocks = data[['rowid']])
      result[['es']] <- effectsize::kendalls_w({{variable}}, {{by}}, 'rowid', data)
      if(pairwise.comp) {
        # Post-Hoc: Durbin test ----
        result[['post-hoc']] <- suppressWarnings(
          expr = { PMCMRplus::durbinAllPairsTest(
          y = data[[variable]]
          , groups = data[[by]]
          , blocks = data[['rowid']]
          , p.adjust.method = p.adjust) })
      }
      result[['method']] <- 'Suma de rangos de Friedman para muestras relacionadas'
      result
    }
  } else { 
    # Muestras independientes ----
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
        result[['test']] <- stats::oneway.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
        result[['es']] <- effectsize::effectsize(result$test, verbose = FALSE, ci = 0.95)
        if(pairwise.comp) {
          # Post-Hoc: T-Student ----
          result[['post-hoc']] <- suppressWarnings(expr = { stats::pairwise.t.test(
            x = data[[variable]]
            , g = data[[by]]
            , p.adjust.method = p.adjust
            , paired = F) })
        }
        result[['method']] <- 'ANOVA de un factor de Fisher'
        result
      } else {
        # ANOVA de Welch, muestras independientes ----
        result[['test']] <- stats::oneway.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
        result[['es']] <- effectsize::effectsize(result$test, verbose = FALSE, ci = 0.95)
        if(pairwise.comp) {
          # Post-Hoc: Games Howell ----
          result[['post-hoc']] <- suppressWarnings(
            expr = { PMCMRplus::gamesHowellTest(
              x = data[[variable]]
              , g = data[[by]]) })
        }
        result[['method']] <- 'ANOVA de un factor de Welch'
        result
      }
    } else {
      # Suma de rangos de Kruskal-Wallis, muestras independientes ----
      result[['test']] <- stats::kruskal.test(data[[variable]] ~ data[[by]])
      result[['es']] <- effectsize::rank_epsilon_squared(data[[variable]] ~ data[[by]], data)
      if(pairwise.comp) {
        # Post-Hoc: Dunn test ----
          result[['post-hoc']] <- suppressWarnings(
            expr = { PMCMRplus::kwAllPairsDunnTest(
              x = data[[variable]]
              , g = data[[by]]
              , p.adjust.method = p.adjust) })
      }
      result[['method']] <- 'Suma de rangos de Kruskal-Wallis para muestras independientes'
      result
    }
  }
}

#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#------------------------- Función principal --------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#

# Test para muestras independientes o relacionadas
.test <- function(data
                    , variable
                    , by
                    , paired = FALSE
                    , type = 'auto'
                    , var.equal = FALSE
                    , sphericity = 'GG'
                    , pairwise.comp = FALSE
                    , p.adjust = 'none'
                    , ...) {
  data <- .c(
    data = data,
    variable = {{variable}},
    by = {{by}},
    paired = paired)
  
  if(nlevels(data[[by]]) >= 3) {
    # 3 o más niveles de comparación ----
    .tmult(
      data = data
      , variable = {{variable}}
      , by = {{by}}
      , paired = paired
      , type = type
      , var.equal = var.equal
      , sphericity = sphericity
      , pairwise.comp = pairwise.comp
      , p.adjust = p.adjust
      , ... )
  } else {
    # 2 niveles de comparación ----
    .tbi(
      data = data
      , variable = {{variable}}
      , by = {{by}}
      , paired = paired
      , type = type
      , var.equal = var.equal
      , ...)
  }
}