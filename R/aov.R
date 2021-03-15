#' ANOVA for factorial designs
#'
#' This is function let you perform automated inferential testing based on certain assumptions, some of which are tested automatically, then the propper test is perform, giving you an APA formated output with your statistical results.
#'
#' @param data Your dataset in long format, can have some missing values.
#' @param response Response variable, numeric.
#' @param between Quoted or unquoted variable indicating the between-subject(s) factor(s)/column(s) in data. Default is NULL indicating no between-subjects factors. Must be character vector if more than one between-subject(s) factor(s)/column(s) is specified.
#' @param within Quoted or unquoted variable indicating the within-subject(s) factor(s)/column(s) in data. Default is NULL indicating no between-subjects factors. Must be character vector if more than one within-subject(s) factor(s)/column(s) is specified.
#' @param id Quoted or unquoted variable (of length 1) indicating the subject identifier column in data.
#' @param type The type of sums of squares for the ANOVA. Possible values are "II", "III", 2, or 3 (default).
#' @param es The effect size used to estimate the effects of the factors on the response variable. Possible values are 'omega' or 'eta' (default).
#' @param sphericity If `"none"`, then sphericity assumption is assumed to be met for within-subject(s) factor(s). "GG": applies Greenhouse-Geisser correction. "HF": applies Hyunh-Feldt correction. 'auto' (Default) choose the appropiate correction based on Mauchly test of sphericity (p-value > 0.05)
#' @param markdown Whether you want the output formated for inline R Markdown or as plain text.
#' @keywords aov_r
#' @return A list with full statistical results, estimates and simple stats in APA style for each factor and interaction term(s).
#' @export

aov_r <- function(data
               , response
               , between = NULL
               , within = NULL
               , id
               , type = 3
               , es = 'eta' # 'omega' (default) or 'eta'
               , sphericity = 'auto' # 'auto' (default), 'GG', 'HF' or 'none'
               , markdown = TRUE # FALSE for plain text
               ) {

  result <- list()
  response <- rlang::ensym(response)
  between <- if(grepl(',', deparse(substitute(between)))) between else rlang::ensym(between)
  within <- if(grepl(',', deparse(substitute(within)))) within else rlang::ensym(within)
  id <- rlang::ensym(id)

  suppressMessages(
    suppressWarnings(model <- afex::aov_ez(id = as.character(id)
                      , dv =  as.character(response)
                      , data = data
                      , between = as.character(between)
                      , within = as.character(within)
                      , type = type) ) )

  spher.test <- suppressWarnings(expr = { afex::test_sphericity(model) })
  sphericity <- if(sphericity == 'auto') {
    # Comprobación de esfericidad ----
    if( purrr::is_empty(spher.test) || all(spher.test[,2] > 0.05) ) {
      'none' } else { ges <- any(model$anova_table$ges <= 0.75)
      if(ges) { 'GG' } else { 'HF' } }
    } else if(purrr::is_empty(spher.test)) { 'none' } else { sphericity }

  efs <- if(es == 'eta') { effectsize::eta_squared(model, ci = 0.95)
    } else if(es == 'omega') { effectsize::omega_squared(model, ci = 0.95)
      } else stop('You have to choose between "eta" or "omega"')

  model <- anova(object = model, correction = sphericity)
  at <- attributes(model)
  class(model) <- 'data.frame'
  model <- within(model, {
    `num Df` <- round(`num Df`, digits = 2)
    `den Df` <- round(`den Df`, digits = 2)
    MSE <- round(MSE, digits = 2)
    F <- round(F, digits = 2)
    `Pr(>F)` <- ifelse(`Pr(>F)` < 0.001, '< 0.001', paste('=', round(`Pr(>F)`, 3) ) )
  })

  efs[,-1] <- round(efs[,-1],2)
  at$correction <- if(at$correction == 'none') 'Fisher'
  et <- if(markdown) paste0('$\\',es,'$^2^ = ') else paste0(es,'^2 = ')

  if(markdown) {
    # Formato en Markdown para R Markdown ----
    for(i in row.names(model)) {
      rt <- model[i,]
      j <- if (grepl(pattern = ':', i)) gsub(':', '_', i) else i
      result[['full']][[j]] <- paste0(
        stats <- paste0("*F* ~", at$correction
                        , "~ (", rt$`num Df`
                        ,", ",rt$`den Df`
                        ,') = ',rt$F
                        ,', *p* ',rt$`Pr(>F)`), ', ',
        es <- paste0(et, efs[efs$Parameter == i, 2]
                     ,', CI~95%~[', efs[efs$Parameter == i,"CI_low"]
                     ,', ',efs[efs$Parameter == i, "CI_high"], ']') )
      result[['stats']][[j]] <- stats
      result[['es']][[j]] <- es
      }
    } else {
      # Formato en texto plano ----
      for(i in row.names(model)) {
      rt <- model[i,]
      j <- if (grepl(pattern = ':', i)) gsub(':', '_', i) else i
      result[['full']][[j]] <- paste0(
        stats <- paste0("F(", rt$`num Df`
                        ,", ",rt$`den Df`
                        ,') = ',rt$F
                        ,', p ',rt$`Pr(>F)`), ', ',
        es <- paste0(et, efs[efs$Parameter == i, 2]
                     ,', CI95% [', efs[efs$Parameter == i,"CI_low"]
                     ,', ',efs[efs$Parameter == i, "CI_high"], ']') )
      result[['stats']][[j]] <- stats
      result[['es']][[j]] <- es
      }
    }
  return(result)
}
