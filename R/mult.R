# Función: Pruebas de tres o más grupos  ========================================
mult <- function(data
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
    multpair(data = data
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
    multgroup(data = data
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
