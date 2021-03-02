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
    mult(
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
    bi(
      data = data
      , variable = {{variable}}
      , by = {{by}}
      , paired = paired
      , type = type
      , var.equal = var.equal
      , markdown = markdown)
  }
}
