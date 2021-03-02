#'
#'
bi <- function(data
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
    bipair(data = data
            , variable = {{variable}}
            , by = {{by}}
            , type = type
            , trim = trim
            , nboot = nboot
            , markdown = markdown
            , ...)
  } else {
    # Muestras independientes ----
    bitwo(data = data
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
