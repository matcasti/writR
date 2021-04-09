#' Inline summary descriptives
#'
#' This function returns a character vector with mean and sd or median and iqr as a inline summary statistics for reports or articles in R markdown or plain text.
#' @param x Numeric vector.
#' @param type To choose a parametric (`type = 'p'`) estimate (i.e. mean and sd) or a non-parametric one (`type = 'np'`, median and iqr). Can be choosen automatically with `type = 'auto`
#' @param k Number of decimals.
#' @param markdown Whether you want the output formated for inline R markdown (TRUE) or as plain text (FALSE).
#' @keywords cent_disp
#' @return A character vector of length one.
#' @export

cent_disp <- function (x
                     , type = 'auto'
                     , k = 2
                     , markdown = TRUE) {
  if(!is.numeric(x)) stop(paste(deparse(substitute(x)), "is not numeric."))
  if(type == 'auto') {
    .norm <- if(length(x) < 50)
      stats::shapiro.test(x)$p.value > 0.05 else
      nortest::lillie.test(x)$p.value > 0.05
    type <- if(.norm) "p" else "np"
    }
  if(type == 'p') .f <- list(cent = base::mean, disp = stats::sd, m = 'M', i = 'SD')
  if(type == 'np') .f <- list(cent = stats::median, disp = stats::IQR, m = 'Mdn', i = 'IQR')
  m <- round(.f$cent(x, na.rm = T), k)
  i <- round(.f$disp(x, na.rm = T), k)
  if(markdown)
    paste0('$',.f$m,'$ = ',m,', $',.f$i,'$ = ',i) else
    paste0(.f$m,' = ',m,', ',.f$i,' = ',i)
}