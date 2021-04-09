#' Report categorical analyses
#'
#' Perform nominal/ordinal analysis on 1 dimensional table for goodnes-of-fit chi-squared, and two dimensional data for Pearson chi-squared, Fisher exact test or (if paired), McNemar test reporting their corresponding stats and effect sizes in APA Style.
#' @param data Data frame containing the variables `x` and `y`.
#' @param x Factor variable, quoted or unquoted.
#' @param y Factor. If `NULL`, a goodness-of-fit is carried, otherwise a two-way analysis is performed.
#' @param paired Logical. If `TRUE` McNemar's Chi-squared test is carried on.
#' @param exact Logical. If `TRUE` then Fisher's Exact Test is carried on, but only when `paired = FALSE` (default). If is a 2 x 2 design, Odds Ratio (OR) is returned as effect size, otherwise it will only return the formated p-value.
#' @param markdown Whether you want the output formated for inline R Markdown or as plain text.
#' @param ... Currently not used.
#' @keywords contingency
#' @return A list of length 3 or 2 with statistical test and `$method` used.
#' @export

contingency <- function(data
                   , x
                   , y = NULL
                   , paired = FALSE
                   , exact = FALSE
                   , markdown = TRUE
                   , ...) {
  .arg <- match.call()

  x.var <- data[[.arg$x]]
  if(is.null(.arg$y)) {
    way <- "One"
  } else {
    y.var <- data[[.arg$y]]
    way <- "Two"
  }

  result <- list()
  test <- if(isTRUE(paired)) "Mcnemar" else
    if(isTRUE(exact)) "Exact" else
      "Chi"

  if(test == "Chi") {
    tab <- if(way == "One")
      list(table(x.var),
           "gof",
           "Chi-squared test for given probabilities") else
      list(table(x.var, y.var),
           "Pearson",
           "Pearson's Chi-squared test")
    test <- stats::chisq.test(
      x = tab[[1]],
      correct = FALSE)
    es <- effectsize::cramers_v(
      x = tab[[1]],
      adjust = FALSE)
    expr <- if(isTRUE(markdown))
      list(a = paste0("$\\chi^2_{~", tab[[2]], "}$ ("),
           b = ", $p$ ",
           c = "$V_{~Cramer}$ = ",
           d = ', CI~95%~[') else
      list(a = "X^2 (",
           b = ", p ",
           c = "V = ",
           d = ', CI95% [')
    result[['full']] <- paste0(
      result[['stats']] <- paste0(
        expr$a,
        test$parameter,
        ") = ",
        round(test$statistic,2),
        expr$b,
        ifelse(
          test$p.value < 0.001,
          '< 0.001',
          paste(
            '=',
            round(test$p.value, 3)
            )
          )
        ), ', ',
      result[['es']] <- paste0(
        expr$c,
        round(es$Cramers_v,2),
        expr$d,
        round(es$CI_low,2),
        ', ',
        round(es$CI_high,2),
        ']')
      )
    result[['method']] <- tab[[3]]
    return(result)

  } else {
    if(test == "Exact") {
      tab <- table(x.var, y.var)
      test <- stats::fisher.test(
        x = tab)
      error <- class(
        try(
          expr = {
            (es <- effectsize::oddsratio(
              x = x.var,
              y =  y.var) )
            },
          silent = TRUE)
        ) == "try-error"
      if(isTRUE(error)) {
        expr <- if(isTRUE(markdown))
          list(a = "$p_{~FET}$ ") else
          list(a = "FET, p ")
        result[['full']] <- paste0(
          result[['stats']] <- paste0(
            expr$a,
            ifelse(
              test = test$p.value < 0.001,
              yes = '< 0.001',
              no = paste(
                '=',
                round(test$p.value, 3)
                )
              )
            )
          )
        result[['es']] <- "Not available"
        result[['method']] <- "Fisher's Exact Test for Count Data"
        return(result)
      } else {
        expr <- if(isTRUE(markdown))
          list(a = "$p_{~FET}$ ",
               b = "$OR$ = ",
               c = ', CI~95%~[') else
          list(a = "FET: p ",
               b = "OR = ",
               c = ', CI95% [')
        result[['full']] <- paste0(
          result[['stats']] <- paste0(
            expr$a,
            ifelse(
              test = test$p.value < 0.001,
              yes = '< 0.001',
              no = paste(
                '=',
                round(test$p.value, 3)
                )
              )
            )
          , ', ',
          result[['es']] <- paste0(
            expr$b,
            round(es$Odds_ratio,2),
            expr$c,
            round(es$CI_low,2),
            ', ',
            round(es$CI_high,2),
            ']')
          )
        result[['method']] <- "Fisher's Exact Test for Count Data"
        return(result)
      }
    } else {
      tab <- table(x.var,y.var)
      test <- stats::mcnemar.test(
        x = tab,
        correct = FALSE)
      es <- effectsize::cohens_g(
        x = tab)
      expr <- if(isTRUE(markdown))
        list(a = paste0("$\\chi^2_{~McNemar}$ ("),
             b = ", $p$ ",
             c = "$g_{~Cohen}$ = ",
             d = ', CI~95%~[') else
        list(a = "X^2 (",
             b = ", p ",
             c = "g = ",
             d = ', CI95% [')
      result[['full']] <- paste0(
        result[['stats']] <- paste0(
          expr$a,
          test$parameter,
          ") = ",
          round(test$statistic,2),
          expr$b,
          ifelse(
            test = test$p.value < 0.001,
            yes = '< 0.001',
            no = paste(
              '=',
              round(test$p.value, 3)
              )
            )
          ),
        ', ',
        result[['es']] <- paste0(
          expr$c,
          round(es$Cohens_g,2),
          expr$d,
          round(es$CI_low,2),
          ', ',
          round(es$CI_high,2),
          ']')
        )
      result[['method']] <- "McNemar's Chi-squared test"
      return(result)
    }
  }
}

