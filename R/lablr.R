#' @title Generate report-ready labels for statistical report
#' @name lablr
#' @description A list containing stats, p value, effectsize, confidence/credible interval and a concatenated string named 'full'.
#'
#' @param t Output from any of the functions autest, k_sample, two_sample or one_sample.
#' @param markdown Logical (default FALSE), indicating if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @importFrom data.table fcase rbindlist
#' @export

lablr <- function(t, markdown = FALSE) {
  # Get method
  method <- t$method
  # And confidence/credible interval
  t$conf.level <- t$conf.level*100

  # Set some common labels
  if(markdown) {
    ci. <- paste0("CI~",t$conf.level,"~[")
    p. <- "*p* "
  } else {
    ci. <- paste0("CI",t$conf.level," [")
    p. <- "p "
  }

  # Check specific effectsizes for ANOVA...
  if(grepl("ANOVA", method)) {
    if(grepl("Eta", t$effectsize)) {
      es <- "eta"
    }
    if(grepl("Omega", t$effectsize)) {
      es <- "omega"
    }
  }
  # Or t-test's
  if(grepl("t-test", method)) {
    if(grepl("Cohens", t$effectsize)) {
      es.a <- "d"
      es.b <- "Cohen"
    }
    if(grepl("Hedges", t$effectsize)) {
      es.a <- "g"
      es.b <- "Hedges"
    }
  }

  out <- switch(
    EXPR = method
  # k_sample - paired samples
  , "Fisher's repeated measures ANOVA" = list(d1 = "F", d2 = "$F_{~Fisher}$ ",
      df = paste0("(",t$df, ", ", t$df.error,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es, "2 = "), es2 = paste0("$\\widehat{\\",es,"}_p^2$ = "))
  , "Repeated measures ANOVA with GG correction" = list(d1 = "F", d2 = "$F_{~GG}$ ",
      df = paste0("(", format(round(t$df, 1), nsmall = 1), ", ", format(round(t$df.error, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es, "2 = "), es2 = paste0("$\\widehat{\\",es,"}_p^2$ = "))
  , "Repeated measures ANOVA with HF correction" = list(d1 = "F", d2 = "$F_{~HF}$ ",
      df = paste0("(", format(round(t$df, 1), nsmall = 1), ", ", format(round(t$df.error, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es, "2 = "), es2 = paste0("$\\widehat{\\",es,"}_p^2$ = "))
  , "Friedman rank sum test" = list(d1 = "X2", d2 = "$\\chi^2_{~Friedman}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "W = ", es2 = "$W_{~Kendall}$ = ")
  , "one-way repeated measures ANOVA for trimmed means" = list(d1 = "F", d2 = "$F_{~trimmed-means}$ ",
      df = paste0("(",format(round(t$df, 1), nsmall = 1), ", ", format(round(t$df.error, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "xi = ", es2 = "$\\widehat{\\xi}$ = ")
  # k_sample - independent samples
  , "Fisher's ANOVA" = list(d1 = "F", d2 = "$F_{~Fisher}$ ",
      df = paste0("(",t$df, ", ", t$df.error,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es, "2 = "), es2 = paste0("$\\widehat{\\",es,"}_p^2$ = "))
  , "Welch's ANOVA" = list(d1 = "F", d2 = "$F_{~Welch}$ ",
      df = paste0("(", format(round(t$df, 1), nsmall = 1), ", ", format(round(t$df.error, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es, "2 = "), es2 = paste0("$\\widehat{\\",es,"}_p^2$ = "))
  , "Kruskal-Wallis rank sum test" = list(d1 = "X2", d2 = "$\\chi^2_{~Kruskal-Wallis}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "epsilon2 = ", es2 = "$\\widehat{\\epsilon}^2$ = ")
  , "one-way ANOVA for trimmed means" = list(d1 = "F", d2 = "$F_{~trimmed-means}$ ",
      df = paste0("(",format(round(t$df, 1), nsmall = 1), ", ", format(round(t$df.error, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "xi = ", es2 = "$\\widehat{\\xi}$ = ")
  # two_sample - independent sample
  , " Two Sample t-test" = list(d1 = "t", d2 = "$t_{~Student}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es.a, " = "), es2 = paste0("$",es.a,"_{~",es.b,"}$ = "))
  , "Welch Two Sample t-test" = list(d1 = "t", d2 = "$t_{~Welch}$ ",
      df = paste0("(",format(round(t$df, 1), nsmall = 1),") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es.a, " = "), es2 = paste0("$",es.a,"_{~",es.b,"}$ = "))
  , "Wilcoxon rank sum test with continuity correction" = list(d1 = "ln(W)", d2 = "$\\ln(W)$ ",
      df = paste0("= ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "r = ", es2 = "$\\widehat{r}_{biserial}$ = ")
  , "Two sample Yuen's test on trimmed means" = list(d1 = "t", d2 = "$t_{~Yuen}$ ",
      df = paste0("(",format(round(t$df, 1), nsmall = 1), ") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "xi = ", es2 = "$\\widehat{\\xi}$ = ")
  # two_sample - paired samples
  , "Paired t-test" = list(d1 = "t", d2 = "$t_{~Student}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es.a, " = "), es2 = paste0("$",es.a,"_{~",es.b,"}$ = "))
  , "Wilcoxon signed rank test with continuity correction" = list(d1 = "ln(V)", d2 = "$\\ln(V)$ ",
      df = paste0("= ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "r = ", es2 = "$\\widehat{r}_{biserial}$ = ")
  , "Paired Yuen's test on trimmed means" = list(d1 = "t", d2 = "$t_{~Yuen}$ ",
      df = paste0("(",format(round(t$df, 1), nsmall = 1), ") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "xi = ", es2 = "$\\widehat{\\xi}$ = ")
  # one_sample
  , "One Sample t-test" = list(d1 = "t", d2 = "$t_{~Student}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = paste0(es.a, " = "), es2 = paste0("$",es.a,"_{~",es.b,"}$ = "))
  , "Bootstrap-t method for one-sample test" = list(d1 = "t ", d2 = "$t_{~Bootstrap}$ ",
      df = paste0("= ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "M = ", es2 = "$\\widehat{\\mu}_{~Trimmed}$ = ")
  # contingency
  , "McNemar's Chi-squared test" =
  , "McNemar's Chi-squared test with continuity correction" = list(d1 = "X2", d2 = "$\\chi^2_{~McNemar}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "g = ", es2 = "$g_{~Cohen}$ = ")
  , "Pearson's Chi-squared test" =
  , "Pearson's Chi-squared test with Yates' continuity correction" = list(d1 = "X2", d2 = "$\\chi^2_{~Pearson}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "V = ", es2 = "$V_{~Cramer}$ = ")
  , "Chi-squared test for given probabilities" = list(d1 = "X2", d2 = "$\\chi^2_{~gof}$ ",
      df = paste0("(",t$df,") = ", format(round(t$statistic, 2), nsmall = 2)),
      es1 = "V = ", es2 = "$V_{~Cramer}$ = ")
  # This two has their own printing methods
  , "Fisher's Exact Test for Count Data" = list(es = if(markdown) "$OR$ = " else "OR = ")
  , "Fisher's Exact Test for Count Data without OR" = list()
  , stop("Method not found")
  )

  if(method == "Fisher's Exact Test for Count Data") {
    res <- list(
      stats = as.character(NA),
      p = (.p <- paste0(p., style.p(t$p.value))),
      es = (.es <- paste0(out$es, format(round(t$estimate, 2), nsmall = 2))),
      ci = (.ci <- paste0(ci., format(round(t$conf.low, 2), nsmall = 2), ", ", format(round(t$conf.high, 2), nsmall = 2), "]")),
      full = paste(.p, .es, .ci, sep = ", ")
    )

    class(res) <- c("writR", "list")

    return(res)
  }
  if(method == "Fisher's Exact Test for Count Data without OR") {
    res <- list(
      stats = as.character(NA),
      p = (.p <- paste0(p., style.p(t$p.value))),
      es = as.character(NA),
      ci = as.character(NA),
      full = .p
    )

    class(res) <- c("writR", "list")

    return(res)
  }

  # Select and set tags for later use
  if(markdown) {
    d. <- out$d2
    es. <- out$es2
  } else {
    d. <- out$d1
    es. <- out$es1
  }

  res <- list(
    stats = (.stats <- paste0(d., out$df)),
    p = (.p <- paste0(p., style.p(t$p.value))),
    es = (.es <- paste0(es., format(round(t$estimate, 2), nsmall = 2))),
    ci = (.ci <- paste0(ci., format(round(t$conf.low, 2), nsmall = 2), ", ", format(round(t$conf.high, 2), nsmall = 2), "]")),
    full = paste(.stats, .p, .es, .ci, sep = ", ")
  )

  class(res) <- c("writR", "list")

  return(res)
}
