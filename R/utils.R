#' @title Style p value
#' @name style.p
#' @description Style p value for pretty printing
#'
#' @param p Vector of p value(s)
#' @param k Number of decimal places
#'
#' @return Character vector of the same length of the input.
#'
#' @export

style.p <- function(p, k = 3) {
  p <- format.default(p, digits = 1, nsmall = 3, drop0trailing = FALSE, trim = TRUE)
}

#' @title Normality check
#' @name is_normal
#'
#' @param x Numeric vector with or without NA's.
#' @param alpha Threshold for rejection of the null hipotesis (of normality).
#' @param test A function that returns a single numeric value (p.value) to be tested.
#'
#' @export

is_normal <- function(x, alpha = 0.05, test) {
  if (missing(test)) {
    test <- function(i) stats::shapiro.test(i)$p.value
  }
  x <- x[!is.na(x)]
  test(x) > alpha
}

#' @title Test for Homogeneity of Variances
#' @name is_var.equal
#'
#' @param y Numeric vector (response).
#' @param x Grouping factor.
#' @param alpha Threshold for null hipotesis (of normality) rejection. Defaults to 0.05
#' @param test A character indicating whether to use Levene's test ("levene", the default) or Fligner's test ("fligner"). The first letter will be used.
#' @param center A function to compute the center of each group (valid only for Levene's Test). The mean gives the original Levene's test; the default, median, provides a more robust test.
#'
#' @return A logical of length one, indicating if the p.value of the test was greater than the alpha (which defaults to 0.05).
#'
#' @export

is_var.equal <- function(y, x, alpha = 0.05, test = "levene", center = stats::median) {
  test <- tolower(test)
  if (grepl("^l", test)) {
    valid <- stats::complete.cases(y, x)
    meds <- tapply(y[valid], x[valid], center)
    resp <- abs(y - meds[x])
    stats::anova(stats::lm(resp ~ x))[["Pr(>F)"]][[1]] > alpha
  } else if (grepl("^f", test)) {
    stats::fligner.test(y, x)$p.value > alpha
  }
}

#' @title Mauchly's Test of Sphericity
#' @name mauchly
#' @description Internal function inside `sphericity_check`. Return a p-value from Mauchly's Test for within-subjects factors.
#'
#' @param model A repeated measures ANOVA model using afex.

mauchly <- function(model) {
  .mauch <- function(SSPE, P, error.df) {
    SSD <- SSPE
    df <- error.df
    if (nrow(SSD) < 2) return(NA)
    Tr <- function(X) sum(diag(X))
    p <- nrow(P)
    I <- diag(p)
    Psi <- t(P) %*% I %*% P
    B <- SSD
    pp <- nrow(SSD)
    U <- solve(Psi, B)
    n <- df
    logW <- log(det(U)) - pp * log(Tr(U/pp))
    rho <- 1 - (2 * pp^2 + pp + 2)/(6 * pp * n)
    w2 <- (pp + 2) * (pp - 1) * (pp - 2) * (2 * pp^3 + 6 * pp^2 + 3 * p + 2)/(288 * (n * pp * rho)^2)
    z <- -n * rho * logW
    f <- pp * (pp + 1)/2 - 1
    Pr1 <- stats::pchisq(z, f, lower.tail = FALSE)
    Pr2 <- stats::pchisq(z, f + 4, lower.tail = FALSE)
    pval <- Pr1 + w2 * (Pr2 - Pr1)
    return(pval)
  }

  .m <- model$Anova
  pval <- vapply(.m$terms, function(i) {
    .mauch(
      SSPE = .m$SSPE[[i]],
      P = .m$P[[i]],
      error.df = .m$error.df
    )
  }, 1)
  pval <- pval[!is.na(pval)]
  return(pval)
}

#' @title Greenhouse-Geisser epsilon
#' @name GG
#' @description Internal function inside `sphericity_check`. Returns Greenhouse-Geisser epsilon for corrections for departure from sphericity
#'
#' @param model A repeated measures ANOVA model using afex.

GG <- function(model) {
  .gg <- function(SSPE, P) {
    p <- nrow(SSPE)
    if (p < 2) return(NA)
    lambda <- eigen(SSPE %*% solve(t(P) %*% P))$values
    lambda <- lambda[lambda > 0]
    ((sum(lambda)/p)^2)/(sum(lambda^2)/p)
  }
  .m <- model$Anova
  eps <- vapply(.m$terms, function(i) {
    .gg(
      SSPE = .m$SSPE[[i]],
      P = .m$P[[i]]
    )
  }, 1)
  return(eps)
}

#' @title Hyunh-Feldt epsilon
#' @name HF
#' @description Internal function inside `sphericity_check`. Returns Hyunh-Feldt epsilon for corrections for departure from sphericity
#'
#' @param model A repeated measures ANOVA model using afex.
#' @param gg Greenhouse-Geisser epsilon. Used for posterior calculations. If is not supplied it will be calculated.

HF <- function(model, gg = NULL) {
  if (length(gg) == 0) {
    gg <- GG(model)
  }
  .hf <- function(SSPE, P, error.df, gg) {
    if (nrow(SSPE) < 2) return(NA)
    p <- ncol(P)
    ((error.df + 1) * p * gg - 2)/(p * (error.df - p * gg))
  }
  .m <- model$Anova
  arg <- match.call()
  eps <- vapply(.m$terms, function(i) {
    .hf(
      SSPE = .m$SSPE[[i]],
      P = .m$P[[i]],
      error.df = .m$error.df,
      gg = gg[[i]]
    )
  }, 1)
  return(eps)
}

#' Suggested sphericity correction for repeated measures ANOVA
#' @name sphericity_check
#'
#' @description Internal function inside `k_sample`. Return the Spherecity correction suggested based on Mauchly test in one-way repeated measures designs
#'
#' @param model A repeated measures ANOVA model using Afex.
#' @export

sphericity_check <- function(model) {
  .m <- model$Anova
  if (any(.m$singular) ||  all(mauchly(model) > 0.05)) "none" else {
    is_hf <- all((gg.eps <- GG(model)) > 0.75, na.rm = TRUE)
    is_hf_too <- all(HF(model, gg.eps) > 0.75, na.rm = TRUE)
    if (is_hf || is_hf_too) "HF" else "GG"
  }
}

#' Mauchly's Test of Sphericity
#'
#' Low-level function behind `mauchly.test()`
#'
#' @param object object of class SSD or mlm
#' @param Sigma matrix to be proportional to.
#' @param T transformation matrix. By default computed from M and X.
#' @param M formula or matrix describing the outer projection.
#' @param X formula or matrix describing the inner projection.
#' @param idata data frame describing intra-block design.
#'
#' @export

sphericity <- utils::getFromNamespace("sphericity", "stats")

#' @export

print.writR <- function(x, ...) {
  x <- data.table::as.data.table(
    x = x[!sapply(x, anyNA)]
  )
  print(x)
}

#' @export

as.data.frame.writR <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(
    x = x[!sapply(x, anyNA)],
    row.names = row.names,
    optional = optional
  )
}

