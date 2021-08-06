#' @title one-sample t-test on trimmed means
#' @description A wrapper around statsExpressions original function trimcibt
#' @name trimcibt
#'
#' @param x Numeric vector with or without NA's.
#' @param nv Test value, default to 0.
#' @param tr Trim for the mean, default to 0.2.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (default: 100L).
#' @param ci Confidence/Credible Interval (CI) level, default to 0.95 (95%).
#' @param ... Currently ignored.
#' @importFrom WRS2 trimse

trimcibt <- function(x,
                    nv = 0,
                    tr = 0.2,
                    nboot = 100L,
                    ci = 0.95,
                    ...) {

  x <- x[!is.na(x)]
  mu <- mean(x, tr)
  sigma <- WRS2::trimse(x, tr)
  test <- (mu - nv) / sigma
  data <- matrix(data = sample(x, size = length(x) * nboot, replace = TRUE), nrow = nboot) - mu
  tval <- sort(
    x = abs(
      x = apply(data, 1, mean, tr) / apply(data, 1, WRS2::trimse, tr)
    )
  )
  icrit <- round(ci * nboot)
  list(
    statistic = test,
    p.value = sum(abs(test) <= tval)/nboot,
    method = "Bootstrap-t method for one-sample test",
    estimate = mu,
    conf.low = mu - tval[icrit] * sigma,
    conf.high = mu + tval[icrit] * sigma,
    conf.level = ci,
    effectsize = "Trimmed mean"
  )
}

#' @title Style p value
#' @name style.p
#' @description Style p value for pretty printing
#'
#' @param p Vector of p value(s)
#' @param k Number of decimal places
#' @return Character vector of length one.
#' @importFrom data.table fcase
#' @export

style.p <- function(p, k = 3) {
  fcase(
    p < 0.001, "< 0.001",
    p >= 0.001, paste0("= ", round(p, digits = k))
  )
}

#' @title Normality check
#' @name is_normal
#'
#' @param x Numeric vector with or without NA's.
#' @param alpha Threshold for rejection of the null hipotesis (of normality).
#' @param test A function that returns a single numeric value (p.value) to be tested.
#' @importFrom nortest lillie.test
#' @importFrom stats shapiro.test
#' @export

is_normal <- function(x, alpha = 0.05, test = NULL) {
  if(is.null(test)) {
    if(length(x) <= 50) {
      test <- function(i) stats::shapiro.test(i)$p.value
    } else {
      test <- function(i) nortest::lillie.test(i)$p.value
    }
  }
  x <- x[!is.na(x)]
  test(x) > alpha
}

#' @title Levene's Test
#' @name is_var.equal
#'
#' @param y Numeric vector (response).
#' @param x Grouping factor.
#' @param alpha Threshold for null hipotesis (of normality) rejection.
#' @param center A function to compute the center of each group; mean gives the original Levene's test; the default, median, provides a more robust test.
#' @importFrom nortest lillie.test
#' @importFrom stats complete.cases anova lm median
#' @export

is_var.equal <- function(y, x, alpha = 0.05, center = median) {
  valid <- complete.cases(y, x)
  meds <- tapply(y[valid], x[valid], center)
  resp <- abs(y - meds[x])
  anova(lm(resp ~ x))[["Pr(>F)"]][[1]] > alpha
}

#' @title Mauchly's Test of Sphericity
#' @name mauchly
#' @description Internal function inside `sphericity_check`. Return a p-value from Mauchly's Test for within-subjects factors.
#'
#' @param model A repeated measures ANOVA model using afex.
#' @importFrom stats pchisq

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
    Pr1 <- pchisq(z, f, lower.tail = FALSE)
    Pr2 <- pchisq(z, f + 4, lower.tail = FALSE)
    pval <- Pr1 + w2 * (Pr2 - Pr1)
    return(pval)
  }
  .m <- model$Anova
  pval <- vapply(.m$terms, \(i) {
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
  eps <- vapply(.m$terms, \(i) {
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
  if(length(gg) == 0) {
    gg <- GG(model)
  }
  .hf <- function(SSPE, P, error.df, gg) {
    if (nrow(SSPE) < 2) return(NA)
    p <- ncol(P)
    ((error.df + 1) * p * gg - 2)/(p * (error.df - p * gg))
  }
  .m <- model$Anova
  arg <- match.call()
  eps <- vapply(.m$terms, \(i) {
    .hf(
      SSPE = .m$SSPE[[i]],
      P = .m$P[[i]],
      error.df = .m$error.df,
      gg = gg[[i]]
    )
  }, 1)
  return(eps)
}

#' @title Suggested sphericity correction for repeated measures ANOVA
#' @name sphericity_check
#' @description Internal function inside `k_sample`. Return the Spherecity correction suggested based on Mauchly test in one-way repeated measures designs
#'
#' @param model A repeated measures ANOVA model using Afex.

sphericity_check <- function(model) {
  .m <- model$Anova
  if(.m$singular ||  all(mauchly(model) > 0.05)) "none" else {
    is_hf <- all((gg.eps <- GG(model)) > 0.75, na.rm = TRUE)
    is_hf_too <- all(HF(model, gg.eps) > 0.75, na.rm = TRUE)
    if(is_hf || is_hf_too) "HF" else "GG"
  }
}
