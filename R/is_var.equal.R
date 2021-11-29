# Check is variance is equal between groups

is_var.equal <- function(y, x, alpha = 0.05, center = stats::median) {
  valid <- stats::complete.cases(y, x)
  meds <- tapply(y[valid], x[valid], center)
  resp <- abs(y - meds[x])
  model <- stats::lm(resp ~ x)
  model <- stats::anova(model)
  model[["Pr(>F)"]][[1]] > alpha
}
