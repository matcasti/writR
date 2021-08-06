#' @title K Sample test
#' @name k_sample
#' @description A list containing results from a multi-sample test.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character for the grouping factor. Must be present in data
#' @param y Character for the response variable. Must be present in data.
#' @param rowid Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Set `"auto"` (default) for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param sphericity Character. Which sphericity correction of the degrees of freedom should be reported for the within-subject factors. Possible values are "GG" corresponding to the Greenhouse-Geisser correction, "HF" (i.e., Hyunh-Feldt correction), and "none" (i.e., no correction).
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param effsize.type Options are `"unbiased"` or `"omega"` for partial omega squared and `"biased"` or `"eta"` for partial eta squared as a measure of effect size. For non-parametric analysis, Kendalls' W is used for paired designs, where rank epsilon squared is used for independent groups designs.
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param ss_type Type of sum of squares for repeated measures ANOVA (defaults to 3). Possible values are "II", "III", 2, or 3.
#' @param lbl Logical (default FALSE) indicating if a report ready output is desired. This will change the output to a list with characters rather than numeric vectors.
#' @param markdown Logical (default FALSE). If `lbl` is TRUE, then this argument specify if the report-ready labels should be formated for inline code for R markdown (using mathjax and markdown syntax), or if the output should be in plain text (the default).
#' @param internal Logical to whether this function is being used inside of internal functions.
#' @param ... Currently ignored.
#' @importFrom data.table dcast %chin%
#' @importFrom effectsize eta_squared omega_squared kendalls_w rank_epsilon_squared
#' @importFrom afex aov_ez
#' @importFrom stats anova oneway.test as.formula friedman.test kruskal.test
#' @importFrom WRS2 rmanova wmcpAKP t1way
#' @export

k_sample <- function(data, x, y,
                     rowid = NULL,
                     type = "auto",
                     paired = FALSE,
                     var.equal = FALSE,
                     sphericity = "GG",
                     trim = 0.2,
                     nboot = 100L,
                     effsize.type = "unbiased",
                     alternative = "two.sided",
                     conf.level = 0.95,
                     ss_type = 3,
                     lbl = if(is.null(markdown)) FALSE else TRUE,
                     markdown = NULL,
                     internal = FALSE,
                     ...) {

  # Avoid unnecessary computation
  if (!internal) {
    # data cleaning
    data <- clean_data(data, x = x, y = y, rowid = rowid, paired = paired, wide = FALSE)
  }

  # Create vectors of variables
  y_var <- data[[y]]
  x_var <- data[[x]]

  # Levels of 'x' variable
  x_lvl <- levels(x_var)

  # Check normality
  if (type == "auto") {
    normal <- vapply(x_lvl, function(i) {
      is_normal(y_var[x_var == i])
    }, NA, USE.NAMES = FALSE)
    type <- if (all(normal)) "check" else "np"
  }

  # Parametric statistics
  if (type %chin% c("p", "check")) {

    # Assign an effect size
    .f <- if (effsize.type %chin% c("biased", "eta")) {
      effectsize::eta_squared
    } else if (effsize.type %chin% c("unbiased", "omega")) {
      effectsize::omega_squared
    }

    # Check model validity for repeated measures design
    if (paired) {
      model <- afex::aov_ez(
        id = "rowid",
        dv = y,
        data = data,
        within = x,
        include_aov = TRUE,
        type = ss_type)
      if (model$Anova$singular) {
        sphericity <- "none"
        type <- "p"
      }
    }

    # Check assumptions
    if (type == "check") {
      # Sphericity
      if (paired) {
        sphericity <- sphericity_check(model)
      # Homogeneity of variances
      } else {
        var.equal <- is_var.equal(y_var, x_var)
      }
    }

    # Parametric statistics
    if (paired) {
      test <- stats::anova(model, correction = sphericity)

      es <- suppressWarnings({
        .f(model = model, ci = conf.level, verbose = FALSE)
      })

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$F,
        df = as.double(test[["num Df"]]),
        df.error = as.double(test[["den Df"]]),
        p.value = test[["Pr(>F)"]],
        method = if (sphericity == "none") {
          "Fisher's repeated measures ANOVA"
        } else {
          paste("Repeated measures ANOVA with", sphericity, "correction")
        },
        alternative = NA_character_,
        estimate = es[[2L]],
        conf.level = es[["CI"]],
        conf.low = es[["CI_low"]],
        conf.high = es[["CI_high"]],
        effectsize = names(es)[2L],
        n_obs = length(y_var) / length(x_lvl)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    } else {

      test <- stats::oneway.test(
        formula = y_var ~ x_var,
        var.equal = var.equal
      )

      es <- .f(test, ci = conf.level, verbose = FALSE)

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$statistic,
        df = test$parameter[["num df"]],
        df.error = test$parameter[["denom df"]],
        p.value = test$p.value,
        method = if (var.equal) "Fisher's ANOVA" else "Welch's ANOVA",
        alternative = NA_character_,
        estimate = es[[1L]],
        conf.level = es[["CI"]],
        conf.low = es[["CI_low"]],
        conf.high = es[["CI_high"]],
        effectsize = names(es)[1L],
        n_obs = length(y_var)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    }
  }
  # Non-parametric statistics
  if (type == "np") {
    # Friedman rank-sum test for dependent samples
    if (paired) {
      test <- stats::friedman.test(
        y = y_var,
        groups = x_var,
        blocks = data$rowid
      )

      es <- effectsize::kendalls_w(
        x = y_var,
        groups = x_var,
        blocks = data$rowid,
        ci = conf.level,
        verbose = FALSE
      )

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$statistic,
        df = as.double(test$parameter),
        df.error = NA_real_,
        p.value = test$p.value,
        method = test$method,
        alternative = NA_character_,
        estimate = es[[1L]],
        conf.level = es[["CI"]],
        conf.low = es[["CI_low"]],
        conf.high = es[["CI_high"]],
        effectsize = names(es)[1L],
        n_obs = length(y_var) / length(x_lvl)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    # Kruskal-Wallis rank-sum test for independent samples
    } else {
      test <- stats::kruskal.test(
        x = y_var,
        g = x_var
      )

      es <- effectsize::rank_epsilon_squared(
        x = y_var,
        groups = x_var,
        ci = conf.level
      )

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$statistic,
        df = as.double(test$parameter),
        df.error = NA_real_,
        p.value = test$p.value,
        method = test$method,
        alternative = NA_character_,
        estimate = es[[1L]],
        conf.level = es[["CI"]],
        conf.low = es[["CI_low"]],
        conf.high = es[["CI_high"]],
        effectsize = names(es)[1L],
        n_obs = length(y_var)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    }
  }
  # Robust statistics
  if (type == "r") {
    # one-way repeated measures ANOVA for trimmed means
    if (paired) {
      test <- WRS2::rmanova(
        y = y_var,
        groups = x_var,
        blocks = data$rowid,
        tr = trim
      )

      formula <- stats::as.formula(
        object = paste0("rowid ~", x)
      )

      es <- WRS2::wmcpAKP(
        x = data.table::dcast(data, formula, value.var = y)[,-1L],
        tr = trim,
        nboot = nboot
      )

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$test,
        df = as.double(test$df1),
        df.error = as.double(test$df2),
        p.value = test$p.value,
        method = "one-way repeated measures ANOVA for trimmed means",
        alternative = NA_character_,
        estimate = es[[1L]],
        conf.level = 0.95,
        conf.low = es[[2L]],
        conf.high = es[[3L]],
        effectsize = "Algina-Keselman-Penfield robust standardized difference average",
        n_obs = length(y_var) / length(x_lvl)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    # one-way ANOVA for trimmed means
    } else {
      test <- WRS2::t1way(
        formula = y_var ~ x_var,
        tr = trim,
        nboot = nboot
      )

      test <- list(
        "y" = y,
        "x" = x,
        statistic = test$test,
        df = as.double(test$df1),
        df.error = as.double(test$df2),
        p.value = test$p.value,
        method = "one-way ANOVA for trimmed means",
        alternative = NA_character_,
        estimate = test$effsize,
        conf.level = 0.95,
        conf.low = test$effsize_ci[[1L]],
        conf.high = test$effsize_ci[[2L]],
        effectsize = "Explanatory measure of effect size",
        n_obs = length(y_var)
      )

      if(lbl) {
        test <- lablr(test, markdown)
      }

      class(test) <- c("writR", "list")

      return(test)
    }
  }
}
