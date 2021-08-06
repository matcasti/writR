#' @title Pairwise Comparisons
#' @name pairwise_test
#' @description Pairwise comparisons for Post-hoc testing.
#'
#' @param data Data frame from which `x` and `y` (and possibly `rowid` if provided) will be searched.
#' @param x Character for the grouping factor. Must be present in data
#' @param y Character for the response variable. Must be present in data.
#' @param rowid Character for the subject-id column. If null, then is assumed that data is sorted for paired designs, creating one. So if your data is not sorted and you leave this argument unspecified, the results can be inaccurate when there are more than two levels in x and there are NAs present.
#' @param type Set `"auto"` (default) for checking the normality and homogeneity of variances for test selection. Other options are `"p"` for parametric, `"np"` for non-parametric and `"r"` for robust tests.
#' @param paired Logical that decides whether the experimental design is repeated measures/within-subjects or between-subjects. The default is `FALSE.`
#' @param var.equal Logical variable indicating whether to treat the two variances as being equal. If TRUE then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param trim Trim level for the mean when carrying out robust tests. In case of an error, try reducing the value of tr, which is by default set to 0.2. Lowering the value might help.
#' @param nboot Number of bootstrap samples for computing confidence interval for the effect size (Default: 100L).
#' @param p.adjust.method Adjustment method for p-values for multiple comparisons. Possible methods are: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none" (default).
#' @param alternative A character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level Confidence/Credible Interval (CI) level. Default to 0.95 (95%).
#' @param internal Logical to whether this function is being used inside of other functions.
#' @param ... Currently ignored.
#' @importFrom data.table data.table %chin% fifelse
#' @importFrom stats pairwise.t.test p.adjust na.omit
#' @importFrom WRS2 lincon rmmcp
#' @importFrom PMCMRplus durbinAllPairsTest kwAllPairsDunnTest gamesHowellTest
#' @export

pairwise_test <- function(data, x, y,
                          rowid = NULL,
                          type = "auto",
                          paired = FALSE,
                          var.equal = FALSE,
                          trim = 0.2,
                          nboot = 100L,
                          p.adjust.method = "none",
                          alternative = "two.sided",
                          conf.level = 0.95,
                          internal = FALSE,
                          ...) {

  # Avoid unnecesary computation
  if (!internal) {
    # Data cleaning
    data <- clean_data(data, x = x, y = y, rowid = rowid, paired = paired, wide = FALSE)
  }

  # Create vectors of variables
  y_var <- data[[y]]
  x_var <- data[[x]]

  # Levels of 'x'
  x_lvl <- levels(x_var)

  # Check for normality
  if (type == "auto") {
    normal <- vapply(x_lvl, function(i) {
      is_normal(y_var[x_var == i])
    }, NA, USE.NAMES = FALSE)
    type <- if (all(normal)) "check" else "np"
  }

  # Parametric statistics
  if(type %chin% c("p", "check")) {

    # Check homogeneity of variances
    if (type == "check") {
      var.equal <- is_var.equal(y_var, x_var)
    }

    # t-test
    if(var.equal) {
      test <- suppressWarnings(
        expr = stats::pairwise.t.test(
          x = y_var,
          g = x_var,
          p.adjust.method = "none",
          paired = paired,
          alternative = alternative
        )
      )

      test <- stats::na.omit(
        object = data.table::data.table(
          group1 = rep(rownames(test$p.value), each = ncol(test$p.value)),
          group2 = rep(colnames(test$p.value), times = nrow(test$p.value)),
          p = stats::p.adjust(as.numeric(t(test$p.value)), method = p.adjust.method)
        )
      )

      message("\n", fifelse(paired, "Paired ", ""),"Pairwise t-test.\n", "Correction: ",p.adjust.method)
      return(test)

    # Games-Howell test
    } else {
      test <- suppressWarnings(
        expr = PMCMRplus::gamesHowellTest(
          x = y_var,
          g = x_var
        )
      )

      test <- stats::na.omit(
        object = data.table::data.table(
          group1 = rep(rownames(test$p.value), each = ncol(test$p.value)),
          group2 = rep(colnames(test$p.value), times = nrow(test$p.value)),
          statistic = as.numeric(t(test$statistic)),
          p = stats::p.adjust(as.numeric(t(test$p.value)), method = p.adjust.method)
        )
      )

      message("\nGames-Howell Test.\n", "Correction: ", p.adjust.method)
      return(test)
    }
  }
  # Non-parametric statistics
  if(type == "np") {
    if(paired) {
      # PMCMRplus::durbinAllPairsTest
      test <- suppressWarnings(
        expr = PMCMRplus::durbinAllPairsTest(
          y = y_var,
          groups = x_var,
          blocks = data[["rowid"]],
          p.adjust.method = "none"
        )
      )

      test <- stats::na.omit(
        data.table::data.table(
          group1 = rep(rownames(test$p.value), each = ncol(test$p.value)),
          group2 = rep(colnames(test$p.value), times = nrow(test$p.value)),
          statistic = as.numeric(t(test$statistic)),
          p = stats::p.adjust(as.numeric(t(test$p.value)), method = p.adjust.method)
        )
      )

      message("\nDurbin-Conover test.\n", "Correction: ", p.adjust.method)
      return(test)
    } else {
      # Dunn test
      test <- suppressWarnings(
        expr = PMCMRplus::kwAllPairsDunnTest(
          x = y_var,
          g = x_var,
          p.adjust.method = "none"
        )
      )

      test <- stats::na.omit(
        data.table::data.table(
          group1 = rep(rownames(test$p.value), each = ncol(test$p.value)),
          group2 = rep(colnames(test$p.value), times = nrow(test$p.value)),
          statistic = as.numeric(t(test$statistic)),
          p = stats::p.adjust(as.numeric(t(test$p.value)), method = p.adjust.method)
        )
      )

      message("\nDunn test.\n", "Correction: ", p.adjust.method)
      return(test)
    }
  }
  # Robust statistics
  if(type == "r") {
    # Yuen's trimmed means test - Paired groups
    if(paired) {
      test <- suppressWarnings(
        expr = WRS2::rmmcp(
          y = y_var,
          groups = x_var,
          blocks = data[["rowid"]],
          tr = trim
        )
      )

      test <- data.table::data.table(
        group1 = test$fnames[test$comp[, 1]],
        group2 = test$fnames[test$comp[, 2]],
        estimate = test$comp[, "psihat"],
        conf.level = 0.95,
        conf.low = test$comp[, "ci.lower"],
        conf.high = test$comp[, "ci.upper"],
        p = stats::p.adjust(as.numeric(t(test$comp[, "p.value"])), method = p.adjust.method)
      )

      message("\nYuen's trimmed means test - Paired groups.\n", "Correction: ", p.adjust.method)
      return(test)
    # Yuen's trimmed means test - Independent groups
    } else {
      test <- WRS2::lincon(
        formula = y_var ~ x_var,
        tr = trim
      )

      test <- data.table::data.table(
        group1 = test$fnames[test$comp[, 1]],
        group2 = test$fnames[test$comp[, 2]],
        estimate = test$comp[, "psihat"],
        conf.level = 0.95,
        conf.low = test$comp[, "ci.lower"],
        conf.high = test$comp[, "ci.upper"],
        p = stats::p.adjust(as.numeric(t(test$comp[, "p.value"])), method = p.adjust.method)
      )

      message("\nYuen's trimmed means test - Independent groups.\n", "Correction: ", p.adjust.method)
      return(test)
    }
  }
}
