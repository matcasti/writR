# # library(data.table)
#
# # file <- "/Users/matiascastilloaguilar/Documents/Proyectos/Clientes/01-Gallardo-Asencio/data/datos.RDS"
# # data <- readRDS(file)
#
# # data <- writR::swimmers
#
# bootr <- function(data,
#                   y,
#                   x = NULL,
#                   conf.int = 0.95,
#                   conf.type = "bca", # "bca" o "perc"
#                   nboot = 5000,
#                   paired = FALSE,
#                   plot_dist = TRUE,
#                   p_value = FALSE,
#                   seed = 12345) {
#
#   arg <- match.call()
#
#   # Check assumptions on arguments ---------------------------
#
#   if(!"data.table" %chin% class(data)) {
#     data <- as.data.table(data)
#   }
#   if(!is.character(arg$y)) {
#     y <- deparse(substitute(y))
#   }
#   if(!is.null(arg$x) && !is.character(arg$x)) {
#     x <- deparse(substitute(x))
#   }
#
#   # Custom functions -----------------------------------------
#
#   # Vectorized mean
#   .mn <- \(i) {
#     j <- i[!is.na(i)]
#     sum(j) / length(j)
#   }
#   # Drop missing values
#   .na_drop <- \(i) {
#     i[!is.na(i)]
#   }
#   # Confidence interval
#   bootci <- \(conf.type, k, conf.int, nboot) {
#     # Bias-corrected and accelerated confidence interval (BCA)
#     .bca <- \(theta, conf.int) {
#       low <- (1 - conf.int)/2
#       high <- 1 - low
#       sims <- length(theta)
#       z.inv <- length(theta[theta < .mn(theta)])/sims
#       z <- qnorm(z.inv)
#       U <- (sims - 1) * (.mn(theta) - theta)
#       top <- sum(U^3)
#       under <- 6 * (sum(U^2))^{
#         3/2
#       }
#       a <- top/under
#       lower.inv <- pnorm(z + (z + qnorm(low))/(1 - a * (z + qnorm(low))))
#       lower <- quantile(theta, lower.inv, names = FALSE)
#       upper.inv <- pnorm(z + (z + qnorm(high))/(1 - a * (z + qnorm(high))))
#       upper <- quantile(theta, upper.inv, names = FALSE)
#       return(c(lower, upper))
#     }
#
#     # Choose confidence interval
#     if(isTRUE(conf.type == "bca")) {
#       .bca(theta = k, conf.int = conf.int)
#     } else if(isTRUE(conf.type == "perc")) {
#       quantile(x = k, c((1 - conf.int)/2, (1 + conf.int)/2))
#     } else stop("conf.type must be one of 'bca' or 'perc'")
#   }
#   # Plot of Bootstrap distribution
#   .plot_boot <- \(k, ci, mu, y) {
#     oldparams <- par()
#     par(mfrow=c(1,2),
#         mai = c(0.8, 0.2, 0.3, 0.2),
#         oma = c(1, 3, 2, 1))
#
#     # Left plot
#     h <- hist(k, plot = F, breaks = 50)
#     plot(h, main = "Bootstrap histogram + CI",
#          col = fifelse(h$breaks %between% ci, "grey", "grey50"),
#          border = FALSE, freq = T, xlab = y)
#     lines(x = c(mu, mu),
#           y = c(0, h$density[which.max(h$density)]))
#     abline(v = 0)
#
#     # Right plot
#     d <- density(k)
#     plot(d, col = "white", xlab = y, ylab = NA,
#          main = "Bootstrap kernel density + CI", frame.plot = FALSE)
#     polygon(x = d$x, y = d$y, col = "grey30", border = NA)
#     polygon(x = c(ci[[1L]], d$x[d$x %between% ci], ci[[2L]]),
#             y = c(0, d$y[d$x %between% ci],0),
#             col = "grey70", border = NA)
#     lines(x = c(mu, mu), y = c(0, d$y[which.min(abs(d$x - mu))]))
#     abline(v = 0)
#     # Reset params
#     suppressWarnings(par(oldparams))
#   }
#
#   # Body of the function -------------------------------------
#
#   if(is.null(arg$x)) {
#
#     # One sample Bootstrap mean ------------------------------
#
#     # Variable
#     y_var = .na_drop(data[[y]])
#     n <- length(y_var)
#
#     # Set seed for reproducibility
#     set.seed(seed)
#
#     # Bootstrap
#     k <- vapply(seq_len(nboot), \(i) {
#       .mn(y_var[sample.int(n = n, replace = TRUE)])
#     }, 1)
#
#     # Global mean
#     mu <- .mn(k)
#
#     # Confidence interval
#     ci <- bootci(conf.type = conf.type, k = k, conf.int = conf.int)
#
#     # Plot bootstrap mean
#     if(plot_dist) {
#       .plot_boot(k, ci, mu, y)
#     }
#
#     # Output list
#     out <- c(mean = mu, lower = ci[[1]], upper = ci[[2]])
#     return(out)
#
#   } else {
#
#     # Mean difference with Bootstrap resampling --------------
#
#     # Check is same length if paired
#     if(isTRUE(paired)) {
#       data[j = .SD, by = x, .SDcols = y][j = .N, by = x][i = 1:2, j = if(diff(N) != 0) {
#           stop("grups in ", x, " dont have the same length")
#         }]
#     }
#
#     # Theoretical stat
#     k_obs <- diff(data[j = lapply(.SD, .mn), by = x, .SDcols = y][i = 1:2][[y]])
#
#     # Get variable
#     y_var <- data[[y]]
#     x_var <- data[[x]]
#
#     # Complete cases
#     ind.complete <- complete.cases(y_var, x_var)
#
#     # Variables to work with
#     y_var <- y_var[ind.complete]
#     x_var <- x_var[ind.complete]
#
#     # Generate group indices
#     lvl <- levels(x = factor(x_var))
#     if(length(lvl) > 2) {
#       warning("Variable ", x, " has more than two levels.\nUsing this the first two: ", paste(lvl[1:2], collapse = " & "))
#     }
#     ind_1 <- x_var == lvl[[1L]]
#     ind_2 <- x_var == lvl[[2L]]
#
#     # Generate variables by group
#     y_var_a <- y_var[ind_1]
#     y_var_b <- y_var[ind_2]
#
#     # Set seed for reproducibility
#     set.seed(seed)
#
#     # Bootstrap parameters
#     n_a <- length(y_var_a)
#     n_b <- length(y_var_b)
#
#     # Bootstrap within matrix (is FASTER)
#
#     # Group A
#     y_boot_a <-  matrix(
#       data = y_var_a[sample.int(n = n_a, size = n_a*nboot, replace = TRUE)],
#       nrow = n_a, ncol = nboot)
#
#     # Group B
#     y_boot_b <-  matrix(
#       data = y_var_b[sample.int(n = n_b, size = n_b*nboot, replace = TRUE)],
#       nrow = n_b, ncol = nboot)
#
#     # Calculate statistic per each bootstrap sample
#     k <- if(isTRUE(paired)) {
#       vapply(seq_len(nboot), \(i) {
#         .mn(y_boot_a[,i] - y_boot_b[,i])
#       }, 1)
#     } else if (isFALSE(paired)) {
#       vapply(seq_len(nboot), \(i) {
#         .mn(y_boot_a[,i]) - .mn(y_boot_b[,i])
#       }, 1)
#     } else stop("Paired must be one of TRUE or FALSE")
#
#     # Global mean
#     mu <- .mn(k)
#
#     # Confidence interval
#     ci <- bootci(conf.type = conf.type, k = k, conf.int = conf.int)
#
#     # Plot bootstrap mean
#     if(plot_dist) {
#       .plot_boot(k, ci, mu, y)
#     }
#
#     # P-value in favour of null
#     if(isTRUE(p_value)) {
#       n <- length(y_var)
#
#       # Resample from original sample assuming the null to be true
#       # without assuming any grouping structure
#       y_boot <- matrix(
#         data = y_var[sample.int(n = n, size = n*nboot, replace = TRUE)],
#         nrow = n,
#         ncol = nboot)
#
#       # Calculate statistic fitting original grouping structure on
#       # bootstrap samples.
#       #
#       # This will show us how likely the differences are assuming
#       # the null distribution to be true.
#       k_boot = vapply(seq_len(nboot), \(i) {
#         .mn(y_boot[ind_2,i]) - .mn(y_boot[ind_1,i])
#       }, 1)
#
#       # Calculate the probability associated to the null distribution
#       p <- .mn(abs(k_boot) >= abs(k_obs))
#     } else if(isFALSE(p_value)) {
#       p <-  NULL
#     } else stop("p_value needs to be one of TRUE or FALSE")
#
#     # Output list
#     out <- c(mean_diff = mu, lower = ci[[1L]], upper = ci[[2L]], p_value = p)
#     return(out)
#   }
# }
#
# # bootr(data, PS, nboot = 1e4)
# #
# # bootr(data, PS, Sexo, p_value = T, nboot = 1e5)
# #
# # writR::clean_data(data, Periodo, PS, rowid = ID, paired = TRUE) |>
# #   bootr(PS, Periodo, p_value = T, paired = TRUE, , nboot = 1e5)
# #
# # bootr(data[Periodo == "POST1"], FFMI, cat_Edad, p_value = T)
