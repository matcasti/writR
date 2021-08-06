# Unit testing

# Load data
data <- readRDS(file = "~/Documents/R/WD/IND/data/swim.RDS")

# Load libraries
library(writR)

# 1. K-samples

## Paired samples

### Parametric - Fisher's rmANOVA
a <- k_sample(data, "Periodo", "FEV1", rowid = "ID", type = "p", paired = TRUE, sphericity = "none")
print(a)
lablr(a)

a. <- data |>
  subset(Periodo %in% c("BASAL", "PRE1", "PRE2")) |> # Other levels from 'Periodo' are empty
  afex::aov_ez(id = "ID", dv = "FEV1", within = "Periodo",
               anova_table = list(correction = "none"))

a$p.value == a.$anova_table$`Pr(>F)` # TRUE

### Parametric - rmANOVA with GG correction
b <- k_sample(data, "Periodo", "FEV1", rowid = "ID", type = "p", paired = TRUE, sphericity = "GG")
print(b)
lablr(b)

b. <- data |>
  subset(Periodo %in% c("BASAL", "PRE1", "PRE2")) |> # Other levels from 'Periodo' are empty
  afex::aov_ez(id = "ID", dv = "FEV1", within = "Periodo",
               anova_table = list(correction = "GG"))

b$p.value == b.$anova_table$`Pr(>F)` # TRUE

### Parametric - rmANOVA with HF correction
c <- k_sample(data, "Periodo", "FEV1", rowid = "ID", type = "p", paired = TRUE, sphericity = "HF")
print(c)
lablr(c)

c. <- data |>
  subset(Periodo %in% c("BASAL", "PRE1", "PRE2")) |> # Other levels from 'Periodo' are empty
  afex::aov_ez(id = "ID", dv = "FEV1", within = "Periodo",
               anova_table = list(correction = "HF"))

c$p.value == c.$anova_table$`Pr(>F)` # TRUE

### Non-parametric - Friedman rank-sum test
d <- k_sample(data, "Periodo", "FEV1", rowid = "ID", type = "np", paired = TRUE)
print(d)
lablr(d)

d. <- clean_data(data, "Periodo", "FEV1", "ID", paired = TRUE, wide = TRUE)[,-1L] |>
  as.matrix() |>
  friedman.test()

d$p.value == d.$p.value # TRUE

### Robust - rmANOVA on trimmed means
e <- k_sample(data, "Periodo", "FEV1", rowid = "ID", type = "r", paired = TRUE)
print(e)
lablr(e)

e. <- clean_data(data, "Periodo", "FEV1", "ID", paired = TRUE) |>
  with({
    WRS2::rmanova(y = FEV1,
              groups = Periodo,
              blocks = rowid)
  })

e$p.value == e.$p.value # TRUE

## Independent samples

### Parametric - Fisher's ANOVA
f <- k_sample(data, "Periodo", "FEV1", type = "p", var.equal = TRUE)
print(f)
lablr(f)

f. <- oneway.test(FEV1 ~ Periodo, data,
            subset = Periodo %in% c("BASAL", "PRE1", "PRE2"),
            var.equal = TRUE)

f$p.value == f.$p.value # TRUE

### Parametric - Welch's ANOVA
g <- k_sample(data, "Periodo", "FEV1", type = "p", var.equal = FALSE)
print(g)
lablr(g)

g. <- oneway.test(FEV1 ~ Periodo, data,
            subset = Periodo %in% c("BASAL", "PRE1", "PRE2"),
            var.equal = FALSE)

g$p.value == g.$p.value # TRUE

### Non-parametric - Kruskal-Wallis
h <- k_sample(data, "Periodo", "FEV1", type = "np")
print(h)
lablr(h)

h. <- kruskal.test(FEV1 ~ Periodo, data,
             subset = Periodo %in% c("BASAL", "PRE1", "PRE2"))

h$p.value == h.$p.value # TRUE

### Robust - ANOVA on trimmed means
i <- k_sample(data, "Periodo", "FEV1", type = "r")
print(i)
lablr(i)

i. <- clean_data(data, "Periodo", "FEV1") |>
  WRS2::t1way(formula = FEV1 ~ Periodo)

i$p.value == i.$p.value # TRUE

# 2. Two-samples

## Paired samples

### Parametric - Student-t test
j <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", rowid = "ID", type = "p", paired = TRUE)
print(j)
lablr(j)

j. <- clean_data(data, "Periodo", "FEV1", paired = TRUE) |>
stats:::t.test.formula(formula = FEV1 ~ Periodo,
                       subset = Periodo %in% c("PRE1", "PRE2"), paired = TRUE)

j$p.value == j.$p.value # TRUE

### Non-parametric - Wilcoxon signed-rank test
k <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", rowid = "ID", type = "np", paired = TRUE)
print(k)
lablr(k)

k. <- clean_data(data, "Periodo", "FEV1", paired = TRUE) |>
  stats:::wilcox.test.formula(formula = FEV1 ~ Periodo, paired = TRUE,
                              subset = Periodo %in% c("PRE1", "PRE2"))

k$p.value == k.$p.value # TRUE

### Robust - Yuen's t-test on trimmed means
l <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", rowid = "ID", type = "r", paired = TRUE, trim = 0.2)
print(l)
lablr(l)

l. <- clean_data(
  data = data[Periodo %in% c("PRE1", "PRE2")],
  x = "Periodo", y = "FEV1", rowid = "ID", paired = TRUE, wide = TRUE)[,-1L] |>
  with({
    WRS2::yuend(x = PRE1, y = PRE2, tr = 0.2)
  })

l$p.value == l.$p.value # TRUE

## Independent samples

### Parametric - Student-t test
m <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", type = "p", var.equal = TRUE)
print(m)
lablr(m)

m. <- clean_data(data, "Periodo", "FEV1") |>
  stats:::t.test.formula(formula = FEV1 ~ Periodo, var.equal = TRUE,
                         subset = Periodo %in% c("PRE1", "PRE2"))

m$p.value == m.$p.value # TRUE

### Parametric - Welch-t test
n <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", type = "p", var.equal = FALSE)
print(n)
lablr(n)

n. <- clean_data(data, "Periodo", "FEV1") |>
  stats:::t.test.formula(formula = FEV1 ~ Periodo, var.equal = FALSE,
                         subset = Periodo %in% c("PRE1", "PRE2"))

n$p.value == n.$p.value # TRUE

### Non-parametric - Wilcoxon sum-rank test
o <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", type = "np")
print(o)
lablr(o)

o. <- clean_data(data, "Periodo", "FEV1") |>
  stats:::wilcox.test.formula(formula = FEV1 ~ Periodo,
                         subset = Periodo %in% c("PRE1", "PRE2"))

o$p.value == o.$p.value # TRUE

### Robust - Yuen's t-test on trimmed means
p <- two_sample(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1", type = "r")
print(p)
lablr(p)

p. <- clean_data(data[Periodo %in% c("PRE1", "PRE2")], "Periodo", "FEV1") |>
  WRS2::yuen(formula = FEV1 ~ Periodo, tr = 0.2)

p$p.value == p.$p.value # TRUE
