# Unit testing

# Load data
data <- readRDS(file = "~/Documents/R/WD/IND/data/swim.RDS")

# Load libraries
library(writR)

# One way

## Fisher's exact test
contingency(data, "Sexo", exact = TRUE) # Should throw an error

## Mcnemar's test
contingency(data, "Sexo", paired = TRUE) # Should throw an error

## Goodness-of-fit Chi-square test
a <- contingency(data, "Sexo")

a. <- table(data$Sexo) |> chisq.test()

a$p.value == a.$p.value # TRUE

# Two way 2 x 2

## Fisher's exact test
b <- contingency(data, "Sexo", "cat_Edad", exact = TRUE)

b. <- table(data$Sexo, data$cat_Edad) |> fisher.test()

b$p.value == b.$p.value # TRUE

## Mcnemar's test
c <- contingency(data, "Sexo", "cat_Edad", paired = TRUE)

c. <- table(data$Sexo, data$cat_Edad) |> mcnemar.test()

c$p.value == c.$p.value # TRUE

## Person's Chi-square test
d <- contingency(data, "Sexo", "cat_Edad")

d. <- table(data$Sexo, data$cat_Edad) |> chisq.test()

d$p.value == d.$p.value # TRUE

# Two way 5 x 3

## Fisher's exact test
e <- contingency(data, "Periodo", "cat_Edad", exact = TRUE)

e. <- table(data$Periodo, data$cat_Edad) |> fisher.test()

e$p.value == e.$p.value # TRUE

## Mcnemar's test
contingency(data, "Periodo", "cat_Edad", paired = TRUE) # Should throw an error

## Person's Chi-square test
f <- contingency(data, "Periodo", "cat_Edad")

f. <- table(data$Periodo, data$cat_Edad) |> chisq.test()

f$p.value == f.$p.value # TRUE
