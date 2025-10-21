library(dplyr)

lizards <- read.csv("datasets/HornedLizards.csv")

living <- filter(lizards, Survival == "living") %>% pull(squamosalHornLength) %>% na.omit()
killed <- filter(lizards, Survival == "killed") %>% pull(squamosalHornLength) %>% na.omit()

# alternatively: filter via base r
# living <- lizards$squamosalHornLength[lizards$Survival == "living"]
# killed <- lizards$squamosalHornLength[lizards$Survival == "killed"]


# population 1 = living
# population 2 = killed

# sample size for each population
n1 <- length(living)
n2 <- length(killed)

# dfs for each population
df1 <- n1 - 1
df2 <- n2 - 1

# means for each population
mean1 <- mean(living)
mean2 <- mean(killed)

# var for each population
s1 <- var(living)
s2 <- var(killed)

sd(living)^2 == s1 #note that standard deviation ^ 2 = variance

# Pooled sample variance
s2p <- ((df1*s1) + (df2*s2))/(df1 + df2)

# Standard error of the means

SEM <- sqrt(s2p * (1/n1 + 1/n2))

# t statistic

t <- (mean1 - mean2)/SEM

# DFs for the whole test? total n - 2

test_dfs <- n1 + n2 - 2

# get our p

pt(t, test_dfs, lower.tail = FALSE)

# compare to R's t test (note: add the var.equal = TRUE argument to the
# t.test to make it do the simple version we did by hand. The default test uses
# a correction for unequal varince)

t.test(living, dead , var.equal = TRUE) # pretty dang close!

