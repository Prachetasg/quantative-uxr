############################################################
# BINOMIAL TEST (N<30)
# Business Question:
# Is the proportion of users who signed up for email 
# greater than the KPI benchmark (p0 = 0.25)?
#
# H0: True probability of success (p) = 0.25
# H1: True probability of success (p) > 0.25
# → One-tailed binomial test
############################################################

## --------------------------
## 1) LOAD DATA
## --------------------------
library(readxl)

dat <- read_excel("binomialtest.xlsx")
View(dat)

## --------------------------
## 2) CREATE VARIABLES
## --------------------------
yes_count   <- sum(dat$Email_Signup == "Yes")
total_count <- nrow(dat)

yes_count
total_count

## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Barplot of counts
barplot(table(dat$Email_Signup),
        col = c("lightblue","lightgreen"),
        main = "Email Signup Counts",
        ylab = "Frequency")

## --------------------------
## 4) ASSUMPTION CHECKS
## --------------------------
# Bernoulli trials assumed independent
# Sample size = total_count
# Each outcome is Yes/No

## --------------------------
## 5) BINOMIAL TEST
## --------------------------
p0 <- 0.25   # KPI benchmark

binom.test(yes_count, total_count, 
           p = p0,
           alternative = "greater",
           conf.level = 0.95)

## --------------------------
## 6) EFFECT SIZE (COHEN'S h)
## --------------------------
p_hat <- yes_count / total_count
cohen_h <- 2 * asin(sqrt(p_hat)) - 2 * asin(sqrt(p0))
cohen_h

## --------------------------
## 7) VISUALIZATION
## --------------------------
ci <- binom.test(yes_count, total_count, p = p0)$conf.int

bp <- barplot(c(p_hat, p0),
              names.arg = c("Observed", "Benchmark"),
              col = c("steelblue", "lightcoral"),
              ylim = c(0,1),
              main = "Observed vs KPI Benchmark",
              ylab = "Proportion of Success")

# Error bar for observed
arrows(x0 = bp[1], y0 = ci[1],
       x1 = bp[1], y1 = ci[2],
       angle = 90, code = 3, length = 0.1, lwd = 2)

# Reference line
abline(h = p0, col = "red", lty = 2)

# Add labels
text(bp, c(p_hat, p0) + 0.05, 
     labels = round(c(p_hat, p0), 2))
