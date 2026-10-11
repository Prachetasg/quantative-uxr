
############################################################
# ONE-SAMPLE T-TEST
# Business Question:
# Following the launch of our redesigned website, do users spend
# significantly more time per session than the historical
# benchmark of 30 minutes?
#
# H0 (null): Users spend the same or less time on the redesigned website
# H1 (alt):  Users spend more time on the redesigned website
# → One-tailed test
############################################################

## --------------------------
## 1) LOAD DATA
## --------------------------
# Run these only once if not installed
# install.packages("readxl")
# install.packages("car")
# install.packages("effsize")

library(readxl)
library(car)
library(effsize)

# Adjust path if needed
onesamplettest <- read_excel("onesamplettest.xlsx")
View(onesamplettest)

## --------------------------
## 2) CREATE VECTOR
## --------------------------
time_spent <- onesamplettest$`Time on New Website (minutes)`

## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Histogram with Normal Curve
hist(time_spent,
     main = "Histogram with Normal Curve: Time Spent",
     xlab = "Session Duration (minutes)",
     col = "lightblue",
     freq = FALSE)

curve(dnorm(x, mean = mean(time_spent), sd = sd(time_spent)), 
      col = "darkgreen", lwd = 2, add = TRUE)

# QQ plot
qqnorm(time_spent, main = "QQ Plot: Session Durations")
qqline(time_spent, col = "hotpink")

## --------------------------
## 4) NORMALITY TEST
## --------------------------
shapiro.test(time_spent)
# p > 0.05 → data not significantly different from normal

## --------------------------
## 5) ONE-SAMPLE T-TEST
## --------------------------
t.test(time_spent, mu = 30, alternative = "greater")

# Following the redesign, average session duration increased from the historical 30 minutes to ~32 minutes.
# The difference is statistically significant (p < 0.05), with an estimated true increase of ~1–3 minutes per session.

## --------------------------
## 6) EFFECT SIZE
## --------------------------
# Cohen’s d & Hedges g for one-sample t-test 
hedges_g_one_sample <- function(x, mu){
  d  <- (mean(x) - mu) / sd(x)
  df <- length(x) - 1
  J  <- 1 - 3/(4*df - 1)
  g  <- d * J
  list(d = d, hedges_g = g)
}
hedges_g_one_sample(time_spent, 30)

# Session duration increased significantly after the redesign (t(19) = 3.37, p < 0.05, Cohen’s d = 0.75),
# representing a medium-to-large effect size — strong evidence of improved user engagement.

## --------------------------
## 7) VISUALIZATION
## --------------------------
boxplot(time_spent,
        main = "Session Duration vs 30-min Benchmark",
        ylab = "Minutes per Session",
        col  = "lightblue")

abline(h = 30, col = "red", lwd = 2, lty = 2)  # Add benchmark line
