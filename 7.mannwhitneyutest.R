############################################################
# MANN–WHITNEY U (WILCOXON RANK-SUM) TEST
# Business Question:
# Do MOBILE users and DESKTOP users of the redesigned site
# differ in session duration (minutes)?
#
# H0: The distributions of session duration are the same
#     for Mobile and Desktop users
# H1: The distributions differ between Mobile and Desktop users
# → Two-tailed non-parametric test (robust to non-normality/outliers)
############################################################

## --------------------------
## 1) LOAD DATA
## --------------------------
# install.packages("readxl")   # run once if not installed
# install.packages("rstatix")  # run once if not installed

library(readxl)
library(rstatix)


mannwhitneyutest <- read_excel("mannwhitneyutest.xlsx")
View(mannwhitneyutest)

## --------------------------
## 2) CREATE GROUP VECTORS
## --------------------------
Mobile <- subset(mannwhitneyutest, 
                 group == "Mobile")$session_duration_min

Desktop <- subset(mannwhitneyutest, 
                  group == "Desktop")$session_duration_min

## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Histogram: Mobile
par(mfrow = c(1, 2))
hist(Mobile, 
     main = "Histogram with Normal Curve: Mobile Users",
     xlab = "Session Duration (min)", col = "lightblue", freq = FALSE)
# Add a normal distribution curve
curve(dnorm(x, mean = mean(Mobile), sd = sd(Mobile)), 
      col = "darkblue", lwd = 2, add = TRUE)

# Histogram: Desktop
hist(Desktop, 
     main = "Histogram with Normal Curve: Desktop Users",
     xlab = "Session Duration (min)", col = "lightgreen", freq = FALSE)
# Add a normal distribution curve
curve(dnorm(x, mean = mean(Desktop), sd = sd(Desktop)), 
      col = "darkgreen", lwd = 2, add = TRUE)
par(mfrow = c(1, 1))

# QQ plots
par(mfrow = c(1, 2))
qqnorm(Mobile, main = "QQ: Mobile Users"); qqline(Mobile)
qqnorm(Desktop, main = "QQ: Desktop Users"); qqline(Desktop)
par(mfrow = c(1, 1))

## --------------------------
## 4) ASSUMPTION TESTS
## --------------------------
# Shapiro–Wilk normality test
shapiro.test(Mobile)
shapiro.test(Desktop)

## --------------------------
## 5) MANN-Whitney U TEST
## --------------------------

wilcox.test(Mobile,Desktop, alternative ="two.sided")


## --------------------------
## 6) EFFECT SIZE RANK BISERIAL CORRELATION (r)
## --------------------------

wilcox_effsize(mannwhitneyutest, session_duration_min ~ group)
levels(as.factor(mannwhitneyutest$group))


## --------------------------
## BASE R: BARPLOT WITH MEDIANS + IQR ERROR BARS
## --------------------------

# 1) Compute medians, Q1, Q3 by group
groups <- unique(mannwhitneyutest$group)

medians <- tapply(mannwhitneyutest$session_duration_min, 
                  mannwhitneyutest$group, median)

q1 <- tapply(mannwhitneyutest$session_duration_min, 
             mannwhitneyutest$group, function(x) quantile(x, 0.25))

q3 <- tapply(mannwhitneyutest$session_duration_min, 
             mannwhitneyutest$group, function(x) quantile(x, 0.75))

# 2) Draw barplot of medians
bp <- barplot(medians,
              names.arg = groups,
              col = c("lightblue", "lightgreen"),
              ylim = c(0, max(q3) * 1.2),
              main = "Median Session Duration by Group",
              ylab = "Median Duration (minutes)")

# 3) Add error bars (IQR: Q1 → Q3)
arrows(bp, q1, bp, q3,
       angle = 90, code = 3, length = 0.1, lwd = 2)


