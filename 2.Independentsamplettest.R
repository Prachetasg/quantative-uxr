############################################################
# INDEPENDENT SAMPLE T-TEST
# Business Question:
# After launching our redesigned website, do NEW USERS and RETURNING USERS 
# show a meaningful difference in session duration?
#
# H0: No difference in mean session duration between groups
# H1: Difference exists in mean session duration
# → Two-tailed test
############################################################


## --------------------------
## 1) LOAD DATA
## --------------------------
# install.packages("readxl")   # run once if not installed
# install.packages("car")      # run once if not installed
# install.packages("effsize")   # run once if not installed
library(readxl)
library(car)
library(effszie)

independentsamplettest <- read_excel("independentsamplettest.xlsx")
View(independentsamplettest)


## --------------------------
## 2) CREATE GROUP VECTORS
## --------------------------
new_users <- subset(independentsamplettest, 
                    User_Type == "New")$Session_Duration_Minutes

returning_users <- subset(independentsamplettest, 
                          User_Type == "Returning")$Session_Duration_Minutes


## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Histogram: New Users
par(mfrow = c(1, 2))
hist(new_users, 
     main = "Histogram with Normal Curve: New Users",
     xlab = "Session Duration (min)", col = "lightblue", freq = FALSE)
# Add a normal distribution curve
curve(dnorm(x, mean = mean(new_users), sd = sd(new_users)), 
      col = "darkblue", lwd = 2, add = TRUE)

# Histogram: Returning Users
hist(returning_users, 
     main = "Histogram with Normal Curve: Returning Users",
     xlab = "Session Duration (min)", col = "lightgreen", freq = FALSE)
# Add a normal distribution curve
curve(dnorm(x, mean = mean(returning_users), sd = sd(returning_users)), 
      col = "darkgreen", lwd = 2, add = TRUE)
par(mfrow = c(1, 1))

# QQ plots
par(mfrow = c(1, 2))
qqnorm(new_users, main = "QQ: New Users"); qqline(new_users)
qqnorm(returning_users, main = "QQ: Returning Users"); qqline(returning_users)
par(mfrow = c(1, 1))


## --------------------------
## 4) ASSUMPTION TESTS
## --------------------------
# Shapiro–Wilk normality test
shapiro.test(new_users)
shapiro.test(returning_users)

# Levene's test for equality of variances
leveneTest(Session_Duration_Minutes ~ User_Type, data = independentsamplettest)

## --------------------------
## 5) T-TEST
## --------------------------
t.test(new_users, returning_users, alternative = "two.sided", var.equal = TRUE)


## --------------------------
## 6) EFFECT SIZE (COHEN'S d)
## --------------------------
# Compute Cohen's d with hedge's correction as n<200 per group
cohen.d(Session_Duration_Minutes ~ User_Type, data = independentsamplettest, hedges.correction = TRUE)
levels(as.factor(independentsamplettest$User_Type))


## --------------------------
## 7) SIMPLE BOX PLOT
## --------------------------
boxplot(Session_Duration_Minutes ~ User_Type, data = independentsamplettest,
        main = "Session Duration by User Type",
        xlab = "User Type", ylab = "Minutes per Session",
        col = c("lightblue", "lightgreen"))
