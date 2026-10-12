############################################################
# PEARSON CORRELATION
# Business Question:
# On the redesigned website, is there a linear relationship between
# session duration (minutes) and satisfaction score (0–100)?
#
# Variables:
# X = Session duration (continuous)
# Y = Satisfaction score (continuous)
#
# H0 (null): ρ = 0 (no linear correlation between duration and satisfaction)
# H1 (alt): ρ ≠ 0 (a linear correlation exists)
# → Two-tailed test
#
# Notes:
# Assumptions — linearity, bivariate normality, independence; check outliers.
# Effect size: r (with 95% CI). Also report r² as variance explained.
############################################################

## --------------------------
## 1) LOAD PACKAGES & DATA
## --------------------------

library(readxl)      
library(dplyr)       
library(tidyr)       
library(rstatix)     
library(ggplot2)     
library(car)         
library(ggpubr)      

dat <- read_excel("pearsoncorrelation.xlsx")
View(dat)


## --------------------------
## 2.1) ASSUMPTION TESTS
## --------------------------

# Shapiro–Wilk normality test
shapiro.test(dat$session_duration_min)
shapiro.test(dat$satisfaction_score)

# QQ plots
qqnorm(dat$session_duration_min)
qqline(dat$session_duration_min, col = "red")

qqnorm(dat$satisfaction_score)
qqline(dat$satisfaction_score, col = "red")


## --------------------------
## 2.2) ASSUMPTION TESTS - LINEAR CORRELATION
## --------------------------

ggplot(dat, aes(x = session_duration_min, y = satisfaction_score)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  labs(
    title = "Relationship Between Session Duration and Satisfaction",
    x = "Session Duration (minutes)",
    y = "Satisfaction Score (0–100)"
  ) +
  theme_minimal(base_size = 14)


## --------------------------
## 4) CORRELATION TEST
## --------------------------

cor.test(dat$session_duration_min, 
         dat$satisfaction_score, 
         method = "pearson")


## --------------------------
## 5) LINEAR MODEL CHECK
## --------------------------

model <- lm(satisfaction_score ~ session_duration_min, data = dat)
plot(model, which = 1)  # Residuals vs Fitted values
