############################################################
# SPEARMAN’S RANK CORRELATION
# Business Question:
# Is there evidence that slower task completion impacts how efficient
# users feel during the experience?
#
# Variables:
# X = Task completion time (ranked or continuous with outliers)
# Y = Task efficiency rating (ordinal, 1 = very low efficiency, 7 = very high efficiency)
#
# H0 (null): ρs = 0 (no monotonic relationship between variables)
# H1 (alt): ρs ≠ 0 (a monotonic relationship exists)
# → Two-tailed non-parametric test
#
# Notes:
# Use when data are not normally distributed, contain outliers,
# or use ordinal scales (e.g., 1–7 ratings).
# Ideal for detecting monotonic trends where users who take longer 
# to complete a task tend to rate their efficiency lower.
#
# Effect size: Spearman’s ρ (rho)
############################################################


## --------------------------
## 1) LOAD DATA
## --------------------------
# Run these only once if not installed
# install.packages("readxl")
# install.packages("ggplot2")
# install.packages("dplyr")

library(readxl)
library(ggplot2)
library(dplyr)

# Adjust path if needed
dat <- read_excel("spearmancorrelation.xlsx")
View(dat)  # uncomment if using RStudio

## --------------------------
## 2) CREATE VECTORS
## --------------------------
task_time_sec      <- dat$task_time_sec
efficiency_rating  <- dat$efficiency_rating  # 1–7 ordinal

## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Scatter with LOESS (monotonic trend check; not assuming linearity)
ggplot(dat, aes(x = task_time_sec, y = efficiency_rating)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Monotonic Relationship Check",
       x = "Task Completion Time (sec)",
       y = "Efficiency Rating (1–7)")

## --------------------------
## 4) TIES & DISTRIBUTION QUICK CHECKS
## --------------------------
# Count tied ranks (ties are OK; they slightly affect exact p, so use exact=FALSE later)
sum(duplicated(rank(task_time_sec)))
sum(duplicated(rank(efficiency_rating)))

# Skim time distribution (non-normal is OK for Spearman)
summary(task_time_sec)

## --------------------------
## 5) SPEARMAN’S RANK CORRELATION
## --------------------------
res_spear <- cor.test(task_time_sec,
                      efficiency_rating,
                      method = "spearman",
                      exact  = FALSE)   # safer with ties / n>30
res_spear

## --------------------------
## 6) EFFECT SIZE
## --------------------------
# Spearman’s rho is the effect size. Also report rho^2 (variance explained in ranks).
rho  <- as.numeric(res_spear$estimate)
rho2 <- rho^2
list(spearman_rho = rho, rho_squared = rho2)

# Guideline (rule-of-thumb): |rho| ≈ .10 small, .30 medium, .50 large

## --------------------------
## 7) OPTIONAL: RANK–RANK PLOT (diagnostic)
## --------------------------
# Create rank columns
dat$rank_task_time     <- rank(dat$task_time_sec, ties.method = "average")
dat$rank_efficiency    <- rank(dat$efficiency_rating, ties.method = "average")

# Scatter plot of ranks
ggplot(dat, aes(x = rank_task_time, y = rank_efficiency)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  labs(title = "Scatter Plot of Ranks (Spearman’s Rank Correlation Diagnostic)",
       x = "Rank of Task Completion Time (1 = Fastest)",
       y = "Rank of Efficiency Rating (1 = Lowest)") +
  theme_minimal()

# Interpretation template:
# “There is a [negative/positive], [small/medium/large] monotonic association between task time and efficiency,
# Spearman’s ρ = {rho}, p = {res_spear$p.value}. Participants who took longer tended to report lower efficiency.”


ata = dat)
plot(model, which = 1)  # Residuals vs Fitted values

