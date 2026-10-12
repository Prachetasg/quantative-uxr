############################################################
# TWO-PROPORTION Z-TEST
# Business Question:
# Is the checkout success rate significantly different
# between the old website and the redesigned website?
#
# H0: p_old = p_new        (no difference in success rates)
# H1: p_old ≠ p_new        (difference exists in success rates)
# → Two-tailed test
############################################################

## --------------------------
## 1) LOAD PACKAGES
## --------------------------
library(readxl)
library(epitools)
library(ggplot2)

## --------------------------
## 2) LOAD DATA
## --------------------------
dat <- read_excel("twoproportionztest.xlsx")
View(dat)  # optional

## --------------------------
## 3) CLEAN / SET LEVELS
## --------------------------
# Ensure consistent ordering of groups and outcome labels
dat$`Website Version`  <- factor(dat$`Website Version`,
                                 levels = c("Old Website", "New Website"))
dat$`Checkout Success` <- factor(dat$`Checkout Success`,
                                 levels = c("Yes", "No"))

## --------------------------
## 4) CONTINGENCY TABLE
## --------------------------
tab <- table(dat$`Website Version`, dat$`Checkout Success`)
tab  # sanity check

## --------------------------
## 5) ASSUMPTIONS (NOT CODED)
## --------------------------
# - Two independent groups (Old vs New users)
# - Each observation is a Bernoulli trial (Success = Yes / Failure = No)
# - Large-sample normal approximation reasonable for z-test
#   (rule of thumb: each group's expected successes/failures ≥ ~5)

## --------------------------
## 6) TWO-PROPORTION Z-TEST (two-tailed)
## --------------------------
# Note: Using prop.test with continuity correction disabled to match z-test
ztest <- prop.test(tab, correct = FALSE)
ztest

## --------------------------
## 7) EFFECT SIZE (COHEN'S h)
## --------------------------
# Cohen's h = 2*arcsin(sqrt(p1)) - 2*arcsin(sqrt(p2))
p_by_group <- prop.table(tab, 1)[, "Yes"]  # success proportions by row
cohen_h <- 2 * asin(sqrt(p_by_group["New Website"])) -
  2 * asin(sqrt(p_by_group["Old Website"]))
cohen_h

## --------------------------
## 8) ODDS RATIO (with CIs)
## --------------------------
# Uses epitools; expects a 2x2 table
or <- oddsratio(tab)
or

## --------------------------
## 9) VISUALIZATION
## --------------------------
# Stacked percentage bar chart of success vs failure by version
ggplot(dat, aes(x = `Website Version`, fill = `Checkout Success`)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Checkout Success Rate by Website Version",
       x = "Website Version",
       y = "Percentage of Users") +
  scale_fill_manual(values = c("No" = "tomato", "Yes" = "steelblue")) +
  theme_minimal()



