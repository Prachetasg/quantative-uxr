############################################################
# FISHER’S EXACT TEST (2×2)
# Business Question:
# In our June 2025 pilot test, did displaying the new “Trust Badge”
# (Shown vs Not Shown) influence whether users completed checkout (Yes vs No)?
#
# H0: Checkout completion is independent of Trust Badge visibility.
# H1: Checkout completion is associated with Trust Badge visibility.
# → Two-tailed exact test (used instead of chi-square for small sample sizes < 5 per cell)
############################################################

## --------------------------
## 1) LOAD PACKAGES
## --------------------------
# install.packages(c("readxl", "vcd", "rcompanion", "ggplot2"))  # run once if needed
library(readxl)
library(vcd)
library(rcompanion)
library(ggplot2)

## --------------------------
## 2) LOAD DATA
## --------------------------
dat <- read_excel("fishersexacttest.xlsx")
View(dat)  # optional

## --------------------------
## 3) SUMMARIZE DATA (CONTINGENCY TABLE)
## --------------------------
tab <- table(dat$trust_badge_shown, dat$completed_checkout)
tab
addmargins(tab)

## --------------------------
## 4) FISHER’S EXACT TEST
## --------------------------
fisher.test(tab)

## --------------------------
## 5) EFFECT SIZE (CRAMÉR’S V / PHI)
## --------------------------
cramerV(tab, bias.correct = TRUE)

## --------------------------
## 6) VISUALIZATION (PERCENT STACKED BAR)
## --------------------------
ggplot(dat, aes(x = trust_badge_shown, fill = completed_checkout)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Checkout Completion by Trust Badge Visibility",
    x = "Trust Badge Visibility",
    y = "Percent of Users",
    fill = "Completed Checkout"
  ) +
  theme_minimal(base_size = 14)
