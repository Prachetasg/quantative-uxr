R Script______________________________________________________________________

############################################################
# CHI-SQUARED GOODNESS-OF-FIT TEST
# Business Question:
# After running our 2-week homepage layout experiment (Apr 1–Apr 14 2025),
# do users show an equal preference among the three homepage versions — A, B, and C —
# or does one layout attract significantly more engagement clicks than expected by chance?
#
# H0: Users click equally on all three layouts (expected = 33.3% each)
# H1: At least one layout receives significantly different click volume than expected
# → Two-tailed test (tests deviation from uniform distribution)
############################################################

## --------------------------
## 1) LOAD PACKAGES
## --------------------------

#install.packages("lsr")

library(readxl)
library(ggplot2)
library(lsr)

## --------------------------
## 2) LOAD DATA
## --------------------------
dat <- read_excel("chisquaredgoodnessoffittest.xlsx")
View(dat)  # optional

## --------------------------
## 3) SUMMARIZE DATA
## --------------------------
# Sum total clicks per layout across all days
a_total <- sum(dat$layout_A_clicks)
b_total <- sum(dat$layout_B_clicks)
c_total <- sum(dat$layout_C_clicks)

layout_table <- c(A = a_total, B = b_total, C = c_total)
layout_table  # sanity check

## --------------------------
## 4) CHI-SQUARED GOODNESS-OF-FIT TEST
## --------------------------
chisqt <- chisq.test(layout_table)
chisqt
chisqt$residuals  # to see which categories deviate most

## --------------------------
## 5) EFFECT SIZE (CRAMÉR’S V)
## --------------------------
cramersV(layout_table)

## --------------------------
## 6) OBSERVED VS EXPECTED VALUES
## --------------------------
# Observed totals (from dataset)
observed <- c(A = 5909, B = 4935, C = 3996)

# Expected totals (equal distribution)
expected <- rep(sum(observed) / 3, 3)

# Combine for plotting
bar_data <- rbind(Observed = observed, Expected = expected)
bar_data  # sanity check

## --------------------------
## 7) VISUALIZATION
## --------------------------
barplot(bar_data,
        beside = TRUE,
        col = c("steelblue", "gray70"),
        ylim = c(0, max(observed) * 1.1),
        main = "Observed vs Expected Clicks per Layout",
        ylab = "Number of Clicks",
        xlab = "Homepage Layout")


