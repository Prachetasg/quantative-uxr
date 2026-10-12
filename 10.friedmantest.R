############################################################
#FRIEDMAN TEST (NON-PARAMETRIC REPEATED-MEASURES ANOVA ALTERNATIVE)
#Business Question:
#  Do users’ satisfaction ratings differ across three interaction modes
#(Voice, Gesture, and Controller) after trying all modes in a lab study?
#  H0: Median satisfaction ratings are the same across all three interaction modes.
#H1: At least one interaction mode has a different median satisfaction rating.
#→ Two-tailed non-parametric test (within-subjects design; used instead of repeated-measures ANOVA when normality fails)
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


dat <- read_excel("friedmantest.xlsx")   
View(dat)


## --------------------------
## 2) DATA TRANSFORMATION TO RANK(S)
## --------------------------

# 1. Take only the three satisfaction columns
scores <- dat[, c("voice_satisfaction", "gesture_satisfaction", "controller_satisfaction")]

# 2. Rank each participant’s three scores (1 = lowest, 3 = highest)
ranks <- t(apply(scores, 1, rank))

# 3. Add these ranks back to your data
dat$voice_rank <- ranks[, 1]
dat$gesture_rank <- ranks[, 2]
dat$controller_rank <- ranks[, 3]

# 4. View the final ranked data
View(dat)

## --------------------------
## 4) FRIEDMAN TEST
## --------------------------

friedman.test(
  y = as.matrix(dat[, c("voice_rank", "gesture_rank", "controller_rank")])
)

## --------------------------
## 5) POST-HOC PAIRWISE COMPARISONS (WILCOX TEST)
## --------------------------

pairwise.wilcox.test(
  x = c(dat$voice_satisfaction,
        dat$gesture_satisfaction,
        dat$controller_satisfaction),
  g = rep(c("Voice", "Gesture", "Controller"), each = nrow(dat)),
  paired = TRUE,
  p.adjust.method = "holm"   # adjusts for multiple comparisons
)

## --------------------------
## 6) EFFECT SIZE KENDALL's W
## --------------------------

dat_long <- dat %>%
  pivot_longer(cols = c(voice_satisfaction, gesture_satisfaction, controller_satisfaction),
               names_to = "mode", values_to = "satisfaction")

# Calculate effect size (Kendall's W)
friedman_effsize(satisfaction ~ mode | participant_id, data = dat_long)

## --------------------------
## 7) VISUALIZATION
## --------------------------

ggplot(dat_long, aes(mode, satisfaction)) +
  geom_boxplot(outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.08, alpha = 0.5, size = 1.6) +
  labs(title = "Satisfaction by Mode",
       x = "Interaction Mode", y = "Satisfaction (1–7)") +
  theme_minimal(base_size = 14)
