############################################################
# WILCOXON SIGNED-RANK TEST (PAIRED)
# Business Question:
# Did user satisfaction improve from Week 1 to Week 3 after the redesign?
#
# H0 (null): Median Week 1 satisfaction ≥ Median Week 3 satisfaction
# H1 (alt):  Median Week 1 satisfaction <  Median Week 3 satisfaction
# → One-tailed test (testing improvement)
############################################################

## --------------------------
## 1) LOAD DATA & PACKAGES
## --------------------------
library(readxl)      # loads functions to read Excel files
library(effsize)     # common effect size helpers (not used below but fine to load)
library(effectsize)  # additional effect size utilities (r, CI, etc.)
library(ggplot2)     # plotting system for visualization
library(tidyr)       # data reshaping (e.g., pivot_longer)
library(rstatix)     # tidy wrappers for statistical tests/effect sizes

dat <- read_excel("wilcoxontest.xlsx")  # read data from Excel into a data frame 'dat'
View(dat)                               # open a spreadsheet viewer for quick inspection

## --------------------------
## 2) ENCODE LIKERT → NUMERIC
## --------------------------
likert_levels <- c(                               # explicit ordered levels to map text → numeric
  "Very Dissatisfied",
  "Moderately Dissatisfied",
  "Slightly Dissatisfied",
  "Neither Satisfied nor Dissatisfied",
  "Slightly Satisfied",
  "Moderately Satisfied",
  "Very Satisfied"
)

dat$Week1_Satisfaction_Num <- as.numeric(         # create numeric 1–7 for Week 1
  factor(dat$Week1_Satisfaction,                  # turn Week1 text into an ordered factor
         levels = likert_levels,                  # enforce the intended order of categories
         ordered = TRUE)                          # mark the factor as ordinal
)

dat$Week3_Satisfaction_Num <- as.numeric(         # create numeric 1–7 for Week 3
  factor(dat$Week3_Satisfaction,                  # turn Week3 text into an ordered factor
         levels = likert_levels,                  # same ordering to ensure comparability
         ordered = TRUE)                          # treat as ordinal
)

View(dat)                                         # verify new numeric columns look correct

## --------------------------
## 3) WILCOXON TEST
## --------------------------
wilcox.test(
  dat$Week1_Satisfaction_Num,   # numeric Week 1 ratings
  dat$Week3_Satisfaction_Num,   # numeric Week 3 ratings
  paired = TRUE,                # paired → same participants measured twice
  alternative = "less",         # H1: Week1 < Week3 (improvement)
  exact = FALSE                 # force normal approximation → no tie/zero warnings
)

#CHECK HOW MANY TIES (no change)
sum(dat$Week1_Satisfaction_Num == dat$Week3_Satisfaction_Num, na.rm = TRUE)  
# counts participants where Week1 == Week3 (zero difference)


## --------------------------
## 4) EFFECT SIZE RANK BISERIAL CORRELATION (r)
## --------------------------

dat_long <- tidyr::pivot_longer(                  # reshape to long form for rstatix formula
  dat,                                            # source data
  cols = c(Week1_Satisfaction_Num, Week3_Satisfaction_Num), # the two numeric columns
  names_to = "Week", values_to = "Satisfaction"   # new factor 'Week' and value 'Satisfaction'
)

# Compute Wilcoxon effect size (paired)
dat_long %>%                                      # pipe long data
  wilcox_effsize(Satisfaction ~ Week, paired = TRUE)  # rank-biserial r for paired design

## --------------------------
## 5) VISUALIZATION
## --------------------------

ggplot(dat) +                                     # initialize plot with 'dat'
  # one line per participant from Week1 → Week3
  geom_segment(aes(x = 1, xend = 2,               # draw segments from x=1 to x=2
                   y = Week1_Satisfaction_Num,    # start at Week1 score
                   yend = Week3_Satisfaction_Num, # end at Week3 score
                   group = Participant),          # group lines by participant
               alpha = 0.4) +                     # make lines semi-transparent
  # points at each week
  geom_point(aes(x = 1, y = Week1_Satisfaction_Num), size = 2) +  # dots at Week1
  geom_point(aes(x = 2, y = Week3_Satisfaction_Num), size = 2) +  # dots at Week3
  scale_x_continuous(breaks = c(1, 2), labels = c("Week 1", "Week 3")) + # nice x labels
  labs(title = "Paired Changes in Satisfaction",  # title and axis labels
       x = "Week",
       y = "Satisfaction (1–7 Likert)") +
  theme_minimal()                                  # clean minimal theme



