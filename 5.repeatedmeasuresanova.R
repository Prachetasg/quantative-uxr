############################################################
# REPEATED MEASURES ANOVA (WITH SPHERICITY + EFFECT SIZES)
# Business Question:
# Over a 4-week onboarding study, does the same group of new sellers
# show a significant change in average session duration across Week 1–4?
#
# H0 (null): Mean session duration is equal across all weeks.
# H1 (alt):  Mean session duration differs for at least one week.
# → Two-tailed within-subjects test (accounts for correlated data)
############################################################

############################################################
# REPEATED MEASURES ANOVA (with assumptions + visuals)
############################################################

## --------------------------
## 1) LOAD PACKAGES & DATA
## --------------------------
install.packages("ggpubr") 
install.packages("rstatix")

library(readxl)      # read Excel files
library(dplyr)       # wrangling
library(tidyr)       # reshape
library(rstatix)     # ANOVA, Mauchly, post-hoc, outliers
library(ggplot2)     # plotting
library(car)         # Levene’s test (if needed)
library(ggpubr)
library(rstatix)

dat <- read_excel("repeatedmeasuresanova.xlsx")   # your long-format dataset
# Columns: participant_id | week | session_duration_min

dat <- dat %>%
  mutate(
    participant_id = factor(participant_id),
    week = factor(week, levels = c("Week1","Week2","Week3","Week4"))
  )

## --------------------------
## 2) QUICK SUMMARY + BOX PLOT
## --------------------------
dat %>%
  group_by(week) %>%
  get_summary_stats(session_duration_min, type = "mean_sd")

ggplot(dat, aes(x = week, y = session_duration_min)) +
  geom_boxplot(outlier.color = "red", alpha = 0.5) +
  geom_jitter(width = 0.05, alpha = 0.6) +
  labs(title = "Session Duration by Week", x = "Week", y = "Duration (min)") +
  theme_minimal()

## --------------------------
## 3) ASSUMPTION CHECKS
## --------------------------

### a) OUTLIERS
dat %>%
  group_by(week) %>%
  identify_outliers(session_duration_min)

### b) NORMALITY — per level + QQ plot
dat %>%
  group_by(week) %>%
  shapiro_test(session_duration_min)

ggqqplot(dat, x = "session_duration_min", facet.by = "week") +
  labs(title = "Q–Q Plot per Week (Normality Check)")

### c) HOMOGENEITY OF VARIANCE (Levene’s test)
leveneTest(session_duration_min ~ week, data = dat)

### d) SPHERICITY — Mauchly’s test
res.aov <- anova_test(
  data = dat,
  dv = session_duration_min,
  wid = participant_id,
  within = week
)
get_anova_table(res.aov)  # includes Mauchly p, GG, HF corrections
res.aov$`Mauchly's Test for Sphericity`


## --------------------------
## 4) RUN RM-ANOVA + CORRECTIONS
## --------------------------
anova_table <- get_anova_table(res.aov)
anova_table #no correction needed

## --------------------------
## 5) POST-HOC COMPARISONS
## --------------------------
posthoc <- dat %>%
  pairwise_t_test(session_duration_min ~ week, paired = TRUE,
                  p.adjust.method = "bonferroni")
posthoc

## --------------------------
## 6) EFFECT SIZE COHEN’S D FOR PAIRED COMPARISONS (WEEK1–WEEK4)
## --------------------------

# convert data to wide format (one column per week)
wide <- tidyr::pivot_wider(dat,
                           id_cols = participant_id,
                           names_from = week,
                           values_from = session_duration_min
)

# helper function for paired Cohen’s d
cohen_d_paired <- function(a, b) {
  diff <- a - b
  mean(diff, na.rm = TRUE) / sd(diff, na.rm = TRUE)
}

# compute d for all week pairs
d_12 <- cohen_d_paired(wide$Week1, wide$Week2)
d_13 <- cohen_d_paired(wide$Week1, wide$Week3)
d_14 <- cohen_d_paired(wide$Week1, wide$Week4)
d_23 <- cohen_d_paired(wide$Week2, wide$Week3)
d_24 <- cohen_d_paired(wide$Week2, wide$Week4)
d_34 <- cohen_d_paired(wide$Week3, wide$Week4)

# show results
data.frame(
  Comparison = c("Week1–Week2", "Week1–Week3", "Week1–Week4",
                 "Week2–Week3", "Week2–Week4", "Week3–Week4"),
  Cohens_d = c(d_12, d_13, d_14, d_23, d_24, d_34)
)


## --------------------------
## 7) MEANS + ERROR BARS PLOT
## --------------------------
means <- dat %>%
  group_by(week) %>%
  summarise(
    mean_duration = mean(session_duration_min),
    sd = sd(session_duration_min),
    se = sd / sqrt(n())
  )

ggplot(means, aes(x = week, y = mean_duration, group = 1)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_duration - se, ymax = mean_duration + se), width = 0.1) +
  labs(title = "Mean Session Duration Across Weeks",
       x = "Week", y = "Mean Duration (±SE)") +
  theme_minimal()



