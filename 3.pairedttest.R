############################################################
# PAIRED SAMPLE T-TEST
# Business Question:
# After 3 weeks of using the redesigned platform, do PARTICIPANTS 
# show a meaningful change in their session duration compared 
# to Week 1?
#
# H0: No difference in mean session duration between Week 1 and Week 3
# H1: Difference exists in mean session duration between Week 1 and Week 3
# → Two-tailed test
############################################################


## --------------------------
## 1) LOAD DATA
## --------------------------
# install.packages("readxl")    # run once if not installed
# install.packages("effsize")   # run once if not installed
# install.packages("effectsize")# run once if not installed
# install.packages("ggplot2")   # run once if not installed
# install.packages("tidyr")     # run once if not installed

library(readxl)
library(effsize)
library(effectsize)
library(ggplot2)
library(tidyr)
library(dplyr)

pairedttest <- read_excel("pairedttest.xlsx")
View(pairedttest)


## --------------------------
## 2) CREATE GROUP VECTORS
## --------------------------
pairedttest$Diff <- pairedttest$Week3_SessionDuration - pairedttest$Week1_SessionDuration


## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Histogram(density)
hist(pairedttest$Diff, main = "Histogram of Session Duration Differences", 
     xlab = "Week3 - Week1", col = "lightblue", border = "black")

# Draw the normal curve
hist(pairedttest$Diff,
     main = "Histogram with Normal Curve: Session Duration Difference",
     xlab = "Week3 - Week1",
     col = "lightblue",
     freq = FALSE)

curve(dnorm(x, mean = mean(pairedttest$Diff, na.rm = TRUE), 
            sd = sd(pairedttest$Diff, na.rm = TRUE)), 
      col = "darkgreen", lwd = 2, add = TRUE)

# QQ plots
qqnorm(pairedttest$Diff)
qqline(pairedttest$Diff, col = "red")


## --------------------------
## 4) ASSUMPTION TESTS
## --------------------------
# Shapiro–Wilk normality test
shapiro.test(pairedttest$Diff)


## --------------------------
## 5) PAIRED T-TEST
## --------------------------
t.test(pairedttest$Week3_SessionDuration,
       pairedttest$Week1_SessionDuration,
       paired = TRUE,
       alternative = "two.sided")


## --------------------------
## 6) EFFECT SIZE (COHEN'S d)
## --------------------------
# Compute Cohen's d with hedge's correction as n<200 per group

cohens_d(
  pairedttest$Week3_SessionDuration,
  pairedttest$Week1_SessionDuration,
  paired = TRUE,
  hedges.correction = TRUE,
  na.rm = TRUE
)

## --------------------------
## 7.1) BOXPLOT OF DIFFERENCES
## --------------------------
# Compute differences
diff <- na.omit(pairedttest$Week3_SessionDuration - pairedttest$Week1_SessionDuration)

# Boxplot of differences
boxplot(diff,
        main = "Distribution of Session Duration Differences",
        ylab = "Week3 - Week1 (minutes)",
        col = "lightblue", border = "darkblue")

abline(h = 0, col = "red", lty = 2)  # reference line at 0


## --------------------------
## 7.2) PAIRED LINE PLOT
## --------------------------

# Long format for plotting
df_long <- pairedttest %>%
  mutate(Change = Week3_SessionDuration - Week1_SessionDuration) %>%
  select(Participant_ID, Week1_SessionDuration, Week3_SessionDuration, Change) %>%
  pivot_longer(
    cols = c("Week1_SessionDuration", "Week3_SessionDuration"),
    names_to = "Week",
    values_to = "SessionDuration"
  ) %>%
  mutate(
    Week = factor(
      Week,
      levels = c("Week1_SessionDuration", "Week3_SessionDuration"),
      labels = c("Week 1", "Week 3")
    ),
    Direction = ifelse(Change > 0, "Increase", "Decrease/No change")
  )

# Spaghetti plot (each participant connected), colored by direction of change
ggplot(df_long, aes(x = Week, y = SessionDuration, group = Participant_ID)) +
  geom_line(aes(color = Direction), alpha = 0.45) +
  geom_point(aes(color = Direction), size = 1.8, alpha = 0.8) +
  # Group mean line/points in red (the "red dashed line")
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               color = "red", linetype = "dashed", linewidth = 1.1) +
  stat_summary(aes(group = 1), fun = mean, geom = "point",
               color = "red", size = 3) +
  labs(
    title = "Paired Changes in Session Duration (Week 1 → Week 3)",
    x = NULL, y = "Session Duration (minutes)",
    color = "Participant Change"
  ) +
  theme_minimal(base_size = 12)

