
############################################################
#KRUSKAL–WALLIS H TEST (NON-PARAMETRIC ANOVA ALTERNATIVE)
#Business Question:
#  Do median session durations differ across three homepage design versions
#(A, B, and C) during our 2-week experiment, where session durations are
#non-normally distributed and contain outliers?
#  H0: The distributions of session duration are the same across all homepage versions.
#H1: At least one homepage version differs in session duration distribution.
#→ Two-tailed non-parametric test (independent groups; used instead of ANOVA when normality fails)
############################################################

## --------------------------
## 1) LOAD PACKAGES & DATA
## --------------------------

install.packages("FSA")

library(readxl)
library(FSA)
library(rstatix)
library(dplyr)
library(ggplot2)

dat <- read_excel("kruskalwallistest.xlsx")
View(dat)

## --------------------------
## 2) CREATE GROUP VECTORS
## --------------------------

A <- subset(dat, homepage_version == "A")$session_duration_min
B <- subset(dat, homepage_version == "B")$session_duration_min
C <- subset(dat, homepage_version == "C")$session_duration_min

median(A)
median(B)
median(C)

## --------------------------
## 3) VISUAL CHECKS
## --------------------------

# Arrange multiple plots side-by-side (1 row, 3 columns)
par(mfrow = c(1, 3))

# QQ plot for A and B
qqnorm(A, main = "A"); qqline(A)
qqnorm(B, main = "B"); qqline(B)
qqnorm(C, main = "C"); qqline(C)


## --------------------------
## 4) ASSUMPTION TESTS
## --------------------------

# Shapiro–Wilk normality test
shapiro.test(A)
shapiro.test(B)
shapiro.test(C)

# Levene's test for equality of variances
leveneTest(session_duration_min ~ homepage_version, data = dat)


## --------------------------
## 5) T-TEST
## --------------------------

kruskal.test(session_duration_min ~ homepage_version, data = dat)

## --------------------------
## 6) POST HOC
## --------------------------

dunnTest(session_duration_min ~ homepage_version, data = dat, method = "bonferroni")

## --------------------------
## 7) EFFECT SIZE 
## --------------------------

kruskal_effsize(dat, session_duration_min ~ homepage_version)

## --------------------------
## 8) VISUALIZATION
## --------------------------

summary_stats <- dat %>%
  group_by(homepage_version) %>%
  summarise(
    median_duration = median(session_duration_min),
    IQR = IQR(session_duration_min),
    Q1 = quantile(session_duration_min, 0.25),
    Q3 = quantile(session_duration_min, 0.75)
  )

summary_stats

library(ggplot2)

ggplot(dat, aes(x = homepage_version, 
                y = session_duration_min, 
                fill = homepage_version)) +
  geom_boxplot(width = 0.65, 
               alpha = 0.85, 
               color = "gray30", 
               outlier.color = "#D55E00", 
               outlier.shape = 16, 
               outlier.size = 2.5) +
  stat_summary(fun = median, 
               geom = "text", 
               aes(label = round(after_stat(y), 1)),  
               vjust = -0.6, 
               color = "black", 
               size = 4.2, 
               fontface = "bold") +
  scale_fill_manual(values = c("#7EB0D5", "#B2E061", "#FD7F6F")) +
  labs(
    title = "📊 Median and IQR of Session Duration by Homepage Version",
    subtitle = "Non-parametric distributions with outliers shown in red",
    x = "Homepage Version",
    y = "Session Duration (minutes)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#333333"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666666"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

