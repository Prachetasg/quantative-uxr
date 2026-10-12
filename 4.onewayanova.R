############################################################
# ONE-WAY ANOVA
# Business Question:
# Does average session duration differ across three homepage design versions
# (A, B, and C) launched during our 2-week experiment?
#
# H0: Mean session duration is the same across all homepage versions.
# H1: At least one homepage version shows a significantly different mean.
# → Two-tailed test (compares means across >2 independent groups)
############################################################


## --------------------------
## 1) LOAD PACKAGES & DATA
## --------------------------
install.packages("rstatix")
install.packages("dplyr")

library(readxl)
library(rstatix)
library(effsize)
library(dplyr)

dat <- read_excel("onewayanova.xlsx")
View(dat)


## --------------------------
## 2) CREATE GROUP VECTORS
## --------------------------
A <- subset(dat, homepage_version == "A")$session_duration
B <- subset(dat, homepage_version == "B")$session_duration
C <- subset(dat, homepage_version == "C")$session_duration

mean(A)
mean(B)
mean(C)


## --------------------------
## 3) VISUAL CHECKS
## --------------------------
# Reset plot layout
par(mfrow = c(1, 3), oma = c(0, 0, 2, 0), mar = c(4, 4, 2, 1))

# Common x and y limits for uniform scaling
xlim <- range(c(A, B, C))
ylim <- c(0, 0.1)

# Homepage A
hist(A,
     main = "Homepage A",
     xlab = "Session Duration (min)",
     col = "lightblue",
     freq = FALSE,
     xlim = xlim, ylim = ylim)
curve(dnorm(x, mean = mean(A), sd = sd(A)),
      col = "darkblue", lwd = 2, add = TRUE)

# Homepage B
hist(B,
     main = "Homepage B",
     xlab = "Session Duration (min)",
     col = "lightgreen",
     freq = FALSE,
     xlim = xlim, ylim = ylim)
curve(dnorm(x, mean = mean(B), sd = sd(B)),
      col = "darkgreen", lwd = 2, add = TRUE)

# Homepage C
hist(C,
     main = "Homepage C",
     xlab = "Session Duration (min)",
     col = "lightpink",
     freq = FALSE,
     xlim = xlim, ylim = ylim)
curve(dnorm(x, mean = mean(C), sd = sd(C)),
      col = "red3", lwd = 2, add = TRUE)

# Optional overall title
mtext("Session Duration Distributions by Homepage Version", outer = TRUE, cex = 1.2)


## --------------------------
## 4) ASSUMPTION TESTS
## --------------------------
shapiro.test(A)
shapiro.test(B)
shapiro.test(C)

# Levene's test for equality of variances
leveneTest(session_duration ~ homepage_version, data = dat)


## --------------------------
## 5) ANOVA
## --------------------------
oneway.test(session_duration ~ homepage_version, data = dat, var.equal = TRUE)


## --------------------------
## 6) POST HOC
## --------------------------
tukey_hsd(dat, session_duration ~ homepage_version)

## --------------------------
## 7) EFFECT SIZE (η²)
## --------------------------
anova_model <- aov(session_duration ~ homepage_version, data = dat)
eta_squared(anova_model)

## --------------------------
## 8) SUMMARY PLOT (BOX + CI)
## --------------------------
# 1) Rename columns once
dat <- dat %>%
  rename(
    homepage_version = `homepage_version`,
    session_duration = `session_duration`
  )

# 2) Summaries for mean ± 95% CI
summary_dat <- dat %>%
  group_by(homepage_version) %>%
  summarise(
    mean = mean(session_duration),
    se   = sd(session_duration) / sqrt(n()),
    ci   = qt(0.975, df = n() - 1) * se,
    .groups = "drop"
  )

# 3) Plot
fill_colors <- c("#F4A6A6", "#A6E3A1", "#A6C8F4")

ggplot(dat, aes(x = homepage_version, y = session_duration, fill = homepage_version)) +
  geom_boxplot(width = 0.55, alpha = 0.5, color = "gray30", outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 1.8, color = "gray40") +
  geom_point(
    data = summary_dat,
    aes(x = homepage_version, y = mean),
    shape = 21, size = 3.5, fill = "black", color = "white",
    inherit.aes = FALSE
  ) +
  geom_errorbar(
    data = summary_dat,
    aes(x = homepage_version, ymin = mean - ci, ymax = mean + ci),
    width = 0.15, color = "black", linewidth = 0.8,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = fill_colors) +
  labs(
    title = "Session Duration by Homepage Version",
    subtitle = "Boxplot with Mean ± 95% Confidence Interval",
    x = "Homepage Version", y = "Session Duration (minutes)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

