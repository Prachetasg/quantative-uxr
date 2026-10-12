
############################################################
# TWO-WAY ANOVA (2×2 DESIGN)
# Business Question:
# Does session duration differ based on device type (Desktop vs Mobile)
# and homepage version (A vs B)?

# H0₁: No difference in mean session duration across device types.
# H0₂: No difference in mean session duration across homepage versions.
# H0₃: No interaction between device type and homepage version.

# H1: At least one main effect or an interaction effect is significant.

# → Two-tailed test (tests main and interaction effects between
#   two independent factors)
############################################################


## --------------------------
## 1) LOAD PACKAGES & DATA
## --------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)         
library(effectsize)  

dat <- read_excel("twowayanova.xlsx")
View(dat)

## --------------------------
## 2) FACTORS & CONTRASTS
## --------------------------
# Set contrasts BEFORE fitting the model for Type III
op <- options(contrasts = c("contr.sum", "contr.poly"))

dat <- dat %>%
  mutate(
    device  = factor(device,  levels = c("Desktop", "Mobile")),
    version = factor(version, levels = c("A", "B"))
  )

## --------------------------
## 3) TWO-WAY ANOVA (TYPE III)
## --------------------------
fit_lm  <- lm(session_duration_min ~ device * version, data = dat)
anova_t3 <- car::Anova(fit_lm, type = 3)
print(anova_t3)

## --------------------------
## 4) ASSUMPTION CHECKS
## --------------------------
# (a) Homogeneity: Brown–Forsythe version of Levene’s test
car::leveneTest(session_duration_min ~ device * version, data = dat, center = median)

# (b) Normality of residuals (what ANOVA assumes)
res <- residuals(fit_lm)
shapiro.test(res)

# Residual diagnostics
par(mfrow = c(1, 2))
plot(fitted(fit_lm), res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted"); abline(h = 0, lty = 2)
qqnorm(res, main = "Q-Q of Residuals"); qqline(res)
par(mfrow = c(1, 1))

## --------------------------
## 5) EFFECT SIZE (partial η², Type III)
## --------------------------
t3 <- as.data.frame(anova_t3)
SS_error <- t3["Residuals", "Sum Sq"]
t3$partial_eta2 <- t3$`Sum Sq` / (t3$`Sum Sq` + SS_error)
t3[, c("Sum Sq", "Df", "F value", "Pr(>F)", "partial_eta2")]

# Optional cross-check (may default to Type II in some setups; keep manual calc above authoritative)
# effectsize::eta_squared(fit_lm, partial = TRUE)

## --------------------------
## 6) VISUALIZATION (Interaction Plot)
## --------------------------
sumtab <- dat %>%
  group_by(device, version) %>%
  summarise(
    mean = mean(session_duration_min),
    se   = sd(session_duration_min)/sqrt(n()),
    .groups = "drop"
  )

ggplot(sumtab, aes(x = version, y = mean, group = device, color = device)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.05) +
  labs(title = "Interaction: Device × Version",
       x = "Homepage Version",
       y = "Mean Session Duration (min)",
       color = "Device") +
  theme_minimal(base_size = 13)

## --------------------------
## 7) CLEANUP
## --------------------------
options(op)  # restore contrasts


