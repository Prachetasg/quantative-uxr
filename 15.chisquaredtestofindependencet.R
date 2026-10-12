############################################################
# CHI-SQUARED TEST OF INDEPENDENCE (> 2 VARIABLE LEVELS)
# Business Question:
# During our 1-month A/B/C onboarding study (May 2025),
# is device type (Desktop / Tablet / Mobile) associated with signup success
# category (Completed / Abandoned / Timeout)?
#
# H0: Device type and signup outcome are independent.
# H1: There is a statistically significant association between device type and signup outcome.
# → Two-tailed test (tests relationship between categorical variables with > 2 levels each)
############################################################

## --------------------------
## 1) LOAD PACKAGES
## --------------------------
# install.packages(c("readxl","vcd"))  # run once if needed
library(readxl)
library(vcd)

## --------------------------
## 2) LOAD DATA
## --------------------------
dat <- read_excel("chisquaredtestofindependence.xlsx")
# View(dat)  # optional
dat <- na.omit(dat)          #the row with missing device_type is dropped.

## --------------------------
## 3) SUMMARIZE DATA (CONTINGENCY TABLE)
## --------------------------
# Rows: Device Type | Columns: Signup Outcome
tab <- with(dat, table(device_type, signup_outcome))
tab
addmargins(tab)

## --------------------------
## 4) CHI-SQUARED TEST OF INDEPENDENCE
## --------------------------
chi <- chisq.test(tab)
chi

## --------------------------
## 5) RESIDUALS (FOR INTERPRETATION)
## --------------------------
chi$expected                 # expected counts under independence
round(chi$stdres, 3)         # standardized residuals (z-scores)

## --------------------------
## 6) EFFECT SIZE (CRAMÉR’S V)
## --------------------------
# Manual computation (no extra packages needed)
cramersV(tab)

## --------------------------
## 7.1) VISUALIZATION SIMPLE BASE MOSAIC (NO RESIDUAL SHADING)
## --------------------------
mosaicplot(tab,
           main = "Signup Outcome by Device Type",
           xlab = "Device Type", ylab = "Signup Outcome",
           color = TRUE, las = 1, border = "gray40")

## --------------------------
## 7.2) VISUALIZATION (MOSAIC WITH RESIDUAL SHADING - OPTIONAL)
## --------------------------
mosaic(tab,
       main = "Signup Outcome by Device Type (Residual-Shaded Mosaic)",
       xlab = "Device Type", ylab = "Signup Outcome",
       shade = TRUE,                 # color by sign & magnitude of std. residuals
       gp = shading_Friendly,        # blue=over, red=under
       labeling = labeling_values,   # show counts
       labeling_args = list(rot_labels = c(0,0)))  # horizontal labels

