
############################################################
# ONE-PROPORTION Z-TEST (N >/= 30)
# Business Question:
# On the redesigned website, is the proportion of users who 
# successfully complete checkout greater than the benchmark (p0 = 0.60)?
#
# H0: True checkout completion rate (p) = 0.60
# H1: True checkout completion rate (p) > 0.60
# → One-tailed test (testing for improvement)
############################################################

## --------------------------
## 1) LOAD DATA
## --------------------------
library(readxl)  # Load library to read Excel files

dat <- read_excel("oneproportionztest.xlsx")  # Import dataset
View(dat)  # Open data in spreadsheet viedw for quick inspection

## --------------------------
## 2) CREATE VARIABLES
## --------------------------
success_count <- sum(dat$Checkout_Result == "Success")  # Count number of successful checkouts
failure_count <- sum(dat$Checkout_Result == "Failure")  # Count number of failed checkouts
total_count   <- nrow(dat)  # Total number of observations

success_count  # Print successes
failure_count  # Print failures
total_count    # Print total sample size

## --------------------------
## 3) VISUAL CHECKS
## --------------------------
barplot(table(dat$Checkout_Result),     # Create a barplot of successes vs failures
        col  = c("skyblue","lightcoral"), # Colors for bars
        main = "Checkout Results",        # Title of chart
        ylab = "Frequency")               # Y-axis label

## --------------------------
## 4) ASSUMPTION CHECKS
## --------------------------
# Independent Bernoulli trials assumed (each user independent)
# Large sample size for Z-test validity:
#   n*p0 >= 5 and n*(1-p0) >= 5
#   ensures normal approximation holds

## --------------------------
## 5) ONE-PROPORTION Z-TEST
## --------------------------
p0 <- 0.60   # Historical benchmark (60% completion rate)

prop.test(success_count, total_count,  # Test observed vs expected proportion
          p = p0,                      # Null hypothesis proportion
          alternative = "greater",     # One-tailed test (looking for improvement)
          correct = FALSE)             # Disable Yates continuity correction for precision

## --------------------------
## 6) EFFECT SIZE (COHEN'S h)
## --------------------------
p_hat <- success_count / total_count   # Observed proportion
cohen_h <- 2 * asin(sqrt(p_hat)) -     # Formula for Cohen’s h
  2 * asin(sqrt(p0))

cohen_h  # Print effect size

# Interpretation (Cohen, 1988):
# |h| = 0.20 → Small effect
# |h| = 0.50 → Medium effect
# |h| = 0.80 → Large effect

## --------------------------
## 7) VISUALIZATION
## --------------------------
expected_success <- total_count * p0   # Expected successes under H0
expected_failure <- total_count - expected_success  # Expected failures under H0

barplot(                                   # Stacked barplot: Observed vs Expected
  rbind(c(success_count, expected_success),# Row 1: Success counts
        c(failure_count, expected_failure)),# Row 2: Failure counts
  beside    = FALSE,                       # Stacked bars (not side by side)
  names.arg = c("Observed", "Expected"),   # X-axis labels
  col       = c("skyblue", "lightcoral"),  # Colors for Success/Failure
  main      = "Observed vs Expected Checkout Outcomes", # Title
  ylab      = "Count",                     # Y-axis label
  legend.text = c("Success", "Failure")    # Legend labels
)

abline(h = expected_success, col = "orange", lty = 2, lwd = 2)  
# Add threshold line at expected number of successes under H0

