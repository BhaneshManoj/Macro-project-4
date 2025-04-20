# Set working directory
setwd("~/Desktop/Spring24_MSECON/MACRO/project 4")

# Load required packages
if (!require("ipumsr")) install.packages("ipumsr")
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("scales")) install.packages("scales")
if (!require("dplyr")) install.packages("dplyr")

library(ipumsr)
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)

# Read CPS data using ipumsr
ddi <- read_ipums_ddi("cps_00003.xml")
data <- read_ipums_micro(ddi)
setDT(data)

# Filter: working-age, valid EMPSTAT, and year >= 2010
data <- data[AGE >= 16 & EMPSTAT %in% 10:36 & YEAR >= 2010]

# =============== PART 1: LEVELS =============== #

# Employment status categories
data[, EMP_STATUS := fifelse(EMPSTAT %in% c(10, 12), 1,  # Employed
                             fifelse(EMPSTAT %in% c(20, 21, 22), 2,  # Unemployed
                                     fifelse(EMPSTAT %in% c(1, 30:36), 3, NA_integer_)))]

# Aggregate by month for levels
monthly_indicators <- data[!is.na(EMP_STATUS),
                           .(
                             employed = sum(EMP_STATUS == 1),
                             unemployed = sum(EMP_STATUS == 2),
                             not_in_lf = sum(EMP_STATUS == 3),
                             total = .N
                           ),
                           by = .(YEAR, MONTH)
][
  ,
  `:=`(
    employment_rate = employed / total,
    unemployment_rate = unemployed / (employed + unemployed),
    nonparticipation_rate = not_in_lf / total,
    date = as.Date(paste(YEAR, MONTH, "01", sep = "-"))
  )
]

# =============== PART 2: TRANSITIONS =============== #

# Panel setup for transitions
panel <- data[, .(CPSIDP, YEAR, MONTH, EMPSTAT)]
setorder(panel, CPSIDP, YEAR, MONTH)
panel[, EMPSTAT_lag := shift(EMPSTAT), by = CPSIDP]
panel[, MONTH_lag := shift(MONTH), by = CPSIDP]
panel[, YEAR_lag := shift(YEAR), by = CPSIDP]
panel[, month_diff := (YEAR - YEAR_lag) * 12 + (MONTH - MONTH_lag)]
panel <- panel[month_diff == 1]

# Define transitions
panel[, transition := fifelse(EMPSTAT_lag %in% 10:12 & EMPSTAT %in% 20:22, "E to U",
                              fifelse(EMPSTAT_lag %in% 10:12 & EMPSTAT %in% 30:36, "E to N",
                                      fifelse(EMPSTAT_lag %in% 20:22 & EMPSTAT %in% 10:12, "U to E",
                                              fifelse(EMPSTAT_lag %in% 20:22 & EMPSTAT %in% 30:36, "U to N",
                                                      fifelse(EMPSTAT_lag %in% 30:36 & EMPSTAT %in% 10:12, "N to E",
                                                              fifelse(EMPSTAT_lag %in% 30:36 & EMPSTAT %in% 20:22, "N to U", NA_character_))))))]

panel <- panel[!is.na(transition)]

# Transition rates by month
transitions_monthly <- panel[, .N, by = .(YEAR, MONTH, transition)]
totals_monthly <- panel[, .N, by = .(YEAR, MONTH)]
transitions_monthly <- merge(transitions_monthly, totals_monthly, by = c("YEAR", "MONTH"),
                             suffixes = c("_trans", "_total"))
transitions_monthly[, rate := N_trans / N_total]
transitions_monthly[, date := as.Date(paste(YEAR, MONTH, "01", sep = "-"))]

# =============== PLOTTING =============== #

# --- Static Labor Market Rates ---

# Employment Rate
ggplot(monthly_indicators, aes(x = date, y = employment_rate)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey40") +
  geom_line(color = "green", size = .75) +
  labs(title = "Monthly Employment Rate ", x = "Date", y = "Employment Rate") +
  theme_minimal()

# Unemployment Rate
ggplot(monthly_indicators, aes(x = date, y = unemployment_rate)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey40") +
  geom_line(color = "turquoise", size = .75) +
  labs(title = "Monthly Unemployment Rate ", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

# Non-Participation Rate
ggplot(monthly_indicators, aes(x = date, y = nonparticipation_rate)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "grey40") +
  geom_line(color = "violet", size = .75) +
  labs(title = "Monthly Labor Force Non-Participation Rate (2010+)", x = "Date", y = "Non Participation Rate") +
  theme_minimal()

# --- Labor Market Transition Rates ---
plot_transition <- function(trans) {
  ggplot(transitions_monthly[transition == trans], aes(x = date, y = rate)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_smooth(method = "loess", se = FALSE, color = "red", span = 0.3) +
    labs(title = paste(trans, "Transition Rate "),
         x = "Date", y = "Transition Rate") +
    scale_y_continuous(labels = percent_format()) +
    theme_minimal(base_size = 14)
}

# Call plot_transition() with desired strings to visualize
plot_transition("E to U")
plot_transition("E to N")
plot_transition("U to E")
plot_transition("U to N")
plot_transition("N to E")
plot_transition("N to U")
