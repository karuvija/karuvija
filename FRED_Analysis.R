############################################################
# Inflation vs Interest Rates (FRED)
# Author: Karuna Vijayakumar
############################################################

library(dplyr)
library(readr)
library(lubridate)
install.packages("zoo")
library(zoo)
library(janitor)

############################################################
# 1. Load Raw Data
############################################################

cpi <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL")
fedfunds <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=FEDFUNDS")
unrate <- read_csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE")
# optional

############################################################
# 2. Standardize Column Names
############################################################

cpi_df <- cpi %>% clean_names()
fed_df <- fedfunds %>% clean_names()
unrate_df <- unrate %>% clean_names()

############################################################
# 3. Convert DATE Column to Date Format
############################################################

cpi_df <- cpi %>% mutate(observation_date = as.Date(observation_date))
fed_df <- fedfunds %>% mutate(observation_date = as.Date(observation_date))
unrate_df <- unrate %>% mutate(observation_date = as.Date(observation_date))

############################################################
# 4. Convert Values to Numeric
############################################################

cpi_df <- cpi_df %>% mutate(CPIAUCSL = as.numeric(CPIAUCSL))
fed_df <- fed_df %>% mutate(FEDFUNDS = as.numeric(FEDFUNDS))
unrate_df <- unrate_df %>% mutate(UNRATE = as.numeric(UNRATE))

############################################################
# 5. Rename Columns for Clarity
############################################################

cpi_df <- cpi_df %>% rename(cpi = CPIAUCSL)
fed_df <- fed_df %>% rename(fed_funds_rate = FEDFUNDS)
unrate_df <- unrate_df %>% rename(unemployment_rate = UNRATE)

############################################################
# 6. Handle Missing Values
############################################################

cpi_df <- cpi_df %>% filter(!is.na(cpi))
fed_df <- fed_df %>% filter(!is.na(fed_funds_rate))
unrate_df <- unrate_df %>% filter(!is.na(unemployment_rate))

############################################################
# 7. Create Derived Variables
############################################################

# Year-over-year inflation
cpi_df <- cpi_df %>%
  arrange(observation_date) %>%
  mutate(inflation_yoy = (cpi / lag(cpi, 12) - 1) * 100)

# Rolling averages
cpi_df <- cpi_df %>%
  mutate(
    cpi_rolling_3m = rollmean(cpi, 3, fill = NA, align = "right"),
    cpi_rolling_6m = rollmean(cpi, 6, fill = NA, align = "right")
  )

fed_df <- fed_df %>%
  mutate(
    fed_rolling_3m = rollmean(fed_funds_rate, 3, fill = NA, align = "right"),
    fed_rolling_6m = rollmean(fed_funds_rate, 6, fill = NA, align = "right")
  )

############################################################
# 8. Merge Datasets
############################################################

merged <- cpi_df %>%
  left_join(fed_df, by = "observation_date") %>%
  left_join(unrate_df, by = "observation_date")

############################################################
# 9. Final Cleaning
############################################################

merged <- merged %>%
  arrange(observation_date) %>%
  filter(observation_date >= "1980-01-01") %>%   # keep modern economic era
  mutate(
    month = month(observation_date, label = TRUE),
    year = year(observation_date)
  )

############################################################
# 10. Save Cleaned Dataset
############################################################

write_csv(merged, "inflation_interest_clean.csv")

############################################################
# 11. Print Summary
############################################################

glimpse(merged)
summary(merged)

############################################################
#Exploratory Data Analysis
# Inflation vs Interest Rates (FRED)
############################################################

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)
install.packages("reshape2")
library(reshape2)
install.packages("ggthemes")
library(ggthemes)
library(zoo)
install.packages("corrplot")
library(corrplot)

############################################################
# 1. Inflation Over Time (CPI)
############################################################

p1 <- ggplot(merged, aes(x = observation_date, y = cpi)) +
  geom_line(color = "#2C3E50", size = 1) +
  labs(
    title = "Inflation (CPI) Over Time",
    x = "Year",
    y = "CPI Index"
  ) +
  theme_minimal()

ggsave("inflation_trend.png", p1, width = 10, height = 6)

# Insight:
# CPI shows long-term inflation cycles, with notable spikes during economic shocks.

############################################################
# 2. Interest Rate Over Time (Fed Funds Rate)
############################################################

p2 <- ggplot(merged, aes(x = observation_date, y = fed_funds_rate)) +
  geom_line(color = "#8E44AD", size = 1) +
  labs(
    title = "Federal Funds Rate Over Time",
    x = "Year",
    y = "Interest Rate (%)"
  ) +
  theme_minimal()

ggsave("interest_rate_trend.png", p2, width = 10, height = 6)

# Insight:
# Interest rates rise sharply during inflationary periods as part of monetary tightening.

############################################################
# 3. CPI vs Fed Funds Rate (Dual-Axis Plot)
############################################################

p3 <- ggplot(merged, aes(x = observation_date)) +
  geom_line(aes(y = cpi, color = "CPI")) +
  geom_line(aes(y = fed_funds_rate * 10, color = "Fed Funds Rate (scaled)")) +
  scale_y_continuous(
    name = "CPI",
    sec.axis = sec_axis(~./10, name = "Fed Funds Rate (%)")
  ) +
  scale_color_manual(values = c("CPI" = "#2C3E50", "Fed Funds Rate (scaled)" = "#E74C3C")) +
  labs(
    title = "Inflation vs Interest Rates (Dual Axis)",
    x = "Year",
    color = ""
  ) +
  theme_minimal()

ggsave("cpi_vs_fedfunds.png", p3, width = 10, height = 6)

# Insight:
# Interest rate hikes tend to follow inflation spikes with a noticeable lag.

############################################################
# 4. Year-over-Year Inflation
############################################################

p4 <- ggplot(merged, aes(x = observation_date, y = inflation_yoy)) +
  geom_line(color = "#27AE60", size = 1) +
  labs(
    title = "Year-over-Year Inflation",
    x = "Year",
    y = "Inflation YoY (%)"
  ) +
  theme_minimal()

ggsave("inflation_yoy.png", p4, width = 10, height = 6)

# Insight:
# YoY inflation highlights periods of rapid price increases more clearly than raw CPI.

############################################################
# 5. Correlation Heatmap
############################################################

corr_data <- merged %>%
  select(cpi, inflation_yoy, fed_funds_rate, unemployment_rate) %>%
  na.omit()

corr_matrix <- cor(corr_data)

png("correlation_heatmap.png", width = 800, height = 600)
corrplot(corr_matrix, method = "color", addCoef.col = "black")
dev.off()

# Insight:
# Inflation and interest rates show a positive correlation, confirming monetary policy response.

############################################################
# 6. Lag Analysis (Interest Rate Lagged 12 Months)
############################################################

merged <- merged %>%
  arrange(observation_date) %>%
  mutate(fed_lag_12 = lag(fed_funds_rate, 12))

p5 <- ggplot(merged, aes(x = inflation_yoy, y = fed_lag_12)) +
  geom_point(alpha = 0.6, color = "#2980B9") +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Lag Analysis: Interest Rate (12-Month Lag) vs Inflation YoY",
    x = "Inflation YoY (%)",
    y = "Fed Funds Rate (Lagged 12 Months)"
  ) +
  theme_minimal()

ggsave("lag_analysis.png", p5, width = 10, height = 6)

# Insight:
# The lag analysis shows that interest rate changes often trail inflation by about a year.

############################################################
# 8. Save Summary Statistics
############################################################

summary_stats <- merged %>%
  summarise(
    avg_inflation = mean(inflation_yoy, na.rm = TRUE),
    avg_interest_rate = mean(fed_funds_rate, na.rm = TRUE),
    max_inflation = max(inflation_yoy, na.rm = TRUE),
    max_interest_rate = max(fed_funds_rate, na.rm = TRUE)
  )

write_csv(summary_stats, "summary_statistics.csv")




