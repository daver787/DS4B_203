# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: INSPECT COURSE DATASETS


# LIBRARIES ----
library(tidyverse)


# DATA -----

# * Establish Relationships ----
#   - Website traffic (Page Views, Sessions, Organic Traffic)
#   - Top 20 Pages

# Google Analytics - Summary Hourly
read_rds(file = "00_data/google_analytics_summary_hourly.rds")

# Google Analytics - Top 20 Pages, Daily
read_rds(file = "00_data/google_analytics_by_page_daily.rds")

# * Build Relationships ----
#   - Collect emails
#   - Host Events

# MailChimp Data

read_rds(file = "00_data/mailchimp_users.rds")

# Learning Labs
read_rds(file = "00_data/learning_labs.rds")


# * Generate Course Revenue ----
#   - Revenue data (aggregated at weekly interval)
#   - Product Events


# Transactions Weekly
read_rds("00_data/transactions_weekly.rds")

# Product Events

read_rds(file = "00_data/product_events.rds")






