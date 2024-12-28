# 5-4 (2)


# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyverse)
library(gt)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


data <- readxl::read_excel("../data/5_4_2.xlsx", sheet = "Sheet1")

# Filter out "きょうだい在園"
filtered_data <- data %>%
  filter(color_label != "きょうだい在園")

# Create the table
filtered_data %>%
  gt() %>%
  tab_header(
    title = "図５−４（２）きょうだい申込状況の推移",
    subtitle = "入所した保育所の平均順位"
  ) %>%
  fmt_number(
    columns = 2:7,  # Format numeric columns
    decimals = 2    # Show 2 decimal places
  ) %>%
  cols_label(
    color_label = "カテゴリ",  # Rename columns
    `2019` = "2019年",
    `2020` = "2020年",
    `2021` = "2021年",
    `2022` = "2022年",
    `2023` = "2023年",
    `2024` = "2024年"
  ) %>%
  tab_options(
    table.font.size = "medium",
    table.align = "center"
  )
