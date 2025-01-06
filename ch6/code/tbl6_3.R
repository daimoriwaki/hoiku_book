# 表６−３

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


data <- readxl::read_excel("../data/6_34.xlsx", sheet = "シート2")

colnames(data) <- c("年齢", "厚生の変化")

# Convert "厚生の変化" to equivalent interpretation
data <- data %>%
  mutate(Interpretation = ifelse(
    厚生の変化 > 0,
    paste0(abs(round(厚生の変化 * 1000, 0)), "mの距離短縮に相当する改善"),
    paste0(abs(round(厚生の変化 * 1000, 0)), "mの距離増加に相当する低下")
  ))

# Create a table with hierarchical style
table_data <- data.frame(
  "Category" = c("年齢", paste0("    ", data$年齢)),
  "Interpretation" = c("", data$Interpretation)
)

# Generate the table
table_data %>%
  kable("html", col.names = c("", "表６−３　各年齢における厚生の変化"), escape = FALSE, align = "l") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(1, bold = TRUE, background = "lightgray") %>%  # Highlight "年齢"
  column_spec(1, width = "30%") # Adjust column width for alignment