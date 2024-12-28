# 表6-2

# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


data <- readxl::read_excel("../data/6_34.xlsx", sheet = "シート1")




# Convert "厚生の変化" to equivalent distance shortening (in meters)
data <- data %>%
  mutate(Interpretation = ifelse(
    厚生の変化 > 0,
    paste0(abs(round(厚生の変化 * 1000, 0)), "mの距離短縮に相当する改善"),
    paste0(abs(round(厚生の変化 * 1000, 0)), "mの距離増加に相当する低下")
  ))

# Create hierarchical structure for the table
table_data <- data.frame(
  "Category" = c(
    "送迎距離に換算した厚生変化",
    "申込者全体",
    "点数による違い", 
    paste0("    上位50%の申込者"), 
    paste0("    下位50%の申込者"),
    "在住年数",
    paste0("    在住1年未満"),
    paste0("    在住1年以上")
  ),
  "Interpretation" = c(
    "",
    data$Interpretation[1],
    "",
    data$Interpretation[2],
    data$Interpretation[3],
    "",
    data$Interpretation[4],
    data$Interpretation[5]
  )
)

# Create the table
table_data %>%
  kable("html", col.names = c("", ""), escape = FALSE, align = "l") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  row_spec(1, bold = TRUE) %>%  # Highlight "申込者全体の厚生"
  row_spec(3, bold = TRUE) %>%  # Highlight "点数"
  row_spec(6, bold = TRUE) %>%  # Highlight "在住年数"
  column_spec(1, width = "30%") # Adjust column width for alignment