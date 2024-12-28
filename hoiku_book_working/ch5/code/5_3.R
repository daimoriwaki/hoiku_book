# 5-3


# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


data <- readxl::read_excel("../data/5_3.xlsx", sheet = "Sheet1")


# Step 1: Pivot data to long format
data_long <- data %>%
  pivot_longer(cols = -color_label, names_to = "year", values_to = "share") %>%
  mutate(year = as.numeric(year))  # Convert year to numeric

# Step 2: Rename columns

data_long <- data_long %>%
  rename(年 = year, 区分 = color_label, 入所率 = share)

# Step 3: Filter for selected categories and convert to percentages
data_filtered <- data_long %>%
  filter(区分 %in% c("きょうだい無し", "きょうだい同時申込")) %>%
  mutate(入所率 = 入所率 * 100)  # Convert to percentages

# Step 4: Create line chart with percentage labels
ggplot(data_filtered, aes(x = 年, y = 入所率, group = 区分, color = 区分)) +
  geom_line(aes(linetype = 区分), size = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = paste0(round(入所率, 1), "%")), vjust = -1, hjust = 1.5, size = 3) + # Add % symbol to labels
  scale_color_grey(start = 0.3, end = 0.5) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  labs(title = "入所率の推移", x = "年", y = "入所率（%）") +
  theme(legend.position = "none") 
