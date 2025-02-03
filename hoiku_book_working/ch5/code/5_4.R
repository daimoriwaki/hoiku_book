# 5-4


# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(rstudioapi)
library(tidyr)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


data <- readxl::read_excel("../data/5_4.xlsx", sheet = "Sheet1")

# Step 1: Pivot data to long format
data_long <- data %>%
  pivot_longer(cols = -assigned_status, names_to = "year", values_to = "rate") %>%
  mutate(year = as.numeric(year))  # Convert year to numeric

# Step 2: Rename columns and convert to percentages
data_long <- data_long %>%
  rename(年 = year, 区分 = assigned_status, 入所率 = rate) %>%
  mutate(入所率 = 入所率 * 100)  # Convert to percentages

# Step 3: Set custom factor levels for flipped category order
data_long <- data_long %>%
  mutate(区分 = factor(区分, levels = c("全部落選", "一部落選", "別所入所", "同所入所")))

# Step 4: Create stacked bar chart with thinner borders
ggplot(data_long, aes(x = 年, y = 入所率, fill = 区分)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8, color = "black", size = 0.25) +  # Thinner border
  geom_text(aes(label = paste0(round(入所率, 0), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +  # Add labels to each segment
  scale_fill_manual(values = c("全部落選" = "darkgrey", 
                               "一部落選" = "grey", 
                               "別所入所" = "lightgrey", 
                               "同所入所" = "white")) +
  scale_x_continuous(breaks = seq(2019, 2024, 1)) +  # Ensure all years are displayed
  theme_minimal(base_family = "HiraKakuPro-W3") +
  labs(title = "", x = "", y = "") +
  theme(
    legend.position = "None",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    axis.ticks.y = element_blank(),        # Remove y-axis ticks
    axis.text.y = element_blank()          # Remove y-axis text
  )