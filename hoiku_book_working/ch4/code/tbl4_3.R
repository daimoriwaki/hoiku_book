library(dplyr)
library(data.table)
library(rstudioapi)
library(ggplot2)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/tbl4_3.csv")

# Pie chart for gpt_experience
data %>%
  count(gpt_experience) %>%
  mutate(percentage = n / sum(n) * 100,
         label = paste0(gpt_experience, " (", round(percentage, 1), "%)")) %>% # Create combined label
  ggplot(aes(x = "", y = n, fill = factor(gpt_experience))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label), 
            family = "HiraKakuPro-W3",
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4) + # Add category and percentage labels
  scale_fill_grey(start = 0.5, end = 0.8) + # Grey scale color
  theme_void(base_family = "HiraKakuPro-W3") + # No axes, clean background
  theme(
    legend.position = "none", # Remove legend
    plot.title = element_blank() # Remove title
  )
