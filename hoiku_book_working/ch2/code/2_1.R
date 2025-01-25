# Load the necessary libraries
library(ggplot2)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/2_1.csv", 
                 header = TRUE, fileEncoding = "UTF-8")

data <- subset(data, 年 == "2020")

# Remove commas from the 人数 column and convert to numeric
data$人数 <- as.numeric(gsub(",", "", data$人数))

# Ensure '月' is numeric for continuous plotting
data$月 <- as.numeric(data$月)

# Summarize total 人数 for each 月 and 年
total_data <- aggregate(人数 ~ 月 + 年, data, sum)



# Add midpoint for horizontal placement
total_data$horizontal_midpoint <- mean(range(data$月)) # Center of the x-axis



## 尾崎調整---------

# Create the area plot with horizontal lines and centrally positioned data labels
ggplot(data, aes(x = 月, y = 人数 / 10000, fill = 年齢, group = 年齢)) +
#  geom_area(alpha = 0.8, position = "stack") + # Stacked area plot
  geom_area(alpha = 0.8, position = "stack", 
            color = "white", size = 0.03 / 2.54 * 72) + # 白線の境界を追加
  geom_hline(
    data = subset(total_data, 月 %in% c(4, 10)), # Add horizontal lines for total values
    aes(yintercept = 人数 / 10000), 
    linetype = "dashed", 
    color = "black"
  ) +
  geom_text(
    data = subset(total_data, 月 %in% c(4, 10)), # Add data labels for April and October
    aes(
      x = horizontal_midpoint -0.4, # Place horizontally in the center of the plot
      y = 人数 / 10000 + 0.1, # Align with the horizontal line
      label = paste0(round(人数 / 10000, 1), "万人")
    ), 
    inherit.aes = FALSE,
    size = 5,
    family = "HiraKakuPro-W3",
    color = "black"
  ) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "HiraKakuProN-W3"),
    axis.text.y = element_text(size = 13, family = "HiraKakuProN-W3"),
    strip.text = element_text(size = 10), # Adjust facet labels for better visibility
    plot.margin = margin(20, 20, 20, 20) # Add consistent padding around the plot
  ) +
  scale_x_continuous(
    breaks = c(4, 10), # Only show April and October on the x-axis
    labels = c("4月", "10月")
  ) +
#  scale_fill_brewer(palette = "Set2") + # Color differences for 年齢 groups
  scale_fill_manual(
    values = c("3歳児以上" = "gray20", "3歳児未満" = "gray80") # グレーを指定
    ) + 
  labs(
    title = "", 
    x = "",
    y = "待機児童数 (万人)",
    fill = "年齢"
  ) 

#ggsave("~/dev/hoiku_book/ch2/figure/2_1.png", width = 8, height = 6, dpi = 300)


