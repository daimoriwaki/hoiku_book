# load data from ~/Downloadas/保育書籍用データ - 10月時点人数.csv

data <- read.csv("~/Downloads/保育書籍用データ - 10月時点人数.csv", header = TRUE, fileEncoding = "UTF-8")


# Load the necessary libraries
library(ggplot2)


# Load the data
data <- read.csv("~/Downloads/保育書籍用データ - 10月時点人数.csv", header = TRUE, fileEncoding = "UTF-8")

# Remove commas from the 人数 column and convert to numeric
data$人数 <- as.numeric(gsub(",", "", data$人数))

# Plot the data with separate bars for each year
ggplot(data, aes(x = factor(月), y = 人数, fill = factor(年齢))) + 
  geom_bar(stat = "identity", position = "stack") + 
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, family = "HiraKakuProN-W3")) + 
  labs(title = "年齢別の待機児童数", x = "月", y = "人数", fill = "年齢") +
  facet_wrap(~ 年)
