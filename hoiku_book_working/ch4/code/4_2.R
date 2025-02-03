library(ggplot2)
library(magrittr)
library(dplyr)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/pdf_pages_summary.csv", row.names  = 1)

data$total_nchar <- data$pages * data$mean_nchar

# remove total_nchar < 100

data <- data[data$total_nchar > 100,]

# show hist 

ggplot(data, aes(x = total_nchar)) +
  geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
  labs(title = "ページ数と文字数の分布", x = "文字数", y = "ページ数") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# count for each page range   0, 50000, 100000

data$Page_Range <- cut(
  data$total_nchar,
  breaks = c(0, 10000, 50000, 100000, Inf),
  labels = c("0-10000文字", "10000-50000文字", "50000-100000文字", "100000文字以上")
)


page_count <- table(data$Page_Range)

page_count_df <- as.data.frame(page_count)

colnames(page_count_df) <- c("Page_Range", "Count")

# pie chart

page_count_df <- page_count_df %>%
  mutate(
    Fraction = Count / sum(Count),
    Cumulative = cumsum(Fraction),
    Label_Position = Cumulative - Fraction / 2,
    Percentage = paste0(round(Fraction * 100), "%"),
    Label = paste0(Page_Range, "\n", Percentage)
  )


pie_chart <- ggplot(page_count_df, aes(x = "", y = Count, fill = Page_Range)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  # geom_text(aes(x = 1.5, y = Label_Position, label = Label), color = "white") +
  # theme(legend.position = "bottom") +
  # labs(title = "文字数の分布", fill = "文字数") +
  scale_fill_grey() +
  theme(plot.title = element_text(hjust = 0.5)
  )

print(pie_chart)

## 尾崎調整 ---------------
library(ggplot2)
library(dplyr)

# データ例 (適宜変更してください)
# page_count_df <- data.frame(
#   Page_Range = c("1-10", "11-20", "21-30", "31-40", "41-50"),
#   Count = c(15, 30, 10, 25, 20)
# )

# 割合を計算して `Page_Range` に改行付きで直接追記
page_count_df <- page_count_df %>%
  mutate(Page_Range = paste0(Page_Range, "\n(", sprintf("%.1f%%", Count / sum(Count) * 100), ")"))

# 作図
pie_chart <- ggplot(page_count_df, aes(x = "", y = Count, fill = Page_Range)) +
  geom_bar(stat = "identity", width = 1.5, color = "white", size = 1.0) +  # 白線で区切る
  coord_polar("y", start = 0) +
  theme_void() +
  theme(
    legend.position = "bottom",                 # 凡例を下に配置
    legend.text = element_text(size = 10),      # 凡例の文字サイズ調整
    legend.key.width = unit(2, "cm"),           # 凡例の幅を調整
    plot.title = element_text(hjust = 0.5)      # タイトルを中央揃え
  ) +
  scale_fill_manual(values = c("black", "grey20", "grey60", "grey80", "grey95")) 
  #labs(title = "文字数の分布", fill = "文字数")  # タイトルと凡例ラベル

# 表示
print(pie_chart)


