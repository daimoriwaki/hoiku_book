library(ggplot2)
library(dplyr)

# データの作成
page_count_df <- data.frame(
  Page_Range = c("利用経験なし", "利用経験あり", "ChatGPTや大規模言語モデルが何かわからない"),
  Count = c(60, 24, 16)  # 割合（%）
)

# 割合を計算して `Page_Range` に直接追記
page_count_df <- page_count_df %>%
  mutate(Page_Range = paste0(Page_Range, "\n(", sprintf("%.1f%%", Count), ")"))

# 円グラフの作成
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
  scale_fill_manual(values = c("grey80", "grey30", "black")) #+  # 手動でグレースケール指定
  #labs(title = "生成AIについての事前知識", fill = "利用状況")  # タイトルと凡例ラベル

# 表示
print(pie_chart)
