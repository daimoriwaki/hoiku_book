# Load the necessary libraries
library(ggplot2)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/2_1.csv",
                 header = TRUE, fileEncoding = "UTF-8")


# データを整形（ggplot2で扱いやすい形に変換）
data[2,] <- gsub(",", "", data[2,])
data[5,] <- gsub(",", "", data[5,])


library(tidyr)


waiting_children_long <- data[c(2,5),] %>% 
  pivot_longer(cols ="X4月":"X3月",
               names_to = "月", values_to = "児童数")

# change X4月 to 4月
waiting_children_long$月 <- gsub("X", "", waiting_children_long$月) 

# 月の順序を因子として設定
month_order <- c("4月",  "5月",  "6月",  "7月",  "8月",  "9月",  "10月", "11月", "12月", "1月",  "2月",  "3月")
waiting_children_long$月 <- factor(waiting_children_long$月, levels = month_order)


# グラフの作成
ggplot(waiting_children_long, aes(x = 月, y = as.numeric(児童数), fill = 項目)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # position_dodgeのwidthを調整
  geom_text(aes(label = 児童数),
            position = position_dodge(width = 0.9), # geom_barとwidthを合わせる
            vjust = -0.25, # バーの上に少し浮かせて表示
            size = 3) + # 文字サイズを調整
  scale_fill_grey(start = 0.8, end = 0.3) + # 灰色スケールで塗りつぶし
  labs(title = "",
       x = "",
       y = "") +
  theme_void(base_family = "HiraKakuPro-W3") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        legend.position = "none",  # 凡例を削除
        # axis.line = element_blank(), # 軸の線を削除
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), # y軸の目盛り線を削除
        panel.grid.major = element_blank(), # 主要な格子線を削除
        panel.grid.minor = element_blank()) + # 補助的な格子線を削除
  # 項目名をプロット内に追加
  geom_text(data = waiting_children_long %>%
              group_by(項目) %>%
              filter(月 == first(月)), # 最初の月の位置にラベルを表示
            aes(x = 月,
                y = as.numeric(児童数),
                label = 項目,
                group = 項目),
            position = position_dodge(width = 0.9),
            family = "HiraKakuPro-W3",
            hjust = 0, # 少し左に寄せる
            vjust = -4, # バーの中央に表示
            size = 3) +
  ylim(0, 15000) # y軸の範囲を指定


ggsave("~/dev/hoiku_book/ch2/figure/2_1.png", width = 4.5, height = 3.5, dpi = 300)