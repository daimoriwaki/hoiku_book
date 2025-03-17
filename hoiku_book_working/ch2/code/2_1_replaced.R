# Load the necessary libraries
#library(ggplot2)
library(rstudioapi)
library(tidyverse)

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

## 尾崎：待機児童比率の追加 ------------
nyuusho_jidousuu <- waiting_children_long %>%
  filter(項目 == "入所児童数")
taiki_jidousuu <- waiting_children_long %>%
  filter(項目 == "待機児童数")

taiki_rate <- as.numeric(taiki_jidousuu$児童数)/as.numeric(nyuusho_jidousuu$児童数) * 100
taiki_rate_percent <- tibble(
  項目 = "待機児童比率",
  月 = month_order,
  児童数 = taiki_rate
)

waiting_children_long_adj <- rbind(waiting_children_long,taiki_rate_percent)


# グラフの作成
ggplot(waiting_children_long, aes(x = 月, y = as.numeric(児童数), fill = 項目)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + # position_dodgeのwidthを調整
  geom_text(aes(label = 児童数),
            position = position_dodge(width = 0.9), # geom_barとwidthを合わせる
            vjust = -0.25, # バーの上に少し浮かせて表示
            size = 3) + # 文字サイズを調整
  scale_fill_grey(start = 0.85, end = 0.6) + # 灰色スケールで塗りつぶし
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


ggsave("./figure/2_1.png", width = 4.5, height = 3.5, dpi = 300)


## グラフ尾崎調整 -----------

# データの前処理
waiting_children_long_adj <- waiting_children_long_adj %>%
  mutate(児童数 = as.numeric(児童数))  # 文字列を数値に変換

# 最大値を取得（スケーリング用）
max_child_count <- max(waiting_children_long_adj$児童数[waiting_children_long_adj$項目 %in% c("入所児童数", "待機児童数")], na.rm = TRUE)

# グラフの作成
ggplot(waiting_children_long_adj, aes(x = 月)) +
  # 棒グラフ（入所児童数・待機児童数）
  geom_bar(data = subset(waiting_children_long_adj, 項目 %in% c("入所児童数", "待機児童数")),
           aes(y = 児童数, fill = 項目),
           stat = "identity",
           position = position_dodge(width = 0.9)) +
  # データラベル（棒グラフの真上に配置）
  geom_text(data = subset(waiting_children_long_adj, 項目 %in% c("入所児童数", "待機児童数")),
            aes(y = 児童数, label = 児童数),
            position = position_dodge(width = 0.9),
            vjust = -0.2, size = 4) +
  # 折れ線グラフ（待機児童比率）
  geom_line(data = subset(waiting_children_long_adj, 項目 == "待機児童比率"),
            aes(y = (児童数 / 20) * max_child_count, 
                group = 1, color = "待機児童比率"),
            size = 1.5) +
  geom_point(data = subset(waiting_children_long_adj, 項目 == "待機児童比率"),
             aes(y = (児童数 / 20) * max_child_count, 
                 color = "待機児童比率"),
             size = 2) +
  # データラベル（折れ線グラフの真上に配置）
  geom_text(data = subset(waiting_children_long_adj, 項目 == "待機児童比率"),
            aes(y = (児童数 / 20) * max_child_count, 
                label = round(児童数, 1)),  # 小数点1桁まで表示
            vjust = -0.5, color = "black", size = 5) +
  # ラベルとスケール
  scale_y_continuous(
    name = "児童数(人)",
    sec.axis = sec_axis(~ . * 20 / max_child_count, 
                        name = "待機児童比率(%)",
                        breaks = seq(0, 15, 5))  # 右軸の範囲を0～20に設定
  ) +
  scale_fill_manual(values = c("grey40", "grey75")) +  # 棒グラフの色設定
  scale_color_manual(values = c("black")) +  # 折れ線グラフの色設定
  labs(title = "",
       x = "",
       fill = "項目",
       color = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 14),  # 横軸の目盛りのフォントサイズ
    axis.text.y = element_text(size = 14),  # 左Y軸の目盛りのフォントサイズ
    axis.text.y.right = element_text(size = 16),  # 右Y軸の目盛りのフォントサイズ
    axis.title.x = element_text(size = 13),  # x軸ラベルのフォントサイズ
    axis.title.y = element_text(size = 13),  # y軸ラベルのフォントサイズ
    axis.title.y.right = element_text(size = 16),  # 右y軸のラベルのフォントサイズ
    legend.position = "bottom"
  )
ggsave("./figure/2_1_adj.png", width = 4.5, height = 3.5, dpi = 300)
