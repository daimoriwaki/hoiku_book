# 5-2
# シミュレーションデータ


# Load the necessary libraries
library(ggplot2)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path load ../data/5_2_sim.xslx sheet グラフ用データ

data <- readxl::read_excel("../data/5_2_sim.xlsx", sheet = "グラフ用データ")

# plot x axis  "きょうだいへの加点"   y axis  "きょうだいありの入所率" and "きょうだい同時申込の入所率"
# add 180 label at x axis
ggplot(data, aes(x = `きょうだいへの加点`)) +
  geom_line(aes(y = `きょうだいなしの入所率` * 100, linetype = "きょうだいなしの入所率")) +
  geom_line(aes(y = `きょうだい同時申込の入所率` * 100, linetype = "きょうだい同時申込の入所率")) +
  geom_vline(xintercept = 180, linetype = "dotted") +
  scale_x_continuous(breaks = c(180, scales::pretty_breaks()(data$`きょうだいへの加点`))) +
  scale_linetype_manual(
    values = c("きょうだいなしの入所率" = "solid", "きょうだい同時申込の入所率" = "dashed"),
    name = "入所率"
  ) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    legend.position = "None",
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = "HiraKakuProN-W3"),
    strip.text = element_text(size = 10), # Adjust facet labels for better visibility
    plot.margin = margin(20, 20, 20, 20) # Add consistent padding around the plot
  ) +
  labs(
    x = "きょうだいへの加点",
    y = "入所率 (%)"
  )


## 尾崎調整 -----------------
library(scales)  # `pretty_breaks()` のため

ggplot(data, aes(x = `きょうだいへの加点`)) +
  geom_line(aes(y = `きょうだいなしの入所率` * 100, linetype = "きょうだいなしの入所率", color = "きょうだいなしの入所率"), size = 1.2) +  
  geom_line(aes(y = `きょうだい同時申込の入所率` * 100, linetype = "きょうだい同時申込の入所率", color = "きょうだい同時申込の入所率"), size = 1.2) +  
  geom_vline(xintercept = 180, linetype = "dotted", size = 1.0, color = "black") +  # 垂直線の色を黒に設定
  scale_x_continuous(breaks = c(180, pretty_breaks()(data$`きょうだいへの加点`))) +
  scale_linetype_manual(
    values = c("きょうだいなしの入所率" = "solid", "きょうだい同時申込の入所率" = "dotdash"),
    name = "入所率"
  ) +
  scale_color_manual(
    values = c("きょうだいなしの入所率" = "grey50", "きょうだい同時申込の入所率" = "grey20"),  # 青と赤
    name = "入所率"
  ) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    legend.position = "bottom",  # 凡例を下に配置
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5, family = "HiraKakuProN-W3"),  # x軸ラベルのサイズ
    axis.text.y = element_text(size = 14, family = "HiraKakuProN-W3"),  # y軸ラベルのサイズ
    axis.title.x = element_text(size = 16, family = "HiraKakuProN-W3"),  # x軸タイトルのサイズ
    axis.title.y = element_text(size = 16, family = "HiraKakuProN-W3"),  # y軸タイトルのサイズ
    strip.text = element_text(size = 12),  
    plot.margin = margin(20, 20, 20, 20)  # 余白を統一
  ) +
  labs(
    x = "きょうだいへの加点",
    y = "入所率 (%)"
  )


