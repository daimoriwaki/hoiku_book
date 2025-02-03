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


## 尾崎調整 ------------------



# データごとにラベル位置を調整



# 各データポイントの `vjust`（高さ）と `hjust`（横位置）を手動調整
data_filtered <- data_filtered %>%
  mutate(
    vjust = case_when(
      年 == 2019 & 区分 == "きょうだい同時申込" ~ -2.0,
      年 == 2019 & 区分 == "きょうだい無し" ~ -1.0,
      年 == 2020 & 区分 == "きょうだい同時申込" ~ -1.5,
      年 == 2020 & 区分 == "きょうだい無し" ~ 1.3,
      年 == 2021 & 区分 == "きょうだい同時申込" ~ 0.2,  
      年 == 2021 & 区分 == "きょうだい無し" ~ 1.5,  
      年 == 2022 & 区分 == "きょうだい同時申込" ~ 2.5,  
      年 == 2022 & 区分 == "きょうだい無し" ~ 2.2,  
      年 == 2023 & 区分 == "きょうだい同時申込" ~ -3.7,  
      年 == 2023 & 区分 == "きょうだい無し" ~ 1.5,  
      年 == 2024 & 区分 == "きょうだい同時申込" ~ 1.0,  
      年 == 2024 & 区分 == "きょうだい無し" ~ -2.0,  
      TRUE ~ 0
    ),
    hjust = case_when(
      年 == 2019 ~ 0.0,  # 2019年は右寄せ
      年 == 2020 ~ 1.0,  
      年 == 2021 ~ 1.3,  
      年 == 2022 ~ 0.5,  
      年 == 2023 ~ 0.5,  
      年 == 2024 ~ 1.2,  # 2024年は左寄せ
      TRUE ~ 0.5  # それ以外は中央
    ),
    fontface = case_when(
      年 %in% c(2021, 2022, 2024) ~ "bold",  # 特定の年を強調
      TRUE ~ "plain"
    )
  )

# グラフ作成
ggplot(data_filtered, aes(x = 年, y = 入所率, group = 区分, color = 区分)) +
  geom_line(aes(linetype = 区分), size = 1.5) +  # 線を太く
  geom_point(size = 4) +  # ポイントを少し大きく
  geom_text(aes(label = paste0(round(入所率, 1), "%"), 
                vjust = vjust, hjust = hjust, fontface = fontface), 
            size = 5) +  # 各データの位置を手動調整
  scale_color_manual(values = c("black", "grey60")) +  # 2つの線の色を明示的に設定
  scale_linetype_manual(values = c("solid", "dashed")) +  # 線の種類を変更
  theme_minimal(base_family = "HiraKakuPro-W3") +
  labs(title = "", x = "年", y = "入所率（%）") +
  theme(
    legend.position = "bottom",  # 凡例を下に配置
    axis.text.x = element_text(size = 14, family = "HiraKakuProN-W3"),  # x軸の数値
    axis.text.y = element_text(size = 14, family = "HiraKakuProN-W3"),  # y軸の数値
    axis.title.x = element_text(size = 16, family = "HiraKakuProN-W3"),  # x軸タイトル
    axis.title.y = element_text(size = 16, family = "HiraKakuProN-W3"),  # y軸タイトル
    plot.title = element_text(size = 18, hjust = 0.5)  # タイトルを中央揃えで大きく
  )


