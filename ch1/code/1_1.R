# load 
library(ggplot2)
library(scales) # comma関数を使用するために必要



script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)


df <- fread("../data/1_1.csv", data.table = F, header = T)

# remove , from the data for 就学前人口 利用定員数  利用人数 待機児童
colnames(df)[1] <- "年度"

df$`就学前人口` <- gsub(",", "", df$`就学前人口`)
df$`利用定員数` <- gsub(",", "", df$`利用定員数`)
df$`利用人数` <- gsub(",", "", df$`利用人数`)
df$`待機児童` <- gsub(",", "", df$`待機児童`)

df$`就学前人口` <- as.numeric(df$`就学前人口`)
df$`利用定員数` <- as.numeric(df$`利用定員数`)
df$`利用人数` <- as.numeric(df$`利用人数`)
df$`待機児童` <- as.numeric(df$`待機児童`)




# スケール計算
scale_factor <- max(df$就学前人口) / max(df$待機児童) - 100




# Get last row for annotation positioning
df_last <- tail(df, 1)

ggplot(df, aes(x = 年度)) +
  # Line for 就学前人口
  geom_line(aes(y = 就学前人口), color = "black", size = 1) +
  
  # Data labels for 就学前人口 (in 万人)
  geom_text(aes(y = 就学前人口,
                label = paste0(round(就学前人口 / 1e4), "万人")),
            vjust = -1, color = "black", size = 3.5, family = "HiraKakuPro-W3") +
  
  # Bars for 待機児童 (scaled)
  geom_col(aes(y = 待機児童 * scale_factor), fill = "gray", alpha = 0.7, width = 0.5) +
  
  # Data labels for 待機児童 (in 千人)
  geom_text(aes(y = 待機児童 * scale_factor,
                label = paste0(round(待機児童 / 1000, 1), "千人")),
            vjust = -1, color = "gray30", size = 3.5, family = "HiraKakuPro-W3") +
  
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  # Primary and secondary axes
  scale_y_continuous(
    name = "就学前人口",
    labels = function(x) paste0(round(x / 1e4), "万人"),
    sec.axis = sec_axis(~ . / scale_factor, name = "待機児童", labels = scales::comma)
  ) +
  
  # Remove legends
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    axis.title.y.left = element_text(color = "black"),
    axis.title.y.right = element_text(color = "gray"),
    legend.position = "none"
  ) +
  
  # Add title and x-axis label
  labs(
    title = "就学前人口と待機児童の推移",
    x = "年度"
  ) +
  
  # Annotate labels directly inside the plot for the line and bars
  # Positioning these text labels slightly to the right of the last data point
  annotate("text", 
           x = df_last$年度 + 0.3, y = df_last$就学前人口, 
           label = "就学前人口", 
           color = "black", size = 4, hjust = 3, family = "HiraKakuPro-W3") +
  
  annotate("text", 
           x = df_last$年度 + 0.3, y = df_last$待機児童 * scale_factor + 2e6, 
           label = "待機児童", 
           color = "gray30", size = 4, hjust = 3,family = "HiraKakuPro-W3")


ggsave("../figure/1-1.png", width = 8, height = 6, dpi = 300)
