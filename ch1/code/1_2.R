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


library(ggplot2)
library(scales)

# Assuming df already has 年度, 利用定員数, 利用人数 columns as numeric
# and that both are of a similar range and can be displayed together.
library(ggplot2)
library(scales)

# Assuming df already has 年度, 利用定員数, 利用人数 as numeric and on a similar scale.

ggplot(df, aes(x = 年度)) +
  # Bars for 利用定員数 (shift to the left)
  geom_col(aes(y = 利用定員数), fill = "black", width = 0.8,
           position = position_nudge(x = -0.8)) +
  # Data labels for 利用定員数 in 万人
  geom_text(
    aes(y = 利用定員数, label = paste0(round(利用定員数 / 1e4), "万人")),
    vjust = -4, color = "black", size = 3.5, family = "HiraKakuPro-W3",
    position = position_nudge(x = -1)
  ) +
  
  # Bars for 利用人数 (shift to the right)
  geom_col(aes(y = 利用人数), fill = "gray", width = 0.8,
           position = position_nudge(x = 0.2)) +
  # Data labels for 利用人数 in 万人
  geom_text(
    aes(y = 利用人数, label = paste0(round(利用人数 / 1e4), "万人")),
    vjust = -3, color = "black", size = 3.5, family = "HiraKakuPro-W3",
    position = position_nudge(x = 0.4)
  ) +
  
  scale_y_continuous(
    name = "利用定員数・利用人数",
    labels = function(x) paste0(round(x / 1e4), "万人")
  ) +
  
  ylim(c(0,4000000)) +
  
  # Ensure all years, including the last, appear on the x-axis
  scale_x_continuous(breaks = seq(min(df$年度), max(df$年度), by = 1)) +
  scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
  labs(
    title = "利用定員数と利用人数の推移",
    x = "年度"
  ) +
  
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    axis.title.y = element_text(color = "black"),
    legend.position = "none"
  ) +
  
  # Annotate category labels inside the plot near the last data bars
  {
    df_last <- tail(df, 1)
    list(
      annotate("text",
               x = df_last$年度 - 1, y = df_last$利用定員数 - 1e6,
               label = "利用定員数",
               color = "black", size = 4, hjust = 2.7, family = "HiraKakuPro-W3"),
      
      annotate("text",
               x = df_last$年度 + 1, y = df_last$利用人数 - 1e6,
               label = "利用人数",
               color = "gray30", size = 4, hjust = 2.1, family = "HiraKakuPro-W3")
    )
  }


ggsave( "../figure/1_2.png", width = 8, height = 6, dpi = 300)

