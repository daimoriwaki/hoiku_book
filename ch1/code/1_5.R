# load data from 0-3章保育書籍用データ - 保育士賃.csv

library(data.table)
library(dplyr)
library(ggplot2)

df <- fread("~/Downloads/0-3章保育書籍用データ - 保育士賃金.csv", data.table = F)

df %>% head()

colnames(df)
library(ggpattern)

# Create bar plot
df %>% filter(職種 == "保育士" | 職種 == "専門的・技術的職業従事者") %>% ggplot(aes(x = 職種, y = 推定年収, fill = 職種, pattern = 都道府県)) +
  geom_bar_pattern(
    stat = "identity",
    position = "dodge",
    pattern_fill = "white",
    pattern_spacing = 0.1
  ) +
  scale_fill_grey(start = 0.4, end = 0.8) + # Use grayscale for fill
  labs(
    title = "推定年収比較: 職種と都道府県別",
    x = "職種",
    y = "推定年収 (万円)"
  ) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(legend.position = "right") +
  guides(pattern = guide_legend(title = "都道府県"))







# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpattern)

# Filter data for 保育士 and 専門的・技術的職業従事者
filtered_data <- df %>% 
  filter(職種 %in% c("保育士", "専門的・技術的職業従事者"))

# Map specific patterns for each prefecture

# Create bar plot
# Load necessary libraries
library(ggplot2)
library(dplyr)



# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter data for 保育士 and 専門的・技術的職業従事者
filtered_data <- df %>% 
  filter(職種 %in% c("保育士", "専門的・技術的職業従事者"))

# Sort prefectures by 推定年収 of 保育士
filtered_data <- filtered_data %>%
  mutate(都道府県 = factor(都道府県, 
                       levels = filtered_data %>%
                         filter(職種 == "保育士") %>%
                         arrange(desc(推定年収)) %>%
                         pull(都道府県)))

# Create bar plot
ggplot(filtered_data, aes(x = 職種, y = 推定年収, fill = 都道府県)) +
  geom_bar(
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette for prefectures
  geom_text(
    aes(label = paste0(都道府県, "\n", round(推定年収/10))),  # Add prefecture label and 推定年収
    position = position_dodge(width = 0.9),
    vjust = -0.5,  # Adjust vertical position of labels
    size = 3,       # Set label size,
    family = "HiraKakuPro-W3"
  ) +
  labs(
    y = "推定年収 (万円)",
    x = ""
  ) +
  theme_minimal(base_family = "HiraKakuPro-W3") +
  theme(
    legend.position = "none",  # Remove legend
    panel.grid.major.x = element_blank()  # Remove vertical grid lines for clarity
  ) +
  ylim(c(0, 6000))  # Set y-axis limits


ggsave("~/dev/hoiku_book/ch1/figure/1_5.png", width = 8, height = 6, dpi = 300)

