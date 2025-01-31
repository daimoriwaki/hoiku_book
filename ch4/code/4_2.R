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

pie_chart
