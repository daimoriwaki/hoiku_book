library(ggplot2)
library(magrittr)
library(dplyr)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/pdf_pages_v2.csv")


# remove page == NA and page == 0

data <- data[!is.na(data$page),]

data <- data[data$page > 0,]

# distribution of pages 0-10, 10-20, 20-40, 40-60, 60-100, 100-

# pie chart

# Calculate the number of pages for each city

page_count <- table(data$Page_Range)

# Create page ranges: 0-20, 20-60, 60-100, 100+
data$Page_Range <- cut(
  data$page,
  breaks = c(0, 40, 60, 100, Inf),
  labels = c("0-40ページ", "40-60ページ", "60-100ページ", "100ページ以上")
)



# Count the number of occurrences for each page range
page_count <- table(data$Page_Range)
page_count_df <- as.data.frame(page_count)
colnames(page_count_df) <- c("Page_Range", "Count")


# Calculate the position for labels and percentages
page_count_df <- page_count_df %>%
  mutate(
    Fraction = Count / sum(Count),
    Cumulative = cumsum(Fraction),
    Label_Position = Cumulative - Fraction / 2,
    Percentage = paste0(round(Fraction * 100), "%"),
    Label = paste0(Page_Range, "\n", Percentage)
  )


# Create a pie chart without data label

pie_chart <- ggplot(page_count_df, aes(x = "", y = Count, fill = Page_Range)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("ページ数の分布")



print(pie_chart)

