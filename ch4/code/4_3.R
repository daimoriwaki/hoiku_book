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

# table for mean_kango 0-40, 40-45,45-100

data$kango_share <- cut(
  data$mean_kango,
  breaks = c(0, 0.4, 0.45, Inf),
  labels = c("0-40", "40-45", "45-100")
)

# kango_share count

kango_share_count <- table(data$kango_share)

kango_share_count_df <- as.data.frame(kango_share_count)

colnames(kango_share_count_df) <- c("kango_share", "Count")

# pie chart

kango_share_count_df <- kango_share_count_df %>%
  mutate(
    Fraction = Count / sum(Count),
    Cumulative = cumsum(Fraction),
    Label_Position = Cumulative - Fraction / 2,
    Percentage = paste0(round(Fraction * 100), "%"),
    Label = paste0(kango_share, "\n", Percentage)
  )

pie_chart <- ggplot(kango_share_count_df, aes(x = "", y = Count, fill = kango_share)) +
  
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  # geom_text(aes(x = 1.5, y = Label_Position, label = Label), color = "white") +
  # labs(title = "平均漢字率の分布") +
  theme(plot.title = element_text(hjust = 0.5)
  )

print(pie_chart)