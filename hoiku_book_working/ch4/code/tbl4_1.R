# 表4-1


library(dplyr)
library(data.table)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/tbl4_1.csv")



# Initialize answer_list
answer_list <- list()

# Loop through each column of the data
for (i in 1:ncol(data)) {
  temp_li <- strsplit(as.character(data[, i]), ",")
  # Flatten the list and trim whitespace, then extend answer_list
  answer_list <- c(answer_list, unlist(lapply(temp_li, trimws)))
}

# Convert answer_list into a table for counts
response_counts <- table(unlist(answer_list))

# percentage table
response_percentages <- prop.table(response_counts) * 100

table <- data.frame(response_percentages)

colnames(table) <- c("回答", "回答者の割合(%)")

table$`回答者の割合(%)` <- table$`回答者の割合(%)` %>% round(0)

# use kable to make table

library(kableExtra)



kable(table, "html") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(0, bold = T, color = "white", background = "#0073C2") %>%
  column_spec(1, bold = T, color = "white", background = "#0073C2") %>%
  scroll_box(width = "100%", height = "500px")


