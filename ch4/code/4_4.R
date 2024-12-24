library(ggplot2)
library(magrittr)
library(dplyr)
library(rstudioapi)

script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Set the working directory to the script's directory
setwd(script_dir)

# Load the CSV file using the relative path
data <- read.csv("../data/sagashi_all_data.csv")