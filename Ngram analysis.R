setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code")

#=======================================================
#Unigrams grouped by Act Name
#========================================================

# Install required packages (if not installed)
packages <- c("dplyr", "tidytext", "tm", "textstem", "data.table", "stringr")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(data.table)
library(stringr)

# Define file paths (automatically set)
csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_1_Full_Unfiltered.csv"

# Validate file existence
if (!file.exists(csv_path)) stop("Error: CSV file not found at specified path.")

# Load dataset
compiled_df <- tryCatch(fread(csv_path), error = function(e) stop("Error loading dataset:", e))

# Standardize column names to remove extra spaces or encoding issues
colnames(compiled_df) <- str_trim(colnames(compiled_df))

# Validate essential columns
if (!"Act Name" %in% colnames(compiled_df) | !"Description" %in% colnames(compiled_df)) {
  stop("Error: Columns 'Act Name' or 'Description' are missing in the dataset.")
}

# Rename column and assign unique row IDs
compiled_df <- compiled_df %>% rename(Act_Name = `Act Name`) %>% mutate(row_id = row_number())

# Clean text data
clean_text <- function(text_data) {
  text_data <- na.omit(text_data) %>% tolower() %>%
    str_replace_all("\\b\\d{1,2}[-/.]\\d{1,2}([-/.]\\d{2,4})?\\b", "") %>%  # Remove dates
    str_replace_all("footnote|SOR/\\d+", "") %>%  # Remove footnotes and citations
    str_replace_all("[^a-zA-Z\\s]", "")  # Remove special characters
  
  return(text_data)
}

compiled_df$Description <- clean_text(compiled_df$Description)

# Convert text to tidy format with Act Name
text_df <- compiled_df %>%
  select(row_id, Act_Name, Description) %>%
  unnest_tokens(word, Description) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = lemmatize_words(word)) %>%
  filter(!str_detect(word, "^\\d+$"))  # Exclude numeric words

# Count Unigrams grouped by Act Name (including words with frequency of 1)
unigram_freq <- text_df %>% group_by(Act_Name, word) %>% summarise(freq = n(), .groups = "drop") %>% arrange(desc(freq))

# Define output path
output_dir <- dirname(csv_path)  # Use same directory as input file
output_unigram_path <- file.path(output_dir, "unigram_word_freq_grouped.csv")

# Save results
fwrite(data.table(unigram_freq), output_unigram_path, row.names = FALSE)

# Print confirmation
cat("Grouped Unigram CSV saved to:", output_unigram_path, "\n")
