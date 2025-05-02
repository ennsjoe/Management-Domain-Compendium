
# Install required packages (only if not installed)
packages <- c("dplyr", "tidytext", "tm", "textstem", "ggplot2", "data.table", "stringr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(dplyr)
library(tidytext)
library(tm)
library(textstem)
library(ggplot2)
library(data.table)
library(stringr)

# Load dataset (adjust path)
file_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_1_Full_Unfiltered.csv"

if (!file.exists(file_path)) stop("Error: File not found. Check the path.")

compiled_df <- fread(file_path)

# Check if 'Description' column exists
if (!"Description" %in% colnames(compiled_df)) stop("Error: Column 'Description' missing in dataset.")

# Extract and clean text data
text_data <- compiled_df$Description

# Ensure text data is not empty
text_data <- na.omit(text_data)  # Remove NA values
text_data <- as.character(text_data)  # Convert explicitly to character

# Basic text preprocessing (removes metadata, footnotes, numbers, and unwanted symbols)
text_data <- tolower(text_data) %>%
  str_replace_all("\\b\\d{1,2}[-/.]\\d{1,2}([-/.]\\d{2,4})?\\b", "") %>%  # Remove dates
  str_replace_all("footnote", "") %>%  # Remove footnotes
  str_replace_all("[^a-zA-Z\\s]", "")  # Remove special characters

# Convert text to tidy format and remove stopwords
text_df <- data.frame(text_column = text_data) %>%
  unnest_tokens(word, text_column) %>%
  anti_join(stop_words, by = "word")  # Ensure stopwords removal works

# Perform lemmatization (instead of stemming)
text_df <- text_df %>% mutate(word = lemmatize_words(word))

# Count Unigrams
word_freq <- text_df %>% count(word, sort = TRUE)

# Ensure correct handling of tokenized text before generating n-grams
text_df <- text_df %>% filter(!is.na(word) & word != "") %>%  
  mutate(word = as.character(word))  # Explicit conversion

# **Fix for Bigrams & Trigrams**: Use `text_column` as source for n-grams
bigram_df <- compiled_df %>%
  select(Description) %>%
  mutate(Description = as.character(Description)) %>%
  unnest_tokens(bigram, Description, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

trigram_df <- compiled_df %>%
  select(Description) %>%
  mutate(Description = as.character(Description)) %>%
  unnest_tokens(trigram, Description, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE)

# Ensure enough data is present before plotting
if (nrow(word_freq) > 20) {
  ggplot(word_freq[1:20,], aes(x=reorder(word, n), y=n)) +
    geom_col() +
    coord_flip() +
    labs(title="Top 20 Words in Description Column", x="Word", y="Frequency")
} else {
  print("Not enough data for visualization.")
}

# Define output paths (Make sure directories exist)
output_dir <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/"
if (!dir.exists(output_dir)) stop("Error: Output directory doesn't exist.")

# Output file paths
output_unigram_path <- paste0(output_dir, "unigram_word_freq.csv")
output_bigram_path <- paste0(output_dir, "bigram_word_freq.csv")
output_trigram_path <- paste0(output_dir, "trigram_word_freq.csv")

# Convert to data.table format
word_freq_dt <- data.table(word_freq)
bigrams_dt <- data.table(bigram_df)
trigrams_dt <- data.table(trigram_df)

# Export results to CSV
fwrite(word_freq_dt, output_unigram_path, row.names = FALSE)
fwrite(bigrams_dt, output_bigram_path, row.names = FALSE)
fwrite(trigrams_dt, output_trigram_path, row.names = FALSE)

# Print confirmation
print(paste("Unigram CSV saved to:", output_unigram_path))
print(paste("Bigram CSV saved to:", output_bigram_path))
print(paste("Trigram CSV saved to:", output_trigram_path))

# Verify output
print(head(bigram_df, 10))  # Check top 10 bigrams
print(head(trigram_df, 10))  # Check top 10 trigrams

