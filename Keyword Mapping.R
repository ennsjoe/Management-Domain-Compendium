setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping")

#==============================================
# Keyword Mapping for Pacific Salmon Management Domains
#==============================================

#################################
#PART A - TYPE 1 LEGISLATION
#################################

#=============================================
# Step 1 - CanlII: Query - Type 1 Keywords: (salmon OR chinook OR sockeye OR coho OR chum)
#Download HTML files of Acts/Regs to a folder - call these Type 1 Legislation
#==============================================

#=============================================
#Step 2a - Parse all Type 1 Legislation by paragraph nodes
#=============================================

library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 1 Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Legislation_Full.csv"

# Get list of all HTML files
html_files <- list.files(html_folder_path, pattern = "\\.html$", full.names = TRUE)

# Initialize list to store extracted data
compiled_list <- list()

# Loop through each HTML file
for (html_file_path in html_files) {
  html_file <- tryCatch(read_html(html_file_path), error = function(e) NULL)
  if (is.null(html_file)) next  # Skip problematic files
  
  # Extract legislation name from <h1 class="main-title">
  legislation_name <- html_file %>% html_node("h1.main-title") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # If missing, extract from <div id="title">
  if (is.na(legislation_name) || legislation_name == "") {
    title_main <- html_file %>% html_node("div#title h2") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    title_chapter <- html_file %>% html_node("div#title h3") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Combine title and chapter if available
    if (!is.na(title_main) && nzchar(title_main)) {
      legislation_name <- title_main
      if (!is.na(title_chapter) && nzchar(title_chapter)) {
        legislation_name <- paste(title_main, title_chapter, sep = " - ")
      }
    }
  }
  
  # Ensure a fallback value
  legislation_name <- ifelse(is.na(legislation_name) || legislation_name == "", "Unknown Legislation", legislation_name)
  
  # Determine Legislation Type based on keywords in Legislation Name
  if (grepl("Act", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Act"
  } else if (grepl("Regulation", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else {
    legislation_type <- "Unknown Type"
  }
  
  # Extract act name
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", "Unknown Act", act_name)
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Extract section number (removing subsection numbers)
    secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
    
    # Remove subsection numbers (keeps only the main section)
    secnum <- gsub("\\..*$", "", secnum)
    
    # Preserve last known section number
    secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
    last_secnum <- secnum  # Update last known section number
    
    # Extract closest preceding heading
    heading_tags <- c("h5", "h4", "h3", "h2", "h1")
    heading <- ""
    for (tag in heading_tags) {
      heading <- p %>% html_node(xpath = paste0("preceding::", tag, "[1]")) %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
      if (!is.na(heading) && nzchar(heading)) break
    }
    
    # Append extracted data
    paragraphs <- c(paragraphs, paragraph_content)
    headings <- c(headings, heading)
    secnums <- c(secnums, secnum)
  }
  
  # Store extracted data
  if (length(paragraphs) > 0) {
    compiled_list[[legislation_name]] <- data.table(
      `Legislation Name` = legislation_name,
      `Legislation Type` = legislation_type,
      `Act Name` = act_name,
      `Heading` = headings,
      `Section` = secnums,
      `Paragraph` = paragraphs
    )
  }
}

# Combine all data into one table and remove duplicates per legislation and section
compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE) %>%
  group_by(`Legislation Name`, `Section`) %>%
  distinct(Paragraph, .keep_all = TRUE) %>%
  ungroup()

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#=============================================
# Step 2b - Parse Type 1 Legislation filtered by Type 1 Keywords
#=============================================

library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 1 Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Procedural_Elements.csv"

# Define keywords
keywords <- c("salmon", "chinook", "coho", "sockeye", "chum")

# Get list of all HTML files
html_files <- list.files(html_folder_path, pattern = "\\.html$", full.names = TRUE)

# Initialize list to store extracted data
compiled_list <- list()

# Loop through each HTML file
for (html_file_path in html_files) {
  html_file <- tryCatch(read_html(html_file_path), error = function(e) NULL)
  if (is.null(html_file)) next  # Skip problematic files
  
  # Extract legislation name from <h1 class="main-title">
  legislation_name <- html_file %>% html_node("h1.main-title") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # If missing, extract from <div id="title">
  if (is.na(legislation_name) || legislation_name == "") {
    title_main <- html_file %>% html_node("div#title h2") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    title_chapter <- html_file %>% html_node("div#title h3") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Combine title and chapter if available
    if (!is.na(title_main) && nzchar(title_main)) {
      legislation_name <- title_main
      if (!is.na(title_chapter) && nzchar(title_chapter)) {
        legislation_name <- paste(title_main, title_chapter, sep = " - ")
      }
    }
  }
  
  # Ensure a fallback value
  legislation_name <- ifelse(is.na(legislation_name) || legislation_name == "", "Unknown Legislation", legislation_name)
  
  # Determine Legislation Type based on keywords in Legislation Name
  if (grepl("Act", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Act"
  } else if (grepl("Regulation", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else {
    legislation_type <- "Unknown Type"
  }
  
  # Extract act name
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", "Unknown Act", act_name)
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- relevancy_types <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Check if paragraph contains any of the keywords
    if (any(grepl(paste(keywords, collapse = "|"), paragraph_content, ignore.case = TRUE))) {
      
      # Extract section number (removing subsection numbers)
      secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
      
      # Remove subsection numbers (keeps only the main section)
      secnum <- gsub("\\..*$", "", secnum)
      
      # Preserve last known section number
      secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
      last_secnum <- secnum  # Update last known section number
      
      # Extract closest preceding heading
      heading_tags <- c("h5", "h4", "h3", "h2", "h1")
      heading <- ""
      for (tag in heading_tags) {
        heading <- p %>% html_node(xpath = paste0("preceding::", tag, "[1]")) %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
        if (!is.na(heading) && nzchar(heading)) break
      }
      
      # Assign relevancy type
      relevancy_types <- c(relevancy_types, "1")
      
      # Append extracted data
      paragraphs <- c(paragraphs, paragraph_content)
      headings <- c(headings, heading)
      secnums <- c(secnums, secnum)
    }
  }
  
  # Store extracted data
  if (length(paragraphs) > 0) {
    compiled_list[[legislation_name]] <- data.table(
      `Legislation Name` = legislation_name,
      `Legislation Type` = legislation_type,  # ✅ Restored and automated column
      `Act Name` = act_name,
      `Heading` = headings,
      `Section` = secnums,
      `Paragraph` = paragraphs,
      `Relevancy Type` = relevancy_types
    )
  }
}

# Combine all data into one table and remove duplicates per legislation and section
compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE) %>%
  group_by(`Legislation Name`, `Section`) %>%
  distinct(Paragraph, .keep_all = TRUE) %>%
  ungroup()

# Remove rows where paragraph contains "repealed" or "revoke"
compiled_df <- compiled_df %>% filter(!grepl("repealed|revoke", Paragraph, ignore.case = TRUE))

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#==========================================================
# Step 3a - Extract Unigrams from the paragraph nodes
# Note: run each section individually
#==========================================================

# Load required libraries
library(data.table)
library(dplyr)
library(stringr)
library(udpipe)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Legislation_Full.csv"
output_word_freq_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_grouped.csv"

# Load the Udpipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Read input data
compiled_df <- fread(input_csv_path) %>% mutate(doc_id = as.character(row_number()))

# Perform annotation and manually retain doc_id
annotated_data <- udpipe_annotate(ud_model, x = compiled_df$Paragraph, doc_id = compiled_df$doc_id) %>%
  as.data.table()

# Ensure doc_id maps correctly
annotated_data <- annotated_data %>%
  left_join(compiled_df %>% select(doc_id, `Legislation Name`), by = "doc_id")

# List of words to exclude
exclude_words <- c("salmon", "sockeye", "chinook", "coho", "chum", "pink")

# Clean text: Remove numbers, blanks, and special characters
clean_lemma <- function(word) {
  word <- str_replace_all(word, "[^a-zA-Z]", "")  # Remove non-alphabetic characters
  word <- tolower(word)  # Convert to lowercase for consistency
  return(word)
}

# Apply cleaning function
annotated_data$lemma <- sapply(annotated_data$lemma, clean_lemma)

# Remove words that are empty after cleaning
annotated_data <- annotated_data %>% filter(lemma != "")

# Remove words that are only one letter long
annotated_data <- annotated_data %>% filter(nchar(lemma) > 1)

# Filter for nouns, verbs, and adjectives and exclude unwanted words
filtered_data <- annotated_data %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ")) %>%
  filter(!lemma %in% exclude_words)

# Generate the initial word frequency table
word_freq <- filtered_data %>%
  group_by(`Legislation Name`, lemma) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(`Legislation Name`, desc(freq))

# Remove words that only occur once in the word frequency table
word_freq_filtered <- word_freq %>%
  group_by(lemma) %>%
  mutate(total_freq = sum(freq)) %>%  # Compute total occurrences across legislation
  ungroup() %>%
  filter(total_freq > 1) %>%  # Remove words appearing only once globally
  select(-total_freq)  # Remove helper column

# Export word frequency data to CSV
fwrite(word_freq_filtered, output_word_freq_path, row.names = FALSE)

# Print confirmation
print(paste("Final cleaned word frequency analysis (removing single-occurrence words at the end) saved to:", output_word_freq_path))

#=================================================

# Load required libraries
library(data.table)
library(dplyr)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_grouped.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_group_frequency.csv"

# Read in the CSV file
word_data <- fread(input_csv_path)

# Ensure 'lemma' column exists
if (!"lemma" %in% colnames(word_data)) {
  stop("Error: 'lemma' column not found in the dataset.")
}

# Perform word frequency analysis
word_freq <- word_data %>%
  count(lemma, name = "freq") %>%
  arrange(desc(freq))

# Export word frequency data to a new CSV file
fwrite(word_freq, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Word frequency summary saved to:", output_csv_path))

#===================================================

# Load required libraries
library(data.table)
library(dplyr)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_group_frequency.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_final.csv"

# Read in the word frequency CSV file
word_data <- fread(input_csv_path)

# Ensure 'freq' column exists
if (!"freq" %in% colnames(word_data)) {
  stop("Error: 'freq' column not found in the dataset.")
}

# Identify the maximum frequency value
max_freq <- max(word_data$freq, na.rm = TRUE)

# Remove words that appear only once or have the maximum frequency
word_data_filtered <- word_data %>%
  filter(freq > 1 & freq < max_freq) %>%
  rename(Keyword = lemma, Frequency = freq)  # Rename columns

# Export filtered word frequency data to a new CSV file
fwrite(word_data_filtered, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Filtered word frequency analysis saved to:", output_csv_path))

#========================================================================
# Step 3b - Extracting Bigrams from Type 1 Legislation
# Note: Run each section individually
#=======================================================================

library(udpipe)
library(dplyr)
library(data.table)
library(stringr)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Legislation_Full.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_bigrams_grouped.csv"

# Load the UDPipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Read input data
compiled_df <- fread(input_csv_path) %>% mutate(doc_id = as.character(row_number()))

# Perform annotation and manually retain doc_id
annotated_data <- udpipe_annotate(ud_model, x = compiled_df$Paragraph, doc_id = compiled_df$doc_id) %>%
  as.data.table()

# Ensure doc_id maps correctly
annotated_data <- annotated_data %>%
  left_join(compiled_df %>% select(doc_id, `Legislation Name`), by = "doc_id")

# Ensure token ID column is numeric
annotated_data <- annotated_data %>%
  mutate(token_id = as.numeric(token_id))

# Create bigrams from consecutive tokens, now properly linked to Legislation Name
bigrams_pos <- annotated_data %>%
  select(doc_id, sentence_id, token_id, lemma, upos, `Legislation Name`) %>%
  mutate(next_token_id = token_id + 1) %>%
  inner_join(annotated_data, by = c("doc_id", "sentence_id", "next_token_id" = "token_id"), suffix = c("_1", "_2")) %>%
  transmute(Legislation_Name = `Legislation Name_1`, 
            bigram = paste(lemma_1, lemma_2, sep = " "), 
            pos_1 = upos_1, pos_2 = upos_2) %>%
  filter((pos_1 == "NOUN" & pos_2 == "NOUN") |
           (pos_1 == "VERB" & pos_2 == "NOUN") |
           (pos_1 == "VERB" & pos_2 == "VERB") |
           (pos_1 == "ADJ"  & pos_2 == "VERB")) %>%
  count(Legislation_Name, bigram, sort = TRUE)

# Define words to remove
exclude_words <- c("salmon", "chinook", "sockeye", "coho", "chum", "pink")

# Filter out bigrams containing excluded words
filtered_bigrams <- bigrams_pos %>%
  filter(!str_detect(bigram, paste(exclude_words, collapse = "|")))

# Remove numbers and special characters but preserve spaces
filtered_bigrams$bigram <- gsub("[^a-zA-Z ]", "", filtered_bigrams$bigram)

# Remove one-letter words
filtered_bigrams <- filtered_bigrams %>%
  filter(!sapply(str_split(bigram, " "), function(x) any(nchar(x) == 1)))

# Export grouped bigram data to CSV
fwrite(filtered_bigrams, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Grouped bigram frequency CSV saved to:", output_csv_path))

#====================================================================================

library(data.table)
library(dplyr)

# Define input file path
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_bigrams_grouped.csv"

# Define output file path
filtered_bigram_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_bigrams_final.csv"

# Read the CSV file
bigram_data <- fread(input_csv_path)

# Count occurrences of each bigram phrase and filter out phrases occurring only once
bigram_counts <- bigram_data %>%
  group_by(bigram) %>%
  summarise(frequency = sum(n, na.rm = TRUE)) %>%
  filter(frequency > 1) %>%
  arrange(desc(frequency))

# Rename columns
bigram_counts <- bigram_counts %>%
  rename(Keyword = bigram, Frequency = frequency)

# Export the filtered bigram phrase frequency data to CSV
fwrite(bigram_counts, filtered_bigram_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Filtered bigram phrase frequency CSV saved to:", filtered_bigram_csv_path))

###################################################
# PART B - TYPE 1A LEGISLATION
##################################################
#=============================================
# Step 1 - CanlII: Download the Acts tied to the Type 1 Regulations
#==============================================

#=============================================
#Step 2 - Parse all Type 1a Legislation by paragraph nodes
#=============================================

# Load required libraries
library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 1a Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_Legislation_Full.csv"

# Get list of all HTML files
html_files <- list.files(html_folder_path, pattern = "\\.html$", full.names = TRUE)

# Initialize list to store extracted data
compiled_list <- list()

# Loop through each HTML file
for (html_file_path in html_files) {
  html_file <- tryCatch(read_html(html_file_path), error = function(e) NULL)
  if (is.null(html_file)) next  # Skip problematic files
  
  # Extract legislation name from <h1 class="main-title">
  legislation_name <- html_file %>% html_node("h1.main-title") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
  # If missing, extract from <div id="title">
  if (is.na(legislation_name) || legislation_name == "") {
    title_main <- html_file %>% html_node("div#title h2") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    title_chapter <- html_file %>% html_node("div#title h3") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Combine title and chapter if available
    if (!is.na(title_main) && nzchar(title_main)) {
      legislation_name <- title_main
      if (!is.na(title_chapter) && nzchar(title_chapter)) {
        legislation_name <- paste(title_main, title_chapter, sep = " - ")
      }
    }
  }
  
  # Ensure a fallback value
  legislation_name <- ifelse(is.na(legislation_name) || legislation_name == "", "Unknown Legislation", legislation_name)
  
  # Determine Legislation Type based on keywords in Legislation Name
  if (grepl("Act", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Act"
  } else if (grepl("Regulation", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else {
    legislation_type <- "Unknown Type"
  }
  
  # Extract act name
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", "Unknown Act", act_name)
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Extract section number (removing subsection numbers)
    secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
    
    # Remove subsection numbers (keeps only the main section)
    secnum <- gsub("\\..*$", "", secnum)
    
    # Preserve last known section number
    secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
    last_secnum <- secnum  # Update last known section number
    
    # Extract closest preceding heading
    heading_tags <- c("h5", "h4", "h3", "h2", "h1")
    heading <- ""
    for (tag in heading_tags) {
      heading <- p %>% html_node(xpath = paste0("preceding::", tag, "[1]")) %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
      if (!is.na(heading) && nzchar(heading)) break
    }
    
    # Append extracted data
    paragraphs <- c(paragraphs, paragraph_content)
    headings <- c(headings, heading)
    secnums <- c(secnums, secnum)
  }
  
  # Store extracted data
  if (length(paragraphs) > 0) {
    compiled_list[[legislation_name]] <- data.table(
      `Legislation Name` = legislation_name,
      `Legislation Type` = legislation_type,
      `Act Name` = act_name,
      `Heading` = headings,
      `Section` = secnums,
      `Paragraph` = paragraphs
    )
  }
}

# Combine all data into one table and remove duplicates per legislation and section
compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE) %>%
  group_by(`Legislation Name`, `Section`) %>%
  distinct(Paragraph, .keep_all = TRUE) %>%
  ungroup()

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#==========================================================
# Step 3a - Extract Unigrams from the paragraph nodes
# Note: run each section individually
#==========================================================

# Load required libraries
library(data.table)
library(dplyr)
library(stringr)
library(udpipe)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_Legislation_Full.csv"
output_word_freq_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_grouped.csv"

# Load the Udpipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Read input data
compiled_df <- fread(input_csv_path) %>% mutate(doc_id = as.character(row_number()))

# Perform annotation and manually retain doc_id
annotated_data <- udpipe_annotate(ud_model, x = compiled_df$Paragraph, doc_id = compiled_df$doc_id) %>%
  as.data.table()

# Ensure doc_id maps correctly
annotated_data <- annotated_data %>%
  left_join(compiled_df %>% select(doc_id, `Legislation Name`), by = "doc_id")

# Clean text: Remove numbers, blanks, and special characters
clean_lemma <- function(word) {
  word <- str_replace_all(word, "[^a-zA-Z]", "")  # Remove non-alphabetic characters
  word <- tolower(word)  # Convert to lowercase for consistency
  return(word)
}

# Apply cleaning function
annotated_data$lemma <- sapply(annotated_data$lemma, clean_lemma)

# Remove words that are empty after cleaning
annotated_data <- annotated_data %>% filter(lemma != "")

# Remove words that are only one letter long
annotated_data <- annotated_data %>% filter(nchar(lemma) > 1)

# Filter for nouns, verbs, and adjectives and exclude unwanted words
filtered_data <- annotated_data %>%
  filter(upos %in% c("NOUN", "VERB", "ADJ"))

# Generate the initial word frequency table
word_freq <- filtered_data %>%
  group_by(`Legislation Name`, lemma) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(`Legislation Name`, desc(freq))

# Remove words that only occur once in the word frequency table
word_freq_filtered <- word_freq %>%
  group_by(lemma) %>%
  mutate(total_freq = sum(freq)) %>%  # Compute total occurrences across legislation
  ungroup() %>%
  filter(total_freq > 1) %>%  # Remove words appearing only once globally
  select(-total_freq)  # Remove helper column

# Export word frequency data to CSV
fwrite(word_freq_filtered, output_word_freq_path, row.names = FALSE)

# Print confirmation
print(paste("Final cleaned word frequency analysis (removing single-occurrence words at the end) saved to:", output_word_freq_path))

#=====================================================

# Load required libraries
library(data.table)
library(dplyr)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_grouped.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_group_frequency.csv"

# Read in the CSV file
word_data <- fread(input_csv_path)

# Ensure 'lemma' column exists
if (!"lemma" %in% colnames(word_data)) {
  stop("Error: 'lemma' column not found in the dataset.")
}

# Perform word frequency analysis
word_freq <- word_data %>%
  count(lemma, name = "freq") %>%
  arrange(desc(freq))

# Identify the maximum frequency value
max_freq <- max(word_freq$freq, na.rm = TRUE)

# Remove unigrams that appear only once or have the maximum frequency
word_freq_filtered <- word_freq %>%
  filter(freq > 1 & freq < max_freq)

# Export filtered word frequency data to a new CSV file
fwrite(word_freq_filtered, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Filtered word frequency summary saved to:", output_csv_path))

#========================================================

# Load required libraries
library(data.table)
library(dplyr)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_group_frequency.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_final.csv"

# Read in the CSV file
word_data <- fread(input_csv_path)

# Ensure 'freq' column exists
if (!"freq" %in% colnames(word_data)) {
  stop("Error: 'freq' column not found in the dataset.")
}

# Identify the maximum frequency value
max_freq <- max(word_data$freq, na.rm = TRUE)

# Remove unigrams that appear only once or have the maximum frequency
word_data_filtered <- word_data %>%
  filter(freq > 1 & freq < max_freq) %>%
  rename(Keyword = lemma, Frequency = freq)  # Rename columns

# Export filtered word frequency data to a new CSV file
fwrite(word_data_filtered, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Filtered word frequency analysis saved to:", output_csv_path))

#========================================================================
# Step 3b - Extracting Bigrams from Type 1a Legislation
# Note: Run each section individually
#=======================================================================

# Load required libraries
library(udpipe)
library(dplyr)
library(data.table)
library(stringr)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_Legislation_Full.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_bigrams_grouped.csv"

# Load the UDPipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Read input data
compiled_df <- fread(input_csv_path) %>% mutate(doc_id = as.character(row_number()))

# Perform annotation and manually retain doc_id
annotated_data <- udpipe_annotate(ud_model, x = compiled_df$Paragraph, doc_id = compiled_df$doc_id) %>%
  as.data.table()

# Ensure doc_id maps correctly
annotated_data <- annotated_data %>%
  left_join(compiled_df %>% select(doc_id, `Legislation Name`), by = "doc_id")

# Ensure token ID column is numeric
annotated_data <- annotated_data %>%
  mutate(token_id = as.numeric(token_id))

# Create bigrams from consecutive tokens, now properly linked to Legislation Name
filtered_bigrams <- annotated_data %>%
  select(doc_id, sentence_id, token_id, lemma, upos, `Legislation Name`) %>%
  mutate(next_token_id = token_id + 1) %>%
  inner_join(annotated_data, by = c("doc_id", "sentence_id", "next_token_id" = "token_id"), suffix = c("_1", "_2")) %>%
  transmute(Legislation_Name = `Legislation Name_1`, 
            bigram = paste(lemma_1, lemma_2, sep = " "), 
            pos_1 = upos_1, pos_2 = upos_2) %>%
  filter((pos_1 == "NOUN" & pos_2 == "NOUN") |
           (pos_1 == "VERB" & pos_2 == "NOUN") |
           (pos_1 == "VERB" & pos_2 == "VERB") |
           (pos_1 == "ADJ"  & pos_2 == "VERB")) %>%
  count(Legislation_Name, bigram, sort = TRUE)

# Remove numbers and special characters but preserve spaces
filtered_bigrams$bigram <- gsub("[^a-zA-Z ]", "", filtered_bigrams$bigram)

# Remove one-letter words
filtered_bigrams <- filtered_bigrams %>%
  filter(!sapply(str_split(bigram, " "), function(x) any(nchar(x) == 1)))

# Export grouped bigram data to CSV
fwrite(filtered_bigrams, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Grouped bigram frequency CSV saved to:", output_csv_path))

#===================================================================
# Load required libraries
library(data.table)
library(dplyr)

# Define input file path
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_bigrams_grouped.csv"

# Define output file path
filtered_bigram_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_bigrams_final.csv"

# Read the CSV file
bigram_data <- fread(input_csv_path)

# Count occurrences of each bigram phrase and filter out phrases occurring only once
bigram_counts <- bigram_data %>%
  group_by(bigram) %>%
  summarise(frequency = sum(n, na.rm = TRUE)) %>%
  filter(frequency > 1) %>%
  arrange(desc(frequency)) %>%
  rename(Keyword = bigram, Frequency = frequency)  # Rename columns

# Export the filtered bigram phrase frequency data to CSV
fwrite(bigram_counts, filtered_bigram_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Filtered bigram phrase frequency CSV saved to:", filtered_bigram_csv_path))

#==========================================================
# Step 4 - Create Type 2 Keywords
#==========================================================

# Load required libraries
library(data.table)
library(dplyr)

# Define input file paths
keyword_files <- c(
  "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_unigrams_final.csv",
  "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_bigrams_final.csv",
  "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_unigrams_final.csv",
  "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1a_bigrams_final.csv"
)

iucn_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_2_Keywords.csv"

# Read the IUCN Keywords file
iucn_data <- fread(iucn_csv_path)

# Check available column names
print("Columns in IUCN Keywords file:")
print(colnames(iucn_data))

# Ensure correct column name is used
keyword_column_name <- "Keyword"  # Adjust if needed based on actual column names
if (!(keyword_column_name %in% colnames(iucn_data))) {
  stop("Error: 'Keyword' column not found in IUCN_Keywords.csv. Please verify column names.")
}

# Extract keyword list
iucn_keywords <- iucn_data[[keyword_column_name]]

# Function to filter unigrams/bigrams based on IUCN keywords
filter_keywords <- function(file_path, iucn_keywords) {
  keyword_data <- fread(file_path)
  
  # Determine if the dataset contains unigrams or bigrams
  keyword_column <- ifelse("Keyword" %in% colnames(keyword_data), "Keyword", colnames(keyword_data)[1])
  
  # Filter keywords that match any word in the IUCN Keywords list
  keyword_data_filtered <- keyword_data %>%
    filter(sapply(get(keyword_column), function(x) any(strsplit(x, " ")[[1]] %in% iucn_keywords)))
  
  return(keyword_data_filtered)
}

# Apply filtering function to all keyword files
filtered_data <- rbindlist(lapply(keyword_files, filter_keywords, iucn_keywords), use.names = TRUE, fill = TRUE)

# Export filtered dataset to CSV
fwrite(filtered_data, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Filtered keyword frequency CSV saved to:", output_csv_path))

#=========================================================================

###############################################
# PART C - TYPE 2 LEGISLATION
###############################################

##############################################
# PART D - TYPE 2A LEGISLATION
##############################################

##############################################
# PART E - TYPE 3 LEGISLATION
##############################################

#############################################
# PART F - TYPE 3A LEGISLATION
##############################################



