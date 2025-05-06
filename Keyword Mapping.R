setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping")

#==============================================
# Keyword Mapping for Pacific Salmon Management Domains
#==============================================

#=============================================
# Step 1 - CanlII: Query - (pacific AND salmon) OR (chinook OR sockeye OR coho OR chum)
# Start with 1st legislation: British Columbia Sport Fishing Regulations, 1996, SOR/96-137, (Fisheries Act)
#==============================================

#=============================================
# Step 2 - Parse and append paragraph nodes by Type 1 Keywords
#=============================================

# Load required libraries
library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 1 Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Keyword_Mapping_Results.csv"

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
  
  # Extract legislation name from <h1 class="main-title solexHlZone mt-0">
  legislation_name <- html_file %>% html_node("h1.main-title") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  legislation_name <- ifelse(is.na(legislation_name) || legislation_name == "", "Unknown Legislation", legislation_name)
  
  # Extract act name from statutes section
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", "Unknown Act", act_name)
  
  # Extract paragraph nodes (including list-based sections)
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- relevancy_types <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Check if paragraph contains any of the keywords
    if (any(grepl(paste(keywords, collapse = "|"), paragraph_content, ignore.case = TRUE))) {
      
      # Extract section number from various structures
      secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("div.paragWrapper span.canlii_section") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("ul.Section li div.paragWrapper span.canlii_section") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("li strong a.sectionLabel span.canlii_section") %>% html_text(trim = TRUE)
      
      # Extract subsection number (if present)
      subsecnum <- p %>% html_node("span.lawlabel") %>% html_text(trim = TRUE)
      
      # Ensure section number exists before combining with subsection
      if (!is.na(secnum) && nzchar(secnum) && !is.na(subsecnum) && nzchar(subsecnum)) {
        secnum <- paste(secnum, subsecnum, sep = "")
      }
      
      # Preserve last known section number
      secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
      last_secnum <- secnum  # Update last known section number
      
      # Extract closest preceding heading (h1-h5)
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
      `Act Name` = act_name,
      `Heading` = headings,
      `Section` = secnums,
      `Paragraph` = paragraphs,
      `Relevancy Type` = relevancy_types
    )
  }
}

# Combine all data into one table
compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE)

# **Remove rows where paragraph contains "repealed" or "revoke" (case-insensitive)**
compiled_df <- compiled_df %>% filter(!grepl("repealed|revoke", Paragraph, ignore.case = TRUE))

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#==========================================================
# Type 2 Bigrams and Trigrams
#==========================================================

# Load required libraries
library(data.table)
library(dplyr)
library(stringr)
library(udpipe)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Keyword_Mapping_Results.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/n_grams_results.csv"
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"

# Define exclusion keywords
excluded_words <- c("salmon", "chinook", "coho", "sockeye", "chum", "act", "regulation")

# Validate file existence
if (!file.exists(input_csv_path)) stop("Error: CSV file not found at specified path.")
if (!file.exists(udpipe_model_path)) stop("Error: UDPipe model file not found.")

# Load dataset
keyword_df <- tryCatch(fread(input_csv_path), error = function(e) stop("Error loading dataset:", e))

# Extract relevant text columns (Paragraphs & Headings)
text_data <- keyword_df %>%
  select(Paragraph, Heading) %>%
  gather(key = "source", value = "text", Paragraph, Heading) %>%
  filter(!is.na(text))

# Load UDPipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Perform POS tagging with UDPipe
annotated_text <- udpipe_annotate(ud_model, x = text_data$text)
df_tokens <- as.data.frame(annotated_text)

# Function to clean unwanted characters
clean_text <- function(text) {
  text <- str_replace_all(text, "[^a-zA-Z\\s]", "")  # Removes non-alphabetic characters
  text <- str_trim(text)  # Removes extra spaces
  text <- paste0("\"", text, "\"")  # Wraps in double quotes
  return(text)
}

# Extract Bigrams (noun/noun, noun/verb, noun/adjective, EXCLUDING duplicates & unwanted words)
bigrams <- df_tokens %>%
  mutate(next_word = lead(token), next_pos = lead(upos)) %>%
  filter(upos == "NOUN" & next_pos %in% c("NOUN", "VERB", "ADJ") & token != next_word) %>%
  mutate(ngram = clean_text(paste(token, next_word))) %>%
  filter(!grepl(paste(excluded_words, collapse = "|"), ngram, ignore.case = TRUE))  # Exclude keywords & terms

# Extract Trigrams (first & last words must be nouns or verbs, middle word can be any type, EXCLUDING duplicates & unwanted words)
trigrams <- df_tokens %>%
  mutate(next_word1 = lead(token), next_pos1 = lead(upos),
         next_word2 = lead(token, 2), next_pos2 = lead(upos, 2)) %>%
  filter((upos %in% c("NOUN", "VERB") & next_pos2 %in% c("NOUN", "VERB")) &
           (token != next_word1 & token != next_word2 & next_word1 != next_word2)) %>%
  mutate(ngram = clean_text(paste(token, next_word1, next_word2))) %>%
  filter(!grepl(paste(excluded_words, collapse = "|"), ngram, ignore.case = TRUE))  # Exclude keywords & terms

# Combine bigrams and trigrams
ngrams_df <- bind_rows(
  bigrams %>% select(ngram),
  trigrams %>% select(ngram)
)

# Count the frequency of each n-gram
ngrams_df <- ngrams_df %>% group_by(ngram) %>% summarise(frequency = n(), .groups = "drop")

# Ensure proper column naming for export
setnames(ngrams_df, c("ngram", "frequency"))

# Export results to CSV
fwrite(ngrams_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("N-grams CSV saved to:", output_csv_path))

#============================================================
# Cross-filtering Bigrams and trigrams
#============================================================

# Load required libraries
library(data.table)
library(dplyr)
library(stringr)
library(udpipe)

# Define file paths
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Keyword_Mapping_Results.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Common_n_grams.csv"
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"

# Define exclusion keywords
excluded_words <- c("salmon", "chinook", "coho", "sockeye", "chum", "act", "regulation")

# Validate file existence
if (!file.exists(input_csv_path)) stop("Error: CSV file not found at specified path.")
if (!file.exists(udpipe_model_path)) stop("Error: UDPipe model file not found.")

# Load dataset
keyword_df <- tryCatch(fread(input_csv_path), error = function(e) stop("Error loading dataset:", e))

# Extract relevant text columns (Grouped by Legislation Name)
text_data <- keyword_df %>%
  select(`Legislation Name`, Paragraph, Heading) %>%
  gather(key = "source", value = "text", Paragraph, Heading) %>%
  filter(!is.na(text))

# Load UDPipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Perform POS tagging with UDPipe
annotated_text <- udpipe_annotate(ud_model, x = text_data$text)
df_tokens <- as.data.frame(annotated_text)

# Function to clean unwanted characters
clean_text <- function(text) {
  text <- str_replace_all(text, "[^a-zA-Z\\s]", "")  # Removes non-alphabetic characters
  text <- str_trim(text)  # Removes extra spaces
  text <- paste0("\"", text, "\"")  # Wraps in double quotes
  return(text)
}

# Extract Bigrams (EXCLUDING unwanted words and one-letter words)
bigrams <- df_tokens %>%
  mutate(next_word = lead(token), next_pos = lead(upos)) %>%
  filter(upos == "NOUN" & next_pos %in% c("NOUN", "VERB", "ADJ") & 
           token != next_word & !str_detect(token, "^.$") & !str_detect(next_word, "^.$")) %>%
  mutate(ngram = clean_text(paste(token, next_word))) %>%
  filter(!grepl(paste(excluded_words, collapse = "|"), ngram, ignore.case = TRUE))

# Extract Trigrams (EXCLUDING unwanted words and one-letter words)
trigrams <- df_tokens %>%
  mutate(next_word1 = lead(token), next_pos1 = lead(upos),
         next_word2 = lead(token, 2), next_pos2 = lead(upos, 2)) %>%
  filter((upos %in% c("NOUN", "VERB") & next_pos2 %in% c("NOUN", "VERB")) &
           (token != next_word1 & token != next_word2 & next_word1 != next_word2) & 
           !str_detect(token, "^.$") & !str_detect(next_word1, "^.$") & !str_detect(next_word2, "^.$")) %>%
  mutate(ngram = clean_text(paste(token, next_word1, next_word2))) %>%
  filter(!grepl(paste(excluded_words, collapse = "|"), ngram, ignore.case = TRUE))

# Combine bigrams and trigrams
ngrams_df <- bind_rows(
  bigrams %>% select(ngram),
  trigrams %>% select(ngram)
)

# Merge n-grams with legislation name
ngrams_df <- left_join(ngrams_df, text_data %>% select(`Legislation Name`) %>% distinct(), by = character())

# Count occurrences **across different legislation names**
ngram_counts <- ngrams_df %>%
  group_by(ngram, `Legislation Name`) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ngram) %>%
  summarise(legislation_count = n_distinct(`Legislation Name`), .groups = "drop")

# **Step 3:** Only keep n-grams that match across multiple legislation names
matching_ngrams <- ngram_counts %>%
  filter(legislation_count > 1)  # Keep only n-grams present in more than one `Legislation Name`

# **Step 4:** Convert to dataframe for export
matching_ngrams_df <- data.frame(ngram = matching_ngrams$ngram)

# Export results to CSV
fwrite(matching_ngrams_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Matched Bigrams & Trigrams CSV saved to:", output_csv_path))
