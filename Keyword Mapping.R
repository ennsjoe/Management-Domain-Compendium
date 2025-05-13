setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping")

#==============================================
# Keyword Mapping for Pacific Salmon Management Domains
#==============================================

#========================================================================
# Step 3b - Extracting Bigrams from Type 1 Legislation
#=======================================================================

library(udpipe)
library(dplyr)
library(data.table)
library(stringr)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Procedural_Elements.csv"
keywords_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"
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

# Read IUCN Keywords data
iucn_keywords_df <- fread(keywords_csv_path)

# Filter bigrams to only include those with words present in the Keyword column of IUCN_Keywords.csv
filtered_bigrams <- filtered_bigrams %>%
  filter(sapply(str_split(bigram, " "), function(x) any(x %in% iucn_keywords_df$Keyword)))

# Enclose all bigrams in double quotes
filtered_bigrams <- filtered_bigrams %>%
  mutate(bigram = paste0('"', bigram, '"'))

# Export grouped bigram data to CSV
fwrite(filtered_bigrams, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Grouped bigram frequency CSV saved to:", output_csv_path))

#==========================================================================
#
#==========================================================================

#========================================================================
# Step 3b - Extracting Trigrams from Type 1 Legislation
# Note: Run each section individually
#=======================================================================

#========================================================================
# Step 3b - Extracting and Filtering Trigrams
#=======================================================================

library(udpipe)
library(dplyr)
library(data.table)
library(stringr)

# Define file paths
udpipe_model_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/english-ewt-ud-2.5-191206.udpipe"
input_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Procedural_Elements.csv"
keywords_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_trigrams_grouped.csv"

# Load the UDPipe model
ud_model <- udpipe_load_model(udpipe_model_path)

# Read input data
compiled_df <- fread(input_csv_path) %>% mutate(doc_id = as.character(row_number()))

# Perform annotation
annotated_data <- udpipe_annotate(ud_model, x = compiled_df$Paragraph, doc_id = compiled_df$doc_id) %>%
  as.data.table()

# Ensure doc_id maps correctly
annotated_data <- annotated_data %>%
  left_join(compiled_df %>% select(doc_id, `Legislation Name`), by = "doc_id")

# Ensure token ID column is numeric and ordered
annotated_data <- annotated_data %>%
  mutate(token_id = as.numeric(token_id)) %>%
  arrange(doc_id, sentence_id, token_id)

# Step 1: Extract all trigrams
trigrams_data <- annotated_data %>%
  mutate(
    lemma_1 = lemma,
    lemma_2 = lead(lemma, 1),
    lemma_3 = lead(lemma, 2),
    pos_1 = upos,
    pos_2 = lead(upos, 1),
    pos_3 = lead(upos, 2)
  ) %>%
  filter(!is.na(lemma_3)) %>%
  transmute(
    Legislation_Name = `Legislation Name`,
    trigram = paste(lemma_1, lemma_2, lemma_3, sep = " "),
    pos_1, pos_3
  )

# Step 2: Remove trigrams where the first word isn't a noun, verb, or adjective
trigrams_filtered <- trigrams_data %>%
  filter(pos_1 %in% c("NOUN", "VERB", "ADJ"))

# Step 3: Remove trigrams where the third word isn't a noun, verb, or adjective
trigrams_filtered <- trigrams_filtered %>%
  filter(pos_3 %in% c("NOUN", "VERB", "ADJ"))

# Step 4: Remove trigrams that don’t contain a word from the Keyword column in IUCN_Keywords
iucn_keywords_df <- fread(keywords_csv_path)
trigrams_filtered <- trigrams_filtered %>%
  filter(sapply(str_split(trigram, " "), function(x) any(x %in% iucn_keywords_df$Keyword)))

# Step 5: Remove trigrams containing "salmon," "chinook," "coho," "chum," or "sockeye" as stand-alone words
exclude_words <- c("salmon", "chinook", "sockeye", "coho", "chum")
trigrams_filtered <- trigrams_filtered %>%
  filter(!sapply(str_split(trigram, " "), function(x) any(x %in% exclude_words)))

# Step 6: Group by Legislation Name and count occurrences
trigrams_grouped <- trigrams_filtered %>%
  count(Legislation_Name, trigram, sort = TRUE)

# Enclose all trigrams in double quotes
trigrams_grouped <- trigrams_grouped %>%
  mutate(trigram = paste0('"', trigram, '"'))

# Export results
fwrite(trigrams_grouped, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("Filtered trigram frequency CSV saved to:", output_csv_path))



