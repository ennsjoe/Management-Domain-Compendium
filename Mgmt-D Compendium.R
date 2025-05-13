setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping")

############################################
# TYPE 1 COMPENDIUM
############################################

library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 1 Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Procedural_Elements.csv"

# Define keywords as stand-alone words using word boundaries
keywords <- c("\\bsalmon\\b", "\\bchinook\\b", "\\bcoho\\b", "\\bsockeye\\b", "\\bchum\\b")

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
  if (grepl("Act", legislation_name, ignore.case = TRUE) && grepl("Regulations", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else if (grepl("Order", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Order"
  } else if (grepl("Act", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Act"
  } else if (grepl("Regulation", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else {
    legislation_type <- "Unknown Type"
  }
  
  # Extract act name, fallback to legislation name if unavailable
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", legislation_name, act_name)
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- relevancy_types <- character()
  
  # Track last known section number
  last_secnum <- ""
  last_part_or_title <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Check if paragraph contains any of the keywords as stand-alone words
    if (any(grepl(paste(keywords, collapse = "|"), paragraph_content, ignore.case = TRUE))) {
      
      # Extract section number (removing subsection numbers)
      secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
      if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
      
      # Also check for section numbers inside list items
      if (is.na(secnum) || secnum == "") {
        secnum <- p %>% html_node("ul.Section.ProvisionList li p.Subsection strong a.sectionLabel span") %>% html_text(trim = TRUE)
      }
      
      # Remove subsection numbers (keeps only the main section)
      secnum <- gsub("\\..*$", "", secnum)
      
      # Preserve last known section number
      secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
      last_secnum <- secnum  # Update last known section number
      
      # Extract closest preceding heading, including marginal notes
      heading_tags <- c("h5", "h4", "h3", "h2", "h1", "p.MarginalNote")
      heading <- ""
      for (tag in heading_tags) {
        heading <- p %>% html_node(xpath = paste0("preceding::", tag, "[1]")) %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
        if (!is.na(heading) && nzchar(heading)) {
          heading <- gsub("^Marginal note:\\s*", "", heading)  # Remove "Marginal note:"
          break
        }
      }
      
      # If no heading exists, extract the nearest preceding part or title
      if (is.na(heading) || heading == "") {
        heading <- p %>% html_node("h2.Part span.HTitleText1") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
        heading <- ifelse(is.na(heading) || heading == "", last_part_or_title, heading)
        last_part_or_title <- heading  # Update last known part or title
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
      `Legislation Type` = legislation_type,
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

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#================================================================
# ASSIGN IUCN L1/L2 VALUES AND CLAUSE TYPES
#================================================================

# Load necessary packages
library(data.table)
library(dplyr)
library(stringr)

# Define file paths
type1_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Procedural_Elements.csv"
iucn_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"
default_threats_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Default_IUCN_threats.csv"
clause_type_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Clause Type Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Compendium.csv"

# Read CSV files
type1_df <- fread(type1_csv_path)
iucn_df <- fread(iucn_csv_path)
default_threats_df <- fread(default_threats_csv_path)
clause_type_df <- fread(clause_type_csv_path, check.names = TRUE)

# Debugging: Print column names to check for discrepancies
print("Column names in clause_type_df:")
print(colnames(clause_type_df))

# Rename 'Clause Type' column if needed
if (!"Clause Type" %in% colnames(clause_type_df)) {
  setnames(clause_type_df, old = colnames(clause_type_df)[which(str_detect(colnames(clause_type_df), "Clause"))], new = "Clause Type")
}

# Trim whitespace from column headers (just in case)
colnames(clause_type_df) <- str_trim(colnames(clause_type_df))

# Ensure keywords are formatted correctly
clause_type_df <- clause_type_df %>% 
  mutate(
    Keyword = str_replace_all(tolower(Keyword), '"', ''),  # Remove quotes, lowercase
    `Clause Type` = str_replace_all(`Clause Type`, '"', '') # Remove quotes
  )

# Separate multi-word phrases and individual words
phrases_clause <- clause_type_df %>% filter(str_detect(Keyword, "\\s"))  # Contains spaces → phrase
words_clause <- clause_type_df %>% filter(!str_detect(Keyword, "\\s"))  # No spaces → single word

# Initialize columns for L1, L2, and Clause Type
type1_df[, `L1` := ""]
type1_df[, `L2` := ""]
type1_df[, `Clause Type` := ""]

# **Step 1: Assign L1/L2 based on keyword matching (Relevancy Type 2 → 3 → 4)**
for (relevancy in c(2, 3, 4)) {
  keywords_subset <- iucn_df %>% filter(`Relevancy Type` == relevancy)
  phrases_subset <- keywords_subset %>% filter(str_detect(Keyword, "\\s"))
  words_subset <- keywords_subset %>% filter(!str_detect(Keyword, "\\s"))
  
  for (i in seq_len(nrow(type1_df))) {
    if (type1_df$L1[i] == "" & type1_df$L2[i] == "") {  # Only process unassigned rows
      paragraph_text <- str_trim(tolower(type1_df$Paragraph[i]))  # Trim and lowercase
      
      phrase_matches <- phrases_subset %>% filter(str_detect(paragraph_text, regex(Keyword, ignore_case = TRUE)))
      word_matches <- words_subset %>% filter(str_detect(paragraph_text, paste0("\\b", Keyword, "\\b", collapse = "|")))
      
      matched_rows <- bind_rows(phrase_matches, word_matches) %>% select(L1, L2) %>% distinct()
      
      if (nrow(matched_rows) > 0) {
        type1_df$L1[i] <- matched_rows$L1[1]
        type1_df$L2[i] <- matched_rows$L2[1]
      }
    }
  }
}

# **Step 2: Assign default L1/L2 values from Default_IUCN_threats.csv for remaining unassigned rows**
for (i in seq_len(nrow(type1_df))) {
  if (type1_df$L1[i] == "" & type1_df$L2[i] == "") {  # Only process still-unassigned rows
    act_match <- default_threats_df %>% filter(`Act Name` == type1_df$`Legislation Name`[i]) %>% select(L1, L2)
    
    if (nrow(act_match) > 0) {
      type1_df$L1[i] <- act_match$L1[1]
      type1_df$L2[i] <- act_match$L2[1]
    }
  }
}

# **Step 3: Iteratively assign Clause Type (only updating unassigned rows)**
for (clause_row in seq_len(nrow(clause_type_df))) {
  clause_keyword <- clause_type_df$Keyword[clause_row]
  clause_type_value <- clause_type_df$`Clause Type`[clause_row]
  
  # Identify if it's a phrase or single word
  is_phrase <- str_detect(clause_keyword, "\\s")
  
  for (i in seq_len(nrow(type1_df))) {
    if (type1_df$`Clause Type`[i] == "") {  # Only update unassigned rows
      paragraph_text <- str_trim(tolower(type1_df$Paragraph[i]))
      
      # Match multi-word phrases exactly
      if (is_phrase) {
        match_found <- str_detect(paragraph_text, regex(clause_keyword, ignore_case = TRUE))
      } else {
        match_found <- str_detect(paragraph_text, paste0("\\b", clause_keyword, "\\b"))
      }
      
      # Assign Clause Type if a match is found
      if (match_found) {
        type1_df$`Clause Type`[i] <- clause_type_value
      }
    }
  }
}

# **Step 4: Clean extra quotation marks from Paragraph text**
type1_df$Paragraph <- str_replace_all(type1_df$Paragraph, '"', '')  # Removes extra quotes

# **Step 5: Export final data to CSV**
fwrite(type1_df, output_csv_path, row.names = FALSE)

# Print debugging output for verification
print(head(type1_df))

# Print sample values to debug
print("Sample Paragraphs (Cleaned):")
print(type1_df$Paragraph[1:10])

print("Clause Type Keywords:")
print(clause_type_df)

print(paste("CSV file saved to:", output_csv_path))

###################################################
# TYPE 2/3 COMPENDIUM
##################################################

library(xml2)
library(data.table)
library(rvest)
library(tools)
library(dplyr)

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type 2-3 Legislation"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_2_3_Procedural_Elements.csv"
keywords_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"

# Load keywords from CSV
keywords_df <- fread(keywords_csv_path, header = TRUE)

# Extract keywords for filtering
keywords <- keywords_df$Keyword

# Create keyword filters for stand-alone words and intact phrases
standalone_keywords <- paste0("\\b", keywords, "\\b")  # Ensuring stand-alone words
phrase_keywords <- keywords  # Keeping exact phrases intact

# Define exclusion words (stand-alone words)
exclusion_words <- c("\\bsalmon\\b", "\\bsockeye\\b", "\\bchinook\\b", "\\bcoho\\b", "\\bchum\\b")

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
  
  # Determine Legislation Type with new rules
  if (grepl("Order", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Order"
  } else if (grepl("Act", legislation_name, ignore.case = TRUE) && grepl("Regulations", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else if (grepl("Act", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Act"
  } else if (grepl("Regulation", legislation_name, ignore.case = TRUE)) {
    legislation_type <- "Regulations"
  } else {
    legislation_type <- "Unknown Type"
  }
  
  # Extract act name or fallback to legislation name
  act_name <- html_file %>% html_node("section#statutesTab a") %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  act_name <- gsub(",.*$", "", act_name)  # Clean formatting
  act_name <- ifelse(is.na(act_name) || act_name == "", legislation_name, act_name)  # Use legislation name if act name is missing
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p, li, div.paragWrapper")
  
  # Initialize vectors for storing data
  paragraphs <- headings <- secnums <- relevancy_types <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # Include paragraphs containing keywords (either stand-alone words or exact phrases)
    matches_keywords <- any(grepl(paste(standalone_keywords, collapse = "|"), paragraph_content, ignore.case = TRUE)) || 
      any(grepl(paste(phrase_keywords, collapse = "|"), paragraph_content, ignore.case = TRUE, fixed = TRUE))
    
    if (!matches_keywords) next
    
    # Exclude paragraphs containing stand-alone exclusion words
    if (any(grepl(paste(exclusion_words, collapse = "|"), paragraph_content, ignore.case = TRUE))) next
    
    # Extract section number
    secnum <- p %>% html_node("span.canlii_section") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("a.sectionLabel span") %>% html_text(trim = TRUE)
    if (is.na(secnum) || secnum == "") secnum <- p %>% html_node("span.sectionLabel") %>% html_text(trim = TRUE)
    
    # Preserve last known section number
    secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
    last_secnum <- secnum  # Update last known section number
    
    # Extract closest preceding heading (including marginal notes)
    heading <- p %>% html_node(xpath = "preceding::p[@class='MarginalNote'][1]") %>% html_text(trim = TRUE)
    if (!is.na(heading) && nzchar(heading)) {
      heading <- gsub("^Marginal note:\\s*", "", heading) # Remove "Marginal note:"
    } else {
      heading <- p %>% html_node(xpath = "preceding::h5[1] | preceding::h4[1] | preceding::h3[1] | preceding::h2[1] | preceding::h1[1]") %>% 
        html_text(trim = TRUE) %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    }
    
    # Append extracted data
    paragraphs <- c(paragraphs, paragraph_content)
    headings <- c(headings, heading)
    secnums <- c(secnums, secnum)
    relevancy_types <- c(relevancy_types, "")
  }
  
  if (length(paragraphs) > 0) {
    compiled_list[[legislation_name]] <- data.table(
      `Legislation Name` = legislation_name,
      `Legislation Type` = legislation_type,
      `Act Name` = act_name,
      `Heading` = headings,
      `Section` = secnums,
      `Paragraph` = paragraphs,
      `Relevancy Type` = relevancy_types
    )
  }
}

compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE) %>%
  group_by(`Legislation Name`, `Section`) %>%
  distinct(Paragraph, .keep_all = TRUE) %>%
  ungroup()

fwrite(compiled_df, output_csv_path, row.names = FALSE)

print(paste("CSV file saved to:", output_csv_path))

#===========================================================
# Assign L1/L2 Values and Clause Types
#===========================================================

# Load necessary packages
library(data.table)
library(dplyr)
library(stringr)

# Define file paths
type2_3_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_2_3_Procedural_Elements.csv"
iucn_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/IUCN_Keywords.csv"
clause_type_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Clause Type Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_2-3_Compendium.csv"

# Read CSV files
type2_3_df <- fread(type2_3_csv_path)
iucn_df <- fread(iucn_csv_path)
clause_type_df <- fread(clause_type_csv_path, check.names = TRUE)

# Ensure column names are properly formatted
colnames(clause_type_df) <- str_trim(colnames(clause_type_df))  # Removes extra spaces
setnames(clause_type_df, old = colnames(clause_type_df)[which(str_detect(colnames(clause_type_df), "Clause"))], new = "Clause Type")

# Remove quotation marks from Clause Type values
clause_type_df$`Clause Type` <- str_replace_all(clause_type_df$`Clause Type`, '"', '')

# Ensure 'Relevancy Type' is treated as a character column
type2_3_df[, `Relevancy Type` := as.character(`Relevancy Type`)]

# Initialize columns for L1, L2, and Clause Type
type2_3_df[, `L1` := ""]
type2_3_df[, `L2` := ""]
type2_3_df[, `Clause Type` := ""]

# **Step 1: Assign Relevancy Type cycling through levels (2 → 3 → 4)**
for (i in seq_len(nrow(type2_3_df))) {
  paragraph_text <- str_trim(tolower(type2_3_df$Paragraph[i]))  # Trim and lowercase
  
  for (relevancy in c(2, 3, 4)) {
    keywords_subset <- iucn_df %>% filter(`Relevancy Type` == relevancy)
    
    for (keyword in keywords_subset$Keyword) {
      match_found <- str_detect(paragraph_text, paste0("\\b", keyword, "\\b"))
      
      if (match_found) {
        type2_3_df$`Relevancy Type`[i] <- as.character(relevancy)
        break  # Stop once a match is found
      }
    }
    
    # Fix: Ensure NA values don’t disrupt the loop
    if (!is.na(type2_3_df$`Relevancy Type`[i]) && type2_3_df$`Relevancy Type`[i] != "") break
  }
}

# **Step 2: Assign L1/L2, cycling through keywords by Relevancy Type**
for (relevancy in c(2, 3, 4)) {
  keywords_subset <- iucn_df %>% filter(`Relevancy Type` == relevancy)
  
  for (keyword in keywords_subset$Keyword) {
    for (i in seq_len(nrow(type2_3_df))) {
      if (type2_3_df$L1[i] == "" & type2_3_df$L2[i] == "") {  # Only process unassigned rows
        paragraph_text <- str_trim(tolower(type2_3_df$Paragraph[i]))  
        
        match_found <- str_detect(paragraph_text, paste0("\\b", keyword, "\\b"))
        
        if (match_found) {
          match_row <- keywords_subset %>% filter(Keyword == keyword) %>% select(L1, L2) %>% distinct()
          if (nrow(match_row) > 0) {
            type2_3_df$L1[i] <- match_row$L1[1]
            type2_3_df$L2[i] <- match_row$L2[1]
          }
        }
      }
    }
  }
}

# **Step 3: Assign Clause Type, cycling through Clause Type keywords**
for (clause_keyword in clause_type_df$Keyword) {
  clause_type_value <- clause_type_df %>% filter(Keyword == clause_keyword) %>% .[["Clause Type"]]
  
  # Ensure clause_type_value is valid before processing
  if (!is.null(clause_type_value) && !is.na(clause_type_value) && clause_type_value != "") {
    for (i in seq_len(nrow(type2_3_df))) {
      if (type2_3_df$`Clause Type`[i] == "") {  # Only update unassigned rows
        paragraph_text <- str_trim(tolower(type2_3_df$Paragraph[i]))
        
        match_found <- str_detect(paragraph_text, paste0("\\b", clause_keyword, "\\b"))
        
        if (match_found) {
          type2_3_df$`Clause Type`[i] <- clause_type_value
        }
      }
    }
  }
}

# **Step 4: Clean extra quotation marks from Paragraph text**
type2_3_df$Paragraph <- str_replace_all(type2_3_df$Paragraph, '"', '')  # Removes extra quotes

# **Step 5: Export final data to CSV**
fwrite(type2_3_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#===================================
# Append Compendiums and apply exclusions
#===================================

# Load necessary packages
library(data.table)
library(stringr)

# Define file paths
type1_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_1_Compendium.csv"
type2_3_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Type_2-3_Compendium.csv"
exclusion_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Exclusion_keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Keyword Mapping/Management_Domain_Compendium_v3.0.csv"

# Read CSV files
type1_df <- fread(type1_csv_path)
type2_3_df <- fread(type2_3_csv_path)
exclusion_df <- fread(exclusion_csv_path)

# Standardize column names to ensure proper merging
common_cols <- c("Legislation Name", "Legislation Type", "Act Name", "Section", "Paragraph", "Relevancy Type", "L1", "L2", "Clause Type", "Heading")  # Added "Heading" column

# Ensure both datasets have the same columns before merging
for (col in common_cols) {
  if (!col %in% colnames(type1_df)) type1_df[, (col) := ""]
  if (!col %in% colnames(type2_3_df)) type2_3_df[, (col) := ""]
}

# Append datasets with correct column alignment
compendium_df <- rbindlist(list(type1_df[, ..common_cols], type2_3_df[, ..common_cols]), use.names = TRUE, fill = TRUE)

# Remove rows where words/phrases match exclusion keywords
standalone_words <- exclusion_df$Keyword[str_detect(exclusion_df$Keyword, "^\\S+$")]  # Words only (no spaces)
phrases <- exclusion_df$Keyword[str_detect(exclusion_df$Keyword, "\\s")]  # Multi-word phrases

# Convert Paragraph text and exclusion keywords to lowercase for case-insensitive matching
compendium_df <- compendium_df[
  !str_detect(tolower(Paragraph), paste0("\\b", paste(tolower(standalone_words), collapse = "|"), "\\b")) & 
    !str_detect(tolower(Paragraph), paste(tolower(phrases), collapse = "|"))
]

# Export cleaned dataset to CSV
fwrite(compendium_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))