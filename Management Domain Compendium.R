setwd("C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code")

#==============================================================
# 1) CREATING DATATABLE FROM HTML FILES, PARSING OUT </P> ELEMENTS, SECTIONS, AND HEADINGS
#==============================================================

# Load required libraries
library(xml2)
library(data.table)
library(rvest)
library(tools)  # For handling file paths

# Define file paths
html_folder_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/html_files"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_1_Full_Unfiltered.csv"

# Get list of all HTML files
html_files <- list.files(html_folder_path, pattern = "\\.html$", full.names = TRUE)

# Initialize list to store extracted data
compiled_list <- list()

# Regular expression pattern to filter out headings resembling dates
date_pattern <- "\\b\\d{1,2}[-/.]\\d{1,2}([-/.]\\d{2,4})?\\b"

# Loop through each HTML file
for (html_file_path in html_files) {
  html_file <- read_html(html_file_path)
  
  # Extract Act Name from filename
  act_name <- file_path_sans_ext(basename(html_file_path))
  
  # Determine jurisdiction based on the Government of Canada identifier
  jurisdiction <- ifelse(length(html_file %>% html_nodes("img#gcwu-sig")) > 0, "Federal", "Provincial")
  
  # Determine legislation category
  category <- ifelse(grepl("Act", act_name, ignore.case = TRUE), "Act", "Regulation")
  
  # Extract paragraph nodes
  p_nodes <- html_file %>% html_nodes("p")
  
  # Initialize vectors for storing data
  headings <- paragraphs <- secnums <- character()
  
  # Track last known section number
  last_secnum <- ""
  
  for (p in p_nodes) {
    paragraph_content <- p %>% html_text() %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")  # Fix encoding issues
    
    # Remove "Marginal note:" from the description
    paragraph_content <- gsub("Marginal note:\\s*", "", paragraph_content, ignore.case = TRUE)
    
    # Extract section number
    secnum <- p %>% html_node("span.secnumholder b") %>% html_text()
    
    # Check alternative section label
    if (is.na(secnum) || secnum == "") {
      secnum <- p %>% html_node("span.sectionLabel") %>% html_text()
    }
    
    # Preserve last known section number
    secnum <- ifelse(is.na(secnum) || secnum == "", last_secnum, secnum)
    last_secnum <- secnum  # Update last known section number
    
    # Extract heading (MarginalNote or nearest heading tag)
    heading <- p %>% html_node(xpath = "preceding::p[@class='MarginalNote'][1]") %>% html_text() %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
    
    # If no MarginalNote, extract nearest heading while **ignoring empty <a> tags**
    if (is.na(heading) || heading == "") {
      heading_tags <- c("h5", "h4", "h3", "h2", "h1")
      for (tag in heading_tags) {
        heading <- p %>% html_node(xpath = paste0("preceding::", tag, "[1]")) %>% html_text() %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
        
        # Ensure extracted heading **isn't just "Link to Point in Time"** or an empty anchor tag
        if (!is.na(heading) && nzchar(heading) && !grepl("Link to Point in Time", heading, ignore.case = TRUE)) break
      }
    }
    
    # Remove "Marginal note:" prefix from headings
    heading <- gsub("Marginal note:\\s*", "", heading, ignore.case = TRUE)
    
    # Exclude headings resembling dates
    heading <- ifelse(grepl(date_pattern, heading, ignore.case = TRUE), "", heading)
    
    # Append extracted data
    headings <- c(headings, heading)
    paragraphs <- c(paragraphs, paragraph_content)
    secnums <- c(secnums, secnum)
  }
  
  # Store extracted data
  compiled_list[[act_name]] <- data.table(
    `Act Name` = act_name,
    `Jurisdiction` = jurisdiction,
    `Category` = category,
    `Procedural Element` = headings,
    `Description` = paragraphs,
    `Section` = secnums
  )
}

# Combine all data into one table
compiled_df <- rbindlist(compiled_list, use.names = TRUE, fill = TRUE)

# Export data to CSV
fwrite(compiled_df, output_csv_path, row.names = FALSE)

# Print confirmation
print(paste("CSV file saved to:", output_csv_path))

#===================================================================
# 2) FILTER DATATABLE FOR RELEVANCY KEYWORDS WITH PRIORITY ORDER (WITH MULTI-WORD PHRASE SUPPORT)
#===================================================================

# Load necessary libraries
library(data.table)
library(stringr)

# Define file paths
full_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Full_Datatable.csv"
keywords_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Relevancy_keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_2_Relevance_Filtered.csv"

# Load datasets
compiled_df <- fread(full_csv_path)
keywords_df <- fread(keywords_csv_path)

# Ensure columns are correctly formatted
keywords_df[, `:=`(Keyword = as.character(Keyword), Type = as.integer(Type))]
compiled_df[, Description := as.character(Description)]

# Sort keywords by priority (ascending order: Type 1 first, Type 4 last)
keywords_df <- keywords_df[order(Type)]

# Remove double quotes from keywords and Description column before filtering
compiled_df[, Description := gsub('"', '', Description)]
keywords_df[, Keyword := gsub('"', '', Keyword)]

# Explicitly initialize `Relevancy Scale` as an integer column
compiled_df[, `Relevancy Scale` := as.integer(NA)]

# Apply keyword filtering efficiently, prioritizing Type values with multi-word phrase support
for (i in seq_len(nrow(keywords_df))) {
  keyword <- keywords_df$Keyword[i]
  type_value <- keywords_df$Type[i]
  
  if (!is.na(keyword) && nzchar(keyword)) {
    matched_rows <- str_detect(compiled_df$Description, fixed(keyword, ignore_case = TRUE))
    
    # Assign Type only if it's a higher priority than the current assigned value
    compiled_df[matched_rows & (is.na(`Relevancy Scale`) | `Relevancy Scale` > type_value), `Relevancy Scale` := type_value]
  }
}

# Keep only rows that matched a keyword
filtered_df <- compiled_df[!is.na(`Relevancy Scale`), ]

# Export cleaned and filtered dataset to CSV
fwrite(filtered_df, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Filtered CSV file saved to:", output_csv_path))

#===================================================================
# 3) FILTER DATATABLE BY EXCLUSION KEYWORDS IN BOTH DESCRIPTION & PROCEDURAL ELEMENT COLUMNS
#===================================================================

# Load necessary libraries
library(data.table)
library(stringr)

# Define file paths
datatable_path <- output_csv_path  # Use the previously filtered dataset
exclusion_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Exclusion_keywords.csv"
output_filtered_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_3_Exclusions_Applied.csv"

# Load datasets
datatable <- fread(datatable_path)
exclusion_df <- fread(exclusion_csv_path)

# Ensure exclusion_word column is character type
exclusion_df$exclusion_word <- as.character(exclusion_df$exclusion_word)

# Initialize exclusion mask
matches <- rep(FALSE, nrow(datatable))

# Apply exclusion filtering on BOTH columns with multi-word phrase support
for (exclusion_phrase in exclusion_df$exclusion_word) {
  if (!is.na(exclusion_phrase) && nzchar(exclusion_phrase)) {
    exclusion_phrase <- str_replace_all(exclusion_phrase, '"', '')  # Remove surrounding quotes
    matched_rows_desc <- str_detect(datatable$Description, fixed(exclusion_phrase, ignore_case = TRUE))
    matched_rows_proc <- str_detect(datatable$`Procedural Element`, fixed(exclusion_phrase, ignore_case = TRUE))
    
    # Mark rows for exclusion if they match in either column
    matches <- matches | matched_rows_desc | matched_rows_proc
  }
}

# Remove rows that matched exclusion keywords/phrases in either column
filtered_datatable <- datatable[!matches, ]

# Export filtered dataset
fwrite(filtered_datatable, output_filtered_path, row.names = FALSE)

# Print confirmation message
print(paste("Filtered CSV file saved to:", output_filtered_path))

#===================================================================
# 4) ASSIGN IUCN THREAT CATEGORIES & REMOVE EXTRA QUOTATION MARKS (WITH MULTI-WORD PHRASE SUPPORT)
#===================================================================

# Load required libraries
library(data.table)
library(stringr)

# Define file paths
filtered_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_3_Exclusions_Applied.csv"
iucn_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/IUCN_Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_4_IUCN_Assigned.csv"

# Verify file existence before reading
if (!file.exists(filtered_csv_path)) stop("Error: Filtered datatable file not found.")
if (!file.exists(iucn_csv_path)) stop("Error: IUCN keywords file not found.")

# Load datasets
filtered_df <- fread(filtered_csv_path)
iucn_df <- fread(iucn_csv_path)

# Remove all double quotes from relevant columns
filtered_df[, Description := gsub('"', '', Description)]
iucn_df[, IUCN_keyword := gsub('"', '', IUCN_keyword)]

# Ensure L1 and L2 columns are initialized as character type
filtered_df[, `:=` (L1 = as.character(NA), L2 = as.character(NA))]

# Apply keyword matching efficiently with multi-word phrase handling
for (i in seq_len(nrow(iucn_df))) {
  keyword <- iucn_df$IUCN_keyword[i]
  if (!is.na(keyword) && nzchar(keyword)) {
    matched_rows <- str_detect(filtered_df$Description, fixed(keyword, ignore_case = TRUE))
    
    # Assign L1 and L2 values where matches occur
    filtered_df[matched_rows, `:=` (L1 = iucn_df$L1[i], L2 = iucn_df$L2[i])]
  }
}

# Save the cleaned and assigned dataset
fwrite(filtered_df, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Final dataset saved as:", output_csv_path))

#===================================================================
# 5) DEFAULT IUCN THREATS BASED ON ACT/REGS (WITH MULTI-WORD PHRASE SUPPORT)
#===================================================================

# Load required libraries
library(data.table)
library(stringr)

# Define file paths
datatable_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/IUCN_Assigned_Datatable.csv"
default_threats_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Default_IUCN_threats.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Datatable_5_Default_IUCNs.csv"

# Verify file existence
if (!file.exists(datatable_path)) stop("Error: Assigned datatable file not found.")
if (!file.exists(default_threats_path)) stop("Error: Default threats file not found.")

# Read datasets
datatable_df <- fread(datatable_path)
default_threats_df <- fread(default_threats_path)

# Remove all double quotes from Description before merging
datatable_df[, Description := gsub('"', '', Description)]

# Merge datasets on legislation_name (matching "Act Name" with "legislation_name")
merged_df <- merge(datatable_df, default_threats_df, by.x = "Act Name", by.y = "legislation_name", all.x = TRUE)

# Fill missing L1 and L2 values using default threats
merged_df[, `:=` (
  L1 = fifelse(is.na(L1.x) | L1.x == "", L1.y, L1.x),
  L2 = fifelse(is.na(L2.x) | L2.x == "", L2.y, L2.x)
)]

# Drop unnecessary columns
merged_df <- merged_df[, .(`Act Name`, Jurisdiction, Category, `Procedural Element`, Description, Section, L1, L2)]

# Save final datatable as CSV
fwrite(merged_df, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Final dataset saved as:", output_csv_path))

#===================================================================
# 6) ASSIGN CLAUSE TYPES & PRESERVE L1 AND L2 VALUES (WITH MULTI-WORD PHRASE SUPPORT)
#===================================================================

# Load required libraries
library(data.table)
library(stringr)

# Define file paths
input_csv_path <- output_csv_path  # Use previous processed dataset
clause_type_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Clause Type Keywords.csv"
output_csv_path <- "C:/Users/ennsj/DFO-MPO/Salmon Science Strategy - working group - working group/5-Reporting/Management Domains/Interactive Tool/R Code/Management_Domains_Compendium_v3.0.csv"

# Verify file existence
if (!file.exists(input_csv_path)) stop("Error: Processed datatable file not found.")
if (!file.exists(clause_type_csv_path)) stop("Error: Clause type keywords file not found.")

# Read datasets
data_df <- fread(input_csv_path)
clause_df <- fread(clause_type_csv_path)

# Ensure Clause Type column is initialized before processing
data_df[, `Clause Type` := NA_character_]

# Assign Clause Type based on keyword matches with multi-word phrase support
for (i in seq_len(nrow(clause_df))) {
  keyword <- clause_df$clause_type_keyword[i]
  clause_category <- clause_df$clause_type_category[i]
  
  if (!is.na(keyword) && nzchar(keyword)) {
    matched_rows <- str_detect(data_df$Description, fixed(keyword, ignore_case = TRUE))
    
    # Assign Clause Type while preserving L1 and L2
    data_df[matched_rows, `Clause Type` := clause_category]
  }
}

# Preserve existing L1 & L2 values before saving
data_df[, `:=` (
  L1 = fifelse(is.na(L1) | L1 == "", NA_character_, L1),
  L2 = fifelse(is.na(L2) | L2 == "", NA_character_, L2)
)]

# Remove unnecessary quotation marks from text columns
data_df[, `Description` := gsub('"', '', Description)]
data_df[, `Clause Type` := gsub('"', '', `Clause Type`)]

# Save the final dataset while preserving L1 and L2 values
fwrite(data_df, output_csv_path, row.names = FALSE)

# Print confirmation message
print(paste("Final dataset saved as:", output_csv_path))

#============================================