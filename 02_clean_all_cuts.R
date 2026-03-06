library(tidyverse)
library(readxl)
library(here)
library(haven)

# ------------------------------------------------------------------------------
# BSR DATA PROCESSING — POPULATION GROUP × BANK GROUP PANEL
# Author : Vijayshree Jayaraman
# Date   : 04/12/25
# Source : RBI DBIE — Statement 4A, District-wise Offices, Deposits and Credit
#          of Scheduled Commercial Banks. Values in Rupees Crores.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# HELPER FUNCTIONS
# ------------------------------------------------------------------------------

safe_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

standardize_name <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", " ", x)
  x <- ifelse(x == "TELANGANA", "ANDHRA PRADESH", x)
  return(x)
}

# ------------------------------------------------------------------------------
# STEP 1: READ AND PROCESS EXCEL FILES
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 1: Reading Excel files from PopGroup-BankGroup folders\n")
cat(rep("=", 80), "\n\n", sep = "")

raw_path    <- here("03_raw")
all_folders <- list.dirs(raw_path, full.names = FALSE, recursive = FALSE)

data_folders <- all_folders[
  !grepl("Payment", all_folders, ignore.case = TRUE) &
    !grepl("All",   all_folders, ignore.case = TRUE) &
    all_folders != ""
]

cat("Found", length(data_folders), "folders:", paste(data_folders, collapse = ", "), "\n\n")

all_data_list      <- list()
all_quarters_found <- c()

for (folder_name in data_folders) {
  
  cat(rep("-", 80), "\n", sep = "")
  cat("Processing:", folder_name, "\n")
  cat(rep("-", 80), "\n", sep = "")
  
  folder_parts <- str_split(folder_name, " ", n = 2)[[1]]
  if (length(folder_parts) != 2) { warning("Cannot parse folder name: ", folder_name); next }
  
  pop_group  <- folder_parts[1]
  bank_group <- folder_parts[2]
  
  folder_path <- file.path(raw_path, folder_name)
  excel_files <- list.files(folder_path, pattern = "^BSR_4A_.*\\.xlsx$", full.names = FALSE)
  
  if (length(excel_files) == 0) { warning("No Excel files found in: ", folder_name); next }
  
  cat("Found", length(excel_files), "Excel files\n")
  
  folder_data_list <- list()
  
  for (filename in excel_files) {
    tryCatch({
      
      raw_excel   <- read_excel(file.path(folder_path, filename), col_names = FALSE)
      quarter_row <- as.character(raw_excel[5, ])
      
      quarter_cols <- which(
        !is.na(quarter_row) & quarter_row != "NA" &
          nchar(trimws(quarter_row)) > 0 & seq_along(quarter_row) > 3
      )
      
      if (length(quarter_cols) == 0) next
      
      quarter_labels <- trimws(quarter_row[quarter_cols])
      
      converted_quarters <- sapply(quarter_labels, function(q) {
        if (grepl("\\d{4}-\\d{2}:Q\\d", q)) {
          return(paste0("Q", substr(q, nchar(q), nchar(q)), "_", substr(q, 1, 4)))
        }
        return(NA_character_)
      }, USE.NAMES = FALSE)
      
      converted_quarters <- converted_quarters[!is.na(converted_quarters)]
      if (length(converted_quarters) == 0) next
      
      all_quarters_found <- unique(c(all_quarters_found, converted_quarters))
      
      data <- read_excel(file.path(folder_path, filename), skip = 6)
      data <- data %>%
        select(where(~!all(is.na(.) | as.character(.) == "" | as.character(.) == "NA")))
      
      new_colnames <- c("Region", "State", "District")
      for (q in converted_quarters) {
        new_colnames <- c(new_colnames, paste0(q, "_OFF"), paste0(q, "_DEP"), paste0(q, "_CRE"))
      }
      
      if (length(new_colnames) != ncol(data)) next
      
      colnames(data) <- new_colnames
      
      data <- data %>%
        filter(!is.na(State) & !is.na(District) &
                 as.character(State) != "" & as.character(District) != "") %>%
        mutate(
          State    = standardize_name(State),
          District = standardize_name(District)
        )
      
      if (nrow(data) == 0) next
      
      numeric_cols <- grep("Q\\d_\\d{4}_(OFF|DEP|CRE)", colnames(data), value = TRUE)
      data <- data %>%
        mutate(across(all_of(numeric_cols), as.numeric)) %>%
        mutate(pop_group = pop_group, bank_group = bank_group)
      
      folder_data_list[[filename]] <- data
      
    }, error = function(e) { warning("Error in ", filename, ": ", e$message) })
  }
  
  if (length(folder_data_list) == 0) next
  
  # Deduplicate overlapping quarters across files within the same folder
  processed_quarters <- c()
  sorted_fnames      <- sort(names(folder_data_list))
  
  for (fname in sorted_fnames) {
    current_cols     <- colnames(folder_data_list[[fname]])
    data_cols        <- grep("Q\\d_\\d{4}_(OFF|DEP|CRE)", current_cols, value = TRUE)
    quarters_in_file <- unique(sub("_(OFF|DEP|CRE)$", "", data_cols))
    duplicates       <- intersect(quarters_in_file, processed_quarters)
    
    if (length(duplicates) > 0) {
      cat("  ! Duplicate quarters in", fname, "- removing:", paste(duplicates, collapse = ", "), "\n")
      cols_to_drop           <- grep(paste(duplicates, collapse = "|"), current_cols, value = TRUE)
      folder_data_list[[fname]] <- folder_data_list[[fname]] %>% select(-all_of(cols_to_drop))
    }
    
    remaining_data_cols <- grep("Q\\d_\\d{4}_(OFF|DEP|CRE)", colnames(folder_data_list[[fname]]), value = TRUE)
    processed_quarters  <- unique(c(processed_quarters, sub("_(OFF|DEP|CRE)$", "", remaining_data_cols)))
  }
  
  # Merge files within folder
  folder_combined <- folder_data_list[[sorted_fnames[1]]]
  
  if (length(folder_data_list) > 1) {
    for (fname in sorted_fnames[-1]) {
      folder_combined <- folder_combined %>%
        full_join(folder_data_list[[fname]],
                  by = c("Region", "State", "District", "pop_group", "bank_group"))
    }
  }
  
  if (any(grepl("\\.x$|\\.y$", colnames(folder_combined)))) {
    stop("CRITICAL: .x/.y columns after merge in ", folder_name, " — check deduplication logic.")
  }
  
  quarter_cols <- grep("Q\\d_\\d{4}_(OFF|DEP|CRE)", colnames(folder_combined), value = TRUE)
  
  folder_collapsed <- folder_combined %>%
    group_by(District, State, pop_group, bank_group) %>%
    summarize(Region = first(Region), across(all_of(quarter_cols), safe_sum), .groups = "drop")
  
  all_data_list[[folder_name]] <- folder_collapsed
  cat("Processed", nrow(folder_collapsed), "districts\n\n")
}

cat("Step 1 complete:", length(all_data_list), "folders processed\n")
if (length(all_data_list) == 0) stop("No data processed!")

# ------------------------------------------------------------------------------
# STEP 2: LOAD DISTRICT MAPPING (BSR → PC01)
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 2: Loading district mapping file\n")
cat(rep("=", 80), "\n\n", sep = "")

match_file_full <- as_tibble(read.csv("../BSR_district_over_years.csv", stringsAsFactors = FALSE))

match_file_full <- match_file_full %>%
  mutate(
    bsrpc01_state    = standardize_name(bsrpc01_state),
    bsrpc01_district = standardize_name(bsrpc01_district),
    bsrpc11_state    = standardize_name(bsrpc11_state),
    bsrpc11_district = standardize_name(bsrpc11_district),
    bsr2022_state    = toupper(trimws(bsr2022_state)),
    bsr2022_district = toupper(trimws(bsr2022_district))
  )

match_pc11 <- match_file_full %>%
  filter(bsrpc11_district != "" & !is.na(bsrpc11_district),
         bsrpc01_district != "" & !is.na(bsrpc01_district)) %>%
  distinct(bsrpc11_state, bsrpc11_district, .keep_all = TRUE) %>%
  select(bsr_state = bsrpc11_state, bsr_district = bsrpc11_district,
         pc01_state = bsrpc01_state, pc01_district = bsrpc01_district)

match_2022 <- match_file_full %>%
  filter(bsr2022_district != "" & !is.na(bsr2022_district),
         bsrpc01_district != "" & !is.na(bsrpc01_district)) %>%
  distinct(bsr2022_state, bsr2022_district, .keep_all = TRUE) %>%
  select(bsr_state = bsr2022_state, bsr_district = bsr2022_district,
         pc01_state = bsrpc01_state, pc01_district = bsrpc01_district)

cat("PC11 name mappings:", nrow(match_pc11), "\n")
cat("2022 name mappings:", nrow(match_2022), "\n")

master_districts <- match_file_full %>%
  filter(bsrpc01_district != "" & !is.na(bsrpc01_district)) %>%
  distinct(pc01_state = bsrpc01_state, pc01_district = bsrpc01_district)

cat("Master PC01 districts:", nrow(master_districts), "\n")

quarter_df <- data.frame(
  quarter_label = all_quarters_found,
  year = as.integer(sub("Q\\d_(\\d{4})", "\\1", all_quarters_found)),
  q    = as.integer(sub("Q(\\d)_\\d{4}", "\\1", all_quarters_found))
) %>% arrange(year, q)

master_quarters <- quarter_df$quarter_label

cat("Time period:", min(quarter_df$year), "Q", min(quarter_df$q),
    "to", max(quarter_df$year), "Q", max(quarter_df$q),
    "(", length(master_quarters), "quarters)\n")

# ------------------------------------------------------------------------------
# STEP 3: TWO-PASS MAPPING TO PC01 + BALANCED PANELS
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 3: Mapping to PC01 and creating PopGroup panels\n")
cat(rep("=", 80), "\n\n", sep = "")

popgroup_panels <- list()

for (folder_name in names(all_data_list)) {
  
  cat("Processing:", folder_name, "\n")
  
  data <- all_data_list[[folder_name]] %>%
    rename(bsr_state = State, bsr_district = District)
  
  # Pass 1: match on PC11 names
  data_mapped     <- data %>% left_join(match_pc11, by = c("bsr_state", "bsr_district"))
  pass1_unmatched <- sum(is.na(data_mapped$pc01_district))
  cat("  Unmatched after PC11 join:", pass1_unmatched, "\n")
  
  # Pass 2: for still-unmatched, try 2022-era names
  if (pass1_unmatched > 0) {
    
    unmatched_lookup <- data_mapped %>%
      filter(is.na(pc01_district)) %>%
      select(bsr_state, bsr_district) %>%
      left_join(match_2022, by = c("bsr_state", "bsr_district")) %>%
      select(bsr_state, bsr_district, pc01_state, pc01_district)
    
    data_mapped <- data_mapped %>%
      left_join(unmatched_lookup, by = c("bsr_state", "bsr_district"), suffix = c("", "_2022")) %>%
      mutate(
        pc01_state    = coalesce(pc01_state, pc01_state_2022),
        pc01_district = coalesce(pc01_district, pc01_district_2022)
      ) %>%
      select(-pc01_state_2022, -pc01_district_2022)
    
    cat("  Unmatched after 2022 join:", sum(is.na(data_mapped$pc01_district)), "\n")
  }
  
  # Log still-unmatched districts to console
  still_unmatched_rows <- data_mapped %>%
    filter(is.na(pc01_district))
  
  if (nrow(still_unmatched_rows) > 0) {
    
    dep_cols <- grep("_DEP$", colnames(still_unmatched_rows), value = TRUE)
    cre_cols <- grep("_CRE$", colnames(still_unmatched_rows), value = TRUE)
    
    still_unmatched_summary <- still_unmatched_rows %>%
      mutate(
        total_deposits = rowSums(across(all_of(dep_cols)), na.rm = TRUE),
        total_credit   = rowSums(across(all_of(cre_cols)), na.rm = TRUE)
      ) %>%
      distinct(bsr_state, bsr_district, pop_group, bank_group,
               total_deposits, total_credit)
    
    cat("  Dropped (unclear mapping):", nrow(still_unmatched_summary), "\n")
    print(still_unmatched_summary %>% select(bsr_state, bsr_district))
  }
  
  # Collapse to PC01 level
  quarter_cols <- grep("Q\\d_\\d{4}_(OFF|DEP|CRE)", colnames(data_mapped), value = TRUE)
  
  data_pc01 <- data_mapped %>%
    filter(!is.na(pc01_district) & !is.na(pc01_state)) %>%
    group_by(pc01_district, pc01_state, pop_group, bank_group) %>%
    summarize(across(all_of(quarter_cols), safe_sum), .groups = "drop")
  
  # Add any missing quarter columns
  for (qtr in master_quarters) {
    for (var in c("OFF", "DEP", "CRE")) {
      col_name <- paste0(qtr, "_", var)
      if (!col_name %in% colnames(data_pc01)) data_pc01[[col_name]] <- NA_real_
    }
  }
  
  # Balanced panel: every PC01 district × every pop/bank group combination
  data_pc01_full <- master_districts %>%
    cross_join(data_pc01 %>% distinct(pop_group, bank_group)) %>%
    left_join(data_pc01, by = c("pc01_district", "pc01_state", "pop_group", "bank_group"))
  
  # Reshape to long
  data_long <- data_pc01_full %>%
    pivot_longer(
      cols = matches("Q\\d_\\d{4}_(OFF|DEP|CRE)"),
      names_to = c("quarter", "year", "variable"),
      names_pattern = "(Q\\d)_(\\d{4})_(OFF|DEP|CRE)",
      values_to = "value"
    ) %>%
    group_by(pc01_district, pc01_state, pop_group, bank_group, quarter, year, variable) %>%
    summarize(value = safe_sum(value), .groups = "drop") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    rename(bsrpc01_district = pc01_district, bsrpc01_state = pc01_state,
           offices = OFF, deposits = DEP, credit = CRE) %>%
    arrange(bsrpc01_state, bsrpc01_district, pop_group, bank_group, year, quarter)
  
  pop_group_name <- unique(data_long$pop_group)[1]
  
  if (is.null(popgroup_panels[[pop_group_name]])) {
    popgroup_panels[[pop_group_name]] <- data_long
  } else {
    popgroup_panels[[pop_group_name]] <- bind_rows(popgroup_panels[[pop_group_name]], data_long)
  }
  
  cat("  ", format(nrow(data_long), big.mark = ","), "rows\n")
}

# ------------------------------------------------------------------------------
# STEP 4: SAVE MASTER PANEL AS CSV
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 4: Saving master panel\n")
cat(rep("=", 80), "\n\n", sep = "")

master_panel  <- bind_rows(popgroup_panels)
master_output <- here("04_clean", "district_qtr_pop_bank_panel.csv")
write_csv(master_panel, master_output)

cat("Saved:", basename(master_output), "\n")
cat("  Rows      :", format(nrow(master_panel), big.mark = ","), "\n")
cat("  Districts :", n_distinct(master_panel$bsrpc01_district), "\n")
cat("  PopGroups :", paste(sort(unique(master_panel$pop_group)),  collapse = ", "), "\n")
cat("  BankGroups:", paste(sort(unique(master_panel$bank_group)), collapse = ", "), "\n")
cat("  Years     :", min(master_panel$year), "-", max(master_panel$year), "\n")
