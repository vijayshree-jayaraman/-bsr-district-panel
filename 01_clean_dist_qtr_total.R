library(tidyverse)
library(readxl)
library(here)
library(haven)

# ------------------------------------------------------------------------------
# ALL BSR DATA PROCESSING
# Processes BSR All Credit files and merges PC01/PC11 data
# Author : Vijayshree Jayaraman
# Date   : 04/12/25
# Source : RBI DBIE — Statement 4A, District-wise Offices, Deposits and Credit
#          of Scheduled Commercial Banks. Values in Rupees Crores.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# STEP 1: PROCESS EXCEL FILES → .dta
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 1: Processing Excel files\n")
cat(rep("=", 80), "\n\n", sep = "")

folder_path <- here("03_raw/All")
excel_files <- list.files(folder_path, pattern = "^BSR_4A_.*\\.xlsx$", full.names = FALSE)

cat("Found", length(excel_files), "files\n\n")

for (filename in excel_files) {
  
  cat(rep("-", 80), "\n", sep = "")
  cat("Processing:", filename, "\n")
  
  quarter_match <- regmatches(filename, regexpr("\\d{4}q\\d-\\d{4}q\\d", filename, ignore.case = TRUE))
  if (length(quarter_match) == 0) { warning("Could not extract quarter range - Skipping"); next }
  
  parts      <- strsplit(quarter_match, "-")[[1]]
  start_year <- as.integer(substr(parts[1], 1, 4))
  start_q    <- as.integer(substr(parts[1], 6, 6))
  end_year   <- as.integer(substr(parts[2], 1, 4))
  end_q      <- as.integer(substr(parts[2], 6, 6))
  
  cat("Period:", start_year, "Q", start_q, "to", end_year, "Q", end_q, "\n")
  
  raw_data <- read_excel(file.path(folder_path, filename), skip = 6)
  raw_data <- raw_data %>% select(where(~!all(is.na(.) | . == "")))
  
  # Build column names (reverse chronological, matching Excel layout)
  quarters <- c()
  for (year in end_year:start_year) {
    qs <- if (year == end_year && year == start_year) end_q:start_q
    else if (year == end_year)                  end_q:1
    else if (year == start_year)                4:start_q
    else                                        4:1
    for (q in qs) {
      quarters <- c(quarters,
                    paste0("Q", q, "_", year, "_OFF"),
                    paste0("Q", q, "_", year, "_DEP"),
                    paste0("Q", q, "_", year, "_CRE"))
    }
  }
  
  new_colnames <- c("Region", "State", "District", quarters)
  if (length(new_colnames) != ncol(raw_data)) {
    warning(paste("Column mismatch:", ncol(raw_data), "vs", length(new_colnames), "- Skipping"))
    next
  }
  
  colnames(raw_data) <- new_colnames
  raw_data <- raw_data[-(1:2), ]
  raw_data <- subset(raw_data, !is.na(Region) & !is.na(State))
  
  cat("Cleaned data:", nrow(raw_data), "rows\n")
  
  output_filename <- sub("\\.xlsx$", ".dta", filename)
  write_dta(raw_data, file.path(folder_path, output_filename))
  cat("Saved:", output_filename, "\n\n")
}

cat("Step 1 complete\n")

# ------------------------------------------------------------------------------
# STEP 2: LOAD AND PREPARE DATASETS
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 2: Loading PC01, PC11, and mapping data\n")
cat(rep("=", 80), "\n\n", sep = "")

# Helper: collapse wide quarterly data efficiently via long format
collapse_wide <- function(df, id_cols) {
  df %>%
    pivot_longer(cols = matches("Q[1-4]_\\d{4}_(OFF|DEP|CRE)"),
                 names_to = "col", values_to = "value") %>%
    group_by(across(all_of(c(id_cols, "col")))) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = col, values_from = value)
}

# PC01 data (2003Q4-2016Q1) — recode Telangana to Andhra Pradesh for PC01 consistency
pc01_data <- read_dta(file.path(folder_path, "BSR_4A_2003q4-2016q1_pc01.dta"))
colnames(pc01_data)[c(2, 3)] <- c("bsrpc01_state", "bsrpc01_district")
pc01_data <- pc01_data %>%
  mutate(bsrpc01_state = ifelse(bsrpc01_state == "TELANGANA", "ANDHRA PRADESH", bsrpc01_state))

cat("PC01 data loaded:", nrow(pc01_data), "rows\n")

pc01_collapsed <- collapse_wide(pc01_data, c("bsrpc01_district", "bsrpc01_state", "Region"))

cat("PC01 collapsed:", nrow(pc01_collapsed), "rows\n")

# PC11 mid data — keep only 2016Q2-2016Q4 to avoid overlap with PC01 (which ends 2016Q1)
pc11_data_mid_full <- read_dta(file.path(folder_path, "BSR_4A_2015q2-2016q4_pc11.dta"))
colnames(pc11_data_mid_full)[c(2, 3)] <- c("bsrpc11_state", "bsrpc11_district")

pc11_data_mid <- pc11_data_mid_full %>%
  select(bsrpc11_state, bsrpc11_district, matches("Q[2-4]_2016_(OFF|DEP|CRE)"))

cat("PC11 mid (2016Q2-2016Q4):", nrow(pc11_data_mid), "rows,", ncol(pc11_data_mid), "columns\n")

# PC11 main data (2017Q1-2022Q3)
pc11_data <- read_dta(file.path(folder_path, "BSR_4A_2017q1-2022q3_pc11.dta"))
colnames(pc11_data)[c(2, 3)] <- c("bsrpc11_state", "bsrpc11_district")

cat("PC11 data loaded:", nrow(pc11_data), "rows\n")

# District mapping — two lookup tables to handle both PC11-era and 2022-era district names
match_file_full <- as_tibble(read.csv("../BSR_district_over_years.csv", stringsAsFactors = FALSE))

match_pc11 <- match_file_full %>%
  filter(!is.na(bsrpc11_district) & bsrpc11_district != "",
         !is.na(bsrpc01_district) & bsrpc01_district != "") %>%
  distinct(bsrpc11_state, bsrpc11_district, .keep_all = TRUE) %>%
  select(bsrpc11_state, bsrpc11_district, bsrpc01_state, bsrpc01_district)

match_2022 <- match_file_full %>%
  filter(!is.na(bsr2022_district) & bsr2022_district != "",
         !is.na(bsrpc01_district) & bsrpc01_district != "") %>%
  distinct(bsr2022_state, bsr2022_district, .keep_all = TRUE) %>%
  select(bsr2022_state, bsr2022_district, bsrpc01_state, bsrpc01_district)

cat("PC11 name mappings:", nrow(match_pc11), "\n")
cat("2022 name mappings:", nrow(match_2022), "\n")

# ------------------------------------------------------------------------------
# STEP 3: MERGE AND COLLAPSE PC11 TO PC01
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 3: Merging and collapsing PC11 to PC01\n")
cat(rep("=", 80), "\n\n", sep = "")

merged_data <- pc11_data %>%
  left_join(pc11_data_mid, by = c("bsrpc11_district", "bsrpc11_state"))

cat("PC11 datasets merged:", nrow(merged_data), "rows\n")

# Pass 1: match on PC11 names
merged_data <- merged_data %>%
  left_join(match_pc11, by = c("bsrpc11_district", "bsrpc11_state"))

pass1_unmatched <- sum(is.na(merged_data$bsrpc01_district))
cat("Unmatched after PC11 name join:", pass1_unmatched, "\n")

# Pass 2: for still-unmatched, try 2022-era names
if (pass1_unmatched > 0) {
  
  unmatched_rows <- merged_data %>%
    filter(is.na(bsrpc01_district)) %>%
    select(bsrpc11_state, bsrpc11_district) %>%
    left_join(match_2022,
              by = c("bsrpc11_state" = "bsr2022_state",
                     "bsrpc11_district" = "bsr2022_district")) %>%
    select(bsrpc11_state, bsrpc11_district, bsrpc01_state, bsrpc01_district)
  
  merged_data <- merged_data %>%
    left_join(unmatched_rows, by = c("bsrpc11_state", "bsrpc11_district"), suffix = c("", "_2022")) %>%
    mutate(
      bsrpc01_state    = coalesce(bsrpc01_state, bsrpc01_state_2022),
      bsrpc01_district = coalesce(bsrpc01_district, bsrpc01_district_2022)
    ) %>%
    select(-bsrpc01_state_2022, -bsrpc01_district_2022)
  
  cat("Unmatched after 2022 name join:", sum(is.na(merged_data$bsrpc01_district)), "\n")
}

# Report remaining unmatched (unclear mapping — see README)
still_unmatched <- merged_data %>%
  filter(is.na(bsrpc01_district)) %>%
  distinct(bsrpc11_state, bsrpc11_district)

cat("Districts dropped (unclear mapping):", nrow(still_unmatched), "\n")
if (nrow(still_unmatched) > 0) print(still_unmatched)

# Collapse to PC01 level using efficient long-format helper
collapsed_pc11 <- merged_data %>%
  filter(!is.na(bsrpc01_district) & !is.na(bsrpc01_state)) %>%
  collapse_wide(c("bsrpc01_district", "bsrpc01_state"))

cat("PC11 collapsed to PC01:", nrow(collapsed_pc11), "rows\n")

# ------------------------------------------------------------------------------
# STEP 4: CREATE FINAL PANEL
# ------------------------------------------------------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("STEP 4: Creating final panel\n")
cat(rep("=", 80), "\n\n", sep = "")

final_panel <- pc01_collapsed %>%
  left_join(collapsed_pc11, by = c("bsrpc01_district", "bsrpc01_state"))

cat("Final panel (wide):", nrow(final_panel), "rows,", ncol(final_panel), "columns\n")

# Diagnostics
overlap_cols <- colnames(final_panel)[grep("\\.x|\\.y", colnames(final_panel))]
cat("Overlapping columns:", length(overlap_cols), "\n")
if (length(overlap_cols) > 0) { cat("WARNING: column collisions:\n"); print(overlap_cols) }

missing_pc11 <- anti_join(pc01_collapsed, collapsed_pc11, by = c("bsrpc01_district", "bsrpc01_state"))
cat("Districts in PC01 with no PC11 match:", nrow(missing_pc11), "\n")
if (nrow(missing_pc11) > 0) print(missing_pc11 %>% select(bsrpc01_state, bsrpc01_district))

balance_check <- final_panel %>%
  group_by(bsrpc01_district, bsrpc01_state) %>%
  summarize(n_obs = n(), .groups = "drop")

cat("Unique districts:", nrow(balance_check), "\n")
if (all(balance_check$n_obs == 1)) cat("Panel is balanced\n\n") else {
  cat("Panel is NOT balanced\n\n")
  print(balance_check %>% filter(n_obs != 1))
}

# Reshape to long format
final_panel_long <- final_panel %>%
  pivot_longer(
    cols = matches("Q[1-4]_\\d{4}_(OFF|DEP|CRE)"),
    names_to = c("quarter", "year", "variable"),
    names_pattern = "(Q[1-4])_(\\d{4})_(OFF|DEP|CRE)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename(offices = OFF, deposits = DEP, credit = CRE) %>%
  select(-Region) %>%
  arrange(bsrpc01_district, bsrpc01_state, year, quarter)

cat("Final panel (long):", nrow(final_panel_long), "rows\n")

output_path <- here("04_clean/district_qtr_total.csv")
write_csv(final_panel_long, output_path)
cat("Saved:", basename(output_path), "\n")