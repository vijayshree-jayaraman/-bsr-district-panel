library(arrow)
library(here)

# ------------------------------------------------------------------------------
# CONVERT OUTPUT PANELS TO PARQUET
# To revert to CSV: comment out write_parquet() lines, uncomment write_csv() lines
# ------------------------------------------------------------------------------

files <- list(
  list(
    input  = here("04_clean", "district_qtr_total.csv"),
    output = here("04_clean", "district_qtr_total.parquet")
  ),
  list(
    input  = here("04_clean", "district_qtr_pop_bank_panel.csv"),
    output = here("04_clean", "district_qtr_pop_bank_panel.parquet")
  )
)

for (f in files) {
  cat("Reading :", basename(f$input), "\n")
  dat <- read_csv(f$input, show_col_types = FALSE)
  
  write_parquet(dat, f$output)
  # write_csv(dat, f$input)   # uncomment to revert to CSV
  
  cat("Saved   :", basename(f$output), "\n")
  cat("  Rows  :", format(nrow(dat), big.mark = ","), "\n\n")
}

cat("Done.\n")

