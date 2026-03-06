# bsr-district-panel

District-level panel data on Scheduled Commercial Bank (SCB) offices, deposits, and credit across India, covering **2003Q4 to 2022Q3** (fiscal years). Coverage will be extended to 2025Q2 in a future update.

**Author:** Vijayshree Jayaraman  
**Data source:** RBI DBIE — [Statement 4A](https://dbie.rbi.org.in), District-wise Number of Reporting Offices, Aggregate Deposits and Bank Credit of Scheduled Commercial Banks  
**Downloaded:** December 2025  
**Values:** Rupees Crores  

---

## What this repository contains

| File | Description |
|---|---|
| `01_clean_dist_qtr_total.R` | Processes all-bank BSR data into a district-quarter panel |
| `02_clean_all_cuts.R` | Processes BSR data cut by population group × bank group |
| `03_convert_to_parquet.R` | Converts both output panels from CSV to Parquet |
| `BSR_district_over_years.csv` | District name mapping file (BSR names → PC01 boundaries) |
| `04_clean/district_qtr_total.parquet` | Final all-bank panel (output of `01_clean_dist_qtr_total.R`) |
| `04_clean/district_qtr_pop_bank_panel.parquet` | Final pop × bank panel (output of `02_clean_all_cuts.R`) |

Raw Excel files (`03_raw/`) are not tracked due to size. See the **Data Source** section above to download them from DBIE.

---

## Output variables

| Variable | Description |
|---|---|
| `district` | District name (PC01 / 2001 Census boundaries) |
| `state` | State name (PC01 boundaries) |
| `year` | Fiscal year start (e.g. 2010 = FY 2010-11) |
| `quarter` | Quarter within fiscal year (Q1 = Apr–Jun, Q2 = Jul–Sep, Q3 = Oct–Dec, Q4 = Jan–Mar) |
| `offices` | Number of reporting offices |
| `deposits` | Deposits (Rs. Crores) |
| `credit` | Credit (Rs. Crores) |
| `pop_group` | Population group — pop × bank panel only |
| `bank_group` | Bank group — pop × bank panel only |

---

## How to replicate

### Requirements

```r
install.packages(c("tidyverse", "readxl", "here", "arrow"))
```

### Folder structure

Scripts expect the following layout. The R project (`.Rproj`) should be set at the project root.

```
project-root/
│
├── BSR_district_over_years.csv   ← must be here
├── README.md
│
├── 02_code/
│   ├ 01_clean_dist_qtr_total.R
│   └- 02_clean_all_cuts.R
|   └- 03_convert_to_parquet.R
│
├── 03_raw/
│   ├── All/                      ← Excel files for BSR_processing.R
│   │   ├── BSR_4A_2003q4-2016q1_pc01.xlsx
│   │   ├── BSR_4A_2015q2-2016q4_pc11.xlsx
│   │   ├── BSR_4A_2017q1-2022q3_pc11.xlsx
│   │   
│   │
│   └── [PopGroup] [BankGroup]/   ← one folder per cut containing three files each, for 02_clean_all_cuts.R
│       └── BSR_4A_*.xlsx           e.g. "Rural Public", "Urban Private" folder with files names as in the "All" folder
│
└── 04_clean/                     ← outputs written here
```

> **Note on pop-bank folder names:** Each folder must follow the format `[PopGroup] [BankGroup]` with a single space separator. The first word becomes `pop_group` and everything after becomes `bank_group` in the output.

> **Parquet / CSV:** Outputs are saved as Parquet by default. To convert back to CSV, uncomment the `write_csv()` lines and comment out the `write_parquet()` lines in `03_convert_to_parquet.R`.

### Running the scripts

Open either script in RStudio with the `.Rproj` active and run it end to end, or use:

```r
source("02_code/01_clean_dist_qtr_total.R")         # agg district- qtr panel
source("02_code/02_clean_all_cuts.R") # dist x qtr x pop group × bank group panel
```

---

## District harmonisation

BSR data across different time periods uses different district names, reflecting boundary changes between the 2001 and 2011 Censuses. Both scripts harmonise all districts to **PC01 (2001 Census) boundaries** using `BSR_district_over_years.csv`.

The mapping file covers three naming vintages:

| Column prefix | Description |
|---|---|
| `bsrpc01_` | PC01 (2001 Census) — the target boundary used throughout |
| `bsrpc11_` | PC11 (2011 Census) boundary names |
| `bsr2022_` | 2022-era names used in recent BSR files for newly carved districts |

**Matching is done in two passes:** first on PC11 names, then on 2022-era names for districts not matched in pass 1.

**Telangana:** Since Telangana was carved from Andhra Pradesh in 2014, all Telangana districts are mapped back to their pre-bifurcation Andhra Pradesh PC01 boundaries.

---

## Important Caveat
The 13 districts listed below appear in the BSR data but are dropped from the panel due to unclear mapping, possibly because they were carved from multiple parent districts or their PC01 boundary equivalent is ambiguous.

| District | State |
|---|---|
| SARANGARH-BILAIGARH | Chhattisgarh |
| LOWER SIANG | Arunachal Pradesh |
| SIANG | Arunachal Pradesh |
| TIRAP | Arunachal Pradesh |
| SAITUAL | Mizoram |
| ALLURI SITHARAMA RAJU | Andhra Pradesh |
| ANNAMAYYA | Andhra Pradesh |
| ELURU | Andhra Pradesh |
| PARVATHIPURAM MANYAM | Andhra Pradesh |
| TIRUPATI | Andhra Pradesh |
| JANGAON | Telangana |
| SIDDIPET | Telangana |
| VIKARABAD | Telangana |

