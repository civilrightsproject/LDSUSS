# combine data from source archive files 1968-1998

# flat source files here:
# noted on ipscr too
#   https://www.datalumos.org/datalumos/project/219621/version/V1/view
# and
# https://www.datalumos.org/datalumos/project/218864/version/V1/view?path=/datalumos/218864/fcr:versions/V1


library(tidyverse)


# Define the archive folder path
archive_folder <- "source_data/CRDCDataArchive/districts"

# List all files and folders within the archive directory
file_list <- list.files(archive_folder, recursive = TRUE, full.names = TRUE)

# Print the list of files
print(file_list)


# List all ZIP files
zip_files <- list.files(archive_folder, pattern = "\\.zip$", full.names = TRUE)
zip_files <- zip_files[!grepl("199[2-9]", zip_files)]  # Removes 1992, 1994, 1997, 199

# Unzip all files (creates subdirectories)
unzip_files <- function(zip_path) {
  unzip(zip_path, exdir = gsub("\\.zip$", "", zip_path)) # Extract in same folder
}

# Unzip each file
lapply(zip_files, unzip_files)

# List all extracted CSVs
csv_files <- list.files(archive_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

csv_files <- csv_files[!grepl("199[2-9]", csv_files)]  # Exclude CSVs in folders from 1992 and later


# Load a sample CSV (first one found)
sample_file <- csv_files[1]
sample_data <- read_csv(sample_file)

# Show column names
colnames(sample_data)

# Show first few rows
glimpse(sample_data)

#######################


# Load necessary library
library(tidyverse)

# Set base directory for data files
data_folder <- "source_data/CRDCDataArchive/districts"

# List all CRDC subdirectories
crdc_folders <- list.dirs(data_folder, recursive = FALSE)
crdc_folders <- crdc_folders[!grepl("199[2-9]", crdc_folders)]  # Exclude CSVs in folders from 1992 and later

# Identify data files (The files with 2 are staff files, I think, eg: DataTS6898_2.csv)
data_files <- c(
  file.path(crdc_folders, "DataTS6898_1.csv")
)
data_files <- data_files[file.exists(data_files)]  # Keep only existing files
n_distinct(data_files)

# names(stacked_district_data)
# [1] "DISCODE"  "DISSTAT"  "Year_"    "COMPSCHL" "DISCITY"  "DISCNTY"  "DISCODE1"
# [8] "DISNAME"  "DISSTAT1" "DISSTRT"  "DISTWGHT" "DISZIP"   "MEM_AI"   "MEM_AS"  
# [15] "MEM_BL"   "MEM_HI"   "MEM_TR"   "MEM_WH"   "NUMSCH"   "RMEM_AI"  "RMEM_AS" 
# [22] "RMEM_BL"  "RMEM_HI"  "RMEM_TR"  "RMEM_WH"  "Year"     "SLEP_TR"  "NLEP_TR" 
# [29] "SUSP_AI"  "SUSP_AS"  "SUSP_BL"  "SUSP_HI"  "SUSP_TR"  "SUSP_WH"  "SLD_AI"  
# [36] "SLD_AS"   "SLD_BL"   "SLD_HI"   "SLD_TR"   "SLD_WH"   "CORP_AI"  "CORP_AS" 
# [43] "CORP_BL"  "CORP_HI"  "CORP_TF"  "CORP_TM"  "CORP_TR"  "CORP_WH"  "GIFT_AI" 
# [50] "GIFT_AS"  "GIFT_BL"  "GIFT_HI"  "GIFT_TF"  "GIFT_TM"  "GIFT_TR"  "GIFT_WH" 
# [57] "GRAD_AI"  "GRAD_AS"  "GRAD_BL"  "GRAD_HI"  "GRAD_TF"  "GRAD_TM"  "GRAD_TR" 
# [64] "GRAD_WH"  "MEM_TF"   "MEM_TM"   "MULT_FT"  "MULT_PT"  "MULT_XT"  "NLEP_AI" 
# [71] "NLEP_AS"  "NLEP_BL"  "NLEP_HI"  "NLEP_WH"  "ORTH_FT"  "ORTH_PT"  "ORTH_XT" 
# [78] "OTHR_FT"  "OTHR_PT"  "OTHR_XT"  "RMEM_TF"  "RMEM_TM"  "SED_AI"   "SED_AS"  
# [85] "SED_BL"   "SED_HI"   "SED_TF"   "SED_TM"   "SED_TR"   "SED_WH"   "SEDIST"  
# [92] "SEEVAL"   "SLD_TF"   "SLD_TM"   "SLEP_AI"  "SLEP_AS"  "SLEP_BL"  "SLEP_HI" 
# [99] "SLEP_WH"  "SPE_FT"   "SPE_PT"   "SPE_XT"   "SUSP_TF"  "SUSP_TM"  "VIS_FT"  
# [106] "VIS_PT"   "VIS_XT"   "DFBL_FT"  "DFBL_PT"  "DFBL_XT"  "HEAR_FT"  "HEAR_PT" 
# [113] "HEAR_XT"  "SED_LE"   "SENODIS"  "SEREQEV"  "SLD_LE"   "DISTSUB" 
# Define focal variables
focal_vars <- c("Year_", "DISCODE", "DISSTAT1" , "DISNAME",
                "DISCNTY", "DISCITY","DISSTRT","DISZIP",
                "DISTWGHT",
                "COMPSCHL", "NUMSCH",
                "MEM_AI", "MEM_AS", "MEM_BL", "MEM_HI", "MEM_TR", "MEM_WH")

# Function to read data and extract only focal variables
read_crdc_data <- function(file_path) {
  year <- gsub("CRDC", "", basename(dirname(file_path)))  # Extract year from folder name
  df <- read_csv(file_path, col_types = cols(.default = "c")) %>%
    select(any_of(focal_vars))  # Keep only focal variables
  return(df)
}

# Read and stack all district-level data
stacked_district_data <- map_dfr(data_files, read_crdc_data)

# Convert membership variables to numeric
stacked_district_data <- stacked_district_data %>%
  mutate(across(c(starts_with("MEM_"),
                  DISTWGHT, 
                  COMPSCHL,
                  Year_,
                  NUMSCH), as.numeric))

# mismatches_year <- stacked_district_data %>%
#   filter(Year_ != Year) 
# I think Year_ is better than Year, because the 1970 value in Year_ seems more accurate than 1979 -- by examining the trend of the number of schools.


# Compare stacked_district_data$DISCODE with stacked_district_data$DISCODE1
# mismatches_discode <- stacked_district_data %>%
#     filter(DISCODE != DISCODE1)
# All seem equal, so just go with DISCODE

library(tidyverse)
library(knitr)

## Validation
## There were approx. 34,082,000 + 16,878,000 =  50,960,000 children between 5-17 years old in 1974 (Table No. 3, Part 2, Statistical Abstract of the United States: 1976, https://www.census.gov/library/publications/1976/compendia/statab/97ed.html)
## There are some within these ages that would a) not be enrolled in any school; b) not be counted in the public school enrollment b/c they were in private school
## There should be 43,713,809 pupils in 1976 according to the 1976 supplement, p. I-3
# and 368,262 American Indians (p. I-3)
# and 6,773,690 Black (Table 6, Page 1 (71 in the PDF), from disciplinary table)
# Create a data frame (tibble) with the data
# NOTE: Edit these numbers/percentages as needed to match your actual table
# "Miscellaneous loose leaf documents.tif"
table_6_1976 <- tribble(
  ~Race,            ~Total_1976, 
  "American Indian",       368262,    
  "Asian American",        535156,    
  "Black",                6773690,    
  "White",               33229249,    
  "Hispanic",            2807452,     
  "Total Enrollment",    43713809,    
)

# Total enrollment in 1978 was 41,836,257 according to Director of Elementary and Secondary School Districts, and Schools in Selected School Districts: School Year 1978-1979, Vol I, found in the 1980 supplemental archive, Table 1 on pdf page 13)


table_6_replicate <- stacked_district_data %>%
  filter(Year_ == 1976) %>%
  mutate(
    # Calculate minority enrollment for each district (all minority races)
    total.sum = MEM_AI + MEM_AS + MEM_BL + MEM_HI + MEM_WH,
    # Calculate weighted totals for each race
    weighted_total.AI = MEM_AI * DISTWGHT,
    weighted_total.AS = MEM_AS * DISTWGHT,
    weighted_total.BL = MEM_BL * DISTWGHT,
    weighted_total.HI = MEM_HI * DISTWGHT,
    weighted_total.WH = MEM_WH * DISTWGHT,
    # Weighted total minority and overall enrollment
    weighted_total.sum = total.sum * DISTWGHT,
    weighted_MEM_TR = MEM_TR * DISTWGHT  
  ) %>%
  summarize(
    total_weighted_minority = sum(weighted_total.sum, na.rm = TRUE),
    total_weighted_enrollment = sum(weighted_MEM_TR, na.rm = TRUE),
    total_weighted_AI = sum(weighted_total.AI, na.rm = TRUE),
    total_weighted_AS = sum(weighted_total.AS, na.rm = TRUE),
    total_weighted_BL = sum(weighted_total.BL, na.rm = TRUE),
    total_weighted_HI = sum(weighted_total.HI, na.rm = TRUE),
    total_weighted_WH = sum(weighted_total.WH, na.rm = TRUE)
  )

# Create a tibble that matches the race labels in table_6_1976
table_6_replicate_long <- tibble(
  Race = c("American Indian", "Asian American", "Black", "White", "Hispanic", "Total Enrollment"),
  replicate_count = c(
    table_6_replicate$total_weighted_AI,
    table_6_replicate$total_weighted_AS,
    table_6_replicate$total_weighted_BL,
    table_6_replicate$total_weighted_WH,
    table_6_replicate$total_weighted_HI,
    table_6_replicate$total_weighted_enrollment
  )
)

table_6_comparison <- table_6_1976 %>%
  left_join(table_6_replicate_long, by = "Race") %>%
  mutate(
    difference = replicate_count - Total_1976,
    pct_diff   = round((difference / Total_1976) * 100, 2)
  )

table_6_comparison
# Close, but not exact


## There should be 10,746,882 "minority" pupils in 1976 according to the 1976 supplement, Table 2, pg. 105
stacked_district_data %>%
  filter(Year_ == 1976) %>%
  mutate(
    # Calculate minority enrollment for each district
    minority = MEM_AI + MEM_AS + MEM_BL + MEM_HI,
    # Multiply by the district’s weight
    weighted_minority = minority * DISTWGHT
  ) %>%
  summarize(total_weighted_minority = sum(weighted_minority, na.rm = TRUE))

# and minority enrollment should be 24% of the total, according to the same source

stacked_district_data %>%
  filter(Year_ == 1976) %>%
  mutate(
    # Calculate minority enrollment for each district (summing MEM_AI, MEM_AS, MEM_BL, MEM_HI, MEM_WH)
    total.minority.sum = MEM_AI + MEM_AS + MEM_BL + MEM_HI,
    total.sum = MEM_AI + MEM_AS + MEM_BL + MEM_HI + MEM_WH,
    # Multiply each by the district’s weight
    weighted_total.min.sum = total.minority.sum * DISTWGHT,
    weighted_total.sum = total.sum * DISTWGHT,
    weighted.MEM_TR = MEM_TR * DISTWGHT  
  ) %>%
  summarize(
    total_weighted_total.min.sum = sum(weighted_total.min.sum, na.rm = TRUE),
    total_weighted_total.sum = sum(weighted_total.sum, na.rm = TRUE),
    weighted.MEM_TR.sum = sum(weighted.MEM_TR, na.rm = TRUE)
  ) %>%
  mutate(perc_minority. = (total_weighted_total.min.sum/ total_weighted_total.sum) * 100,
         perc_minority_MEM_TR = (total_weighted_total.min.sum/ weighted.MEM_TR.sum) *100)

# At 23.5%, our calculation comes close to the stat in the published report, and is similar, regardless of summing method

# there are ~40 duplicates, so remove here
stacked_district_data <- stacked_district_data %>%
  distinct()

saveRDS(stacked_district_data, "inter/archive_stacked_district_data.RDS")

write.csv(stacked_district_data, "inter/stacked_district_data.csv", row.names = FALSE)

### EXPLORE ###

# It seems that member total is not consistently increasing over time as expected. Could be do to sampling procedures.
