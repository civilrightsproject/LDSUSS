# Load required libraries
library(tidyverse)

# File paths (update these if necessary)
crdc_1968_path <- "source_data/CRDCDataArchive/CRDC1968/DataTS6898_1.csv"

# Read datasets (ensuring DISCODE and LEAID are character types)
crdc_1968 <- read_csv(crdc_1968_path, col_types = cols(DISCODE = col_character()))


# Perform an outer join to check for matches
district_match_df <- full_join(crdc_1968, ccd_enr_district, by = c("DISCODE" = "leaid"), keep = TRUE) %>%
  mutate(match_status = case_when(
    !is.na(DISCODE) & !is.na(leaid) ~ "Match",
    is.na(DISCODE) & !is.na(leaid) ~ "Only in CCD 1988",
    !is.na(DISCODE) & is.na(leaid) ~ "Only in CRDC 1968",
    TRUE ~ "Unknown"
  ))


table(district_match_df$match_status)

# THERE ARE 21,000 matches, but 6,000 that have no matches in 1988. Seems possible given districts changing over 2 decades
# Next: state-wise breakdowns of missing districts to see if there are coding issues that prevent matching