# combine data from source archive files 1968-1998

# flat source files here:
# In the supplmentary files for many, but not all, years: https://civilrightsdata.ed.gov/archive


# No unique school_id, so merging is complex

library(tidyverse)


# Define the archive folder path
path <- "source_data/Summaries and Supplemental/1976/1976"
# List files ending with "schools.csv"
files <- list.files(path, pattern = "schools\\.csv$", full.names = TRUE)
# Use lapply to import each file
data_list <- lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE))
# Combine the list of data frames using rbind
stacked_schools_archive <- do.call(rbind, data_list) # column types vary across states, so rbind is better than bind_rows at the moment
# Print a preview of combined_data
head(stacked_schools_archive)

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

saveRDS(stacked_schools_archive, "inter/stacked_schools_archive.RDS")


### EXPLORE ###

# It seems that member total is not consistently increasing over time as expected. Could be do to sampling procedures.
