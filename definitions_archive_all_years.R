# Load necessary library
library(tidyverse)

# Set the base directory
definitions_folder <- "source_data/CRDCDataArchive"

# List all CRDC subdirectories
crdc_folders <- list.dirs(definitions_folder, recursive = FALSE)

# Identify only the subfolders containing Definitions.csv
definition_files <- file.path(crdc_folders, "Definitions.csv")
definition_files <- definition_files[file.exists(definition_files)]  # Keep only existing files

# Function to read each Definitions.csv and add a Year column
read_definitions <- function(file_path) {
  year <- gsub("CRDC", "", basename(dirname(file_path)))  # Extract year from folder name
  df <- read_csv(file_path, col_types = cols(.default = "c")) %>% 
    mutate(Year = year)  # Add year column
  return(df)
}

# Read and stack all definitions files
stacked_definitions <- map_dfr(definition_files, read_definitions)

# Display the stacked definitions dataframe
stacked_definitions %>% glimpse()

table(stacked_definitions$Year)  # See years included


# check 
# Check variable consistency over time
# Define the membership-related variable names
membership_vars <- c("MEM_AI", "MEM_AS", "MEM_BL", "MEM_HI", "MEM_TR", "MEM_WH")

# Filter definitions for membership-related variables AND ensure Module Name is "Membership"
cleaned_membership_definitions <- stacked_definitions %>%
  filter(`Variable Name` %in% membership_vars, `Module Name` == "Membership") %>%
  select(Year, `Variable Name`, `Variable Title`, `Table Name`, `Sub-Module Name`, `Module Name`) %>%
  arrange(Year, `Variable Name`)

# Check if we successfully removed misclassified variables
table(cleaned_membership_definitions$`Module Name`)  # Should only contain "Membership"

# Create a pivot table to confirm remaining variables over time
membership_pivot <- cleaned_membership_definitions %>%
  pivot_wider(names_from = Year, values_from = `Module Name`, values_fill = "Not Present")

# Display results
View(membership_pivot)  # Opens a table to verify the cleaned dataset

# so hopefully modeule name is mislabeled in 1968 and we can just use the variable name and variable title.