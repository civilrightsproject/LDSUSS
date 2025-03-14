# 1.1_validate
# Summarize membership counts by district and year
district_summary <- stacked_district_data %>%
  group_by(DISCODE, Year) %>%
  summarise(across(starts_with("MEM_"), sum, na.rm = TRUE), .groups = "drop") %>%
  arrange(DISCODE, Year)

# Check for inconsistencies in total membership counts over time
district_summary <- district_summary %>%
  mutate(membership_diff = MEM_TR - lag(MEM_TR, order_by = Year))

# Display inconsistent records
inconsistencies <- district_summary %>%
  filter(membership_diff != 0 & !is.na(membership_diff))

# Show summary of stacked data
glimpse(stacked_district_data)
View(district_summary)  # View the stacked data
View(inconsistencies)   # View inconsistencies in membership counts
