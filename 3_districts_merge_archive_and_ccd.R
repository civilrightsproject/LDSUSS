# 3 merge archiv and 1990 CCD
library(tidyverse)
library(magrittr)

# read archive
stacked_district_data <- readRDS("inter/archive_stacked_district_data.RDS")
stacked_district_data %<>% 
  filter(Year_ == 1976) %>%
  filter(DISTWGHT != 0)

# should be total enrollment:
sum(stacked_district_data$MEM_TR* stacked_district_data$DISTWGHT)
  
#read ccd
ccd_enr_district <- readRDS("inter/ccd_enr_district.RDS")
# glimpse(ccd_enr_district)

ccd_enr_district %<>%
  filter(year == 1988)
# sum(ccd_enr_district$total_enrollment, na.rm=T)



# Probably need to work on the district code format in ccd_districts
# e.g., BESSEMER CITY has DISTCODE 1000330 in the archive, but 100330 in the 1988 CCD 

# Describe leaid and DISCODE in terms of them being keys -- same length, style, etc?
# Average character length

# stacked_district_data %>%
#   group_by(DISSTAT1) %>%
#   summarize(mean = mean(nchar(DISCODE), na.rm = TRUE)) %>%
#   View

# ccd_enr_district %>%
#   group_by(fips) %>%
#   summarize(mean = mean(nchar(leaid), na.rm = TRUE)) %>%
#   View

# Step 3: Find matches by performing an inner join on the district ID
# match across the two district ids

# need the below to be long, not wide, so stack, not merge
# use something like bind_rows, not inner_join, but only include districts that have 
# the same DISCODE and leaid across years. 
# This will be a long df, with the same DISCODE and leaid repeated for each year
# then, inner join on DISCODE and leaid
# Year_ should match year
# DISCODE should match leaid
# DISSTAT1 should match fips (I think state might already be "included" in the leaid)
# Start by normalizing the vars that will be used as keys
# Here's the normalizing for ccd_enr_district, converting state names to abbreviation using the built-in data in fips,
# all created vars should start with "l." to designate they come from LDDUSS

ccd_enr_district %<>%
  mutate(l.year = year,
         l.state.abb = state.abb[match(fips, state.name)],
         l.district.id = leaid,
         l.leaname = toupper(lea_name),
         l.black = Black,
         l.latino = Latino,
         l.white = White,
         l.asian = Asian,
         l.american_indian = `American Indian`,
         l.total.enrollment = total_enrollment)

stacked_district_data %<>%
  mutate(l.year = Year_,
         l.state.abb = DISSTAT1,
         l.district.id = DISCODE,
         l.leaname = toupper(DISNAME),
         l.black = MEM_BL,
         l.latino = MEM_HI,
         l.white = MEM_WH,
         l.asian = MEM_AS,
         l.american_indian = MEM_AI,
         l.total.enrollment = MEM_TR)

stacked_all <- bind_rows(stacked_district_data, ccd_enr_district)
stacked_all %<>%
  select(l.year, l.state.abb, l.district.id, l.leaname, l.black, l.latino, l.white, l.asian, l.american_indian, l.total.enrollment,
         everything())


# Examples of should-be-matches:
# 1976
# AL
# 1000030 #NOTE THE additional 0
# ALEXANDER CITY
# 
# 1988
# AL
# 100030 $NOTE THE missing 0
# ALEXANDER CITY CITY #NOTE THE additional CITY

# need either cleaning, fuzzy match, or both


# explore the distribution of l.state.abb 
# within each district id-state.abb combination, how many unique states are there?:
stacked_all %>%
  group_by(l.district.id, l.state.abb, l.leaname) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  View

# find rows with the same district name-state using stacked_all
stacked_all %>%
  group_by(l.district.id, l.state.abb) %>%
  summarize(n = n_distinct(na.omit(l.state.abb))) %>%
  View




# keep just districts that have a matching l.district.id 
# AND a matching l.state.abb in all years -- that is, the district id AND l.state.abb is consistent across years
# for example, if i have l.district.id 1000 in 1976 and 1988, and l.state.abb "AL" in 1976 and 1988 for those same two rows, keep those two rows
# if i have l.district.id 1000 in 1976 and 1988, but l.state.abb "AL" in 1976 and "AK" in 1988, drop those rows
stacked_all %<>%
  group_by(l.district.id) %>%
  filter(n_distinct(na.omit(l.state.abb)) == 1) %>% 
  ungroup() %>%
  arrange(l.district.id, l.year)

# let's make a new variable, district_id, which is either the DISCODE and leaid
# then stack the data 
stacked_district_data <- stacked_all %>%
  mutate(district_id = case_when(
    !is.na(DISCODE) ~ DISCODE,
    !is.na(leaid) ~ leaid
  ))


matched_districts <- inner_join(stacked_district_data, 
                                ccd_enr_district, 
                                by = join_by("DISCODE" == "leaid"),
                                keep = TRUE)

# place all identifying variables at the start of the df
matched_districts <- matched_districts %>%
  select(Year_, DISCODE, DISSTAT1, fips, lea_name, lea_name, fips,everything())


# matched districts by state, because I think keys by state are not consistent
matched_districts %>%
  group_by(DISSTAT1) %>%
  summarize(n = n()) %>%
  arrange(n) 

# why does the above not work


#anti-join to find which leaids were not in ccd_enr_district that were in the archive
# include all variables 
not_matched_districts <- anti_join(stacked_district_data,
                                   ccd_enr_district, 
                                   by = join_by("DISCODE" == "leaid"))

# Step 4: Summarize the results
num_archived <- nrow(archived_1976_districts)
num_matched  <- nrow(matched_districts)
percent_matched <- round((num_matched / num_archived) * 100, 2)

comparison_summary <- tibble(
  Archived_Districts = num_archived,
  Matched_Districts = num_matched,
  Percent_Matched = percent_matched
)

# Display the summary table
comparison_summary
