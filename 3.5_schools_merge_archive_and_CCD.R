# 3.5 SCHOOLS: merge archive and CCD
library(tidyverse)
library(magrittr)

# read archive
stacked_schools_archive <- readRDS("inter/stacked_schools_archive.RDS")

stacked_schools_archive %<>% 
  filter(YEAR == "1976-77") 

stacked_schools_archive %<>% 
  rename_with(~str_c("crdc.", .), everything())

#read ccd
ccd.enr.dir_school_wide <- readRDS("inter/ccd_enr_schools.RDS")


ccd.enr.dir_school_wide %<>%
  filter(year == ccd.focal.year)
sum(ccd.enr.dir_school_wide$`American Indian`, na.rm=T)

#prepend each column names with "ccd." to designate they come from CCD
ccd.enr.dir_school_wide %<>%
  rename_with(~str_c("ccd.", .), everything())
#clean into the "l." vars that will harmonize with the archive using the source vars below
# names(ccd.enr.dir_school_wide)
# [1] "ccd.year"            "ccd.ncessch"         "ccd.school_name"     "ccd.fips"           
# [5] "ccd.leaid"           "ccd.lea_name"        "ccd.zip_mailing"     "ccd.American Indian"
# [9] "ccd.Asian"           "ccd.Black"           "ccd.Latino"          "ccd.White"          
# [13] "ccd.NA"  

ccd.enr.dir_school_wide %<>%
  mutate(l.year = ccd.year,
         l.state.abb = state.abb[match(ccd.fips, state.name)],
         l.district.id = as.character(ccd.leaid),
         l.leaname = toupper(ccd.lea_name),
         l.school.name = toupper(ccd.school_name),
         l.city = toupper(ccd.city_mailing),
         l.black = ccd.Black,
         l.latino = ccd.Latino,
         l.white = ccd.White,
         l.asian = ccd.Asian,
         l.american_indian = `ccd.American Indian`)

ccd.enr.dir_school_wide %<>%
  select(starts_with("l."), everything())

  


# Step 3: Find matches by performing an inner join on the school ID
# match across the two school ids

# need the below to be long, not wide, so stack, not merge
# use something like bind_rows, not inner_join, but only include schools that have 
# the same DISCODE and ncessch across years. 
# This will be a long df, with the same DISCODE and leaid repeated for each year
# then, inner join on DISCODE and leaid
# Year_ should match year
# DISCODE should match leaid
# DISSTAT1 should match fips (I think state might already be "included" in the leaid)
# Start by normalizing the vars that will be used as keys
# Here's the normalizing for ccd_enr_district, converting state names to abbreviation using the built-in data in fips,
# all created vars should start with "l." to designate they come from LDDUSS

# names(stacked_schools_archive)
# [1] "YEAR"          "STATE"         "COUNTY"        "DISTRICT"      "CITY"         
# [6] "NO_OF_SCHOOLS" "OE_CODE"       "SCHOOL_NAME"   "AM_IND"        "AM_IND_PCT"   
# [11] "ASIAN"         "ASIAN_PCT"     "BLACK"         "BLACK_PCT"     "WHITE"        
# [16] "WHITE_PCT"     "HISP"          "HISP_PCT"      "TOTAL"         "GRADE_SPAN"   
# [21] "l.year" 

stacked_schools_archive %<>%
  mutate(l.year = as.numeric(substr(crdc.YEAR,1,4)),
         l.state.abb = state.abb[match(toupper(crdc.STATE), toupper(state.name))],
         # l.state.abb = state.abb[match(STATE, state.name)],
         l.district.id = as.character(crdc.OE_CODE),
         l.leaname = toupper(crdc.DISTRICT),
         l.school.name = toupper(crdc.SCHOOL_NAME),
         l.city = toupper(crdc.CITY),
         l.black = crdc.BLACK,
         l.latino = crdc.HISP,
         l.white = crdc.WHITE,
         l.asian = crdc.ASIAN,
         l.american_indian = crdc.AM_IND,
         l.total.enrollment = crdc.TOTAL)

# select begins with "l." and everything else
stacked_schools_archive %<>%
  select(starts_with("l."), everything())
  
glimpse(stacked_schools_archive)
glimpse(ccd.enr.dir_school_wide)

# using city, state and school name
matched_schools <- inner_join(stacked_schools_archive %>%
                                select(l.school.name,l.city, l.state.abb,starts_with("crdc.")),
                              ccd.enr.dir_school_wide  %>%
                                select(l.school.name,l.city, l.state.abb,starts_with("ccd.")),
                              by = c("l.school.name", "l.state.abb", "l.city"),
                              keep = FALSE)

# #using just state and school name, which is best to explore how the best fuzzy-match could be created
# matched_schools <- inner_join(stacked_schools_archive %>%
#                                 select(l.school.name, l.state.abb,starts_with("crdc.")), 
#                               ccd.enr.dir_school_wide  %>%
#                                 select(l.school.name, l.state.abb,starts_with("ccd.")),
#                                 by = c("l.school.name", "l.state.abb"),
#                                 keep = FALSE)
# # need to deal with this warning: 
# Warning message:
#   In inner_join(stacked_schools_archive %>% select(l.school.name,  :
#                                                      Detected an unexpected many-to-many relationship between `x` and `y`.
#                                                    ℹ Row 1585 of `x` matches multiple rows in `y`.
#                                                    ℹ Row 2585 of `y` matches multiple rows in `x`.
#                                                    ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence
#                                                    this warning.
                                          


# reorder to look at matches (keeping school names, district names, states, etc. next to each other)
matched_schools <- matched_schools %>%
  select(l.state.abb, l.school.name, crdc.CITY, ccd.city_mailing, 
         crdc.DISTRICT, ccd.lea_name, crdc.OE_CODE, ccd.leaid, 
         # crdc.TOTAL, ccd.total_enrollment, 
         crdc.BLACK, ccd.Black, 
         crdc.HISP, ccd.Latino, 
         crdc.WHITE, ccd.White, 
         crdc.ASIAN, ccd.Asian, 
         crdc.AM_IND, `ccd.American Indian`,
         everything())

View(matched_schools)


# VALIDATE
# A brief view suggests many good matches: close matches for ...
# district name 
# (TEMPE UNION HIGH 213
# TEMPE UNION HS DIST 213)
#OECODE and leaid (where the last 4 digits seem to often match)
# City and city mailing
# THERE ARE CLEARLY EXCEPTIONS, like:

# HOW MANY SCHOOL MATCHES ARE THERE?
nrow(matched_schools)
#OUT OF ??? SCHOOLS IN 1976
nrow(stacked_schools_archive)
# percent is?
nrow(matched_schools)/nrow(stacked_schools_archive)
# 32%
# This likely includes matches that SHOULD NOT be matched'
# e.g., there are at 3 matched schools in Massachucetts named Abraham Lincoln, but there should likely only be 1
# The one most likely match has a crdc.district of NEW BEDFORD and ccd.lea_name of NEW BEDFORD SCH DIST.
# The other two have ccd.lea_names that are very different than "New Bedford"
# A fuzzy-matching probability that prioritizes state, school name, lea name, city, and more (prioritizing in that order) would give us better unique probalistic matches

# make a long df, instead of a merged inner join, to do longitudinal analysis -- 
# key is state, city, and school name... we should have 2 rows per (with variation in year and racial groups): > glimpse(matched_schools)


# Inspect the resulting data frame
# Stack the two datasets with a source indicator.
stacked_schools <- bind_rows(
  stacked_schools_archive %>% 
    select(starts_with("l.")) %>% 
    mutate(source = "crdc"),
  ccd.enr.dir_school_wide %>% 
    select(starts_with("l.")) %>% 
    mutate(source = "ccd")
)

# Keep only schools (by school name, city, and state) that appear in both sources.
matched_schools <- stacked_schools %>%
  group_by(l.school.name, l.city, l.state.abb) %>%
  filter(n_distinct(source) == 2) %>%  # assuming two sources/years
  ungroup() %>%
  arrange(l.state.abb, l.city, l.school.name)

# Inspect the final data frame
glimpse(matched_schools)           
  
# analyze how these schools have changed in terms of percent black over time
# Summarize
race.groups <- c("l.black", "l.white", "l.latino", "l.asian", "l.american_indian")

school_changes <- matched_schools %>%
  # Ensure l.year is numeric for proper ordering
  mutate(l.year = as.integer(l.year)) %>%
  group_by(l.state.abb, l.city, l.school.name) %>%
  # Sort rows within each group by year
  arrange(l.year, .by_group = TRUE) %>%
  summarise(
    year_first = first(l.year),
    year_last  = last(l.year),
    # For each race group, extract the first value, the last value, and compute the difference
    across(all_of(race.groups),
           list(first = ~ first(.),
                last  = ~ last(.),
                diff  = ~ last(.) - first(.)),
           .names = "{.col}_{.fn}"),
    .groups = "drop"
  ) %>%
  arrange(l.state.abb, l.city, l.school.name)

# View the resulting data frame
View(school_changes)

  
# Summarize race (black and white) across years in the matched_schools dataset


matched_schools %>%
  group_by(l.state.abb, l.school.name) %>%
  summarize(across


