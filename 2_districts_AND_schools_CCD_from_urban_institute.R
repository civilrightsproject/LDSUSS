# Import and merge CCD from urban institute

library(educationdata)
library(tidyverse)
library(data.table)
library(magrittr)
library(dtplyr)


# https://educationdata.urban.org/documentation/schools.html#ccd_directory

### DON'T NEED TO RE-DOWNLOAD IF ALREADY ON LOCAL DISK ####
ccd.focal.year <- 1988:2022 ## 2023 not yet available, race data before 1988 in the CCD filled with anomoalies
years <- ccd.focal.year

dir.create("ccd_dls")
dir.create("ccd_dls/ccd_dls_dir")
dir.create("ccd_dls/ccd_dls_enr")

#### CCD directory download ####

for(i in years){
  
  filename <- paste0("ccd_", i, ".RDS")
  filename.to.save <- paste0("ccd_dls/ccd_dls_dir/",filename)
  if (filename %in% list.files("ccd_dls/ccd_dls_dir/")) {
    cat("\nAlready downloaded: ", i)
    next()
  }
  
  print(timestamp())
  
  cat(i)
  
  resp_df <- get_education_data(level = "schools",
                                source = "ccd",
                                topic = "directory",
                                filters = list(year = i),
                                add_labels = TRUE)
  
  saveRDS(resp_df, filename.to.save)
}


##############################
#### DIRECTORY ###############
##############################

###########################################################################
#### THERE ARE FUNCTIONS TO GET MORE/OTHER/NEWER DATA IN CA REPORT CODE ###
###########################################################################

# combine raw dir downloaded files
ccd_dir <- list.files(path = "ccd_dls/ccd_dls_dir",
                      pattern = ".RDS",
                      full.names = TRUE) %>%
  purrr::keep(~str_detect(.x, paste0(ccd.focal.year, ".RDS"))) %>%
  purrr::map(readRDS) %>%
  data.table::rbindlist(fill=TRUE)

ccd_dir <- as.data.frame(ccd_dir)

# ccd_dir.for.shiny.app <- ccd_dir %<>%
#   select(ncessch,
#          year,
#          school_name,
#          charter,
#          school_status,
#          enrollment) %>%
#   filter(year >= 1998)
# saveRDS(ccd_dir.for.shiny.app, "inter/ccd_dir.for.shiny.app.RDS")

ccd_dir %>%
  group_by(year) %>%
  filter(year ==ccd.focal.year) %>%
  summarise(
    n.schools = length(unique(ncessch)),
    state_location.not.na = sum(!is.na(state_location)))

######################################################
### DOWNLOAD ENROLLMENT ##############################
######################################################


# georgia 1988 IS inside ccd_dir here

# ccd_dir <- lazy_dt(ccd_dir)

# THE BELOW WORKED BEFORE, BUT NOW I HAVE REMOVED IT BECAUSE OF ERROR????
# ccd_dir %<>% mutate_all(na_if,"")

# View(ccd_dir %>% as_tibble)
# georgia 1988 IS inside ccd_dir here

#### ccd enrollment download ####


download.the.enr.data <- function(i){
  filename <- paste0("ccd_", i, ".RDS")
  filename.to.save <- paste0("ccd_dls/ccd_dls_enr/",filename)
  if (filename %in% list.files("ccd_dls/ccd_dls_enr/")) {
    cat("\nAlready downloaded: ", i)
    return()
  }
  
  print(timestamp())
  
  cat(i)
  
  resp_df <- educationdata::get_education_data(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    filters = list(year = i, grade = 99),
    subtopic = list("race"),
    add_labels = TRUE
  )
  
  saveRDS(resp_df, filename.to.save)
}



for(i in years){

  filename <- paste0("ccd_", i, ".RDS")
  filename.to.save <- paste0("ccd_dls/ccd_dls_enr/",filename)
  if (filename %in% list.files("ccd_dls/ccd_dls_enr/")) {
    cat("\nAlready downloaded: ", i)
    next()
  }

  print(timestamp())

  print(i)

  resp_df <- get_education_data(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    filters = list(year = i, grade = 99),
    subtopic = list("race"),
    add_labels = TRUE
  )

  saveRDS(resp_df, filename.to.save)
}

#############
############# MERGE ENR's ###
#############

# combine raw dir downloaded files
ccd_enr <- list.files(path = "ccd_dls/ccd_dls_enr",
                      pattern = ".RDS",
                      full.names = TRUE) %>%
  #just include files that end in the focal.years
  purrr::keep(~str_detect(.x, paste0(ccd.focal.year, ".RDS"))) %>%
  purrr::map(readRDS) %>%
  data.table::rbindlist(fill=TRUE)

ccd_enr <- as.data.frame(ccd_enr)

################################################################################
##### *ENR* DOWNLOAD NEW (2021), NOT AVAILABLE FROM URBAN INSTITUTE ############
################################################################################




ccd_enr %>%
  filter(year == ccd.focal.year) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)),
            nce.n.11 = sum(nchar(ncessch) == 11)
  )

# table(ccd_enr$year)

# NOT missing 1988 georgia here!
# View(ccd_enr)

# ccd_enr %>%
#   filter(fips == "Georgia") %>%
#   group_by(year) %>%
#   summarize(sum.enr = sum(enrollment, na.rm=TRUE))


##################################################################
### GET CRDC DATA -- SEE CA DOWNLOAD FILE ########################
##################################################################


###### END GET CRDC DATA #################

ccd_enr %<>%
  filter(race != "Total") %>%
  # mutate_all(na_if,"") %>%
  mutate(enrollment = case_when(enrollment < 0 ~ NA_integer_,
                                TRUE ~ enrollment) )# recodes missing, etc. as NA

#asian in illinos 2008, here

#missing 1988 georgia here, problem is there are no data for GA in 1988 by race
#
# test <- ccd_enr %>%
#   filter(race != "Total")  %>%
#   mutate_all(na_if,"") %>%
#   mutate(enrollment = ifelse(enrollment < 0,NA_integer_,
#                                 enrollment) )# recodes missing, etc. as NA
#
# test %>%
#   filter(fips == "Georgia") %>%
#   group_by(year) %>%
#   summarize(sum.enr = sum(enrollment, na.rm=TRUE))
# test <- ccd_enr %>% as_tibble()
# table(test$race)
# Sum Asian and Native Hawaiian or other Pacific Islander

# The below excludes Asian for 2008 for illinios and other states!!

ccd_enr <- ccd_enr %>%
  mutate(
    # Keep the original race column
    race_original = race,
    
    # Create a recoded race variable for 2025 standardization
    race_recoded_2025 = case_when(
      race %in% c("Native Hawaiian or other Pacific Islander", "Native Hawaiian or Other Pacific Islander") ~ "Asian",
      race == "American Indian or Alaska Native" ~ "American Indian",
      race %in% c("Hispanic", "Hispanic/Latino") ~ "Latino",
      race == "Black or African American" ~ "Black",
      race %in% c("Two or more races", "Unknown", "Not Specified") ~ "Multiracial",
      TRUE ~ as.character(race) # Keep unchanged if not recoded
    )
  ) %>%
  group_by(year, ncessch, fips, leaid, race_original, race_recoded_2025) %>%
  summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop")


ccd_enr %<>% ungroup()

# ccd_enr %>%
#   ungroup()%>%
#   filter(year == 2021) %>%
#   summarise(total.enr = sum(enrollment, na.rm = TRUE),
#             n.schools = length(unique(ncessch)))

# View(ccd_enr %>% filter(year ==2008, fips == "Illinois", race == "Asian") %>% arrange(desc(enrollment)) %>% as_tibble())


#### Vars to keep from dir ####
# Vars to remove from dir, b/c they are in enr

ccd_dir %<>%
  select(-enrollment, -fips, -leaid)

# ccd_dir %<>%
#   select(year,
#          ncessch,
#          school_id,
#          school_name,
#          lea_name,
#          # fips,
#          
#          # street_mailing,
#          # city_mailing,
#          # zip_mailing,
#          #
#          # street_location,
#          # city_location,
#          state_location,
#          # zip_location,
#          #
#          latitude,
#          longitude,
#          # fips,
#          
#          # phone,
#          # school_level,
#          school_type,
#          
#          charter,
#          school_status,
#          "enrollment_total" = enrollment,
#          teachers_fte,
#          free_or_reduced_price_lunch,
#          lunch_program,
#          direct_certification
#          # lowest_grade_offered,
#          # highest_grade_offered,
#          # bureau_indian_education
#   )



# ccd_enr <- as_tibble(ccd_enr)
# table(ccd_dir$year)
# table(ccd_enr$year)
#### MERGE ####

# the problem is that the ccd_dir for 2021 is missing schools
# 
# ccd_dir_2021 <- ccd_dir %>% filter(year == 2021)
# 
# ccd_enr_2021 <- ccd_enr %>% filter(year == 2021)
# test <- merge(ccd_dir_2021, ccd_enr_2021, all=TRUE)
# View(test)

# The below takes a long time!
ccd.enr.dir <- merge(ccd_dir, ccd_enr,  
                     by = c("year", "ncessch"), 
                     all.x = TRUE)

# table(ccd.enr.dir$year)
#Validate
ccd.enr.dir %>%
  filter(year == 1988) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)))

ccd.enr.dir %>%
  filter(year == 2019) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)))
# TOTAL ENROLLMENT FROM https://nces.ed.gov/programs/coe/indicator/cga/public-school-enrollment:
# 50.8
ccd.enr.dir %>%
  filter(year == 2020) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)))
# TOTAL ENROLLMENT FROM https://nces.ed.gov/programs/coe/indicator/cga/public-school-enrollment:
# 49.4 million
ccd.enr.dir %>%
  filter(year == 2021) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)))
# TOTAL ENROLLENT FROM https://nces.ed.gov/whatsnew/press_releases/08_16_2022.asp#:~:text=%E2%80%9CCompared%20with%20fall%202020%2C%20total,Carr.
# 49.5 
# TOTAL ENROLLMENT IN 2021 HERE WAS INCONSTISTENT (probably because of provisional data)!!!


# TOTAL ENROLLENT FROM https://nces.ed.gov/programs/digest/d23/tables/dt23_203.20.asp
# 49.6

ccd.enr.dir %>%
  filter(year == 2022) %>%
  summarise(total.enr = sum(enrollment, na.rm = TRUE),
            n.schools = length(unique(ncessch)))

# example of school that didn't report disaggregated race data, but did report total school enrollment
# resp_df %>%
#   filter(race != "Total") %>%
#   filter(fips == "California") %>%
#   filter(ncessch == "062271007118") %>%
#   summarize(tot.enr = sum(enrollment, na.rm = T))
# 

# View the results
# View(zero_enrollment_percentage_district)  

## THE CCD CSV FROM https://nces.ed.gov/ccd/files.asp#Fiscal:2,LevelId:7,SchoolYearId:37,Page:1
# SAME PROBLEM IN SOURCE FILES!
# ccd.from.nces <- read.csv("in/ccd_sch_052_2223_l_1a_083023.csv")
# 
# ccd.from.nces %>%
#   filter(TOTAL_INDICATOR == "Category Set A - By Race/Ethnicity; Sex; Grade") %>%
#   group_by(LEAID, NCESSCH) %>%
#   summarise(
#     all_zero = all(STUDENT_COUNT == 0, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>%
#   group_by(LEAID) %>%
#   summarise(
#     total_schools = n(),
#     zero_enrollment_schools = sum(all_zero),
#     percentage_zero = (zero_enrollment_schools / total_schools) * 100
#   ) %>%
#   arrange(desc(percentage_zero)) %>%  # This line will sort the results to show LEAs with the highest percentage of zero enrollment schools first
#   View

# How many school names have chavez?




# View(ccd.enr.dir)
# table(ccd.enr.dir$school_status)
ccd.enr.dir %<>%
  filter(year %in% ccd.focal.year) %>%
  mutate(fips = as.character(fips)) %>%
  mutate(fips = stringr::str_to_title(fips))
table(ccd.enr.dir$fips)

#### SAVE SCHOOL-LEVEL ####


ccd.enr.dir_school_wide <- ccd.enr.dir  %>%
  group_by(year, ncessch, school_name,fips, state_leaid, seasch, leaid,lea_name, race_recoded_2025, zip_mailing,
           city_mailing, state_mailing) %>% # this will be restrictive over time, because ncessch isn't stable and dirty data across time
  summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop")

# Pivot to wide format so that each district (leaid) is one row,
# and we have one column per race category.
ccd.enr.dir_school_wide %<>%
  pivot_wider(
    names_from = race_recoded_2025, 
    values_from = enrollment, 
    values_fill = list(enrollment = NA)
  ) %>%
  ungroup()

saveRDS(ccd.enr.dir_school_wide, "inter/ccd_enr_schools.RDS")



########## SUMMARIZE BY DISTRICT TO MATCH AVAILABLE ARCHIVE CRDC #####
ccd_enr_district <- ccd.enr.dir %>%
  group_by(year, leaid, race_recoded_2025) %>%
  summarize(enrollment = sum(enrollment, na.rm = TRUE), .groups = "drop")

# Pivot to wide format so that each district (leaid) is one row,
# and we have one column per race category.
ccd_enr_district_wide <- ccd_enr_district %>%
  pivot_wider(
    names_from = race_recoded_2025, 
    values_from = enrollment, 
    values_fill = list(enrollment = NA)
  ) %>%
  ungroup()


# 2. Extract district-level info from the record with the largest year for each district.
# This assumes that ccd.enr.dir (or a similar dataset) includes location variables 
# (e.g., state_location, lea_name, address, city, zip) and the district ID (leaid).
# Here, first() simply takes the first available value for each variable within each district group. This selection is arbitrary—and it might be worth finding a specific criteria 
district_info <- ccd.enr.dir %>%
  group_by(leaid) %>%
  filter(year == max(year, na.rm = TRUE)) %>%  # select the record with the largest (most recent) year per district
  summarize(
    fips         = first(fips),
    lea_name     = first(lea_name),
    state_leaid = first(state_leaid),
    city_mailing = first(city_mailing),
    
    .groups = "drop"
  )

# 3. Merge the aggregated enrollment data with the district-level information
final_district_data <- ccd_enr_district_wide %>%
  left_join(district_info, by = "leaid") %>%
  relocate(year, lea_name,  .before = everything())


# 4. Optionally, calculate total enrollment and minority enrollment (assuming "White" is nonminority)
final_district_data <- final_district_data %>%
  mutate(
    total_enrollment = rowSums(across(c("American Indian", "Asian", "Black", "Latino", "White")), na.rm = TRUE)
  )


saveRDS(final_district_data, "inter/ccd_enr_district.RDS")
