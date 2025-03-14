# 1_archive_sampling_exploration

library(tidyverse)

# If 1976 was a complete sample year, it is odd that there is a DISTWGHT variable, which varies, for that year

# Let's assume DISTWGHT represents the sample weight.
table(stacked_district_data$DISTWGHT)
table(stacked_district_data$DISSTAT1)

# There are a lot of DISTWGHT's of 0, which I think means they weren't included in the measured sample. All have 0 enrollment, which I think means they simply didn't report.
# These were mostly smaller districts with less than 10 schools.
length(stacked_district_data$DISTWGHT == 0)

stacked_district_data_1976 <- stacked_district_data %>%
  filter(Year_ == 1976)
#unexpected small correlation between enrollement and weight -- expected negative and strong
cor(stacked_district_data$DISTWGHT, stacked_district_data$MEM_TR, use = "complete.obs")

# Basic scatter plot of sample weight vs. total enrollment,
# faceted by year to see if the relationship changes over time.
stacked_district_data %>%
  filter(Year_ == 1976) %>%
  group_by(DISSTAT1) %>%
  ggplot(aes(x = MEM_TR, y = DISTWGHT)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Year_) +
  theme_minimal() +
  labs(title = "Relationship between Total Enrollment and District Weight by Year")

# Could examine whether location matters. If you have state information (say in a column "fips" or "State"),
# do a similar plot by state or include it as a covariate in a regression.

glimpse(stacked_district_data)

# This model would give you an idea of how the weight scales with enrollment and whether there are systematic differences by state or year.