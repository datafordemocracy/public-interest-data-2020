######################################################################################
# LP 5440: Public Interest Data Lab, Spring 2020 
# Generate visuals on data grouped by census tract (Section III)
# 1. Load libraries and data
# 2. Acquire 2013-2017 5-year Sex by Age ACS estimates for Cville
# 3. Generate Cville child population estimates by race, create childpop dataframe
# 4. Load referral data, generate counts by race
# 5. Pulling acs 2014-2018 survey data to group census tracts (by race and poverty)
# Updated: October 2020 (Hannah, mpc) 
######################################################################################

# ..........................................................................................
# 1. Load libraries and data ----

# install.packages("tidycensus")
library(tidycensus)
library(scales) # check palette
library(tidyverse)
library(knitr)
library(kableExtra)


setwd("/Volumes/NO NAME")

# ..........................................................................................
# 2. Acquire 2013-2017 5-year Sex by Age ACS estimates for Cville ----
# census_api_key("", install = TRUE)

data(fips_codes) # built in dataset for looking up state and county
fips_codes %>% filter(state == "VA") # find county code for CVille

### a. get data: total population (combined) ###
cville_acs_all <- get_acs(geography = "county", table = "B01001", 
                          year = 2018, state = "VA", county = "540", 
                          survey = "acs5", cache_table = TRUE)

### b. get data: white alone ###
cville_acs_white <- get_acs(geography = "county", table = "B01001A", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### c. get data: Black or African-American alone ###
cville_acs_black <- get_acs(geography = "county", table = "B01001B", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### d. get data: American Indian and Alaska Native alone ###
cville_acs_ai_an <- get_acs(geography = "county", table = "B01001C", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### e. get data: Asian alone ###
cville_acs_asian <- get_acs(geography = "county", table = "B01001D", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### f. get data: Native Hawaiian and Other Pacific Islander alone ###
cville_acs_nh_pi <- get_acs(geography = "county", table = "B01001E", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### g. get data: Some other race alone ###
cville_acs_other <- get_acs(geography = "county", table = "B01001F", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### h. get data: Two or more races ###
cville_acs_multi <- get_acs(geography = "county", table = "B01001G", 
                            year = 2018, state = "VA", county = "540", 
                            survey = "acs5", cache_table = TRUE)

### i. get data: white alone, not hispanic or latino ###
cville_acs_whitenh <- get_acs(geography = "county", table = "B01001H", 
                              year = 2018, state = "VA", county = "540", 
                              survey = "acs5", cache_table = TRUE)

### j. get data: hispanic or latino ###
cville_acs_hisp <- get_acs(geography = "county", table = "B01001I", 
                           year = 2018, state = "VA", county = "540", 
                           survey = "acs5", cache_table = TRUE)


# ..........................................................................................
# 3. Generate Cville child population estimates by race ----
# create childpop dataframe

# calculate number of children in cville with moe (age broken into finer intervals for total pop)
kids <- cville_acs_all[c(3:6,27:30),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Total")

# function for generating and adding racial counts
childsum <- function(d,r){
  sum <- d[c(3:6,18:21),] %>% 
    summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
    mutate(race = r)
  tot <- rbind(kids, sum)
}

# apply childsum function to cville_acs_xxxxx dataframes
# kids <- childsum(cville_acs_white, "White")
kids <- childsum(cville_acs_whitenh, "White")
kids <- childsum(cville_acs_black, "Black")
kids <- childsum(cville_acs_multi, "MultiRace")
kids <- childsum(cville_acs_asian, "Asian")
kids <- childsum(cville_acs_ai_an, "ai_an")
kids <- childsum(cville_acs_nh_pi, "nh_pi")
kids <- childsum(cville_acs_other, "Other")
kids <- childsum(cville_acs_hisp, "Hispanic")

# Cville 2014-2018 child pop estimates (race_ethn categories) 
# add ai_an, nh_pi, and other together
kids2 <- kids[c(6,7,8),] %>% 
  summarize(estimate = sum(estimate), moe = moe_sum(moe, estimate)) %>% 
  mutate(race = "Other")

childpop <- rbind(kids[c(1:5,9),], kids2) # add "Other" estimate to total, white, black estimates

# Calculate proportions
totpop <- as_vector(c(childpop[1,1], childpop[1,2]))
childpop <- childpop %>% 
  mutate(prop = estimate/totpop[1], pmoe = moe_prop(estimate, totpop[1], moe, totpop[2]),
         race = fct_recode(race, "Multiracial" = "MultiRace"))

# save work
save.image("data2020/acs2018.RData")
# load("data2020/acs2018.Rdata")

# ..........................................................................................
# 4. Load referral data, generate counts by race ----

ref <- readRDS("data2020/ref_supp_long.RDS")

# referral child counts by race (3 year total, only 1 ref per child)
refpop <- ref %>% 
  mutate(source = "ref") %>% 
  group_by(race_ethn) %>% 
  summarize(source = first(source), number = n_distinct(cid)) %>% 
  mutate(moe = NA,
         race_ethn = fct_recode(race_ethn, "Other" = "Unknown",
                                "Multiracial" = "MultiRace")) %>% 
  rename(race = race_ethn) %>% 
  select(source, everything()) 

# ..........................................................................................
# 5. Create combined data frames for analysis ----

# reformat childpop to bind onto refpop (long)
childlong <- childpop %>% 
  mutate(source = "acs17") %>% 
  rename(number = estimate) %>% 
  filter(race != "Total") %>% 
  select(source, race, number, moe)

# bind acs to ref data
acs_ref <- bind_rows(childlong, refpop)
# make race a factor (and order for visualization)
acs_ref <- acs_ref %>% 
  mutate(race = factor(race, levels = c("White", "Black", "Hispanic", "Multiracial", "Asian", "Other")))

# reformat refpop to join onto childpop (wide) ###
refpopwide <- refpop %>% select(source, race, number) %>% 
  spread(key = source, value = number)

# generate total counts for each source 
reftot <- refpopwide %>% 
  summarize_at(vars(ref), list(~ sum(.))) %>% 
  mutate(race = "Total") %>% 
  select(race, ref)

# calculate proportions 
refpopwide <- refpopwide %>% 
  mutate(refprop = ref/reftot$ref)
refpopwide <- bind_rows(refpopwide, reftot)

# join acs and cws
ref_acs <- left_join(childpop, refpopwide, by="race")

# clean up and save work
rm(refpopwide, reftot, fips_codes, totpop, 
   cville_acs_ai_an, cville_acs_asian, cville_acs_black,
   cville_acs_hisp, cville_acs_multi, cville_acs_nh_pi,
   cville_acs_other, cville_acs_white, cville_acs_whitenh,
   cville_acs_all, kids, kids2, childlong, childpop)

save.image("data2020/cville_acs.Rdata")
# load("data2020/cville_acs.Rdata")


# ..........................................................................................
# 6. Pulling acs 2014-2018 survey data to group census tracts ----
# by race and by poverty

## Pulling from table B01001 "Sex by Age" 
## to find the total population of each tract
total_pop <- get_acs(geography = "tract", variables = "B01001_001", year = 2018,
                     state = "VA", county = "540", survey = "acs5") %>%
  select(-variable) %>%
  rename(tot_pop = estimate, tot_pop_moe = moe)

## Pulling from table B01001H "Sex by Age (NON_HISPANIC WHITE ALONE)" 
wt_pop <- get_acs(geography = "tract", variables = "B01001H_001", year = 2018,
                  state = "VA", county = "540", survey = "acs5") %>%
  select(-variable) %>%
  rename(wt_pop = estimate, wt_pop_moe = moe) 

wt_prop <- left_join(total_pop, wt_pop) %>%
  mutate(prop = wt_pop/tot_pop, prop_moe = (moe_prop(wt_pop, tot_pop, wt_pop_moe, tot_pop_moe))*100) %>%
  arrange(desc(prop)) %>%
  mutate(prop = label_percent()(prop)) %>%
  mutate(NAME = substr(NAME, 1, nchar(NAME)-32)) %>%
  mutate(prop_moe = paste("(+/-) ", round(prop_moe, digits = 0), "%", sep = ""))

wt_prop %>%
  select("Census Tract" = NAME, "Percent White" = prop, "Moe" = prop_moe) %>%
  kable() %>%
  kable_styling()

## Proportion of Residents living in Poverty 
## Variables to pull: Total Population, Population in Poverty
pov <- get_acs(geography = "tract", variables = "B17001_002", year = 2018,
               state = "VA", county = "540", survey = "acs5") %>%
  select(-variable) %>%
  rename(pov = estimate, pov_moe = moe)

pov_prop <- left_join(total_pop, pov) %>%
  mutate(pov_prop = pov/tot_pop, pov_prop_moe = (moe_prop(pov, tot_pop, pov_moe, tot_pop_moe))*100) %>%
  arrange(desc(pov_prop)) %>%
  mutate(pov_prop = label_percent()(pov_prop)) %>%
  mutate(NAME = substr(NAME, 1, nchar(NAME)-32)) %>%
  mutate(pov_prop_moe = paste("(+/-) ", round(pov_prop_moe, digits = 0), "%", sep = ""))

pov_prop %>%
  select("Census Tract" = NAME, "Percent in Poverty" = pov_prop, "Moe" = pov_prop_moe) %>%
  kable() %>%
  kable_styling()


# ..........................................................................................
# 7. Calculating referrals by tract groups ----

# par down CPS data to fips_code data, limit observations to one per child
ref2 <- distinct(ref, cid, .keep_all = TRUE)

ref_tract <- ref2 %>%
  select(cid, race2, fips_ref, tract_ref, geoid_ref, tract_name, refnum) %>%
  filter(!is.na(fips_ref)) %>%
  filter(fips_ref == 540) %>%
  mutate(fips_ref = as.factor(fips_ref),
         race2 = fct_recode(race2, "Multiracial" = "Multi-Race")) 

# rsl_tract_wt <- rsl_tract %>%
#   filter(race2 == "Black")

# Calculate proportion of referals white
#204/850

# Calculate proportion of referals white
#532/850

ref_tract <- ref_tract %>%
  mutate(perc_wt = fct_collapse(ref_tract$tract_name, 
                                ">75" = c("Census Tract 9", "Census Tract 10", "Census Tract 4.02", "Census Tract 7"),
                                "50-75" = c("Census Tract 3.02", "Census Tract 5.02", "Census Tract 2.01", 
                                            "Census Tract 2.02", "Census Tract 6", "Census Tract 8"),
                                "<50" = c("Census Tract 4.01", "Census Tract 5.01")),
         perc_pov = fct_collapse(ref_tract$tract_name,
                                 ">20" = c("Census Tract 2.02", "Census Tract 6", "Census Tract 4.01", 
                                           "Census Tract 2.01", "Census Tract 5.01"),
                                 "10-20" = c("Census Tract 7", "Census Tract 10", "Census Tract 8", 
                                             "Census Tract 4.02"),
                                 "<10" = c("Census Tract 9", "Census Tract 3.02", "Census Tract 5.02"))) 

## create data table of referrals by perc_wt categories
by_wt <- ref_tract %>%
  select(cid, race2, perc_wt) %>%
  count(perc_wt, race2) %>%
  group_by(perc_wt) %>%
  mutate(pop = sum(n)) %>%
  rename(ref = n, race = race2) %>%
  mutate(refprop = ref/pop) %>%
  select(-pop)

## create data table of referrals by perc_pov categories
by_pov <- ref_tract %>%
  select(cid, race2, perc_pov) %>%
  count(perc_pov, race2) %>%
  group_by(perc_pov) %>%
  mutate(pop = sum(n)) %>%
  rename(ref = n, race = race2) %>%
  mutate(refprop = ref/pop) %>%
  select(-pop)

save.image("data2020/cville_acs.Rdata")
# load("data2020/cville_acs.Rdata")


# ..........................................................................................
# 8. Calculating Tract level Child pop by race ----
# For later disproportionality

state <- "VA"
county <- "540"
year <- 2018

## Pulling from table B01001 "Sex by Age" 
## to find the total number of children
variables <- c(maleto5 = 'B01001_003',
               male5to9 = 'B01001_004',
               male10to14 = 'B01001_005',
               male15to17 = 'B01001_006',
               
               femaleto5 = 'B01001_027',
               female5to9 = 'B01001_028',
               female10to14 = 'B01001_029',
               female15to17 = 'B01001_030'
)

pop_tract <- get_acs(geography = "tract", variables = variables, year = year,
                     state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop = sum(estimate),
            pop_moe = moe_sum(moe, estimate, na.rm = TRUE))

## Pulling from table B01001B "Sex by Age (BLACK or AFRICAN AMERICAN ALONE)" 
variablesB <- c(maleto5 = 'B01001B_003',
                male5to9 = 'B01001B_004',
                male10to14 = 'B01001B_005',
                male15to17 = 'B01001B_006',
                
                femaleto5 = 'B01001B_018',
                female5to9 = 'B01001B_019',
                female10to14 = 'B01001B_020',
                female15to17 = 'B01001B_021'
)

pop_tract_B <- get_acs(geography = "tract", variables = variablesB, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_B = sum(estimate),
            pop_moe_B = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001C "Sex by Age (American Indian and Alaska Native Alone)" 
variablesC <- c(maleto5 = 'B01001C_003',
                male5to9 = 'B01001C_004',
                male10to14 = 'B01001C_005',
                male15to17 = 'B01001C_006',
                
                femaleto5 = 'B01001C_018',
                female5to9 = 'B01001C_019',
                female10to14 = 'B01001C_020',
                female15to17 = 'B01001C_021'
)

pop_tract_C <- get_acs(geography = "tract", variables = variablesC, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_C = sum(estimate),
            pop_moe_C = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001D "Sex by Age (Asian Alone)" 
variablesD <- c(maleto5 = 'B01001D_003',
                male5to9 = 'B01001D_004',
                male10to14 = 'B01001D_005',
                male15to17 = 'B01001D_006',
                
                femaleto5 = 'B01001D_018',
                female5to9 = 'B01001D_019',
                female10to14 = 'B01001D_020',
                female15to17 = 'B01001D_021'
)

pop_tract_D <- get_acs(geography = "tract", variables = variablesD, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_D = sum(estimate),
            pop_moe_D = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001E "Sex by Age (Native Hawaiian and Pacific Islander Alone)" 
variablesE <- c(maleto5 = 'B01001E_003',
                male5to9 = 'B01001E_004',
                male10to14 = 'B01001E_005',
                male15to17 = 'B01001E_006',
                
                femaleto5 = 'B01001E_018',
                female5to9 = 'B01001E_019',
                female10to14 = 'B01001E_020',
                female15to17 = 'B01001E_021'
)

pop_tract_E <- get_acs(geography = "tract", variables = variablesE, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_E = sum(estimate),
            pop_moe_E = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001F "Sex by Age (Some Other Race Alone)" 
variablesF <- c(maleto5 = 'B01001F_003',
                male5to9 = 'B01001F_004',
                male10to14 = 'B01001F_005',
                male15to17 = 'B01001F_006',
                
                femaleto5 = 'B01001F_018',
                female5to9 = 'B01001F_019',
                female10to14 = 'B01001F_020',
                female15to17 = 'B01001F_021'
)

pop_tract_F <- get_acs(geography = "tract", variables = variablesF, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_F = sum(estimate),
            pop_moe_F = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001G "Sex by Age (Two or more races)" 
variablesG <- c(maleto5 = 'B01001G_003',
                male5to9 = 'B01001G_004',
                male10to14 = 'B01001G_005',
                male15to17 = 'B01001G_006',
                
                femaleto5 = 'B01001G_018',
                female5to9 = 'B01001G_019',
                female10to14 = 'B01001G_020',
                female15to17 = 'B01001G_021'
)

pop_tract_G <- get_acs(geography = "tract", variables = variablesG, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_G = sum(estimate),
            pop_moe_G = moe_sum(moe, estimate, na.rm = TRUE))


## Pulling from table B01001H "Sex by Age (WHITE ALONE, NOT HISPANIC)" 
variablesH <- c(maleto5 = 'B01001H_003',
                male5to9 = 'B01001H_004',
                male10to14 = 'B01001H_005',
                male15to17 = 'B01001H_006',
                
                femaleto5 = 'B01001H_018',
                female5to9 = 'B01001H_019',
                female10to14 = 'B01001H_020',
                female15to17 = 'B01001H_021'
)

pop_tract_H <- get_acs(geography = "tract", variables = variablesH, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_H = sum(estimate),
            pop_moe_H = moe_sum(moe, estimate, na.rm = TRUE))

## Pulling from table B01001I "Sex by Age (HISPANIC)" 
variablesI <- c(maleto5 = 'B01001I_003',
                male5to9 = 'B01001I_004',
                male10to14 = 'B01001I_005',
                male15to17 = 'B01001I_006',
                
                femaleto5 = 'B01001I_018',
                female5to9 = 'B01001I_019',
                female10to14 = 'B01001I_020',
                female15to17 = 'B01001I_021'
)

pop_tract_I <- get_acs(geography = "tract", variables = variablesI, year = year,
                       state = state, county = county, survey = "acs5") %>%
  select(-variable) %>%
  group_by(GEOID, NAME) %>%
  summarize(pop_I = sum(estimate),
            pop_moe_I = moe_sum(moe, estimate, na.rm = TRUE))

# Combine these 
tract_pop <- pop_tract %>% 
  left_join(pop_tract_B) %>% 
  left_join(pop_tract_C) %>% 
  left_join(pop_tract_D) %>% 
  left_join(pop_tract_E) %>% 
  left_join(pop_tract_F) %>% 
  left_join(pop_tract_G) %>% 
  left_join(pop_tract_H) %>% 
  left_join(pop_tract_I) %>% 
  ungroup()

# Generate child pop estimates by race, add tract characteristic groups
tract_pop1 <- tract_pop %>% 
  rename(Black = pop_B, AI_AN = pop_C, Asian = pop_D, NH_PI = pop_E, 
         Other = pop_F, Multiracial = pop_G, 
         White = pop_H, Hispanic = pop_I, total = pop) %>% 
  mutate(tract_name = substr(NAME, 1, nchar(NAME)-32),
         Remaining = Other + AI_AN + Asian + NH_PI + Hispanic) %>% 
  select(tract_name, Black, Multiracial, White, Remaining, total) %>% 
  mutate(perc_wt = fct_collapse(tract_name, 
                                ">75" = c("Census Tract 9", "Census Tract 10", "Census Tract 4.02", "Census Tract 7"),
                                "50-75" = c("Census Tract 3.02", "Census Tract 5.02", "Census Tract 2.01", 
                                            "Census Tract 2.02", "Census Tract 6", "Census Tract 8"),
                                "<50" = c("Census Tract 4.01", "Census Tract 5.01")),
         perc_pov = fct_collapse(tract_name,
                                 ">20" = c("Census Tract 2.02", "Census Tract 6", "Census Tract 4.01", 
                                           "Census Tract 2.01", "Census Tract 5.01"),
                                 "10-20" = c("Census Tract 7", "Census Tract 10", "Census Tract 8", 
                                             "Census Tract 4.02"),
                                 "<10" = c("Census Tract 9", "Census Tract 3.02", "Census Tract 5.02")))
  
tract_pop1 <- tract_pop1 %>% 
  gather(key = race, value = pop, Black:Remaining)

# do the same thing but for margin of error
tract_pop_moe <- tract_pop %>%
  rename(Black = pop_moe_B, AI_AN = pop_moe_C, Asian = pop_moe_D, 
         NH_PI = pop_moe_E, Other = pop_moe_F, Multiracial = pop_moe_G, 
         White = pop_moe_H, Hispanic = pop_moe_I, total_moe = pop_moe) %>% 
  mutate(tract_name = substr(NAME, 1, nchar(NAME)-32),
         Remaining = sqrt(Other^2 + AI_AN^2 + Asian^2 + NH_PI^2 + Hispanic^2)) %>%
  select(tract_name, Black, Multiracial, White, Remaining, total_moe) %>% 
  mutate(perc_wt = fct_collapse(tract_name, 
                                ">75" = c("Census Tract 9", "Census Tract 10", "Census Tract 4.02", "Census Tract 7"),
                                "50-75" = c("Census Tract 3.02", "Census Tract 5.02", "Census Tract 2.01", 
                                            "Census Tract 2.02", "Census Tract 6", "Census Tract 8"),
                                "<50" = c("Census Tract 4.01", "Census Tract 5.01")),
         perc_pov = fct_collapse(tract_name,
                                 ">20" = c("Census Tract 2.02", "Census Tract 6", "Census Tract 4.01", 
                                           "Census Tract 2.01", "Census Tract 5.01"),
                                 "10-20" = c("Census Tract 7", "Census Tract 10", "Census Tract 8", 
                                             "Census Tract 4.02"),
                                 "<10" = c("Census Tract 9", "Census Tract 3.02", "Census Tract 5.02"))) 

tract_pop_moe <- tract_pop_moe %>% 
  gather(key = race, value = pop_moe, Black:Remaining)

# join population and margin of error data frames
tract_pop_all <- left_join(tract_pop1, tract_pop_moe)

# clean up and save
rm(tract_pop1, tract_pop_moe, pov, pov_prop, ref2, refpop,
   total_pop, tract_pop, county, state, year,
   list=ls(pattern="(^pop_tract)|(^variables)"))

save.image("data2020/cville_acs.Rdata")
# load("data2020/cville_acs.Rdata")


# find proportion of children of a given race estimated to live in the tract
# calculate racial population proportion of census tract

# Calculating RDI values for every race per census tract
# Calculating 'Black,' 'White,' 'Multi-Racial' based on Poverty and % White groupings


