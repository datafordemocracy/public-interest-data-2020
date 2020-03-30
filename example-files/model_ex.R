# Public Interest Data Lab
# Example of visualization and modeling of an outcome
# Created: March 2019
# Updated: March 2020

# Additional resources/references
#   R for Data Science, Model Basics: https://r4ds.had.co.nz/model-basics.html
#   Linear Modeling review for another policy lab: https://datafordemocracy.github.io/lppp5540_sld/linearmodel.html
#   Linear Modeling workshop by Clay Ford (UVA StatLab): https://uvastatlab.github.io/phdplus2020/linear_modeling_in_r.html

# ..........................................................................................

# load libraries ----
library(tidyverse)
library(lubridate) # for date transformations
library(RColorBrewer) # for color palettes
library(car) # for Anova function
library(MASS) # for ordered logit, negative binomial models


# read in file ----
# read data from encrypted volume 
#   (this is the primary dataset we worked with last year)
setwd("/Volumes/PIDL20")
dss <- readRDS("data2020/dss.rds")
# created in data_clean.R (2019)


# EXAMPLE ANALYSIS: removals from home
# ..........................................................................................

# Visualization ----
# Create a data frame to generate a graph
#    subset data: children in foster care; only whte, black, multirace; 
#    only removal variables and race
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "MultiRace")) %>% 
  dplyr::select(race, remove_physabuse:remove_house)

# drop unused race levels, and recode reasons
dss_remove$race <- droplevels(dss_remove$race)
var <- c(names(dss)[100:114])
dss_remove <- dss_remove %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))

# reshape to long
remove <- dss_remove %>% 
  gather(reason, count, -race) %>% 
  group_by(race, reason) %>% 
  summarize(tot = sum(count))

# add total number of children in each racial category to data frame
remove_race <- dss_remove %>% count(race)
remove <- left_join(remove, remove_race, by = "race")
remove <- remove %>% 
  mutate(prop = tot/n)

# make reason a factor and order by prop
remove <- remove %>% 
  mutate(reason = factor(reason),
         reason = fct_reorder(reason, prop))

# Generate a figure of reasons
ggplot(remove, aes(x = reason, y = prop, fill = race)) + 
  geom_col(width=0.9, position=position_dodge(.75)) +
  scale_fill_manual(values = brewer.pal(9, "Oranges")[c(8,6,4)]) +
  coord_flip() +
  labs(title = "Reasons for Removal from Home", subtite = "By Race", 
       y = "Proportion of Children Removed for Reason", x = "Reason for Removal")
# needs work
#   get rid of unused reasons, 
#   rename reasons to remove "remove",
#   rename legend (e.g., Race of Child)
#   add p-values for differences

# get p-values for differences to include in graph
chisq.test(dss_remove$remove_neglect, dss_remove$race, simulate.p.value = TRUE)
# because many of the cell counts are quite small, 
#   the distributional approximations can be poor; 
#   simulating the p-values is one way of accounting for this 
#   (these p-values will generally be larger);
#   another approach is to use a Fisher's exact test
fisher.test(dss_remove$remove_neglect, dss_remove$race)
# I don't do it here, but once I have the p-values, I could add them 
#   as text to the figure


# ..........................................................................................

# Generate a model for a reason ----
# re-create subsetted data frame for analysis (keep all vars), recode age
dss_remove <- dss %>% 
  filter(fc_enter == "Yes") %>% 
  filter(race %in% c("White", "Black", "Multi-Race")) %>% 
  mutate(age_rem = interval(start = dob, end = remove_currdate) /
           duration(num = 1, units = "years"))

# 1. most basic model: race, age, gender
house1 <- glm(remove_house ~ race + age_rem + gender, 
              data = dss_remove, family = "binomial")
# model coefficients and summary stats
summary(house1)
# anova/F-test for variables -- useful to see joint effect of factor
# with Anova (from car), I can make an explict call to the type 3 test 
#   (which is what I want)
Anova(house1, type = 3) 

# 2. adding polynomial for age
house2 <- glm(remove_house ~ race + poly(age_rem, 2) + gender, 
              data = dss_remove, family = "binomial")
summary(house2)
Anova(house2, type = 3)

# is model with age-squared "better"?
# AIC comparison
AIC(house1)
AIC(house2)
# lower values are better -- so, house 1 (without age-squared) fits better

# anova test of nested models: does fuller model improve fit over reduced model?
anova(house2, house1, test = "Chisq")
# no, the null is that the fit equally well, and we have no 
#   evidence against the null; the reduced/simpler model is preferred

# 3. adding family structure (recode to single/two parents)
dss_remove <- dss_remove %>% 
  mutate(care_structure2 = fct_recode(care_structure,
                                      "Single" = "Single mom",
                                      "Single" = "Single dad",
                                      "Dual" = "Married couple",
                                      "Dual" = "Unmarried couple"))

house3 <- glm(remove_house ~ race + age_rem + gender + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house3)
Anova(house3, type = 3)

# 4. adding numref
house4 <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref, 
              data = dss_remove, family = "binomial")
summary(house4)
Anova(house4, type = 3)

# 5. adding ever_find
house5 <- glm(remove_house ~ race + age_rem + gender + care_structure2 + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(house5)
Anova(house5, type = 3)

# 6. adding race/care_structure2 interaction
house6 <- glm(remove_house ~ race*care_structure2 + age_rem + gender + numref + ever_find, 
              data = dss_remove, family = "binomial")
summary(house6)
Anova(house6, type = 3)

# 7. adding race/ever_find interaction
house7 <- glm(remove_house ~ race*ever_find + age_rem + gender + numref + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house7)
Anova(house7, type = 3)

# 8. adding race/numref interaction
house8 <- glm(remove_house ~ race*numref + age_rem + gender + ever_find + care_structure2, 
              data = dss_remove, family = "binomial")
summary(house8)
Anova(house8, type = 3)


# APPENDIX
# ..........................................................................................

# Other model syntax, briefly ----
# 1. ordinal logit/proportional odds (outcome is an ordered category)
# for more: https://uvastatlab.github.io/2015/10/05/fitting-and-interpreting-a-proportional-odds-model/
find1 <- polr(ever_find ~ race_ethn + age_ref1 + gender + numref, 
              data = dss, Hess = TRUE)
summary(find1)
Anova(find1, type = 3)
# polr doesn't generate p-values for coefficients; 
# but you can create them in if you really want to
find1.coef <- data.frame(coef(summary(find1)))
find1.coef$pval = round((pnorm(abs(find1.coef$t.value), lower.tail = FALSE) * 2),2)
find1.coef


# 2. count/negative binomial (outcome is number of times an event occurs)
# for more: https://uvastatlab.github.io/2016/05/05/getting-started-with-negative-binomial-regression-modeling/
# create number of allegations
var <- c(names(dss)[9:13])
dss_allege <- dss %>% 
  mutate_at(var, as.numeric) %>% 
  mutate_at(var, list(~ dplyr::recode(., `1` = 0L, `2` = 1L)))
dss_allege <- dss_allege %>% 
  mutate(allege_sum = reduce(dplyr::select(., c(var)), `+`))
table(dss_allege$race, dss_allege$allege_sum)

allege1 <- glm.nb(allege_sum ~ race_ethn + age_ref1 + gender + numref,
                  data = dss_allege)
summary(allege1)
Anova(allege1, type = 3)


# 3. duration/survival (outcome is how long until an event occurs)
library(survival)
# binary indicator for whether the child is still in the system
dss_remove <- dss_remove %>% 
  mutate(fc_exit = if_else(is.na(discharge_date), 0, 1)) # 1 if exit
query_date <- "2018-12-31 12:00:00 UTC" # data retrieval data for clients still in care

dss_remove$duration <- ifelse((dss_remove$fc_exit==0), 
                              difftime(query_date, dss_remove$remove_currdate, units="weeks"), 
                              difftime(dss_remove$discharge_date, dss_remove$remove_currdate, units="weeks"))

# Estimated survival function: race only
S <- Surv(dss_remove$duration, dss_remove$fc_exit)
dur1 <- coxph(S ~ race, data = dss_remove)
summary(dur1)

# race, gender, age
dur2 <- coxph(S ~ race + gender + age_rem, data=dss_remove)  
summary(dur2)  

# Plot of "survival" function
plot(survfit(dur1), xlab="Weeks", ylab="Cases Active")
race_treat <- with(dss_remove,
                   data.frame(
                     race = c("White", "Black", "MultiRace"),
                     gender = c("Male", "Male", "Male"),
                     age_rem = c(7,7,7))
)

plot(survfit(dur1, newdata = race_treat), 
     xlab="Weeks", ylab="Cases Active",
     col = c("lightblue", "blue", "darkblue")) 
