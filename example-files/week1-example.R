######################################################################################
# LP 5440: Public Interest Data Lab - Spring 2020
# Week 1 Examples: reading in the data, basic summaries
######################################################################################

# ..........................................................................................

# 1. Load libraries and data ----

# load libraries
library(tidyverse) # tidyverse contains most of what we want to do in R
# libraries contain the functions we use in R 

# set working directory
setwd("/Volumes/PIDL20/data2020") 
# the working directory tells R where to go look for files

# load data
rsl <- readRDS("ref_supp_long.RDS")

# ..........................................................................................

# 2. Look at data structure ----

# first let's check out all the variables names so we know what we're working with
names(rsl)

# now let's look at what's in each variable
str(rsl) # str gives the structure of the data, including variable types
summary(rsl) # summary gives descriptive statistics on each variable

# how many rows and columns are in the data?
nrow(rsl) # nrow gives the number of rows in the data frame
dim(rsl) # dim gives dimensions (rows x columns)

# we use the $ to call a variable (gender) from a data frame (rsl)
rsl$gender # running this line prints out the variable
# we can also call a variable with square brackets
rsl[,"gender"] # ,but this is less common unless you need more than one variable

# if we need multiple variables, we can use the c() function 
rsl[,c("gender","race")]

# to view the whole data frame, click on it in the Environment panel or run 
View(rsl) # R is case sensitive - the capital letter matters!
# only the first 50 columns are shown - click the > to see the next 50

# ..........................................................................................

# 3. Basic tables and summary statistics  ----

# for categorical variables, we can look at tables with counts
table(rsl$gender) # table gives us counts for categorical variables
# pretty evenly split by gender
table(rsl$gender, rsl$race) # we can add more variables to get intersections
# slightly more white girls and black boys 

# for numeric variables, we can use summary() to get summary statistics
summary(rsl$age) # the median at at referral is 7, the min is 0 and the max is 17

# for numeric/continuous variables, we can also look at histogram
hist(rsl$age) # big peak around 0 - what might this mean??

# ..........................................................................................

# 4. Grouped summary statistics  ----

# start with a meaningful question then translate the question into R functions
# do black children and multiracial children have more referrals to CPS?

# start with the data, group by race, and find mean age at referral
rsl %>% group_by(race) %>% summarize(mean(age)) # most are NA! let's remove those
rsl %>% group_by(race) %>% summarize(mean(age, na.rm=TRUE)) # much better

# we can also look at the median or any other measures by adding it after a comma
rsl %>% group_by(race) %>% summarize(mean(age,  na.rm=TRUE), median(age, na.rm=TRUE))

# another useful grouping function is count. since children appear once in the data for each 
# referral, we might want to know how many times each child appears in the data
rsl %>% group_by(cid) %>% count() # group by client id and count 
# here printing to the console is not as helpful - we only see the first 10 children

# let's assign it to an object so we can view and sort it
arb <- rsl %>% group_by(cid) %>% count() # i often use arb as a variable name for temporary
# data frames. others use temp, tmp, check, or df - it's up to you

# what is the higher number of referrals during the time period? 
arb <- arrange(arb, n) # arrange the data frame by number of referrals
head(arb, 10) # first 10 rows - these are the lowest
tail(arb, 10) # last 10 rows - these are the highest

# ..........................................................................................

# now your turn! take these functions and apply them to other variables 
# comment out what you learn about the data as you work

