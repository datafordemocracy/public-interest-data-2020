######################################################################################
# LP 5440: Public Interest Data Lab - Spring 2020
# Week 2 Examples: more descriptive statistics
######################################################################################

# ..........................................................................................

# 1. Load libraries and data ----

# load libraries
library(tidyverse) # ggplot is in the tidyverse
library(lubridate) # new library to work with dates

# set working directory
setwd("/Volumes/PIDL19") 

# load data
load("cps_clean.RData") 
foster <- readRDS("foster_clean.rds")
# files created in cps_clean.R & fc_clean.R

# ..........................................................................................

# 1. Exploring and learning more about R 

# to read about how a function works, put a question mark before the function name
?summary # this is often useful to find argument names and learn how something works


# ..........................................................................................

# 2. Variable classes, mutate, recode, relevel

# we can use the class() function to find out how R is storing the information
# classes: character, numeric, integer, factor, date (POSIX)

# factor classes are used for character strings we use as categories (gender, race, etc.)
# numeric and integer classes are for quantities and amounts
# character strings are generally for non uniform descriptions

class(dss$gender) # gender is already stored as a factor, good
class(dss$dob) # birthdate is stored as a date
class(dss$cid) # client id is stored as an integer! we want it to be a factor

# let's change the class of cid
as.factor(dss$cid)
class(dss$cid)
# that didn't work - why?
# when you're changing variable in R, remember to re-assign it to the variable in the data
dss$cid <- as.factor(dss$cid)
class(dss$cid)
# note that in order to change the 
# there are also functions to coerce for as.numeric, as.character, and more 

class(dss$resp_priority1) # response priority is coded as numeric but is categorical
# let's coerce it to factor class
dss$resp_priority1 <- as.factor(dss$resp_priority1)
# once a variable is stored as a factor, we can use levels() to see the categories
levels(dss$resp_priority1)

# here we might want something more descriptive so that we remember which level is high
# priority - is 1 higher priority than 3 or 3 higher priority than one?
# after checking with the codebook, we find that 3 is higher priority. 
# let's change the categories to reflect that 
dss <- mutate(dss, # mutate the data set
              resp_priority1=fct_recode(resp_priority1, # note that the variable is here 2x
                                        "3 - High"="3", # new name comes first, then old
                                        "2 - Medium"="2", 
                                        "1 - Low"="1"))
# now we can see the categories have been updated
levels(dss$resp_priority1)


# say we wanted to change the order for a visualization
# we use mutate again, but this time with fct_relevel
dss <- mutate(dss, # mutate the data set
              resp_priority1=fct_relevel(resp_priority1, # list in preferred order
                                         "3 - High", "2 - Medium", "1 - Low"))
levels(dss$resp_priority1) # now the order has changed

# we can also use fct_rev() to reverse the order back
dss <- mutate(dss, # mutate the data set
              resp_priority1=fct_rev(resp_priority1))
levels(dss$resp_priority1) # back to the original order

# ..........................................................................................






