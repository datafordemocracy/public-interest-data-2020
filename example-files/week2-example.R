######################################################################################
# LP 5440: Public Interest Data Lab - Spring 2020
# Week 2 Examples: more descriptive statistics
######################################################################################

# .....................................................................................

# 1. Load libraries and data ----

# load libraries
library(tidyverse) # ggplot is in the tidyverse
library(lubridate) # new library to work with dates

# set working directory
setwd("/Volumes/PIDL20/data2020") 

# load data
rsl <- readRDS("ref_supp_long.RDS")


# ......................................................................................

# 1. Exploring and learning more about R 

# to read about how a function works, put a question mark before the function name
?summary # this is often useful to find argument names and learn how something works
# if you put two question marks before a function name, R will search for it 
??summary

# ..........................................................................................

# 2. Variable classes, mutate, recode, relevel

# we can use the class() function to find out how R is storing the information
# classes: character, numeric, integer, factor, date (POSIX)

# factor classes are used for character strings we use as categories (gender, race, etc.)
# numeric and integer classes are for quantities and amounts
# character strings are generally for non uniform descriptions

class(rsl$gender) # gender is already stored as a factor, good
class(rsl$dob) # birthdate is stored as a date
class(rsl$cid) # client id is stored as an integer! we want it to be a factor

# let's change the class of cid
as.factor(rsl$cid)
class(rsl$cid)
# that didn't work - why?
# when you're changing variable in R, remember to re-assign it to the variable in the data
rsl$cid <- as.factor(rsl$cid)
class(rsl$cid)
# note that in order to change the 
# there are also functions to coerce for as.numeric, as.character, and more 

class(rsl$prior_ref) # response priority is coded as factor
# once a variable is stored as a factor, we can use levels() to see the categories
levels(rsl$prior_ref)

# here we might want something more descriptive so that we remember what the levels mean
# let's change the categories to reflect that 
rsl <- mutate(rsl, # mutate the data set
              prior_ref=fct_recode(prior_ref, # note that the variable is here 2x
                                    "Prior Referral(s)"="Yes", # new name comes first, then old
                                    "No Prior Referral(s)"="No"))
# this has already been done for the data you're using 
# now we can see the categories have been updated
levels(rsl$prior_ref)


# say we wanted to change the order for a visualization
# we use mutate again, but this time with fct_relevel
rsl <- mutate(rsl, # mutate the data set
              prior_ref=fct_relevel(prior_ref, # list in preferred order
                                         "Prior Referral(s)", "No Prior Referral(s)"))
levels(rsl$prior_ref) # now the order has changed

# we can also use fct_rev() to reverse the order back
rsl <- mutate(rsl, # mutate the data set
              prior_ref=fct_rev(prior_ref))
levels(rsl$prior_ref) # back to the original order

# ..........................................................................................

# 3. Investigating NA values

# we can use is.na() to check out missing values
is.na(rsl$cid) # prints out a list
sum(is.na(rsl$cid)) # we can wrap sum around is.na to get a count of NA values

sum(is.na(rsl$resp_priority)) # 1662 NAs
sum(is.na(rsl$reporter)) # 1152 NAs
# can you find any pattern in the missingness?

# ..........................................................................................

# 4. Working with dates in R

# let's check out a date variable
class(rsl$first_vic_contact) # this is stored as Date format
# we want POSIX (better date format)

# let's look at the order of the dates - year, month, day
rsl$first_vic_contact

# we're going to use functions from the lubridate package
rsl$first_vic_contact <- ymd(rsl$first_vic_contact) # year month day

# let's look at the time difference between refer date and first contact
difftime(rsl$first_contact, rsl$ref_dt) # all crazy high
difftime(rsl$first_contact, rsl$ref_dt, units="days") # we need to specify units of time

# let's store this in our data frame
rsl$timetofirstcontact <- difftime(rsl$first_contact, rsl$ref_dt, units="days")

# and look at a summary (we need as.numeric to get R to treat the time diff as numeric)
summary(as.numeric(rsl$timetofirstcontact))

# ..........................................................................................

# now your turn! use these techniques on your subset of variables. 


