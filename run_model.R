library(rstan)
library(brms)
library(tidyverse)
library(forcats)
library(ggplot2)
library(cmdstanr)

options(mc.cores=parallel::detectCores())

##-----------------------------------------------------------------------------------------------------##
##-----------------------------------------------------------------------------------------------------##
## The goal of this script is to bring in the data for the financial literacy component for the four 
## countries of interest, conduct a small bit of wrangling to prep the dataset, sample to fit the model,
## and output the resulting samples to in .rds format.
## 
##-----------------------------------------------------------------------------------------------------##


#-----------------------------------------------------------------------------------------------------#
# This first section brings in the data, drops some unnecessary columns, recodes columns, and otherwise
# wrangles the dataset into a final cleaned form to be run through the model.
#-----------------------------------------------------------------------------------------------------#

# Bring in the dataset previously created by the 'Wranlge_Data_2015' script
finlit_data <- read.csv("Data/PISA_wrangled/finlit_data.csv")

# Subset to remove an unecessary column prodcued by the read in
finlit_data <- finlit_data[,!names(finlit_data) %in% c('X')]

# Cleaning up the names of the columns
names(finlit_data) <- c('cntstuid', 'lang_spoken', 'escs', 'sex', 'country_by_language', 'native_status', 
                        'item', 'scored_response')

# Dropping rows with missing values. Some missing values are simply questions that particular students
# were not shown or did not reach. By then end of wrangling, dropping missing data removes ~4.5459% 
# of the original data frame read in.
lang_logical <- !is.na(finlit_data['lang_spoken'])
finlit_data_clean <- finlit_data[lang_logical,]

native_logical <- !is.na(finlit_data_clean['native_status'])
finlit_data_clean <- finlit_data_clean[native_logical,]

escs_logical <- !is.na(finlit_data_clean['escs'])
finlit_data_clean <- finlit_data_clean[escs_logical,]

lang_logical_invalid <- !finlit_data_clean['lang_spoken']=='INVALID'
finlit_data_clean <- finlit_data_clean[lang_logical_invalid,]

# A bit of refactoring two columns
finlit_data_clean$lang_spoken <- as.factor(as.character(finlit_data_clean$lang_spoken))
finlit_data_clean$cntstuid <- as.factor(as.character(finlit_data_clean$cntstuid))

# Some minor recoding. I need the 'scored_response' to be recoded as an ordered factor with the three 
# levels. Employing the 'forcats' package here. 
# I have made the choice not to include the 'Not Reached' instances of 'scored_response'. PISA have 
# generally included these and generated estimates of latent variables even for those using just the bq 
# variables. I do not find this to be necessary here.
finlit_data_clean$scored_response <- fct_collapse(finlit_data_clean$scored_response, 
                                                   No_Credit = c("0 - NO CREDIT", "00 - NO CREDIT", "NO CREDIT", 
                                                                 "NO RESPONSE"), 
                                                   Partial_Credit = c("1 - PARTIAL CREDIT", "11 - PARTIAL CREDIT", 
                                                                      "12 - PARTIAL CREDIT"), 
                                                   Full_Credit = c("1 - FULL CREDIT", "11 - FULL CREDIT", 
                                                                   "12 - FULL CREDIT", "13 - FULL CREDIT", 
                                                                   "2 - FULL CREDIT","21 - FULL CREDIT", 
                                                                   "FULL CREDIT"),
                                                   NULL = c("NOT APPLICABLE", "NOT REACHED"))

# Now drop null rows created by the recoding of 'scored_response'
response_logical <- !is.na(finlit_data_clean['scored_response'])
finlit_data_clean <- finlit_data_clean[response_logical,]                 

# Convert the refactored 'scored_response' column to a three-level ordinal factor
finlit_data_clean$scored_response <- factor(as.character(finlit_data_clean$scored_response),
                                            levels = c('No_Credit', 'Partial_Credit', 'Full_Credit'),
                                            ordered = TRUE)

# Clean up memory by dropping frames and vectors no longer needed
rm('finlit_data', 'lang_logical', 'lang_logical_invalid', 'native_logical', 'response_logical', 
   'escs_logical')
   
# First, the data needs to be subset for the following countries: U.S.English, Belgium Dutch, 
# Netherlands Dutch, Poland Polish
logical <- finlit_data_clean$country_by_language == 'UNITED STATES ENGLISH' | finlit_data_clean$country_by_language == 'NETHERLANDS DUTCH' |
	   finlit_data_clean$country_by_language == 'BELGIUM DUTCH' | finlit_data_clean$country_by_language == 'POLAND POLISH'
data_subset <- finlit_data_clean[logical, ]
data_subset$cntstuid <- as.factor(as.character(data_subset$cntstuid))
data_subset$lang_spoken <- as.factor(as.character(data_subset$lang_spoken))
data_subset$country_by_language <- as.factor(as.character(data_subset$country_by_language))
rm('logical')
#-----------------------------------------------------------------------------------------------------#


#-----------------------------------------------------------------------------------------------------#
# The following section establishes the model from a stan file, establishes priors, samples, and 
# outputs the final dataset.
#-----------------------------------------------------------------------------------------------------#

# Format the data properly for easy passing through cmdstanr
pisa_model_subset_data <- brms::make_standata(
  formula = pisa_formula_finlit,
  data = data_subset,
  family = brmsfamily("cumulative", "logit"),
  prior = my_priors
)
attr(pisa_model_subset_data, 'class') <- NULL

# Compile C++ code from .stan file
mod <- cmdstan_model('europe_na_model.stan')

# Begin sampling
fit <- mod$sample(
  data = pisa_model_subset_data,
  chains = 4,
  refresh = 10,
  init = .01,
)

# Finally, output the resulting file to a .rds file that can be used later
fit$save_object(file = 'Samples/pisa_model_europe_na.rds')

#-----------------------------------------------------------------------------------------------------#
