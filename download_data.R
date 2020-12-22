##----------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------##
## File Description:
##
## The following script sections are used to download and read in data from a directory containing datasets for the 
## 2015 edition of the PISA dataset, as well as wrangle the financial literacy component dataset for the four 
## countries of interest to the study. 2018 is unavailable at the time of writing for download through the PISA API 
## used by the EdSurvey packages.
##----------------------------------------------------------------------------------------------------------------##
##----------------------------------------------------------------------------------------------------------------##


#----------------------------------------------------------------------------------------------------------------#
# The below block of code is used to create and population a directory with all of the desired data.
#
# For reference on using downloadPISA() function and using edsurvey.data.frame format, please see:
# https://rdrr.io/cran/EdSurvey/man/downloadPISA.html
#----------------------------------------------------------------------------------------------------------------#

# Install and bring in necessary package
install.packages("EdSurvey")
library(EdSurvey)
library(tidyverse)

# Point to project data root directory
data_root <- "Data/"

# Download all available data for the years 2012 and 2015 into the project data root directory
# This will create a directory called 'PISA' within the 'Data' directory. 'PISA' will contain a directory
# for each year with all of the data downloaded for that year
downloadPISA(years = c(2015), database = c("INT"), root = data_root)

#----------------------------------------------------------------------------------------------------------------#


#----------------------------------------------------------------------------------------------------------------#
# The block of code below reads in the desired data for a given year and country of focus from the 
# directory of data created by the downloadPISA function. For 2012, there are three database types, while for all later
# editions of PISA there is only "INT" to read in. Documentation on the three types and their purpose can be 
# found at the information link for readPISA() provided below.
#
# The initial read in produces an edsurvey.data.frame, from which one can 
# extract data frames of particular variables or conduct various forms of survey-based statistical analysis. 
# If reading in more than on country, readPISA produces an edsurvey.data.frame.list . The first read will take 
# quite a while to unpack and properly compile the datasets. For the 2015 data, this could take up to an hour. 
# After the initial read, if 'forceReread' is set to FALSE, reading in data for a particular country will only 
# take a few seconds.
#
# For reference on using readPISA() function and using edsurvey.data.frame format, please see:
# https://rdrr.io/cran/EdSurvey/man/readPISA.html
# 
# Reading in a particular dataset for a country from the downloaded dataset requires the three letter country
# code. A list of codes can be found here:
# https://en.wikipedia.org/wiki/ISO_3166-1#Current_codes
#----------------------------------------------------------------------------------------------------------------#

# Point to downloaded data for 2015
data_2015 <- "Data/PISA/2015"
  
# Create a vector of country codes that points to the countries of interest to this study
finLit_countries <- c("BEL", "NLD", "POL", "USA")

# Read in the data for the four countries of interest
pisa2015 <- readPISA(data_2015, database = "INT", countries = finLit_countries)

# Create a vector of columns for index/test identification variables
index_vars <- c("cnt", "cntschid", "cntstuid", "stratum", "option_fl", "langtest_cog")

# Create a vector of column names for each of the test items across the four subjects
subject_vars <- lapply(X=colnames(pisa2015), 
                       FUN=function(x){grep(value=TRUE, x=x, pattern = '^[cd][msrf].*[0-9][cs]$')})

# Create a vector of column names for each of the background questionnaire reponse variables
# Variables st019(a/b/c)q01t will be used to recode for a generational variable in the next section
bg_vars <- c("st019aq01t", "st019bq01t", "st019cq01t", "st022q01ta", "escs", "st004d01t")

# Extract, reformat, and export the financial literacy subject data
# Begin by finding the variable names for all financial literacy test items
finlit_vars <- grep(value=TRUE, x=colnames(pisa2015_data[[1]]), pattern = '^[cd][f]')

# Create a vector of all variable names needed to construct the financial literacy dataset
finlit_all_vars <- c(index_vars, bg_vars, finlit_vars)

# Initialize a dataframe to fill
finlit_data <- data.frame()

# Iteratively extract the variable for each country studied and bind to the finlit dataframe
for (i in 1:length(pisa2015_data)){
  tmp_data <- pisa2015_data[[i]] %>% select(all_of(finlit_all_vars))
  finlit_data <- rbind(finlit_data, tmp_data)
}


#----------------------------------------------------------------------------------------------------------------#

#----------------------------------------------------------------------------------------------------------------#
# The following code wrangles the read-in data and writes out a cleaned dataset for use in model fitting.
#----------------------------------------------------------------------------------------------------------------#

# Construct a variable for coutry-by-language groups
finlit_data$country_by_language <- paste(finlit_data$cnt, finlit_data$langtest_cog)

# Construct a generational variable for whether the student is an immigrant, first-generation, 
# or second-generation/ up
finlit_data$native_status <- ifelse(finlit_data$st019aq01t == "OTHER COUNTRY", 0,
                                    ifelse(finlit_data$st019bq01t == "OTHER COUNTRY" | finlit_data$st019cq01t == "OTHER COUNTRY", 1,
                                           ifelse(finlit_data$st019aq01t == "NO RESPONSE", NA, 2)))

# Convert the dataframe to long-form for the financial literacy items
finlit_data <- gather(finlit_data, key = "item", value = "scored_response", all_of(finlit_vars))

# Drop rows for items that individual students did not answer
finlit_data <- finlit_data[!is.na(finlit_data$scored_response),]

# Drop rows for students who did not take the financial literacy portion
finlit_data <- finlit_data[finlit_data$option_fl!="NO",]

# Select only the columns needed for the model
final_vars <- c("cntstuid", "st022q01ta", "escs", "st004d01t", "country_by_language", "native_status", "item", "scored_response")

finlit_data <- finlit_data[,final_vars]

# Write out the dataset
write.csv(finlit_data, "Data/PISA_wrangled/finlit_data.csv")

# Remove the dataset from memory
rm("finlit_data")

#----------------------------------------------------------------------------------------------------------------#
