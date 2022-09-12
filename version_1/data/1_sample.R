
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###               r-script 1_sample.R             ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Clean the raw copayment data, and define the sample.
rm(list=ls())

# Install and load the following packages:

library(data.table)


# Inputs:
input_municipalities = "W:/ASMA2/data/raw/municipalities_2013.csv"
input_copayments = "W:/ASMA2/data/raw/copay_raw.csv"
input_muni_data = "W:/ASMA2/data/raw/municipal_data.rds"

# Outputs:
output_file = "W:/ASMA2/data/interim/sample.rds"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create a data frame for copayments. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Data that contains all municipalities existing in 2013.
# Expand it to cover all months in 2013-2018.

data = as.data.table(
  read.csv(file=input_municipalities, sep = ";", header = F, 
           encoding = "UTF-8")
)
colnames(data) = c('no', 'name')

month = c(1:12)
year = c(2013:2018)

# Expand:
setkey(data, no)
combs = data[, CJ(unique(no), month, year)]
data = data[combs]
setkey(data, NULL)


# Read the raw copayment data and remove empty rows.

df = data.table::fread(
  input_copayments, dec = ',',
  select = c('no', 'date', 'copayment', 'policy', 'annual_pay', 'note')
)
df = df[!is.na(no)]

# Transform the date variable to the Date format:

df = df[, c('day', 'month', 'year') := tstrsplit(date, '\\.')
        ][, date := as.Date(paste(year, month, day, sep='-'))
          ][, ':=' (day = NULL,
                    year = as.integer(year),
                    month = as.integer(month))]

# Merge data and df:
copay = merge(data, df, by=c("no", "year", "month"), all.x = T)


# Right now, we have only copayment changes (time of change by month and year) 
# in the matrix.

# Find rows that contain a date and save row numbers to a vector.
filled_rows = copay[!is.na(date), which = TRUE]


# Fill up empty rows between policy changes.

for (i in 1:length(filled_rows)) {
  
  row_no = filled_rows[i]
  municipality = copay[row_no, no]
  values = unlist(copay[row_no, c('copayment', 'policy', 'annual_pay')])
  
  while(is.na(copay$date[row_no+1]) & copay$no[row_no+1] == municipality) {
    copay[row_no+1, ':=' (copayment = values[1],
                          policy = values[2],
                          annual_pay = values[3])]
    row_no = row_no + 1
    if(row_no == nrow(copay)) {
      break
    }
  }
  
}

copay[, time := as.Date(paste(as.character(year), 
                              as.character(month), '1', sep='-'))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Clean the data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Copayment data were collected by observing websites of health centers, and 
# it contains some uncertainties. These uncertainties are stated in Finnish 
# in variable "note". Remainder of this script takes the uncertainty into 
# account. We exclude some observations and include some of the others by making 
# assumptions. 

# First, some cleaning without dropping or making assumptions:

# Municipalities that were merged with another municipalities:

copay[no %in% c(476, 413) & year >= 2015, policy := NA]
copay[no %in% c(164, 283, 319) & year >= 2016, policy := NA]
copay[no %in% c(442, 174) & year >= 2017, policy := NA]

# Adding a specific policy code (50) to Helsinki (no copayments).
copay[no==91, policy := 50]

# Some policy changes occurred after the beginning of a month.
# We define that such changes took into effect from the beginning of the 
# next full month.
copay[no %in% c(905, 399) & year==2017 & month==3, policy := 3]
copay[no %in% c(105, 205, 290, 578, 697, 765, 777) & year==2016 & month==2, 
      policy := 3]
copay[no %in% c(224, 927, 607) & year==2014 & month==1, copayment := 13.8]
copay[no==607 & year==2015 & month==1, copayment := 14.7]

# Some observations have to be removed (namely set policy = NA) because of 
# too much uncertainty or conflicting information. In addition, complete the 
# observations of Lavia in 2013.
copay[no %in% c(140, 263, 762, 925, 102, 52, 233, 403, 143) & year<=2015, 
      policy := NA]
copay[no==18 & year==2018 & month==1, policy := NA]
copay[no %in% c(686, 778), policy := NA]
copay[no==620 & year==2014 & month==2, policy := NA]
copay[no==413 & year==2013, policy := 3]
copay[no %in% c(604, 922) & year==2015, policy := NA]
copay[no==413 & year==2013, copayment := 13.8]

# There were nine municipality mergers between 2014 and 2018 
# (see file "municipal_mergers"). In eight of these cases, the pre-merger 
# policies were similar. However, in one case the policies differed in 2015. 
# This is a problem because when we construct per-capita statistics, our data 
# is based on municipal boundary divisions of 2018. If the policies were 
# similar, we could just sum up the pre-merger visits made in the two merging 
# municipalities. We exclude the problematic observations.
copay[no %in% c(532, 398) & year == 2015, policy := NA]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Restrictions on the sample. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

 
# The copayment data currently uses the 2013 municipal borders.
# However, we want the 2020 borders. Drop the merged municipality and 
# municipality-year observations where pre-merger policies were not similar.
merged_munies = c(911, 442, 174, 164, 283, 319, 532, 476, 413, 838)
munies = copay[!(no %in% merged_munies)
               ][!(no==398 & year==2015)][!is.na(policy)]


# Exclude those municipalities that are not observed the whole time in 2013-14. 
# Thus, calculate, for each municipality, the number of observations in 2013-14. 
# If it is less than 24, drop the municipality.

remove = munies[year %in% c(2013:2014), .N, by='no'][N < 24, no]
remove
munies = munies[!(no %in% remove)]


# Drop those who made a change in policy between 2013-2014.
# That is, drop those for whom we observe at least two policies in that period.

remove = munies[year %in% c(2013:2014), .N, by=c('no', 'policy')
                ][, .N, by='no'][N > 1, no]
remove
munies = munies[!(no %in% remove)]

# After restrictions on the copayment data, the potential sample municipalities 
# are the following:
munies = munies[year==2013 & month==1, c('no', 'name')]


# Espoo adopted exemptions for some low-income groups in August 2011. For this
# reason, Espoo is excluded
munies = munies[no != 49]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Data on municipal characteristics. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read municipal data.
df = as.data.table(readRDS(input_muni_data))

# The covariates are from the end of 2012. We take municipalities having more
# than 30,000 residents:
df = df[year==2012][, in_sample := as.integer(population > 30000)]

# 36 municipalities have more than 30,000 residents
print(df[, .N, by=in_sample])

# The sample municipalities should satisfy our restrictions on the copayment
# policy (details above):
df = df[!(no %in% munies$no), in_sample := 0]

# 28 municipalities satisfy both the population restrictions and the copayment
# policy restrictions:
print(df[, .N, by=in_sample])

# NOTE: this is not the final sample size. Some municipalities are dropped
# due to quality issues in the primary care data.


# In sample: 1: Helsinki, 2: comparisons, 3: not in sample
df[in_sample==0, in_sample := 3]
df[in_sample==1 & no != 91, in_sample := 2]


# Keep the following covariates:
cols = 
  c('no', 'name', 'in_sample', 'degree_of_urbanisation_percent', 'population',
    'proportion_of_pensioners_of_the_population_percent',  
    'share_of_household_dwelling_units_living_in_rental_dwellings_percent',
    'share_of_persons_aged_15_or_over_with_tertiary_level_qualifications_percent',
    'social_assistance_euro_per_capita', 'employment_rate_percent',
    'students_as_percent_of_total_population_year_2018_preliminary_data',
    'medicine_reimbursement_recipients_per_1000_inhabitants',
    'visits_to_private_physicians_per_inhabitant', 'geom')

df = df[, mget(cols)]

saveRDS(df, output_file)

# End.
