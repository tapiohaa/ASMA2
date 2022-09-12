
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 1_link_datasets.R        ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Link datasets to create analysis data.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating data. 
library(ggplot2)          # Plotting data. 

# Inputs:
input_sample = "W:/ASMA2/data/interim/sample_cleaned.rds"
input_postal_codes = "W:/ASMA2/data/interim/postal_codes_cleaned.rds"
input_folk = "W:/ASMA2/data/interim/folk_data_201" # folk_data_201X.csv, X in 1:4
input_pc = "W:/ASMA2/data/interim/visits_201" # visits_201X.csv, X in 1:4
input_dent = "W:/ASMA2/data/interim/dentist_visits_201" # dentist_visits_201X.csv, X in 1:4
input_specialist = "W:/ASMA2/data/interim/specialist_visits_201" # specialist_visits_201X.csv, X in 1:4
input_assist = "W:/ASMA2/data/interim/social_assistance_201" # social_assistance_201X.csv, X in 1:4
input_municipalities = "W:/ASMA2/data/raw/municipalities_2013.csv"

# Outputs:
output_data = "W:/ASMA2/data/processed/analysis_data.rds"
output_data_aggr = "W:/ASMA2/data/processed/analysis_data_aggr.rds"
output_data_synth = "W:/ASMA2/data/processed/analysis_data_synth.rds"

###
###

# Read the data on municipalities and postal codes:
sample = readRDS(input_sample)
post_codes = readRDS(input_postal_codes)

# Study years:
years = c(1:4)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) The sample individuals. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Extract the sample:

ids = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, 
                           select=c('id', 'year', 'municipality', 'age'))
  
  # Keep ID-year observations where the person 
  # 1) lives in a sample municipality AND
  # 2) is aged 25 or more at the end of that year:
  folk = folk[municipality %in% sample[in_sample %in% c(1,2), unique(no)] &
                age >= 25]
  
  return(folk)
  
})
ids = do.call(rbind.data.frame, ids)


# Our sample: We take individuals who 
# 1) were 25 or more at the end of 2011 AND
# 2) lived at the end of years 2011-2014 in a (same) sample municipality.
ids = ids[, .N, by=c('id', 'municipality')][N==4][, N := NULL]

# Sample sizes: 380k in Helsinki and 1.35 million in comparison areas.
ids[, hki := as.integer(municipality == 91)]
print(ids[, .N, by='hki'])


# Merge postal codes to the 'ids'. 
ids = merge(ids, post_codes, by='id', all.x=TRUE)

# The postal code area is missing for 34% living in Helsinki and for 22% living
# in comparison areas:
colMeans(is.na(ids[hki==1]))
colMeans(is.na(ids[hki==0]))

# If the postal code area is missing, use the municipality of residence:
ids[is.na(postal_code), postal_code := -1 * municipality]

# Summary statistics about the postal code areas:
summary(ids[, .N, by='postal_code'])
summary(ids[postal_code > 0, .N, by='postal_code'])


# Create a person-month panel covering 2011-2014:
df = CJ(id=ids$id, year=c(2011:2014), month=c(1:12))
df = merge(df, ids, by='id', all.x = TRUE)

# Link to the 'df' the data which municipalities are included in the sample:
sample = dcast(sample, no ~ paste0('drop_', outcome), value.var = 'in_sample')
df = merge(df, sample, by.x='municipality', by.y='no', all.x = TRUE)
  
# Reallocate some memory:
rm(post_codes, sample)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) GP visits in public primary care. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read GP visits in public primary care:

pc_visits = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data, and aggregate at the id-year-month level:
  source = paste(input_pc, year, ".csv", sep="")
  df = unique(data.table::fread(source))
  df = df[id %in% ids[, unique(id)] & year==yr, 
          .(contacts = .N), by=c('id','year','month')]
  
})

pc_visits = do.call(rbind.data.frame, pc_visits)
pc_visits = pc_visits[, mget(colnames(pc_visits))]

pc_visits[, gp_visits := 12 * contacts]
pc_visits[, contacts := NULL]

# Merge gp visits to the panel and impute zero if no visits are observed.
df = merge(df, pc_visits, by=c('id', 'year', 'month'), all.x = TRUE)
df[is.na(gp_visits), gp_visits := 0]

rm(pc_visits)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Dentist visits in public primary care. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read dentist visits in public primary care:

dentist_visits = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data, and aggregate at the id-year-month level:
  source = paste(input_dent, year, ".csv", sep="")
  df = unique(data.table::fread(source))
  df = df[id %in% ids[, unique(id)] & year==yr, 
          .(contacts = .N), by=c('id','year','month')]
  
})

dentist_visits = do.call(rbind.data.frame, dentist_visits)
dentist_visits = dentist_visits[, mget(colnames(dentist_visits))]

dentist_visits[, dentist_visits := 12 * contacts]
dentist_visits[, contacts := NULL]

# Merge dentist visits to the panel and impute zero if no visits are observed.
df = merge(df, dentist_visits, by=c('id', 'year', 'month'), all.x = TRUE)
df[is.na(dentist_visits), dentist_visits := 0]

rm(dentist_visits)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) ED visits and specialist consultations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data on ED visits and specialist consultations:

hilmo = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read:
  source = paste(input_specialist, year, ".csv", sep="")
  df = data.table::fread(
    source, select=c('id', 'service_area', 'year', 'month', 'day')
  )
  
  # Extract unique emergency department visits, appointments, and 
  # consultation visits:
  df = unique(df[service_area %in% c(91, 92, 94)])
  df[, booking := as.integer(service_area %in% c(92, 94))]
  df[, service_area := NULL]
  df = unique(df)
  
  # Aggregate at the id-year-month-booking level:
  df = df[id %in% ids[, unique(id)] & year==yr, 
          .(contacts = .N), by=c('id','year','month','booking')]

})

hilmo = do.call(rbind.data.frame, hilmo)
hilmo = hilmo[, mget(colnames(hilmo))]
hilmo[, contacts := 12 * contacts]

# Pivot wider:
hilmo = dcast(hilmo, id + year + month ~ paste0('booking_', booking),
              value.var = 'contacts')
setnames(hilmo, old=c('booking_0', 'booking_1'), 
         new=c('ed_visits', 'specialist_consultations'))

# Merge contacts to the panel and impute zero if no visits are observed.
df = merge(df, hilmo, by=c('id', 'year', 'month'), all.x = TRUE)
df[is.na(ed_visits), ed_visits := 0]
df[is.na(specialist_consultations), specialist_consultations := 0]

rm(hilmo)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Social assistance use. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data on social assistance use (monthly indicators):

assistance = lapply(setdiff(years, 1), function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read social assistance data:
  source = paste(input_assist, year, ".csv", sep="")
  df = data.table::fread(source, drop='social_assistance_family')
  df = df[id %in% ids[, unique(id)]]
  
  # Pivot longer:
  df = melt(
    df, id.vars = c('id', 'year'),
    measure.vars = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                     'july', 'aug', 'sep', 'oct', 'nov', 'dec'),
    variable.name = 'month', value.name = 'social_assistance',
    variable.factor = FALSE
  )
  
  # To save memory, drop rows where social assistance is not received:
  df = df[social_assistance > 0]
  
  # Currently, social_assistance contains the number of social
  # assistance applicants in family who received social assistance (may be >1). 
  # We mutate these values to dummies whether the family received the benefit.
  df[social_assistance > 1, social_assistance := 1]
  
  # Replace month abbreviations with dates:
  
  months = data.table(month_no = c(1:12),
                      month = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                                'july', 'aug', 'sep', 'oct', 'nov', 'dec'))
  
  df = merge(df, months, by='month', all.x = TRUE)
  df[, month := NULL]
  setnames(df, old='month_no', new='month')
  
  
})

assistance = do.call(rbind.data.frame, assistance)
assistance = assistance[, mget(colnames(assistance))]

# Merge social assistance date to the panel and impute zero to NAs.
df = merge(df, assistance, by=c('id', 'year', 'month'), all.x = TRUE)
df[year %in% c(2012:2014) & is.na(social_assistance), social_assistance := 0]

rm(assistance)


# Add relative time to the data frame:

years = 2011:2014

time = data.table(
  month = rep(c(1:12), times=length(years)),
  year = rep(years, each=12))
time$relative_time = c(1:nrow(time))

df = merge(df, time, by=c('year', 'month'), all.x = TRUE)
df[, relative_time := relative_time - 25]


# Add income deciles to the data frame:

# Read FOLK data:
source_folk = paste(input_folk, '2', ".csv", sep="")
folk = data.table::fread(source_folk, drop=c('year', 'municipality'))
folk = folk[id %in% ids[, unique(id)]]

# Create variable income_decile:
folk[, income_decile := cut(income_eq,
                            breaks = quantile(income_eq, probs = 0:10/10),
                            labels = 0:9, right = FALSE)]

folk[, ':=' (income_eq = NULL, family_id = NULL, age = NULL)]

df = merge(df, folk, by=c('id'), all.x = TRUE)
rm(folk)

# Replace the character ID with a more memory efficient integer:

person.id = unique(ids[, .(id)])
person.id[, person.id := c(1:nrow(person.id))]

# Merge to ids and paste:
df = merge(df, person.id, by='id', all.x = TRUE)
df[, id := NULL]
setnames(df, old='person.id', new='id')
rm(ids, person.id)

# Save:
df[, ':=' (postal_code_hki=NULL, hki=NULL, year=NULL, month=NULL)]
saveRDS(df, output_data, compress = FALSE)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Aggregated data for synth analyses and DD robustness checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Collect indicators for whether a municipality is in a given sample:
sample = unique(df[, .(municipality, drop_gp_visits)])

# Keep the following covariates:
cols = c('municipality', 'gp_visits', 'relative_time', 'income_decile')
df = df[, mget(cols)]


# Aggregate:

# All individuals:
df.all = df[, .(all_contacts = sum(gp_visits), population = .N), 
            by=c('municipality', 'relative_time')]
df.all[, all_contacts := all_contacts / population]

# Bottom 40% and top 40%:
df.bot = df[income_decile %in% c(0:3, 6:9)
            ][, bottom_d := as.integer(income_decile %in% c(0:3))
              ][, .(contacts = sum(gp_visits), population = .N), 
                by=c('municipality', 'relative_time', 'bottom_d')]
df.bot[, ':=' (contacts = contacts / population)]


# Pivot wider:
df.bot = dcast(df.bot, municipality + relative_time ~ bottom_d, 
               value.var = c('contacts', 'population'))

# Compute differences and ratios between the bottom 40% and the top 40%:
df.bot[, ':=' (diff_contacts = contacts_1 - contacts_0,
               ratio_contacts = contacts_1 / contacts_0)]

# Column bind:
DT = merge(df.all, df.bot, by=c('municipality', 'relative_time'), all.x = TRUE)
DT = merge(DT, sample, by='municipality', all.x=TRUE)

# Keep only sample observations:
DT = DT[drop_gp_visits %in% c(1, 2)][, drop_gp_visits := NULL]

# Municipality names:

muni_names = read.csv(input_municipalities, sep=';', 
                      encoding = 'UTF-8', header = FALSE)

colnames(muni_names) = c('municipality', 'muni_name')

DT = merge(DT, muni_names, by='municipality', all.x=TRUE)
setnames(DT, old='population', new='all_population')


# Synth data:
synth = melt(DT, id.vars = c('municipality', 'relative_time', 'muni_name'),
             measure.vars= c('all_contacts', 'diff_contacts', 'ratio_contacts'),
             value.name = 'otc', variable.name = c('otc_type'))
synth[, ':=' (otc_type = gsub('_contacts', '', otc_type))]


# DD data - population:

pop = unique(DT[, mget(c('municipality', 'all_population', 
                         'population_1', 'population_0'))])

pop = melt(pop, id.vars='municipality', 
           measure.vars = c('all_population', 'population_1', 'population_0'),
           value.name = 'population')

pop[variable=='all_population', group := 'all']
pop[variable=='population_1', group := 'bottom_40']
pop[variable=='population_0', group := 'top_40']
pop[, variable := NULL]


# DD data - outcomes:

dd = DT[, mget(c('municipality', 'relative_time', 'all_contacts', 
                 'contacts_1', 'contacts_0'))]

dd = melt(dd, id.vars = c('municipality', 'relative_time'), 
          measure.vars = c('all_contacts', 'contacts_1', 'contacts_0'),
          value.name = 'otc')

dd[variable=='all_contacts', group := 'all']
dd[variable=='contacts_1', group := 'bottom_40']
dd[variable=='contacts_0', group := 'top_40']
dd[, variable := NULL]

dd = merge(dd, pop, by=c('municipality', 'group'), all.x = TRUE)


# Add population size to the synth data:
synth = merge(synth, 
              unique(dd[group=='all', .(municipality, population)]),
              by='municipality', all.x = TRUE)


# Synth: Construct demeaned outcomes:

synth.means.24 = synth[relative_time < 0, .(pre_mean_24 = mean(otc)), 
                       by=c('municipality', 'otc_type')]
synth.means.12 = synth[relative_time %in% c(-12:-1), .(pre_mean = mean(otc)), 
                       by=c('municipality', 'otc_type')]

synth = merge(synth, synth.means.24, by=c('municipality', 'otc_type'), 
           all.x = TRUE)
synth = merge(synth, synth.means.12, by=c('municipality', 'otc_type'), 
           all.x = TRUE)

synth[, ':=' (otc_demean = otc - pre_mean_24, pre_mean_24 = NULL)]
setnames(synth, old='otc', new='otc_raw')


# Pivot longer:
synth = melt(
  synth, id.vars = c('municipality', 'muni_name', 'population', 
                     'relative_time', 'otc_type', 'pre_mean'),
  measure.vars = c('otc_raw', 'otc_demean'),
  value.name = 'otc', variable.name = 'value_type'
)
synth[, value_type := gsub('otc_', '', value_type)]

saveRDS(synth, output_data_synth)


# DD: Construct detrended outcomes:

trends = dd[relative_time < 0, 
            c(reg = as.list(coef(lm(otc ~ relative_time)))), 
              by=c('municipality', 'group')]

dd = merge(dd, trends, by=c('municipality', 'group'), all.x=TRUE)

dd[, otc_detrend := otc - 
     (get('reg.(Intercept)') + relative_time * get('reg.relative_time'))]

dd[, ':=' (hki = as.integer(municipality==91),
           post = as.integer(relative_time >= 0),
           `reg.relative_time` = NULL,
           `reg.(Intercept)` = NULL)]

dd.means.12 = dd[relative_time %in% c(-12:-1), .(pre_mean = mean(otc)), 
                 by=c('municipality', 'group')]

dd = merge(dd, dd.means.12, by=c('municipality', 'group'), all.x = TRUE)
setnames(dd, old='otc', new='otc_raw')

# pivot longer:
dd = melt(
  dd, id.vars = c('municipality', 'relative_time', 'population', 
                  'group', 'hki', 'post', 'pre_mean'),
  measure.vars = c('otc_raw', 'otc_detrend'),
  value.name = 'otc', variable.name = 'value_type'
)
dd[, value_type := gsub('otc_', '', value_type)]

saveRDS(dd, output_data_aggr)

# End.
