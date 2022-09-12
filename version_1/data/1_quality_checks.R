
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 1_quality_checks.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki         ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Assess the data quality, and illustrate the sample.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating data. 
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 
library(sf)               # simple features.

# Inputs:
input_sample = "W:/ASMA2/data/interim/sample.rds"
input_postal_codes = "W:/ASMA2/data/interim/postal_codes.csv"
input_postal_codes_15 = "W:/ASMA2/data/raw/postal_codes_2015.csv"
input_pc = "W:/ASMA2/data/interim/visits_201" # visits_201X.csv, X in 1:4
input_folk = "W:/ASMA2/data/interim/folk_data_201" # folk_data_201X.csv, X in 1:4
input_dent = "W:/ASMA2/data/interim/dentist_visits_201" # dentist_visits_201X.csv, X in 1:4
input_specialist = "W:/ASMA2/data/interim/specialist_visits_201" # specialist_visits_201X.csv, X in 1:4
input_assist = "W:/ASMA2/data/interim/social_assistance_201" # social_assistance_201X.csv, X in 1:4
functions = "W:/ASMA2/data/1_shared_functions.R"

# Outputs:
output_postal_codes = "W:/ASMA2/data/interim/postal_codes_cleaned.rds"
output_plot_gp = "W:/ASMA2/analysis/figures/trend_plot_gp.pdf"
output_plot_ed = "W:/ASMA2/analysis/figures/trend_plot_ed.pdf"
output_plot_spes = "W:/ASMA2/analysis/figures/trend_plot_spes.pdf"
output_plot_dent = "W:/ASMA2/analysis/figures/trend_plot_dentist.pdf"
output_plot_assist = "W:/ASMA2/analysis/figures/trend_plot_assist.pdf"
output_plot_map = "W:/ASMA2/analysis/figures/sample_plot_map.pdf"
output_table_summary = "W:/ASMA2/analysis/tables/summary_table"
output_sample = "W:/ASMA2/data/interim/sample_cleaned.rds"


###
###

# Read the data on municipalities:
sample = readRDS(input_sample)

# Study years:
years = c(1:4)

# Read shared functions
source(functions) # save.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Postal codes. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Read the ID-postal code observations:
df = as.data.table(read.csv(input_postal_codes))

# Read the postal code areas of 2015:
postal_codes = 
  data.table::fread(input_postal_codes_15, encoding = 'UTF-8', 
                    select = c('postal_code', 'municipality'))

# Merge the 2015 postal codes to the ID-postal code data, and drop rows where
# the postal code does not match any of the 2015 postal codes:

df = merge(df, postal_codes, by='postal_code', all.x = TRUE)
100 * colMeans(is.na(df))
df = df[!is.na(municipality)][, postal_code_hki := as.integer(municipality==91)]
df[, municipality := NULL]


# 82% of the observed individuals have only one postal code observed in a
# 4-year period. We will later restrict to individuals who are observed to be 
# residing in a specific sample municipality at the end of year in 2011-2014.
# Consistently, we only include those persons with one observed postal code.

count = df[, .N, by='id']
100 * nrow(count[N==1]) / nrow(count)
df = merge(df, count, by='id', all.x = TRUE)
df = df[N==1][, N := NULL]

saveRDS(df, output_postal_codes)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Create primary care data for quality checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read GP visits in public primary care:

pc_visits = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data:
  source_pc = paste(input_pc, year, ".csv", sep="")
  visits = data.table::fread(source_pc)
  visits = unique(visits)
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'visits':
  visits = merge(visits, folk, by='id', all.x=TRUE)
  visits = visits[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to visits is done")
  
  # Aggregate at the municipality-year-month level:
  muni = visits[, .(visits = .N), by=c('municipality','year','month')]
  
  # Aggregate FOLK data to population stats:
  folk = folk[, .(population = .N), by='municipality'][, year := yr]
  
  # Merge 'folk' to 'muni':
  muni = merge(muni, folk, by=c('municipality','year'), all=TRUE)
  
  rm(visits)
  
  return(muni)
  
})
pc_visits = do.call(rbind.data.frame, pc_visits)
pc_visits = pc_visits[, mget(colnames(pc_visits))]

pc_visits[, ann_visits_per_capita := 12 * visits / population]
pc_visits[, visits := NULL]


# Add to the covariates the mean number of GP visits in 2012:

gp_visits_mean = 
  pc_visits[year==2012, .(gp_visits = 
                            weighted.mean(ann_visits_per_capita, w=population)),
            by='municipality']

sample = 
  merge(sample, gp_visits_mean, by.x = 'no', by.y = 'municipality', all.x=TRUE)


# Extract the data on sample municipalities:
pc_visits = pc_visits[municipality %in% sample[in_sample %in% c(1,2), no]]
setnames(pc_visits, old='ann_visits_per_capita', new='gp_visits')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Create primary care dentist visits for quality checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read dentist visits in public primary care:

dentist_visits = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read AvoHilmo data:
  source_pc = paste(input_dent, year, ".csv", sep="")
  visits = data.table::fread(source_pc)
  visits = unique(visits)
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'visits':
  visits = merge(visits, folk, by='id', all.x=TRUE)
  visits = visits[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to visits is done")
  
  # Aggregate at the municipality-year-month level:
  muni = visits[, .(visits = .N), by=c('municipality','year','month')]
  
  # Aggregate FOLK data to population stats:
  folk = folk[, .(population = .N), by='municipality'][, year := yr]
  
  # Merge 'folk' to 'muni':
  muni = merge(muni, folk, by=c('municipality','year'), all=TRUE)
  
  rm(visits)
  
  return(muni)
  
})
dentist_visits = do.call(rbind.data.frame, dentist_visits)
dentist_visits = dentist_visits[, mget(colnames(dentist_visits))]

dentist_visits[, dentist_visits := 12 * visits / population]
dentist_visits[, ':=' (visits=NULL, population=NULL)]

# Extract the data on sample municipalities:
dentist_visits = 
  dentist_visits[municipality %in% sample[in_sample %in% c(1,2), no]]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Specialist consultations data for quality checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data on specialist consultations:

specialist = lapply(years, function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read:
  source_spesialist = paste(input_specialist, year, ".csv", sep="")
  df = data.table::fread(
    source_spesialist, select=c('id', 'service_area', 'year', 'month', 'day')
  )
  
  # Extract unique emergency department visits, appointments, and 
  # consultation visits:
  df = unique(df[service_area %in% c(91, 92, 94)])
  df[, booking := as.integer(service_area %in% c(92, 94))]
  df[, service_area := NULL]
  df = unique(df)
  
  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'df':
  df = merge(df, folk, by='id', all.x=TRUE)
  df = df[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to visits is done")
  
  # Aggregate at the municipality-year-month-booking level:
  muni = df[, .(visits = .N), by=c('municipality','year','month', 'booking')]
  
  # Aggregate FOLK data to population stats:
  folk = folk[, .(population = .N), by='municipality'][, year := yr]
  
  # Merge 'folk' to 'muni':
  muni = merge(muni, folk, by=c('municipality','year'), all=TRUE)
  
  rm(df)
  
  return(muni)
  
})
specialist = do.call(rbind.data.frame, specialist)
specialist = specialist[, mget(colnames(specialist))]

specialist[, ann_visits_per_capita := 12 * visits / population]
specialist[, visits := NULL]


# Add to the covariates the mean number of contacts in 2012:

specialized_mean = 
  specialist[year==2012, 
             .(contacts = weighted.mean(ann_visits_per_capita, w=population)),
             by=c('municipality', 'booking')]

specialized_mean = 
  dcast(specialized_mean, municipality ~ booking, value.var = 'contacts')

colnames(specialized_mean) = 
  c('municipality', 'ed_visits', 'specialist_appointments')

sample = merge(sample, specialized_mean, 
               by.x = 'no', by.y = 'municipality', all.x=TRUE)


# Extract the data on sample municipalities:
specialist = specialist[municipality %in% sample[in_sample %in% c(1,2), no]]

# Pivot wider:
specialist = dcast(specialist, municipality + year + month + population ~ 
                     booking, value.var = 'ann_visits_per_capita')
setnames(specialist, old=c('0', '1'), 
         new=c('ed_visits', 'specialist_consultations'))
specialist[, population := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Data on social assistance for quality checks. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Read data on social assistance use (monthly indicators):

assistance = lapply(setdiff(years, 1), function(year) {
  
  yr = as.integer(paste("201", year, sep=""))
  print(yr)
  
  # Read social assistance data:
  source_assist = paste(input_assist, year, ".csv", sep="")
  print(source_assist)
  df = data.table::fread(source_assist, drop='social_assistance_family')
  
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
  # assistance applicants who received social assistance (may be >1). Next,
  # we mutate these values to dummies whether the family received the benefit.
  df[social_assistance > 1, social_assistance := 1]
  
  # Replace month abbreviations with dates:
  
  months = data.table(month_no = c(1:12),
                      month = c('jan', 'feb', 'mar', 'apr', 'may', 'june', 
                                'july', 'aug', 'sep', 'oct', 'nov', 'dec'))
  
  df = merge(df, months, by='month', all.x = TRUE)
  df[, month := NULL]
  setnames(df, old='month_no', new='month')
  

  # Read FOLK data:
  source_folk = paste(input_folk, year, ".csv", sep="")
  folk = data.table::fread(source_folk, select=c('id', 'municipality'))
  
  # Merge 'folk' to 'df':
  df = merge(df, folk, by='id', all.x=TRUE)
  df = df[year==yr & !is.na(municipality)][, id := NULL]
  print("Merging municipalities of residence to df is done")
  
  # Aggregate at the municipality-year-month level:
  muni = df[, .(social_assistance = .N), by=c('municipality','year','month')]
  
  # Aggregate FOLK data to population stats:
  folk = folk[, .(population = .N), by='municipality'][, year := yr]
  
  # Merge 'folk' to 'muni':
  muni = merge(muni, folk, by=c('municipality','year'), all=TRUE)
  
  rm(df)
  
  return(muni)
  
})
assistance = do.call(rbind.data.frame, assistance)
assistance = assistance[, mget(colnames(assistance))]
assistance[, ':=' (social_assistance = social_assistance / population,
                   population = NULL)]

# Extract the data on sample municipalities:
assistance = assistance[municipality %in% sample[in_sample %in% c(1,2), no]]
                        

# Merge 'pc_visits', 'specialist', 'dentist_visits', and 'assistance':
df = merge(pc_visits, specialist, by=c('municipality', 'year', 'month'),
           all.x = TRUE, all.y = TRUE)
df = merge(df, dentist_visits, by=c('municipality', 'year', 'month'),
           all.x = TRUE, all.y = TRUE)
df = merge(df, assistance, by=c('municipality', 'year', 'month'),
           all.x = TRUE, all.y = TRUE)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Assess data quality. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# It is a known issue that in early years of AvoHilmo primary care data 
# collection some areas had problems in transferring data from their IT 
# systems to the national register. This results in very low numbers of 
# visits per capita for some periods (note that this figure does not need 
# to be exactly zero as residents may also have used services in other 
# municipalities). Here, we try to discover those municipality-period pairs 
# so that these municipalities can be dropped from analysis data.


# Impute relative time to the data frame:

years = 2011:2014

time = data.table(
  month = rep(c(1:12), times=length(years)),
  year = rep(years, each=12))
time$relative_time = c(1:nrow(time))

df = merge(df, time, by=c('year','month'), all.x = TRUE)


# We will proceed in the following way:
# 	1) For each municipality, compute a distribution of mean contacts
#			by dropping every combination of four consecutive months.
#	2) Take the largest mean.
#	3) Mark an observation "weirdly low" if it is less than X % of the mean.


find.weird.obs = function(data, outcome, threshold) {
  # INPUTS:
  # data: 'data' from above
  # outcome: 'gp_visits', 'ed_visits', 'specialist_consultations', 
  #         'dentist_visits', or 'social_assistance'
  # threshold: mark an observation "weirdly low" if it is less than 
  #         X % of the largest mean, X determined by the threshold.
  # OUTPUT:
  # a list containing a mutated copy of 'data' and a table containing the
  # weird municipality-year observations.
  
  
  # Extract the outcome:
  df = data[, mget(colnames(data))]
  df[, otc := mget(outcome)]
  
  
  # Create combinations of four consecutive months:
  
  min_month = min(df$relative_time)
  months = max(df$relative_time) - 3
  municipalities = unique(df$municipality)
  combinations = lapply(c(min_month:months), function(int) { 
    vec = c(0:3) + int 
  })
  print(combinations[[1]])
  
  
  # Compute the largest mean for every municipality:
  
  means = lapply(municipalities, function(muni) {
    
    avr = lapply(combinations, function(combo) {
      table = df[municipality==muni & !(relative_time %in% combo), 
                 .(avr = mean(otc, na.rm = TRUE))]
    })
    avr = max(unlist(avr))
    
    table = data.table(municipality=muni, avr_largest=avr)
    
  })
  means = do.call(rbind.data.frame, means)

  
  # Merge 'means' to 'df' and find 'weirdly' low values:
  
  df = merge(df, means, by='municipality', all.x=TRUE)
  df[, weird := (otc - avr_largest) / avr_largest
     ][, weird_d := as.integer(weird < -(1-threshold) & month != 7)]
  
  # These municipality-year pairs will be dropped:
  weird_d = unique(df[weird_d == 1, mget(c('municipality', 'year', 'weird_d'))])
  setnames(weird_d, old='weird_d', new='drop')
  weird_d[, outcome := outcome]
  
  return(weird_d)
  
}


weird_gp = find.weird.obs(data=df, outcome = 'gp_visits', threshold = 0.50)
weird_ed = find.weird.obs(data=df, outcome = 'ed_visits', threshold = 0.70)
weird_spes = find.weird.obs(data=df, outcome = 'specialist_consultations',
                            threshold = 0.60)
weird_dent = find.weird.obs(data=df, outcome = 'dentist_visits', 
                            threshold = 0.45)
weird_assist = find.weird.obs(data=df, outcome = 'social_assistance',
                            threshold = 0.60)

weird = rbind(weird_gp, weird_ed, weird_spes, weird_dent, weird_assist)

# Out of the 28 municipalities left after restrictions based on population
# size and copayment policies in mainland Finland, 8 have weird
# municipality-year observations in GP visits and 4 in ED visits, 3
# in specialist consultations, 10 in dentist visits, and 0 in social assistance.
weird[, unique(municipality), by='outcome']


# Pivot 'df' longer:
df = melt(
  df, id.vars = c('year', 'month', 'municipality', 
                  'relative_time', 'population'),
  measure.vars = c('gp_visits', 'ed_visits', 'specialist_consultations', 
                   'dentist_visits', 'social_assistance'),
  variable.name = 'outcome', value.name = 'ann_contacts')

# Merge the drop indicators to 'df':
df = merge(df, weird, by=c('year', 'municipality', 'outcome'), all.x=TRUE)
df[is.na(drop), drop := 0]


# For each municipality, we will plot the evolution in contacts per capita:

plot.problems = function(data, otc) {
  # INPUTS:
  # data: 'data' from above
  # otc: 'gp_visits', 'ed_visits', 'specialist_consultations', 'dentist_visits',
  #     or 'social_assistance'
  # OUTPUT:
  # a list of ggplot objects
  
  
  # Copy the data and store the vector of municipalities:
  df = data[outcome==otc]
  municipalities = sort(unique(df$municipality))
  
  
  plots = lapply(municipalities, function(muni) {
    
    # Initiate a plot:
    
    p = ggplot2::ggplot() +
      scale_x_continuous(breaks = seq(1, length(c(2011:2015)) * 12, by=12),
                         labels = as.character(c(11:15))) + 
      ggtitle(muni) + 
      theme(text = element_text(size=20),     
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill = NA, 
                                        size = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) 
    
    
    # Highlight by yellow background those municipality-year pairs that contain 
    # weird months:
    
    DT = df[municipality == muni & drop == 1 & month %in% c(1, 6, 7, 12), 
            mget(c('year', 'relative_time')) ]
    
    if(nrow(DT) > 0) {
      DT = DT[, .(min = min(relative_time),
                  max = max(relative_time)), by='year']
      
      p = p +
        geom_rect(data = DT, 
                  mapping = aes(xmin=min, xmax=max, ymin=-Inf, ymax=Inf, 
                                alpha=0.5), fill='grey') +
        guides(alpha = "none")
      
    }
    
    # Finally, plot the evolution of GP visits:
    
    p = p +
      geom_line(data = df[municipality == muni], 
                mapping = aes(x=relative_time, y=ann_contacts),
                colour="black")
    
    # Y-axis limits depend on the outcome:
    if(otc=='gp_visits') { p = p + ylim(0, 1.6) }
    if(otc=='ed_visits') { p = p + ylim(0, 0.6) }
    if(otc=='specialist_consultations') { p = p + ylim(0, 0.8) }
    if(otc=='dentist_visits') { p = p + ylim(0, 1.2) }
    if(otc=='social_assistance') { p = p + ylim(0, 0.08) }
    
    return(p)
    
  })
  
  
  names(plots) = municipalities
  return(plots)
  
}

plots.gp = plot.problems(data=df, otc = 'gp_visits')
plots.ed = plot.problems(data=df, otc = 'ed_visits')
plots.spes = plot.problems(data=df, otc = 'specialist_consultations')
plots.dent = plot.problems(data=df, otc = 'dentist_visits')
plots.assist = plot.problems(data=df, otc = 'social_assistance')


# Save:

cairo_pdf(filename = output_plot_gp, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots.gp, ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_ed, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots.ed, ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_spes, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots.spes, ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_dent, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots.dent, ncol=6, byrow=TRUE) )
dev.off()

cairo_pdf(filename = output_plot_assist, width = 15.0, height = 12.5)
print( patchwork::wrap_plots(plots.assist, ncol=6, byrow=TRUE) )
dev.off()


# The sample municipalities differ depending on the outcome:

outcomes = c('gp_visits', 'ed_visits', 'specialist_consultations', 
             'dentist_visits', 'social_assistance')

dfs = lapply(outcomes, function(otc) {
  data = sample[, mget(colnames(sample))]
  data[, outcome := otc]
})
dfs = do.call(rbind.data.frame, dfs)

dfs = merge(
  dfs, unique(weird[, .(municipality, outcome, drop)]), 
  by.x=c('no', 'outcome'), by.y=c('municipality', 'outcome'), 
  all.x = TRUE, all.y=TRUE
)

dfs[drop==1, in_sample := 3]
dfs[, drop := NULL]

dfs[, .N, by=c('outcome', 'in_sample')]
sample = dfs


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Show sample municipalities on a map. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

df = sample[, .(no, outcome, in_sample, geom, population)]
df[in_sample==1, in_sample.f := 'Treated: Helsinki']
df[in_sample==2, in_sample.f := 'Comparisons']
df$in_sample.f = factor(df$in_sample.f, 
                        levels = c('Treated: Helsinki', 'Comparisons'))

# Plot the sample municipalities by outcome on maps:

outcomes = c('gp_visits', 'ed_visits', 'specialist_consultations', 
             'social_assistance')

plots.maps = lapply(outcomes, function(otc) {
  
  if(otc=='gp_visits') {title = 'GP Visits'}
  if(otc=='ed_visits') {title = 'ED Visits'}
  if(otc=='specialist_consultations') {title = 'Specialist Consultations'}
  if(otc=='social_assistance') {title = 'Social Assistance Use'}
    
  df.otc = df[outcome == otc]
  
  p = ggplot() +
    geom_sf(data=df.otc$geom, fill="white") +
    geom_sf(data=df.otc[in_sample %in% c(1, 2)]$geom, 
            aes(fill=df.otc[in_sample %in% c(1, 2)]$in_sample.f)) +
    stat_sf_coordinates(data=df.otc[in_sample==1]$geom, 
                        aes(size=df.otc[in_sample==1]$population), 
                        color='black') + 
    scale_fill_manual(values = c('black', 'grey50'), ) +
    coord_sf(datum = NA) + 
    theme(text = element_text(size=20),   
          panel.background = element_rect(fill = "white", colour = "white"),
          legend.position = "bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ggtitle(title) +
    guides(size='none') +
    labs(fill='Area')
  
})


# Save the plots:

cairo_pdf(filename = output_plot_map, width = 8, height = 18)
print(patchwork::wrap_plots(plots.maps)  + plot_layout(guides='collect') &
        theme(legend.position = 'bottom'))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 8) Summary statistics. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Extract the data:
df = sample[outcome=='gp_visits', 
            mget(setdiff(colnames(sample), c('no', 'outcome', 'name', 'geom')))]

# Tidy columns:
df[, ':=' (medicine_reimbursement_recipients_per_1000_inhabitants =
             100 * medicine_reimbursement_recipients_per_1000_inhabitants / 
             1000)]


# Create a list containing variables that will be shown in a table and their 
# type and label:

variables = list(
  # Healthcare use:
  list(var_name='gp_visits', type='continuous', var_label='Primary care GP visits'),
  list(var_name='ed_visits', type='continuous', var_label='Emergency department visits'),
  list(var_name='specialist_appointments', type='continuous', var_label='Specialist consultations'),
  list(var_name='visits_to_private_physicians_per_inhabitant', type='continuous', var_label='Private doctor visits'),
  list(var_name='medicine_reimbursement_recipients_per_1000_inhabitants', type='percent', var_label='Medicine reimbursements'),
  
  # Sociodemographic outcomes:
  list(var_name='population', type='population', var_label='Population mean'),
  list(var_name='proportion_of_pensioners_of_the_population_percent', type='percent', var_label='Pensioners'),
  list(var_name='students_as_percent_of_total_population_year_2018_preliminary_data', type='percent', var_label='Students'),
  
  # Socioeconomic outcomes:
  list(var_name='employment_rate_percent', type='percent', var_label='Employment rate'),
  list(var_name='share_of_persons_aged_15_or_over_with_tertiary_level_qualifications_percent', type='percent', var_label='Tertiary education'),
  list(var_name='social_assistance_euro_per_capita', type='continuous', var_label='Social assistance (euros)'),
  list(var_name='share_of_household_dwelling_units_living_in_rental_dwellings_percent', type='percent', var_label='Rental households'),
  list(var_name='degree_of_urbanisation_percent', type='percent', var_label='Urbanization rate')
)

# Treatment groups:
groups = list(c(1), c(2), c(3))


# Loop over treatment groups:
table.groups = lapply(groups, function(grp) {
  
  # Extract the right subset of observations:
  DT = df[in_sample %in% grp]
  
  # Sample sizes:
  n = paste('N=', nrow(DT), sep='')
  n = data.table(variable='Variable', value=n)
  
  print(paste('Group', grp))
  print(paste('Observations', nrow(DT)))
  print(colSums(!is.na(DT)))
  
  
  # Loop over variables:
  table.vars = lapply(variables, function(var) {
    
    
    if(var$type == 'continuous') {
      
      value = weighted.mean(DT[, mget(var$var_name)][[1]], 
                            w=DT$population, na.rm=TRUE)
      value = format(round(value, digits=2), nsmall=2)
      
    } else if (var$type == 'percent') {
      
      value = weighted.mean(DT[, mget(var$var_name)][[1]], 
                            w=DT$population, na.rm=TRUE)
      value = paste(format(round(value, digits=1), nsmall=1), '%', sep='') 
    
    } else if(var$type == 'population') {
      
      value = DT[, mean(population)]
      value = as.character(round(value, digits=0))
      
    }
    
    table = data.table(variable=var$var_label, value=value)
  })
  table.vars = do.call(rbind.data.frame, table.vars)
  table.vars = rbind(n, table.vars)
  
})
table.groups = do.call(cbind.data.frame, table.groups)[, c(1:2,4,6)]
colnames(table.groups) = c('Variable', 'Helsinki', 'Comparisons', 'The rest')

# Save:
save.table(table = table.groups, output = output_table_summary,
           label_tex = 'tab:summary_table',
           title_tex = 'Summary Statistics.')


# Save the data on sample municipalities:
saveRDS(sample[, .(no, outcome, in_sample)], output_sample)

# End.
