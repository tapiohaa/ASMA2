
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 2_results_ddd.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Helsinki.       ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate the DDD results.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(fixest)           # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.

# Inputs:
functions = "W:/ASMA2/data/1_shared_functions.R"
input_data = "W:/ASMA2/data/processed/analysis_data.rds"

# Outputs:
output_gp = "W:/ASMA2/analysis/tables/table_ddd_gp"
output_dent = "W:/ASMA2/analysis/tables/table_ddd_dent"

###
###

# Read the datasets:
df = readRDS(input_data)

# Read shared functions
source(functions) # save.table() and create.table()

wcb.data.global <- NA


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create functions for estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that returns a DDD analysis dataset. ###

extract.data.ddd = function(
  data, outcome, treat.deciles, control.deciles, has_visits=FALSE, 
  detrended=TRUE, trend.post=1) {
  # INPUTS:
  # data: 'df'
  # outcome: 'gp_visits', 'ed_visits', 'specialist_consultations', 
  #       or 'dentist_visits'
  # treat.deciles: the more affected group, e.g., c(0:3)
  # control.deciles: the less affected group, e.g., c(6:9)
  # has_visits: if TRUE, use an indicator of having any contact in a given month
  # detrended: a boolean on whether the data should be detrended
  # trend.post: the assumption regarding the extrapolation of a linear pre-trend
  #         difference. If 0, assume that there are no trend differences in 
  #         the post periods in the absence of treatment. If 2, assume that the
  #         slope of the trend difference for the post-treatment periods are
  #         twice the slope of the pre-trends.
  # OUTPUT:
  # a DT for regression estimation
  
  
  # Extract the right data:
  
  DT = data[income_decile %in% c(treat.deciles, control.deciles), 
            mget(c('id', 'municipality', 'postal_code', 'relative_time',
                   'income_decile', outcome, paste('drop', outcome, sep='_')))]
  
  setnames(DT, old = c(outcome, paste('drop', outcome, sep='_')),
           new = c('outcome', 'drop'))
  
  DT = DT[drop %in% c(1,2)][, drop := NULL]
  
  
  # Outcome can be the number of contacts or an indicator for having any 
  # contacts (in a given month):
  if(has_visits == TRUE) { DT[, outcome := 100 * as.integer(outcome > 0)] }
  
  
  # Create treatment indicators:
  DT[, ':=' (hki = as.integer(municipality==91),
             post = as.integer(relative_time >= 0),
             affected = as.integer(income_decile %in% treat.deciles))]
  
  
  # Detrending (subtract a linear pre-trend difference separately at the bottom
  # and at the top):
  
  if(detrended==TRUE) {
    
    # Aggregate and pivot wider:
    
    trend = DT[post==0, .(outcome = mean(outcome)), 
               by=c('hki', 'relative_time', 'affected')]
    
    trend = dcast(
      trend, relative_time ~ paste0('area_', hki) + paste0('group_', affected), 
      value.var = 'outcome')
    
    # Compute the difference in outcomes between the areas over time:
    trend[, ':=' (difference.treat = area_1_group_1 - area_0_group_1,
                  difference.control = area_1_group_0 - area_0_group_0)]
    
    # Estimate the linear pre-trend difference:
    
    ols.treat = fixest::feols(difference.treat ~ relative_time, data=trend)
    ols.treat = as.data.table(broom::tidy(ols.treat))
    
    ols.control = fixest::feols(difference.control ~ relative_time, data=trend)
    ols.control = as.data.table(broom::tidy(ols.control))
    
    # Collect predictions to a table:
    
    preds = data.table(
      relative_time=c(-24:23),
      preds_1 = c( # The more affected group
        c(ols.treat[term=='(Intercept)', estimate] + 
            c(-24:-1) * ols.treat[term=='relative_time', estimate]),
        c(ols.treat[term=='(Intercept)', estimate] + 
            c(0:23) * trend.post * ols.treat[term=='relative_time', estimate])
      ),
      preds_0 = c( # The less affected group
        c(ols.control[term=='(Intercept)', estimate] + 
            c(-24:-1) * ols.control[term=='relative_time', estimate]),
        c(ols.control[term=='(Intercept)', estimate] + 
            c(0:23) * trend.post * ols.control[term=='relative_time', estimate])
      )
    )
    
    
    # Pivot longer:
    preds = melt(preds, id.vars = 'relative_time',
                 measure.vars = c('preds_1', 'preds_0'))
    preds[, affected := as.integer(gsub('preds_', '', variable))]
    preds[, variable := NULL]
    
    # Merge to the 'DT' and remove the pre-trend difference:
    DT = merge(DT, preds, by=c('relative_time', 'affected'), all.x=TRUE)
    DT[hki==0, outcome := outcome + value]
    DT[, value := NULL]
    
  }
  
  return(DT)
  
}

test = extract.data.ddd(
  data=df, outcome='gp_visits', treat.deciles=c(0:3), control.deciles=c(6:9),
  detrended=TRUE, trend.post = 1)


### A function that estimates a DDD model on the extracted data. ###

estimate.ddd = function(data) {
  # INPUTS: 
  # data: output from extract.data.ddd
  # OUTPUTS:
  # regression results as a table
  
  
  # Estimate:
  DT = data[, mget(colnames(data))]
  spec = 'outcome ~ hki + affected + post + hki:affected + hki:post + affected:post + hki:affected:post'
  ols = fixest::feols(as.formula(spec), data=DT)
  results = as.data.table(broom::tidy(summary(ols, cluster='postal_code')))
  results.muni = 
    as.data.table(broom::tidy(summary(ols, cluster='municipality')))
  rm(ols)
  
  # Collect results to a table:
  results = results[term=='hki:affected:post', .(estimate, std.error, p.value)
  ][, ':=' (std.error.muni = results.muni[term=='hki:affected:post', std.error],
            p.muni = results.muni[term=='hki:affected:post', p.value])]
  
  # Sample sizes:
  results[, n.persons := length(DT[, unique(id)])]
  
  # Pre-treatment mean in Helsinki in the more affected group, 
  # and relative change:
  results[, pre.mean := DT[hki==1 & affected==1 & relative_time %in% c(-12:-1), 
                           mean(outcome)]]
  results[, change := 100 * estimate / pre.mean]
  
  
  # Aggregate data for wild cluster bootstrap: 
  
  pop = DT[, .(population = .N), 
           by=c('relative_time', 'municipality', 'income_decile', 
                'hki', 'post', 'affected')]
  
  DT = merge(DT, pop, by=c('relative_time', 'municipality', 'income_decile', 
                           'hki', 'post', 'affected'), all.x=TRUE)
  
  DT = 
    DT[, .(outcome = weighted.mean(outcome, w = population),
           population = mean(population)), 
       by=c('relative_time', 'municipality', 'income_decile', 
            'hki', 'post', 'affected')]
  
  wcb.data.global <<- DT
  
  ols = fixest::feols(
    outcome ~ hki + affected + post + hki:affected + hki:post + affected:post + 
      hki:affected:post, data = wcb.data.global, 
    weights = wcb.data.global$population, cluster = 'municipality'
  )
  
  
  # Restricted and unresticted wild cluster bootstrap:
  
  wcr = fwildclusterboot::boottest(
    ols, clustid='municipality', param='hki:affected:post', B=9999, 
    impose_null=TRUE, seed=12345)
  
  wcu = fwildclusterboot::boottest(
    ols, clustid='municipality', param='hki:affected:post', B=9999, 
    impose_null=FALSE, seed=12345)
  
  # Add WCB confidence intervals to the results:
  
  results[, ':=' (conf.low.wcr = wcr$conf_int[[1]],
                  conf.high.wcr = wcr$conf_int[[2]],
                  conf.low.wcu = wcu$conf_int[[1]],
                  conf.high.wcu = wcu$conf_int[[2]])]
  
  return(results)
}

r = estimate.ddd(test)
r
rm(test)


### A function that estimates the result for a set of models and for
# a set of specifications (see Section 2 of this script to see examples
# of models and specifications). ###

results.ddd = function(models, specs, data) {
  
  ddd = lapply(models, function(mod) {
    print(mod$label)
    
    
    specs = lapply(1:nrow(specs), function(row) {
      print(paste('Specification', row))
      
      
      # Store parameters outcome and has_visits:
      spec = specs[row, ]
      
      # Extract the data:
      DT = extract.data.ddd(
        data, outcome = spec$outcome, treat.deciles = mod$treat.deciles,
        control.deciles = mod$control.deciles, has_visits = spec$has_visits, 
        detrended = spec$detrended, trend.post = spec$trend.post
      )
      
      # Estimate the regressions:
      results = estimate.ddd(DT)
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcome,
                      has_visits = spec$has_visits,
                      detrended = spec$detrended,
                      trend.post = spec$trend.post)]
      
      return(results)
    })
    specs = do.call(rbind.data.frame, specs)
    specs = specs[, mget(colnames(specs))]
    specs[, mod := mod$label]
  })
  ddd = do.call(rbind.data.frame, ddd)
  
  return(ddd)
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: DDD models: estimate the results. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will estimate the effects in the following subgroups:

models = list(
  all = list(treat.deciles=c(0:3), control.deciles=c(6:9), label='Bottom 40% vs. Top 40%')
)


### DDD estimates. ###

outcome = c('gp_visits', 'dentist_visits')
has_visits = c(TRUE, FALSE)
detrended = c(TRUE, FALSE)
trend.post = c(1, 0, 1.5)

specs = CJ(outcome, has_visits, detrended, trend.post)
specs = specs[detrended==TRUE | (detrended==FALSE & trend.post==0)]
specs = specs[order(outcome, has_visits, detrended, trend.post)]

ddd = results.ddd(models=models, specs=specs, data=df)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: DDD models: result tables. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Tidy tables and save:

table.gp = ddd[outcome=='gp_visits']
table.dent = ddd[outcome=='dentist_visits']

table.gp = create.table(table=table.gp, rows=2, columns=4)
table.dent = create.table(table=table.dent, rows=2, columns=4)

save.table(table.gp, output=output_gp,
           label_tex = 'tab:table_ddd_gp',
           title_tex = 'DDD Results: GP Visits')

save.table(table.dent, output=output_dent,
           label_tex = 'tab:table_ddd_dent',
           title_tex = 'DDD Results: Dentist Visits')

# End.