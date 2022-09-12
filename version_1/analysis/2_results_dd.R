
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 2_results_dd.R           ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Helsinki.       ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate the DD results.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(fixest)           # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.
library(fwildclusterboot) # Wild cluster bootstrap.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 

# Inputs:
functions = "W:/ASMA2/data/1_shared_functions.R"
input_data = "W:/ASMA2/data/processed/analysis_data.rds"

# Outputs:
output_gp = "W:/ASMA2/analysis/tables/table_dd_gp"
output_rest = "W:/ASMA2/analysis/tables/table_dd_rest"
output_dent = "W:/ASMA2/analysis/tables/table_dd_dent"
output_gp_indicator = "W:/ASMA2/analysis/tables/table_dd_gp_indicator"
output_gp_assumptions = "W:/ASMA2/analysis/tables/table_dd_gp_assumptions"
output_plot_inc = "W:/ASMA2/analysis/figures/dd_income.pdf"
output_gp_placebo = "W:/ASMA2/analysis/tables/table_dd_gp_placebo"

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


### A function that returns a DD analysis dataset. ###

extract.data.dd = function(
  data, outcome, deciles, has_visits=FALSE, trend.post=1,
  placebo.year=0) {
  # INPUTS:
  # data: 'df'
  # outcome: 'gp_visits', 'ed_visits', 'specialist_consultations', 
  #       'dentist_visits', or 'social_assistance'
  # has_visits: if TRUE, use an indicator of having any contact in a given month
  # trend.post: the assumption regarding the extrapolation of a linear pre-trend
  #         difference. If 0, assume that there are no trend differences in 
  #         the post periods in the absence of treatment. If 2, assume that the
  #         slope of the trend difference for the post-treatment periods are
  #         twice the slope of the pre-trends.
  # placebo.year: if 1, analyse a placebo abolition in Helsinki in 2011, using
  #         only pre-treatment data
  # OUTPUT:
  # a DT for regression estimation
  
  
  # Pre-treatment periods:
  
  if(placebo.year==0) {
    
    if(outcome=='social_assistance') {
      pre_treat_length = 12
    } else { pre_treat_length = 24 }
    
    post_treat_length = 24
    
  } else if (placebo.year==1) {
   
    pre_treat_length = 24
    post_treat_length = 0

  }
  
  
  # Extract the right data:
  
  DT = data[income_decile %in% deciles & 
              relative_time %in% c(-pre_treat_length : (post_treat_length-1)), 
            mget(c('id', 'municipality', 'postal_code', 'relative_time',
                   outcome, 'income_decile', paste('drop', outcome, sep='_')))]
  
  setnames(DT, old = c(outcome, paste('drop', outcome, sep='_')),
           new = c('outcome', 'drop'))
  
  DT = DT[drop %in% c(1,2)][, drop := NULL]
  
  
  # Outcome can be the number of contacts or an indicator for having any 
  # contacts (in a given month):
  if(has_visits == TRUE) { DT[, outcome := 100 * as.integer(outcome > 0)] }
  if(outcome == 'social_assistance') { DT[, outcome := 100 * outcome] }
  
  
  if(placebo.year==1) {
    DT[, relative_time := relative_time + 12]
    DT = DT[relative_time %in% c(-12:11)]
    pre_treat_length = 12
    post_treat_length = 12
  }
  
  # Create treatment indicators:
  DT[, ':=' (hki = as.integer(municipality==91),
             post = as.integer(relative_time >= 0))]
  
  
  # Detrending (subtract a linear pre-trend difference):
  
  trend = DT[post==0, .(outcome = mean(outcome)), by=c('hki', 'relative_time')]
  trend = dcast(trend, relative_time ~ paste0('area_', hki), 
                value.var = 'outcome')
  trend[, difference := area_1 - area_0]
  
  ols.trend = fixest::feols(difference ~ relative_time, data=trend)
  ols.trend = as.data.table(broom::tidy(ols.trend))
  
  preds = data.table(
    relative_time=c(-pre_treat_length : (post_treat_length-1)),
    preds = c(
      c(ols.trend[term=='(Intercept)', estimate] + 
          c(-pre_treat_length:-1) * ols.trend[term=='relative_time', estimate]),
      c(ols.trend[term=='(Intercept)', estimate] + 
          c(0:(post_treat_length-1)) * trend.post * 
          ols.trend[term=='relative_time', estimate])
    )
  )
  
  DT = merge(DT, preds, by=c('relative_time'), all.x=TRUE)
  DT[hki==0, outcome := outcome + preds]
  DT[, preds := NULL]
  
}

test = extract.data.dd(data=df, outcome='gp_visits', deciles=c(0:3),
                       trend.post = 1)


### A function that estimates a DD model on the detrended data. ###

estimate.dd = function(data, fe.var) {
  # INPUTS: 
  # data: output from extract.data.dd
  # fe.var: either 'municipality' or 'postal_code'
  # OUTPUTS:
  # regression results as a table
  
  
  # Estimate:
  DT = data[, mget(colnames(data))]
  DT[, fe.var := get(fe.var)]
  
  ols = fixest::feols(outcome ~ post + hki:post | fe.var, data=DT)
  results = as.data.table(broom::tidy(summary(ols, cluster='postal_code')))
  results.muni = 
    as.data.table(broom::tidy(summary(ols, cluster='municipality')))
  rm(ols)
  
  # Collect results to a table:
  results = results[term=='post:hki', .(estimate, std.error, p.value)]
  results[, ':=' (std.error.muni = results.muni[term=='post:hki', std.error],
                  p.muni = results.muni[term=='post:hki', p.value])]
  
  # Sample sizes:
  results[, n.persons := length(DT[, unique(id)])]
  
  # Pre-treatment mean in Helsinki, and relative change:
  results[, pre.mean := 
            DT[hki==1 & relative_time %in% c(-12:-1), mean(outcome)]]
  results[, change := 100 * estimate / pre.mean]
  
  
  # Aggregate data for wild cluster bootstrap: 
  
  pop = DT[, .(population = .N), by=c('relative_time', 'municipality', 
                                      'income_decile', 'hki', 'post')]
  
  DT = merge(DT, pop, by=c('relative_time', 'municipality', 'income_decile', 
                           'hki', 'post'), all.x=TRUE)
  
  DT = 
    DT[, .(outcome = weighted.mean(outcome, w = population),
           population = mean(population)), 
       by=c('relative_time', 'municipality', 'income_decile', 'hki', 'post')]
  
  wcb.data.global <<- DT
  
  ols = fixest::feols(
    outcome ~ post + hki:post | municipality, data = wcb.data.global, 
    weights = wcb.data.global$population, cluster = 'municipality'
  )
  
  # Restricted and unresticted wild cluster bootstrap:
  
  wcr = fwildclusterboot::boottest(
    ols, clustid='municipality', param='post:hki', B=9999, impose_null=TRUE,
    seed=12345)
  
  wcu = fwildclusterboot::boottest(
    ols, clustid='municipality', param='post:hki', B=9999, impose_null=FALSE,
    seed=12345)

  # Add WCB confidence intervals to the results:
  
  results[, ':=' (conf.low.wcr = wcr$conf_int[[1]],
                  conf.high.wcr = wcr$conf_int[[2]],
                  conf.low.wcu = wcu$conf_int[[1]],
                  conf.high.wcu = wcu$conf_int[[2]])]
  
  return(results)
}

r = estimate.dd(test, fe.var='municipality')
r
rm(test)


### A function that estimates the result for a set of models and for
# a set of specifications (see Section 2 of this script to see examples
# of models and specifications). ###

results.dd = function(models, specs, data, time.placebo=0) {
  
  dd = lapply(models, function(mod) {
    print(mod$label)
    
    
    specs = lapply(1:nrow(specs), function(row) {
      print(paste('Specification', row))
      
      
      # Store parameters outcome and has_visits:
      spec = specs[row, ]
      
      # Extract the data:
      DT = extract.data.dd(
        data, outcome = spec$outcome, deciles = mod$deciles,
        has_visits = spec$has_visits, trend.post = spec$trend.post,
        placebo.year = time.placebo
      )
      
      # Estimate the regressions:
      results = estimate.dd(DT, fe.var = spec$fe.var)
      
      # Add information about the specification:
      results[, ':=' (outcome = spec$outcome,
                      has_visits = spec$has_visits,
                      trend.post = spec$trend.post,
                      fe.var = spec$fe.var)]
      
      return(results)
    })
    specs = do.call(rbind.data.frame, specs)
    specs = specs[, mget(colnames(specs))]
    specs[, mod := mod$label]
  })
  dd = do.call(rbind.data.frame, dd)
  
  return(dd)
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: DD models: estimate the results. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will estimate the effects in the following subgroups:

models = list(
  all = list(deciles=c(0:9), label='All'),
  D14 = list(deciles=c(0:3), label='Bottom 40%'), 
  D710 = list(deciles=c(6:9), label='Top 40%')
)


### Main estimates: three income groups and four outcomes. ###

outcome = c('gp_visits', 'ed_visits', 'specialist_consultations', 
            'dentist_visits')
has_visits = FALSE
trend.post = 1
fe.var = 'municipality'

specs = CJ(outcome, has_visits, trend.post, fe.var)

dd = results.dd(models=models, specs=specs, data=df)


### Supplementary analyses: has any contacts as the outcome, and varying
# fixed effects. ###

outcome = c('gp_visits')
has_visits = c(TRUE, FALSE)
fe.var = c('municipality', 'postal_code')
trend.post = 1

specs = CJ(outcome, has_visits, trend.post, fe.var)

dd.has.fe = results.dd(models=models, specs=specs, data=df)


### Supplementary analyses: assumptions on extrapolating pre-trends. ###

outcome = c('gp_visits')
has_visits = FALSE
fe.var = 'municipality'
trend.post = c(0, 0.5, 1.5)

specs = CJ(outcome, has_visits, trend.post, fe.var)

dd.pta = results.dd(models=models, specs=specs, data=df)


### Supplementary analyses: placebo treatment in 2012. ###

trend.post = c(1)
specs = CJ(outcome, has_visits, trend.post, fe.var)

dd.placebo = results.dd(models=models, specs=specs, data=df, time.placebo=1)


### Supplementary analyses: effects by income decile. ###

models = list(
  all = list(deciles=c(0:9), label='all'),
  D1 = list(deciles=c(0), label='1'), 
  D2 = list(deciles=c(1), label='2'),
  D3 = list(deciles=c(2), label='3'), 
  D4 = list(deciles=c(3), label='4'),
  D5 = list(deciles=c(4), label='5'), 
  D6 = list(deciles=c(5), label='6'),
  D7 = list(deciles=c(6), label='7'), 
  D8 = list(deciles=c(7), label='8'),
  D9 = list(deciles=c(8), label='9'), 
  D10 = list(deciles=c(9), label='10')
)

outcome = c('gp_visits')
has_visits = FALSE
fe.var = 'municipality'
trend.post = 1

specs = CJ(outcome, has_visits, trend.post, fe.var)

dd.decile = results.dd(models=models, specs=specs, data=df)



### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: DD models: result tables. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Drop such results that will not be shown in tables, and change the order
# for tables:

dd$outcome = factor(
  dd$outcome, levels = c('gp_visits', 'ed_visits', 'specialist_consultations', 
                         'social_assistance', 'dentist_visits'))
dd = dd[order(outcome)]

table.gp = dd[outcome=='gp_visits']
table.rest = dd[outcome %in% c('ed_visits', 'specialist_consultations')]
table.dent = dd[outcome=='dentist_visits']

table.has.fe = dd.has.fe[!(has_visits==FALSE & fe.var=='municipality')
                         ][order(has_visits, fe.var)]
table.pta = dd.pta[order(trend.post)]
table.placebo = dd.placebo[order(trend.post)]


# Tidy tables:
table.gp = create.table(table=table.gp, rows=1, columns=3)
table.rest = create.table(table=table.rest, rows=2, columns=3)
table.dent = create.table(table=table.dent, rows=1, columns=3)
table.has.fe = create.table(table=table.has.fe, rows=3, columns=3)
table.pta = create.table(table=table.pta, rows=3, columns=3)
table.placebo = create.table(table=table.placebo, rows=3, columns=3)


# Save tables:

save.table(table.gp, output=output_gp,
           label_tex = 'tab:table_dd_gp',
           title_tex = 'Main Results: GP Visits')

save.table(table.rest, output=output_rest,
           label_tex = 'tab:table_dd_rest',
           title_tex = 'Main Results: Additional Outcomes')

save.table(table.dent, output=output_dent,
           label_tex = 'tab:table_dd_dent',
           title_tex = 'Supplementary Results: Placebo Outcome')

save.table(table.has.fe, output=output_gp_indicator,
           label_tex = 'tab:table_dd_gp_indicator',
           title_tex = 'Supplementary Results: GP Visits')

save.table(
  table.pta, output=output_gp_assumptions,
  label_tex = 'tab:table_dd_gp_assumptions',
  title_tex = 'Supplementary Results: GP Visits, and the PTA Assumption')

save.table(
  table.placebo, output=output_gp_placebo,
  label_tex = 'tab:table_dd_gp_placebo',
  title_tex = 'Supplementary Results: GP Visits, and a Time Placebo')



### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: DD models: result plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Confidence intervals:
DT = dd.decile[, mget(colnames(dd.decile))]
DT[, ':=' (conf.low = estimate - 1.96 * std.error,
           conf.high = estimate + 1.96 * std.error)]
DT[, ':=' (change.conf.low = 100 * conf.low / pre.mean,
           change.conf.high = 100 * conf.high / pre.mean)]
           

# The estimates for the whole population:
ate = DT[mod=='all', .(estimate, conf.low, conf.high,
                       change, change.conf.low, change.conf.high)]
DT.ate = data.table(Decile=c(1:10))
DT.ate = cbind(DT.ate, ate)

# Estimates by decile:
DT = DT[mod != 'all'][, Decile := as.integer(mod)]


# Plot 1 (the effect in levels):

p.1 = ggplot(data=DT, aes(x=Decile, y=estimate)) +
  geom_line(data=DT.ate, aes(x=Decile, y=estimate)) + 
  geom_ribbon(data=DT.ate, 
              aes(ymin=conf.low, ymax=conf.high), alpha = 0.1,
              color='black') +
  geom_segment(aes(x=Decile, xend=Decile, y=conf.low, yend=conf.high)) +
  ylab('Effect: GP Visits')


# Plot 2 (the effect in relative terms):

p.2 = ggplot(data=DT, aes(x=Decile, y=change)) +
  geom_line(data=DT.ate, aes(x=Decile, y=change)) + 
  ylab('Effect: Change GP Visits (%)') 
  

# Add similar layers to the plots:
plots = lapply(list(p.1, p.2), function(p) {
  
  p = p +   
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks=c(1:10)) +
    geom_hline(yintercept = 0, linetype='dashed') +
    theme(text = element_text(size=20),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size= 0.5))
  
})


# Save:

cairo_pdf(filename = output_plot_inc, width = 12.0, height = 6.0)
print(patchwork::wrap_plots(plots))
dev.off()

# End.
