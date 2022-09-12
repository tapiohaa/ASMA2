
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 2_results_synth.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Helsinki.       ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate the synth results.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(Synth)            # Synthetic controls estimation.
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 

# Inputs:
functions = "W:/ASMA2/data/1_shared_functions.R"
input_data = "W:/ASMA2/data/processed/analysis_data_synth.rds"

# Outputs:
synth_weights = "W:/ASMA2/analysis/tables/table_synth_weights"
plots_synth_gp_main = "W:/ASMA2/analysis/figures/plots_synth_gp_main.pdf"
plots_synth_gp_rob = "W:/ASMA2/analysis/figures/plots_synth_gp_rob.pdf"

###
###

# Read the datasets:
df = readRDS(input_data)

# Read shared functions
source(functions) # save.table() and create.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Functions for estimation and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that extracts data for Synth. ###

extract.data.synth = 
  function(data, otc_type_c='all', value_type_c='raw', pop_threshold=40000) {
  # INPUTS:
  # data: 'df'
  # otc_type_c: 'all' (all individuals), 'diff' (difference between the bottom 
  #         40% and the top 40% in levels), or 'ratio' (ratio between the bottom 
  #         40% and the top 40%)
  # value_type_c: 'raw' or 'demean'
  # pop_threshold: an integer; we keep municipalities with at least the
  #         threshold number of sample individuals
  # OUTPUTS:
  # a data.table
  
  df = data[otc_type==otc_type_c & value_type==value_type_c & 
              population > pop_threshold]
  
}
  
test = extract.data.synth(data=df, otc_type_c = 'all', value_type_c = 'demean')

# The number of municipalities in the donor pool (GP visits): 9
length(test[, unique(municipality)]) - 1


### A function that fits the synthetic control and estimates the results. ###

estimate.synth = function(data, dropped.controls=NULL) {
  # INPUTS:
  # data: an output from extract.data.synth()
  # dropped.controls: an integer vector of municipality codes that are not
  #       used in the donor pool.
  # OUTPUT:
  # a list containing dynamic and static estimates and a weights table.
  

  df = data[, mget(colnames(data))]
  
  # Predictors are the pre-treatment outcomes:
  predictors = lapply(c(-24:-1), function(i) { list('otc', i, 'mean') })
  
  
  # Data for the Synth::synth() function:
  dataprep.out = Synth::dataprep(
    foo = df, dependent = c('otc'),
    unit.variable = c('municipality'),
    time.variable = c('relative_time'),
    special.predictors = predictors,
    treatment.identifier = 91,
    controls.identifier = setdiff(unique(df$municipality), 
                                  c(91, dropped.controls)), 
    time.predictors.prior = c(-24:-1),
    time.optimize.ssr = c(-24:-1),
    unit.names.variable = c('muni_name'),
    time.plot = c(-24:23)
  )
  
  # Construct the synthetic control.
  synth.out = Synth::synth(data.prep.obj = dataprep.out)
  
  
  # Store the gaps:
  
  gaps = data.table(
    relative_time = as.integer(rownames(dataprep.out$Y1plot)),
    otc_treat = dataprep.out$Y1plot,
    otc_synth = dataprep.out$Y0plot %*% synth.out$solution.w,
    gaps = dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
  )
  colnames(gaps) = c('relative_time', 'otc_treat', 'otc_synth', 'gaps')
  
  
  # Estimate and remove a linear pre-trend difference from the gaps:
  
  ols = lm(gaps ~ relative_time, data=gaps[relative_time < 0])
  
  preds = predict(ols, newdata = data.table(relative_time = c(-24:23)))
  preds = data.table(relative_time = c(-24:23), pred = preds)
  
  gaps = merge(gaps, preds, by='relative_time', all.x = TRUE)
  gaps[, ':=' (gaps_detrend = gaps - pred, pred = NULL)]
  setkey(gaps, NULL)

  
  # Static estimates:
  
  att = data.table(
    pre_mean = unique(df[municipality==91, pre_mean]),
    att = gaps[relative_time >= 0, mean(gaps)],
    att_detrend = gaps[relative_time >= 0, mean(gaps_detrend)]
  )
  att[, ':=' (change = 100 * att / pre_mean,
              change_detrend = 100 * att_detrend / pre_mean)]
  
  # Weights:
  weights = synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)$tab.w
  
  # Return the results:
  results = list(dynamic = gaps, static = att, weights = weights)
  return(results)
  
}

estimate.synth(data=test, dropped.controls = NULL)


### A function that plots gaps plots. ###

plot.synth.gaps = function(data) {
  
  # Extract the data:
  dynamic = data$dynamic
  static = data$static
  
  # Pivot longer:
  dynamic = melt(dynamic[, .(relative_time, gaps, gaps_detrend)],
                 id.vars = 'relative_time',
                 measure.vars = c('gaps', 'gaps_detrend'),
                 value.name = 'gaps')
  
  # Proper labeling for estimates:
  dynamic[variable=='gaps', Results := 'Raw']
  dynamic[variable=='gaps_detrend', Results := 'Detrended']
  
  # Store the minimum and maximum values for y-axis limits:
  min_value = dynamic[, min(gaps)]
  max_value = dynamic[, max(gaps)]
  difference = max_value - min_value
  
  # Collect signs of the effects before rounding:
  if(static$att >= 0) {sign_att = '+'}
  if(static$att < 0) {sign_att = '-'}
  if(static$att_detrend >= 0) {sign_att_detrend = '+'}
  if(static$att_detrend < 0) {sign_att_detrend = '-'}
  
  # Collect results to a character string:
  
  raw = paste(
    'Raw: ', sign_att, 
    format(abs(round(static$att, digits = 3)), nsmall = 3),
    ' (', sign_att, 
    format(abs(round(static$change, digits = 1)), nsmall = 1),
    '%)', sep='')
  
  detrend = paste(
    'Detrended: ', sign_att_detrend, 
    format(abs(round(static$att_detrend, digits = 3)), nsmall = 3),
    ' (', sign_att_detrend, 
    format(abs(round(static$change_detrend, digits = 1)), nsmall = 1),
    '%)', sep='')
  
  results_text = paste(detrend, raw, sep='\n')
  
  
  # Plot:
  
  p = ggplot(data=dynamic, aes(x=relative_time, y=gaps, linetype=Results)) +
    geom_line(size=1) +
    geom_vline(xintercept = -0.5, linetype='dashed') +
    geom_hline(yintercept = 0, linetype='dashed') +
    annotate(geom = 'label', x=-1, y=(max_value + difference / 6), 
             label=results_text, hjust=1) + 
    ylim(min_value - difference / 5, max_value + difference / 5) + 
    labs(x = 'Months relative to the abolition', y = 'Gaps: Helsinki - SC') +
    theme(text = element_text(size=20),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill= NA, size = 0.5),
          legend.position = "bottom") 
  
  return(p)
  
}


### A function that does the leave-X-out estimation. ###

synth.leave.x.out = function(data, x.out=1) {
  # INPUTS: 
  # data: output from extract.data.synth()
  # x.out: how many controls are excluded from the donor pool, an integer
  # OUTPUT:
  # aggregated results as a data.table
  
  
  df = data[, mget(colnames(data))]
  
  # The combinations of municipalities that will be excluded:
  munies = setdiff(test[, unique(municipality)], 91)
  combs = combn(munies, x.out)
  
  # Loop over the combinations and estimate the results:
  
  lxo = lapply(combs, function(i) {
    results = estimate.synth(data=df, dropped.controls = i)
    results = results$dynamic
    results[, dropped := paste(i, collapse='_')]
  })
  
  # Average over the runs:
  lxo = do.call(rbind.data.frame, lxo)
  lxo = lxo[, .(gaps = mean(gaps)), by=c('relative_time')]
 
  
  # Estimate and remove a linear pre-trend difference from the gaps:
  
  ols = lm(gaps ~ relative_time, data=lxo[relative_time < 0])
  
  preds = predict(ols, newdata = data.table(relative_time = c(-24:23)))
  preds = data.table(relative_time = c(-24:23), pred = preds)
  
  lxo = merge(lxo, preds, by='relative_time', all.x = TRUE)
  lxo[, ':=' (gaps_detrend = gaps - pred, pred = NULL)]
  setkey(lxo, NULL)
  
  
  # Static estimates:
  
  att = data.table(
    pre_mean = unique(df[municipality==91, pre_mean]),
    att = lxo[relative_time >= 0, mean(gaps)],
    att_detrend = lxo[relative_time >= 0, mean(gaps_detrend)]
  )
  att[, ':=' (change = 100 * att / pre_mean,
              change_detrend = 100 * att_detrend / pre_mean)]
  
  
  # Return the results:
  results = list(dynamic = lxo, static = att)
  return(results)
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Main estimates. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Demeaned data (GP visits) with the population threshold of 40,000:

otc_types = c('all', 'diff', 'ratio')


# loop over the outcome types:
results.main = lapply(otc_types, function(otc_type) {
  print(otc_type)
  
  # Extract the data:
  DT = extract.data.synth(
    data=df, otc_type_c = otc_type, value_type_c = 'demean', pop_threshold=40000
  )
  
  # Estimate the results:
  tables = estimate.synth(data=DT, dropped.controls = NULL)
  
  # Plot the results:
  plots = plot.synth.gaps(data=tables)
  
  return(list(table=tables, plot=plots))
  
})

names(results.main) = otc_types


### Create a table that shows the weights of the main analysis. ###

w.table = data.table(
  Municipality = results.main$all$table$weights$unit.names, NA,
  All = results.main$all$table$weights$w.weights,
  Difference = results.main$diff$table$weights$w.weights, 
  Ratio = results.main$ratio$table$weights$w.weights
)

save.table(w.table, output=synth_weights, label_tex = 'tab:table_synth_weights',
           title_tex = 'Synth weights')


### Result plots. ###

cairo_pdf(filename = plots_synth_gp_main, width = 15, height = 5.5)
print(
  results.main$all$plot + ggtitle('All visits') +
    results.main$diff$plot + ggtitle('Difference: B40% - T40%') +
    results.main$ratio$plot + ggtitle('Ratio: B40% / T40%') +
    plot_layout(guides='collect') & theme(legend.position = 'bottom')
)
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Leave-X-out estimates. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Demeaned data with the population threshold of 40,000 and
# leave-two-out estimation:

otc_types = c('all', 'diff', 'ratio')


# loop over the outcome types:
results.rob = lapply(otc_types, function(otc_type) {
  print(otc_type)
  
  # Extract the data:
  DT = extract.data.synth(
    data=df, otc_type_c = otc_type, value_type_c = 'demean', pop_threshold=40000
  )
  
  # Estimate the results:
  tables = synth.leave.x.out(data=DT, x.out=2)
  
  # Plot the results:
  plots = plot.synth.gaps(data=tables)
  
  return(list(table=tables, plot=plots))
  
})

names(results.rob) = otc_types


# Save plots:

cairo_pdf(filename = plots_synth_gp_rob, width = 15, height = 5.5)
print(
  results.rob$all$plot + ggtitle('All visits') +
    results.rob$diff$plot + ggtitle('Difference: B40% - T40%') +
    results.rob$ratio$plot + ggtitle('Ratio: B40% / T40%') +
    plot_layout(guides='collect') & theme(legend.position = 'bottom')
)
dev.off()

# End.
