
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 2_trend_plots.R            ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: Helsinki.        ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Plot the evolution of outcomes to assess pre-trends and effects.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_data = "W:/ASMA2/data/processed/analysis_data.rds"

# Outputs:
output_all_gp = "W:/ASMA2/analysis/figures/plot_all_gp.pdf"
output_all_rest = "W:/ASMA2/analysis/figures/plot_all_rest.pdf"
output_inc_gp = "W:/ASMA2/analysis/figures/plot_inc_gp.pdf"
output_inc_dent = "W:/ASMA2/analysis/figures/plot_inc_dent.pdf"
outcome_detrend_gp = "W:/ASMA2/analysis/figures/plot_detrend_gp.pdf"
outcome_detrend_rest = "W:/ASMA2/analysis/figures/plot_detrend_rest.pdf"
outcome_detrend_dent = "W:/ASMA2/analysis/figures/plot_detrend_dent.pdf"


###
###

# Read the datasets:
df = readRDS(input_data)
df[, ':=' (hki = as.integer(municipality==91), municipality=NULL, id=NULL)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Functions for data extraction and plotting. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function to extract and aggregate a relevant subset of data. ###

extract.data = function(data, otc, deciles, data.type) {
  # INPUTS:
  # data: 'df'
  # otc: 'gp_visits', 'ed_visits', 'specialist_consultations', or 
  #     'dentist_visits', 'social_assistance'
  # deciles: c(0:9) (all), c(0:3) (bottom 40%), or c(6:9) (top 40%)
  # data.type: 'trends_plot' or 'detrended'
  # OUTPUT:
  # a data.table


  # Extract the data and aggregate:
  DT = data[income_decile %in% deciles & get(paste('drop_', otc, sep='')) != 3
            ][, outcome := get(otc)
              ][, .(outcome = mean(outcome), population = .N), 
                by=c('hki', 'relative_time')
                ][!is.na(outcome)]
  
  if(otc=='social_assistance') { DT[, outcome := 100 * outcome]}
  
  DT[hki==1, Area := 'Helsinki (treated)']
  DT[hki==0, Area := 'Comparisons']
  DT$Area = factor(DT$Area, levels = c('Helsinki (treated)', 'Comparisons'))
  
  # Treatment indicator (treatment on):
  DT[, treat := hki==1 & relative_time >= 0]
  
  
  if(data.type=='detrended') {
    
    # Mutate the data:
    
    DT = dcast(DT[, .(relative_time, Area, outcome)], 
               relative_time ~ Area, value.var = 'outcome' )
    
    setnames(DT, old=c('Comparisons', 'Helsinki (treated)'), 
             new=c('comparison', 'treat'))
    DT[, estimate := treat-comparison]
    setnames(DT, old='relative_time', new='t')
    DT = DT[, .(estimate, t)]
    
  }
  
  return(DT)

}


### A function that plots trend plots using the raw data. ###

plot.treat.control = function(data, otc, deciles, plot.type) {
  # INPUTS:
  # data: output from 'extract.data()'
  # otc: 'gp_visits', 'ed_visits', 'specialist_consultations', or 
  #       'dentist_visits', 'social_assistance'
  # deciles: c(0:9) (all), c(0:3) (bottom 40%), or c(6:9) (top 40%)
  # plot.type: 'Raw Data', 'Smoothed', or 'Difference'
  # OUTPUT:
  # a data.table
  
  DT = data[, mget(colnames(data))]
  
  # Y-axis title depends on the outcome:
  if(otc=='gp_visits') { y.title = 'GP visits'}
  if(otc=='ed_visits') { y.title = 'ED visits'}
  if(otc=='dentist_visits') { y.title = 'Dentist visits'}
  if(otc=='specialist_consultations') { y.title = 'Specialist consultations'}
  if(otc=='social_assistance') { y.title = 'On social assistance (%)'}
  
  # Plot title depends on the income group:
  if(identical(deciles, c(0:9))) { group = 'All Individuals'}
  if(identical(deciles, c(0:3))) { group = 'Bottom 40%'}
  if(identical(deciles, c(6:9))) { group = 'Top 40%'}
  
  # Plot title:
  p.title = paste(group, plot.type, sep=': ')
  
  
  if(plot.type %in% c('Raw Data', 'Smoothed')) {
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=outcome, linetype=Area)
    ) +
      ggtitle(p.title) +
      labs(x = 'Months relative to the abolition', y = y.title) +
      theme(text = element_text(size=20),   
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill= NA, size = 0.5),
            legend.position = "bottom") +
      geom_vline(xintercept = -0.5, linetype="dashed")
    
    
    if (plot.type=="Smoothed") {
      p = p + geom_smooth(se=FALSE, color='black') +
        guides(linetype='none')
      
    } else if (plot.type=="Raw Data") { 
      p = p + geom_line(size=1) }
    
    
  } else if (plot.type == 'Difference') {
    
    
    # Mutate the data:
    
    DT = dcast(DT, relative_time ~ Area, value.var = 'outcome' )
    
    setnames(DT, old=c('Comparisons', 'Helsinki (treated)'), 
             new=c('comparison', 'treat'))
    DT[, outcome := treat-comparison]
    
    
    # Plot:
    p = ggplot(
      data=DT, aes(x=relative_time, y=outcome)
    ) +
      geom_line(color='grey40') +
      geom_point(color='grey40') + 
      geom_smooth(se=FALSE, color='black') +
      ggtitle(p.title) +
      labs(x = 'Months relative to the abolition', y = y.title) +
      theme(text = element_text(size=20),    
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
            legend.position = "bottom") +
      geom_vline(xintercept = -0.5, linetype="dashed") +
      geom_hline(yintercept = 0, linetype="dashed")
    
  }
  
  
  return(p)
  
}

test = extract.data(data=df, otc='gp_visits', deciles=c(0:3), 
                    data.type='trends_plot')

plot.treat.control(data=test, otc='gp_visits', deciles=c(0:9),
                   plot.type = 'Smoothed')


### A function plotting trend plots with detrended data. ###

plot.trends = function(data, otc, deciles) {
  # INPUTS:
  # data: output from 'extract.data()'
  # otc: 'gp_visits', 'ed_visits', 'specialist_consultations', or 
  #       'dentist_visits', 'social_assistance'
  # deciles: c(0:9) (all), c(0:3) (bottom 40%), or c(6:9) (top 40%)
  # OUTPUT:
  # a data.table
  
  
  # Y-axis title depends on the outcome:
  if(otc=='gp_visits') { y.title = 'GP visits'}
  if(otc=='ed_visits') { y.title = 'ED visits'}
  if(otc=='dentist_visits') { y.title = 'Dentist visits'}
  if(otc=='specialist_consultations') { y.title = 'Specialist consultations'}
  if(otc=='social_assistance') { y.title = 'On social assistance (%)'}
  
  y.title = paste('Effect:', y.title)
  
  # Plot title depends on the income group:
  if(identical(deciles, c(0:9))) { group = 'All Individuals'}
  if(identical(deciles, c(0:3))) { group = 'Bottom 40%'}
  if(identical(deciles, c(6:9))) { group = 'Top 40%'}
  
  
  # First, we need to get the right data for plotting:
  
  results = data[, mget(colnames(data))]
  
  ols = lm(estimate ~ t, data=results[t < 0])
  
  if(otc=='social_assistance') { periods=c(-12:23) } else { periods=c(-24:23)}
  preds = ols$coefficients['(Intercept)'] + ols$coefficients['t'] * periods
  
  results[, preds := preds]
  results[, estimate := estimate - preds]
  
  
  # Next, proceed to plotting:
  
  p = ggplot(
    data=results, aes(x=t, y=estimate)
  ) +
    geom_line(color='grey40') +
    geom_point(color='grey40') + 
    geom_smooth(se=FALSE, color='black') +
    ggtitle(group) +
    labs(x = 'Months relative to the abolition', y = y.title) +
    theme(text = element_text(size=20),    
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom") +
    geom_vline(xintercept = -0.5, linetype="dashed") +
    geom_hline(yintercept = 0, linetype="dashed") 
  
  return(p)
  
}

test = extract.data(data=df, otc='gp_visits', deciles=c(0:3), 
                    data.type='detrended')

plot.trends(data=test, otc='gp_visits', deciles=c(0:3))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Plot the pre-trend plots using the raw data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will loop over the following outcomes, plot types, and income groups:
outcomes = c('gp_visits', 'ed_visits', 'specialist_consultations', 
             'dentist_visits', 'social_assistance')
p.types = c('Smoothed', 'Raw Data', 'Difference')
i.groups = list(c(0:9), c(0:3), c(6:9))


# Loop over income groups:

plots = lapply(i.groups, function(i.group) {
  
  
  # Loop over outcomes:
  
  plots.otc = lapply(outcomes, function(outcome) {
    
    # Extract the data:
    DT = extract.data(data=df, otc=outcome, deciles=i.group, 
                      data.type = 'trends_plot')
    
    
    # Loop over plot types:
    plots = lapply(p.types, function(p.type) {
      
      p = 
        plot.treat.control(data=DT, otc=outcome, deciles=i.group, 
                           plot.type=p.type)
      
    })
    names(plots) = p.types
    
    return(plots)
    
  })
  names(plots.otc) = outcomes
  
  return(plots.otc)
})

names(plots) = c('all', 'bot40', 'top40')


# Save plots:

cairo_pdf(filename = output_all_gp, width = 15, height = 5)
print(patchwork::wrap_plots(plots$all$gp_visits) + 
        plot_layout(guides='collect') & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = output_all_rest, width = 15, height = 15)
print(patchwork::wrap_plots(c(plots$all$ed_visits,
                              plots$all$specialist_consultations,
                              plots$all$social_assistance)) + 
        plot_layout(guides='collect', ncol=3) & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = output_inc_gp, width = 15, height = 10)
print(patchwork::wrap_plots(c(plots$bot40$gp_visits,
                              plots$top40$gp_visits)) + 
        plot_layout(guides='collect', ncol=3) & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = output_inc_dent, width = 15, height = 15)
print(patchwork::wrap_plots(c(plots$all$dentist_visits,
                              plots$bot40$dentist_visits,
                              plots$top40$dentist_visits)) + 
        plot_layout(guides='collect', ncol=3) & 
        theme(legend.position = 'bottom'))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Plot the pre-trend plots using the detrended data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will loop over the outcomes and income groups listed above.


# Loop over income groups:
plots = lapply(i.groups, function(i.group) {
  
  # Loop over outcomes:
  plots.otc = lapply(outcomes, function(outcome) {
    
    
    # Extract the data and plot:
    print(paste('Outcome: ', outcome, sep=''))
    DT = 
      extract.data(data=df, otc=outcome, deciles=i.group, data.type='detrended')
    p = plot.trends(data=DT, otc=outcome, deciles=i.group)
   
     
  })
  names(plots.otc) = outcomes
  return(plots.otc)
})

names(plots) = c('all', 'bot40', 'top40')


# Save plots:

cairo_pdf(filename = outcome_detrend_gp, width = 15, height = 5)
print(plots$all$gp_visits + plots$bot40$gp_visits + 
        plots$top40$gp_visits + plot_layout(guides='collect') & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = outcome_detrend_rest, width = 15, height = 10)
print(plots$all$ed_visits + 
        plots$bot40$ed_visits + 
        plots$top40$ed_visits + 
        
        plots$all$specialist_consultations + 
        plots$bot40$specialist_consultations + 
        plots$top40$specialist_consultations +
        
        plot_layout(guides='collect', ncol=3, byrow = TRUE) & 
        theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = outcome_detrend_dent, width = 15, height = 5)
print(plots$all$dentist_visits + 
        plots$bot40$dentist_visits + 
        plots$top40$dentist_visits + 
        plot_layout(guides='collect', ncol=3, byrow = TRUE) & 
        theme(legend.position = 'bottom'))
dev.off()

# End.
