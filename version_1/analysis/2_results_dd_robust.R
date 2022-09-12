
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 2_results_dd_robust.R      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###          Funded Primary Care: Helsinki.       ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Robustness checks and sensitivity analyses.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(ggplot2)          # Plotting data. 
library(broom)            # Statistical objects into tidy tibbles.
library(fixest)           # linear fixed effects estimation.
library(Rglpk)            # R/Gnu linear programming.
library(mvtnorm)          # Multivariate normal probabilities.
library(dplyr)            # data wrangling.
library(HonestDiD)        # Robust inference for DD designs.

# Inputs:
functions = "W:/ASMA2/data/1_shared_functions.R"
input_data = "W:/ASMA2/data/processed/analysis_data_aggr.rds"

# Outputs:
table_hagemann = "W:/ASMA2/analysis/tables/table_dd_hagemann"
table_se = "W:/ASMA2/analysis/tables/table_se_aggr"
plot_honestdid = "W:/ASMA2/analysis/figures/plot_honestdid.pdf"
plot_leave_x_out = "W:/ASMA2/analysis/figures/plot_leave_x_out.pdf"
plot_pta_sensitivity = "W:/ASMA2/analysis/figures/plot_pta_sensitivity.pdf"
  
###
###

# Read the datasets:
df = readRDS(input_data)

# Read shared functions
source(functions) # save.table() and create.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Functions for the rearrangement test inference (Hageman, 2020). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that calculates w as in Table 1 (Hagemann, 2020). ###

stc.weight <- function(q, alpha=.05, rho=5, steps=10^4) {
  wgrid <- (1:steps)/steps
  bnd <- function(w) {
    minb <- function(b) pnorm(w*sqrt(q-1)*b)^(q-1) + 2*pnorm(-b*q)
    f0 <- function(y) pnorm((1-w)*rho*y)^(q-1) * dnorm(y)
    2^(-q-1) + integrate(f0, 0, Inf)$val + optimize(minb, c(0,2))$ob
  }
  wres = sapply(wgrid, bnd)
  winf = min(which(wres <= alpha))
  if(is.finite(winf)) wgrid[winf]
  else stop("Feasible w does not exist. Decrease rho, increase q, or increase alpha.")
}


### Calculates test decision as in Algorithm 3.4 (Hagemann, 2020). ###

stc <- function(x1, x0, alpha=.05, rho=2, steps=10^4, w=NULL, verbose = TRUE) {
  q <- length(x0)
  if(is.null(w))
    w <- stc.weight(q=q, alpha=alpha, rho=rho, steps=steps)
  S <- c( (1+w)*(x1-mean(x0)), (1-w)*(x1-mean(x0)), x0-mean(x0) )
  dec <- mean(S[1:2]) - mean(S[3:(q+2)]) == 
    mean(sort(S, T)[1:2]) - mean(sort(S, T)[3:(q+2)])
  if(verbose == TRUE) {
    cat(paste("One-sided (>), alpha=", alpha, ".\n", sep=''))
    cat(paste("Decision:", ifelse(dec==TRUE, "reject.", "do not reject.")))
  } else {
    dec
  }
}


### Robustness check. ###

stc.robust <- function(x1, x0, alpha=.05, rhostart=0, steps=10^3, inc=.001, 
                       verbose=TRUE) {
  rho  <- rhostart
  dec <- stc(x1=x1, x0=x0, alpha=alpha, rho=rho, steps=steps, verbose=FALSE)
  if(dec==FALSE) {
    return(NA)
  } else {
    while(dec==TRUE) {
      rho <- rho+1
      dec <- stc(x1=x1, x0=x0, alpha=alpha, rho=rho, steps=steps, verbose=FALSE)
    }
  }
  rho <- max(rho-1, rhostart)
  dec <- TRUE
  while(dec==TRUE) {
    dec <- stc(x1=x1, x0=x0, alpha=alpha, rho=rho, steps=steps, verbose=FALSE)
    rho <- rho + inc
  }
  if(verbose==TRUE) {
    cat(paste("H0 at alpha=", alpha, " can no longer be rejected at rho=",
              rho - inc, ".", sep=""))
  } else {
    rho - inc
  }
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: The Rearrangement test by Hagemann (2020). ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# NOTE: there is no population weighting here.

### A table that shows the post-period effects for all municipalities. ###

est = df[, c(reg = as.list(coef(lm(otc ~ post)))), 
         by=c('municipality', 'group', 'value_type')]

# Loop over the following groups:
groups = c('all', 'bottom_40', 'top_40')

table.eff = lapply(groups, function(grp) {
  data = est[value_type=='detrend' & group==grp, .(municipality, reg.post)]
  data = data[order(-reg.post)]
  setnames(data, old=c('municipality', 'reg.post'), new=c('No.', 'Post'))
  
})
names(table.eff) = groups

table.eff = do.call(cbind.data.frame, table.eff)

# Tidy column names:
c_names = c('all.no', 'all.val', 'bot.no', 'bot.val', 'top.no', 'top.val')
colnames(table.eff) = c_names

table.eff = data.table(order=c(1:nrow(table.eff)), table.eff)

save.table(table.eff, output = table_hagemann, label_tex = 'table_dd_hagemann',
           title_tex = 'Differences in Means after Detrending.')


# CONCLUSION: even after detrending, the estimated "effects" (difference
# in means after and prior to the treatment) are large for some municipalities
# compared to Helsinki. Thus, the estimated effect in Helsinki is not an outlier
# compared to these "placebo effects". Hagemann's rearrangement test essentially
# requires any single cluster could be used as the counterfactual, thus
# ruling out cluster-specific heterogeneity in trends in untreated potential 
# outcomes. The data do not support this assumption. The test does not find
# any effect at 10% level for any rho:

rat = lapply(groups, function(grp) {
  stc.robust(
    x1=est[municipality==91 & value_type=='detrend' & group==grp, reg.post], 
    x0=est[municipality!=91 & value_type=='detrend' & group==grp, reg.post],
    alpha=0.1, rhostart = 0, steps=10^3, inc=.001)
})
rat


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Inference after removing the time series information. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Here, we aggregate the data so that each municipality has only 2 observations
# (before and after the abolition); Bertrand, Duflo and Mullainathan (2004).


# We will loop over the following subgroups:
groups = c('all', 'bottom_40', 'top_40')

results.se = lapply(groups, function(grp) {
  
  # First, get the right data:
  DT = df[value_type=='raw' & group==grp]
  
  # Estimate the linear pre-trend difference:
  ols.trend = fixest::feols(
    otc ~ hki + relative_time:hki | relative_time, 
    weights = DT[post==0, population], data=DT[post==0]
  )
  ols.trend = as.data.table(broom::tidy(ols.trend))

  # Subract the linear pre-trend difference from the data:
  DT[hki==0, otc := otc + 
       (ols.trend[term=='hki', estimate] + relative_time *
          ols.trend[term=='hki:relative_time', estimate])]
  
  # Aggregate the data - remove the time series information:
  DT = DT[, .(otc = mean(otc)),
          by=c('municipality', 'population', 'group', 'post', 'hki','pre_mean')]
  
  # Then, estimate the regressions:
  reg = fixest::feols(otc ~ post + hki:post | municipality, 
                      weights = DT$population, data=DT)
  
  # Standard errors:
  reg.het = summary(reg, vcov='hetero', ssc = ssc(adj=TRUE))
  reg.iid = summary(reg, vcov='iid', ssc = ssc(adj=TRUE))
  
  # Collect data to a table:
  
  pre_mean = format(round(DT[hki==1, unique(pre_mean)], digits=3), nsmall=3)
  estimate = format(round(reg$coefficients['post:hki'], digits=3), nsmall=3)
  change = 
    paste(format(round(100 * reg$coefficients['post:hki'] /
                         DT[hki==1, unique(pre_mean)], digits=2), nsmall=2), 
          '%', sep='')
  
  se.iid = paste(
    format(round(reg.iid$se['post:hki'], digits=3), nsmall=3), ' (p=', 
    format(round(reg.iid$coeftable['post:hki','Pr(>|t|)'], digits=3), nsmall=3), 
    ')', sep=''
  )
  
  se.het = paste(
    format(round(reg.het$se['post:hki'], digits=3), nsmall=3), ' (p=', 
    format(round(reg.het$coeftable['post:hki','Pr(>|t|)'], digits=3), nsmall=3), 
    ')', sep=''
  )
  
  data.table(
    Variable = c('Mean', 'Estimate', 'Change (%)', 'SE (IID)', 'SE (HC1)'),
    Value = c(pre_mean, estimate, change, se.iid, se.het)
  )
  
})

results.se = do.call(cbind.data.frame, results.se)[, c(1:2, 4, 6)]
colnames(results.se) = c('Variable', 'All', 'Bottom 40', 'Top 40')

save.table(
  results.se, output = table_se, label_tex = 'table_se_aggr',
  title_tex = 'Standard Errors after Removing the Time Series Dimension.')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Rambachan & Roth (2021) robust inference. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# We will loop over the following subgroups:
groups = c('all', 'bottom_40', 'top_40')

results.rob = lapply(groups, function(grp) {
  
  # Extract the data:
  DT = df[value_type=='raw' & group==grp]
  
  # Estimate the event-study-specification:
  reg = summary(
    fixest::feols(
      otc ~ i(relative_time, hki, ref=-1) | municipality + relative_time, 
      weights = DT$population, data=DT
    )
  )
  
  
  # Create l_vec to define the parameter of interest, the 
  # average of post-treatment effects:
  l_vector = rep(1/24, times=24)
  
  # Create vector of M values. First, estimate how much the trend moves
  # between consecutive periods before the treatment:
  
  trend = fixest::feols(otc ~ hki + relative_time:hki | relative_time,
                        weights = DT[relative_time < 0, population],
                        data=DT[relative_time < 0])

  trend = abs(trend$coefficients['hki:relative_time'])
  m_vector = unname(c(0, trend * c(0.05, 0.1, 0.15)))
  m_labels = c('0', '0.05PT', '0.10PT', '0.15PT')
  
  # Construct robust confidence intervals for Delta^{SD}(M):
  robust = HonestDiD::createSensitivityResults(
    betahat = reg$coefficients, sigma = reg$cov.iid,
    numPrePeriods=23, numPostPeriods=24, l_vec = l_vector,
    Mvec = m_vector, alpha=0.10
  )
  
  # Transform estimates to percentage changes:
  robust = as.data.table(robust)
  robust[, ':=' (lb_change = 100 * lb / DT[hki==1, unique(pre_mean)],
                 ub_change = 100 * ub / DT[hki==1, unique(pre_mean)])]
  print(robust)
  
  
  # Plot:
  
  # Title depends on the group:
  if(grp=='all') {
    gg_title = 'All individuals.'
  } else if (grp=='bottom_40') {
    gg_title = 'Bottom 40%.'
  } else if (grp=='top_40') {
    gg_title = 'Top 40%' }
  
  p.level = ggplot(data=robust, aes(x=M, ymin=lb, ymax=ub)) + 
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype='dashed') +
    ylab('Effect: GP Visits') + xlab('Smoothness Restriction') + 
    scale_x_continuous(breaks=m_vector, labels = m_labels) +
    ggtitle(gg_title) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
  p.change = ggplot(data=robust, aes(x=M, ymin=lb_change, ymax=ub_change)) + 
    geom_errorbar() +
    geom_hline(yintercept = 0, linetype='dashed') +
    ylab('Effect: Change GP Visits (%)') + xlab('Smoothness Restriction') + 
    scale_x_continuous(breaks=m_vector, labels = m_labels) +
    ggtitle(gg_title) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
  return(list(level=p.level, change=p.change))
  
})


# Save plots:

cairo_pdf(filename = plot_honestdid, width = 15, height = 10)
print(patchwork::wrap_plots(
  c(results.rob[[1]], results.rob[[2]], results.rob[[3]]), 
  byrow = FALSE, ncol=3)
)
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5: Leave-X-out estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that examines robustness by using leave-X-out estimation. ###

leave.x.out = function(data, x) {
  # INPUTS:
  # x = how many municipalities are left out for each run.
  # OUTPUTS:
  # regression results as a data.table
  
  
  # Copy the data to not mutate the original source:
  cols = colnames(data)
  DT = data[, mget(cols)]
  
  # List all two-municipality combinations that do not include Helsinki (91):
  
  munies = c(91, setdiff(unique(DT$municipality), 91))
  no_map = data.table(municipality = munies,
                      no = c(1:length(munies)))
  combinations = combn(c(2:length(munies)), x, simplify=FALSE)
  
  # Merge new municipality numbers to 'DT':
  DT = merge(DT, no_map, by="municipality", all.x = TRUE)
  
  
  # Loop over combinations to get a distribution of estimates:
  
  lxo = lapply(combinations, function(combo) {
    
    # First, get the right data:
    DT.lxo = DT[!(no %in% combo)]
    
    # Estimate the linear pre-trend difference:
    ols.trend = fixest::feols(
      otc ~ hki + relative_time:hki | relative_time, 
      weights = DT.lxo[post==0, population], data=DT.lxo[post==0]
    )
    ols.trend = as.data.table(broom::tidy(ols.trend))
    
    DT.lxo[hki==0, 
           otc := otc + 
             (ols.trend[term=='hki', estimate] + relative_time *
                ols.trend[term=='hki:relative_time', estimate])]
    
    # Then, estimate the regressions
    res = summary(fixest::feols(otc ~ post + hki:post | municipality, 
                                weights = DT.lxo$population, data = DT.lxo))
    
    # Municipalities that were dropped:
    dropped = DT[no %in% combo, unique(municipality)]
    dropped = paste(dropped, collapse=' & ')
    
    # Return results:
    res = data.table(estimate = res$coefficients['post:hki'],
                     change = 100 * res$coefficients['post:hki'] /
                       DT.lxo[hki==1, unique(pre_mean)],
                     dropped = dropped)
    
  })
  
  lxo = do.call(rbind.data.frame, lxo)
  lxo = lxo[order(-estimate)
            ][, ':=' (order = c(1:nrow(lxo)),
                      mean = mean(estimate),
                      mean_change = mean(change))]
  
  return(lxo)
  
}


### A function that plots the leave.x.out estimates. ###

plot.leave.x.out = function(data, x) {
  # INPUTS:
  # data: output from leave.x.out()
  # x = how many municipalities are left out for each run.
  # OUTPUTS:
  # a ggplot object
  
  
  DT = data[, mget(colnames(data))]
  
  # Collect largest and smallest values:
  minmax = c(DT[, min(estimate)], DT[, max(estimate)])
  minmax_change = c(DT[, min(change)], DT[, max(change)])
  
  # Signs that should be preserved after rounding:
  is.positive = minmax >= 0
  signs = ifelse(is.positive, '+', '-')
  
  # Round:
  minmax = format(abs(round(minmax, digits=3)), nsmall=3)
  minmax_change = format(abs(round(minmax_change, digits=1)), nsmall=1)
  
  # This text will be shown in the plot:
  plot_text = paste(
    'Largest: ', signs[2], minmax[2], ' (', signs[2], minmax_change[2], '%)\n',
    'Smallest: ', signs[1], minmax[1], ' (', signs[1], minmax_change[1], '%)',
    sep='')
  
  # Y-axis limits:
  y_max = DT[, max(estimate)]
  y_min = DT[, min(estimate)]
  difference = y_max - y_min
  if(y_min > 0) { y_min = 0 }
  
  # Plot:
  
  p = ggplot(data=DT, aes(x=order, y=estimate)) + 
    geom_point() +
    geom_hline(yintercept = 0, linetype='dashed') +
    geom_hline(yintercept = DT[, unique(mean)]) +
    ylim(y_min, (y_max + difference/6)) +
    ylab('Effect: GP Visits') +
    annotate(geom = 'label', x=DT[, max(order)], y=y_max, 
             label=plot_text, hjust=1, size=5) +
    theme(text = element_text(size=20),
          axis.text.x = element_text(angle = 90),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
  if(x==1) {
    p = p +
      scale_x_continuous(breaks = DT$order, labels = DT$dropped) +
      xlab('Excluded Municipality')
  } else if (x > 1) {
    p = p + 
      scale_x_continuous(breaks = c(1, length(DT$dropped))) +
      theme(axis.title.x = element_blank())
  }
  
  return(p)
  
}


# We will loop over the following subgroups:
groups = c('all', 'bottom_40', 'top_40')

plots.lxo = lapply(groups, function(grp) {
  
  # Extract the data:
  DT = df[value_type=='raw' & group==grp]
  
  # Conduct the estimation:
  l1o = leave.x.out(DT, x=1)
  l2o = leave.x.out(DT, x=2)
  l3o = leave.x.out(DT, x=3)
  
  # Title for the plots depends on the group:
  if(grp=='all') {
    gg_title = 'All individuals:'
  } else if (grp=='bottom_40') {
    gg_title = 'Bottom 40%:'
  } else if (grp=='top_40') {
    gg_title = 'Top 40%:' }
  
  p.1 = plot.leave.x.out(l1o, x=1) + ggtitle(paste(gg_title, 'leave-1-out.'))
  p.2 = plot.leave.x.out(l2o, x=2) + ggtitle(paste(gg_title, 'leave-2-out.'))
  p.3 = plot.leave.x.out(l3o, x=3) + ggtitle(paste(gg_title, 'leave-3-out.'))
  
  return(list(p.1, p.2, p.3))
  
})
  

# Save plots:

cairo_pdf(filename = plot_leave_x_out, width = 15, height = 12)
print(patchwork::wrap_plots(c(plots.lxo[[1]], plots.lxo[[2]], plots.lxo[[3]]), 
                            byrow = FALSE, ncol=3))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6: Sensitivity to deviations from linear trend-differences. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# We will loop over the following groups:
groups = c('all', 'bottom_40', 'top_40')

# We allow for trend change in the post-treatment period. 
# 1= linear pre-trend is extrapolated
# 1.5= the slope increases by 50%
# 0= no trend-difference is assumed
trend.post = seq(0, 2.5, by=0.2)


results.trends = lapply(groups, function(grp) {
  
  results = lapply(trend.post, function(slope) {
    
    # Extract the data:
    DT = df[value_type=='raw' & group==grp]
    
    # Estimate the linear pre-trend difference:
    ols.trend = fixest::feols(
      otc ~ hki + relative_time:hki | relative_time, 
      weights = DT[post==0, population], data=DT[post==0]
    )
    ols.trend = as.data.table(broom::tidy(ols.trend))
    
    # Detrend the outcome:
    DT[hki==0 & post==0, 
       otc := otc + (ols.trend[term=='hki', estimate] + relative_time *
                       ols.trend[term=='hki:relative_time', estimate])]
    DT[hki==0 & post==1, 
       otc := otc + 
         (ols.trend[term=='hki', estimate] + relative_time * 
            slope * ols.trend[term=='hki:relative_time', estimate])]
    
    # Estimate the results on the detrended data:
    reg = summary(fixest::feols(otc ~ post + hki:post | municipality, 
                                weights = DT$population, data = DT))
    
    # Results collected to a table:
    data.table(estimate = reg$coefficients['post:hki'],
               trend.post = slope,
               pre.mean = DT[hki==1, unique(pre_mean)])
    
  })
  
  results = do.call(rbind.data.frame, results)
  
  
  # Proceed to plotting:
  
  # Signs that should be preserved after rounding:
  sign = ifelse(results[trend.post==1, estimate] >= 0, '+', '-')
  
  # Round:
  est = format(abs(round(results[trend.post==1, estimate], digits=3)), nsmall=3)
  est_change = format(abs(
    round(results[trend.post==1, 100 * estimate / pre.mean], digits=1)), 
    nsmall=1)
  
  # This text will be shown in the plot:
  plot_text = paste('Baseline:\n',
                    sign, est, ' (', sign, est_change, '%)', sep='')
  
  # Title for the plots depends on the group:
  if(grp=='all') {
    gg_title = 'All individuals.'
  } else if (grp=='bottom_40') {
    gg_title = 'Bottom 40%.'
  } else if (grp=='top_40') {
    gg_title = 'Top 40%.' }
  
  # Plot:
  
  p = ggplot(data=results, aes(x=trend.post, y=estimate)) +
    geom_line() + 
    geom_point(data=results[trend.post==1], 
               aes(x=trend.post, y=estimate)) + 
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_vline(xintercept = 1, linetype = 'dashed') +
    annotate(geom = 'label', x=0.9, 
             y=results[trend.post==1, estimate], 
             label=plot_text, hjust=1, vjust=1, size=5) +
    scale_x_continuous(breaks = seq(0,2, by=0.5)) + 
    xlab('Pre-Trend Slope Multiplier') + 
    ylab('Effect: GP Visits') + 
    ggtitle(gg_title) + 
    theme(text = element_text(size=20),
          panel.background = element_rect(fill='white', colour='white'),
          panel.grid.major = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype='solid', 
                                          colour='lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
})
names(results.trends) = groups

# Save plots:

cairo_pdf(filename = plot_pta_sensitivity, width = 15, height = 5)
patchwork::wrap_plots(results.trends)
dev.off()

print('ready')

# End.
