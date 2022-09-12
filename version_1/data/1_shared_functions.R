
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 1_shared_functions.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: Helsinki      ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Functions that are used in many R scripts.

# Install and load the following packages:
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file.


###
###


### A function that saves tables in three formats: tex, xlsx, rds ###

save.table = function(table, output, label_tex, title_tex) {
  # INPUTS:
  # table: a table to be saved
  # output: file path excluding the file type (e.g. .csv)
  # label_tex: character label for .tex tables
  # title_tex: character title for .tex tables
  
  # tex:
  stargazer::stargazer(
    table, type = "text", summary = F,
    out = paste(output, "tex", sep = "."),
    title = title_tex, label = label_tex,
    rownames = F, header=F)
  
  # xlsx:
  openxlsx::write.xlsx(table, file = paste(output, "xlsx", sep = "."),
                       overwrite = TRUE)
  
  #rds:
  saveRDS(table, file = paste(output, "rds", sep = "."))
  
}


### A function that tidies the results tables. ###

create.table = function(table, rows, columns) {
  # INPUTS:
  # table: 'table.gp', 'table.rest', 'table.dent', 'table.has.fe' or 'table.pta'
  # rows: the number of rows in the final table
  # columns: the number of columns in the final table
  # OUTPUT: 
  # a tidied results table as a data.table
  
  
  # Loop over cells (= result table rows):
  
  cells = lapply(c(1:nrow(table)), function(i) {
    
    # Get the results:
    x = table[i, ]
    
    # Tidy and transform to character:
    
    results = 
      data.table(value = c(
        # Pre-mean and estimate:
        format(round(c(x$pre.mean, x$estimate), digits=3), nsmall=3),
        
        # % Change:
        paste(format(round(x$change, digits=2), nsmall=2), '%', sep=''),
        
        # Analytical std. errors and respective p-values:
        
        paste(format(round(x$std.error, digits=3), nsmall=3), ' (p=', 
              format(round(x$p.value, digits=3), nsmall=3), ')', sep=''),
        
        paste(format(round(x$std.error.muni, digits=3), nsmall=3), ' (p=', 
              format(round(x$p.muni, digits=3), nsmall=3), ')', sep=''),
        
        # Wild cluster bootstrap confidence intervals:
        
        paste('[', format(round(x$conf.low.wcu, digits=3), nsmall=3), '; ', 
              format(round(x$conf.high.wcu, digits=3), nsmall=3), ']', sep=''),
        
        paste('[', format(round(x$conf.low.wcr, digits=3), nsmall=3), '; ', 
              format(round(x$conf.high.wcr, digits=3), nsmall=3), ']', sep=''),
        
        # Sample sizes:
        as.character(x$n.persons)
      )
      )
    
  })
  
  # Bind the results to a table:
  
  tab.rows = lapply(seq(1, rows*columns, by=columns), function(i) {
    
    cols = lapply(c(0:(columns-1)), function(j) {
      cells[i+j]
    })
    cols = do.call(cbind.data.frame, cols)
    
  })
  tab.rows = do.call(rbind.data.frame, tab.rows)
  
  results = data.table(
    metric=rep(c('Mean', 'Estimate', 'Change (%)', 'SE (postal code)', 
                 'SE (municipality)', 'CI WCU', 'CI WCR', 'Individuals'), 
               times=rows)
  )
  results = cbind(results, tab.rows)
  
}

# End.
