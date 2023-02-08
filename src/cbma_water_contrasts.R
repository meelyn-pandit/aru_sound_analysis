# load("src/cbma_water_table.R")
cbma_water_contrasts = function(data,
                                 pc){
  m = lm(pc ~ ws_site*water*gh*mas_bin + date, data = data)
  summary = summary(m)
  diagnostics = assump(m)
  emm = emtrends(m, ~ ws_site*water|mas_bin, var = "gh", type = 'response',weights = "cells")
  # # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0)
  ws2w0 = c(0,1,0,0)
  ws1w1 = c(0,0,1,0)
  ws2w1 = c(0,0,0,1)
  
# Only comparisons to site 2. Site1Open: Site2Open. Site1Closed: Site2Open
    emm_cntrst = contrast(emm, 
                          method = list("ws_site1 water 1 - ws_site2 water1" = ws1w1-ws2w1,
                                        "ws_site1 water 0 - ws_site2 water1" = ws1w0-ws2w1))

  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # cbmaw_table = cbma_water_table(emm_cntrst);cbmaw_table
  my_list = list(summary, 
                 diagnostics, 
                 summary(emm),
                 emm_cntrst_summary, 
                 emm_confi_summary 
                 # cbmaw_table
                 )
  return(my_list)
}
 
