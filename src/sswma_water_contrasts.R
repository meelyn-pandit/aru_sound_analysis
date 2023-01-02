# load("src/sswma_water_table.R")
sswma_water_contrasts = function(data,
                                 pc){
  m = lm(pc ~ ws_site*water*arid_withinf + mas_bin + date, data = data)
  summary = summary(m)
  diagnostics = assump(m)
  emm = emmeans(m, ~ ws_site*water|arid_withinf)
  # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0,0,0)
  ws2w0 = c(0,1,0,0,0,0)
  ws3w0 = c(0,0,1,0,0,0)
  ws1w1 = c(0,0,0,1,0,0)
  ws2w1 = c(0,0,0,0,1,0)
  ws3w1 = c(0,0,0,0,0,1)
  emm_cntrst = contrast(emm,
                        method = list("ws_site1 water 1 - ws_site2 water0" = ws1w1-ws2w0,
                                      "ws_site2 water 1 - ws_site1 water0" = ws2w1-ws1w0,
                                      "ws_site1 water 1 - ws_site3 water0" = ws1w1-ws3w0,
                                      "ws_site2 water 1 - ws_site3 water0" = ws2w1-ws3w0))
  
  emm_cntrst_summary = summary(emm_cntrst)
  # emm_cntrst_summary = summary(emm$contrasts)
  
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # sswma_pairwise_pc = sswma_water_table(emm_cntrst);sswma_pairwise_pc
  my_list = list(summary, diagnostics, emm_cntrst_summary,
                  emm_confi_summary
                 # , sswma_pairwise_pc
                 )
  return(my_list)
}
 
