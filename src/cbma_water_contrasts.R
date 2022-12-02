load("src/cbma_water_table.R")
cbma_water_contrasts = function(data,
                                 pc){
  m = lm(pc ~ ws_site*water*arid_within + mas_bin + date, data = data)
  summary = summary(m)
  diagnostics = assump(m)
  emm = emmeans(m, pairwise ~ ws_site*water|arid_within)
  # # Setting up comparisons for emmeans contrast function
  # ws1w0 = c(1,0,0,0,0,0)
  # ws2w0 = c(0,1,0,0,0,0)
  # ws3w0 = c(0,0,1,0,0,0)
  # ws1w1 = c(0,0,0,1,0,0)
  # ws2w1 = c(0,0,0,0,1,0)
  # ws3w1 = c(0,0,0,0,0,1)
  emm = emmeans(m, pairwise ~ ws_site*water|arid_within)
  emm_cntrst = contrast(emm, "trt.vs.ctrl", ref = "ws_site2 water1", exclude = 2)

  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  cbmaw_table = cbma_water_table(emm_cntrst);cbmaw_table
  my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, cbmaw_table)
  return(my_list)
}
 
