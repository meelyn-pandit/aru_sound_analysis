# load("src/cbma_water_table.R")
aridity_contrasts_lmer = function(data,
                                 pc){
  m = lmer(pc ~ arid_withinf*site + mas_bin + date + (1|site), data = data, REML = FALSE)
  summary = summary(m)
  diagnostics = assump(m)
  emm = emmeans(m, pairwise ~ site*arid_withinf)
  # # Setting up comparisons for emmeans contrast function
  lwma  = c(1,0,0,0)
  sswma = c(0,1,0,0)
  cbma  = c(0,0,1,0)
  kiowa = c(0,0,0,1)

  emm = emmeans(m, pairwise ~ site|arid_withinf)
  emm_cntrst = contrast(emm, method = list("SSWMA - LWMA" = sswma-lwma,
                                           "CBMA - LWMA"  = cbma -lwma,
                                           "KIOWA - LWMA" = kiowa-lwma,
                                           "CBMA - SSWMA" = cbma-sswma,
                                           "KIOWA - SSWMA" = kiowa-sswma,
                                           "KIOWA - CBMA" = kiowa-cbma))

  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  arid_table = aridity_table(emm_cntrst);arid_table
  my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, arid_table)
  return(my_list)
}
 
