# load("src/cbma_water_table.R")
aridity_contrasts_mas2 = function(pc,
                                 arid_factor){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(pc ~ arid_factor*mas_bin*site + scale(date), data = aw6)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma1  = c(1,0,0,0,0,0,0,0,0,0,0,0)
  lwma2  = c(0,1,0,0,0,0,0,0,0,0,0,0)
  lwma3  = c(0,0,1,0,0,0,0,0,0,0,0,0)
  sswma1 = c(0,0,0,1,0,0,0,0,0,0,0,0)
  sswma2 = c(0,0,0,0,1,0,0,0,0,0,0,0)
  sswma3 = c(0,0,0,0,0,1,0,0,0,0,0,0)
  cbma1  = c(0,0,0,0,0,0,1,0,0,0,0,0)
  cbma2  = c(0,0,0,0,0,0,0,1,0,0,0,0)
  cbma3  = c(0,0,0,0,0,0,0,0,1,0,0,0)
  kiowa1 = c(0,0,0,0,0,0,0,0,0,1,0,0)
  kiowa2 = c(0,0,0,0,0,0,0,0,0,0,1,0)
  kiowa3 = c(0,0,0,0,0,0,0,0,0,0,0,1)
  
  emm = emmeans(m, ~ arid_factor*site|mas_bin)
  emm_cntrst = contrast(emm,
                        method = list(
                          "SSWMA:Humid - LWMA:Humid" = sswma1-lwma1,
                          "CBMA:Humid - LWMA:Humid"  = cbma1 -lwma1,
                          "KIOWA:Humid - LWMA:Humid" = kiowa1-lwma1,
                          "CBMA:Humid - SSWMA:Humid"  = cbma1 -sswma1,
                          "KIOWA:Humid - SSWMA:Humid" = kiowa1-sswma1,
                          "KIOWA:Humid - CBMA:Humid" = kiowa1-cbma1,
                          "SSWMA:Normal - LWMA:Normal" = sswma2-lwma2,
                          "CBMA:Normal - LWMA:Normal"  = cbma2 -lwma2,
                          "KIOWA:Normal - LWMA:Normal" = kiowa2-lwma2,
                          "CBMA:Normal - SSWMA:Normal"  = cbma2 -sswma2,
                          "KIOWA:Normal - SSWMA:Normal" = kiowa2-sswma2,
                          "KIOWA:Normal - CBMA:Normal" = kiowa2-cbma2,
                          "SSWMA:Arid - LWMA:Arid" = sswma3-lwma3,
                          "CBMA:Arid - LWMA:Arid"  = cbma3 -lwma3,
                          "KIOWA:Arid - LWMA:Arid" = kiowa3-lwma3,
                          "CBMA:Arid - SSWMA:Arid"  = cbma3-sswma3,
                          "KIOWA:Arid - SSWMA:Arid" = kiowa3-sswma3,
                          "KIOWA:Arid - CBMA:Arid" = kiowa3-cbma3
                        ))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  # emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # arid_table = aridity_table_mas2(emm_cntrst);arid_table
  my_list = list(summary, 
                 diagnostics, 
                 emm_cntrst_summary,
                 # emm_confi_summary,
                 # arid_table,
                 emm)
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}

