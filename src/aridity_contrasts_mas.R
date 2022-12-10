# load("src/cbma_water_table.R")
aridity_contrasts_mas = function(data,
                                 x1,
                                 x2,
                                 x3,
                                  y){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(y ~ x1*x2*x3 + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma1  = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma2  = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma3  = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma4  = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma5  = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma1 = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma2 = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma3 = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma4 = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
  sswma5 = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  cbma1  = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
  cbma2  = c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  cbma3  = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  cbma4  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  cbma5  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  kiowa1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  kiowa2 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
  kiowa3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  kiowa4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
  kiowa5 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  
  emm = emmeans(m, pairwise ~ x1*x3|x2)
  emm_cntrst = contrast(emm, 
               method = list(
               "SSWMA:Extremely Humid - LWMA:Extremely Humid" = sswma1-lwma1,
               "CBMA:Extremely Humid - LWMA:Extremely Humid"  = cbma1 -lwma1,
               "KIOWA:Extremely Humid - LWMA:Extremely Humid" = kiowa1-lwma1,
               "CBMA:Extremely Humid - SSWMA:Extremely Humid"  = cbma1 -sswma1,
               "KIOWA:Extremely Humid - SSWMA:Extremely Humid" = kiowa1-sswma1,
               "KIOWA:Extremely Humid - CBMA:Extremely Humid" = kiowa1-cbma1,
               "SSWMA:Humid - LWMA:Humid" = sswma2-lwma2,
               "CBMA:Humid - LWMA:Humid"  = cbma2 -lwma2,
               "KIOWA:Humid - LWMA:Humid" = kiowa2-lwma2,
               "CBMA:Humid - SSWMA:Humid"  = cbma2 -sswma2,
               "KIOWA:Humid - SSWMA:Humid" = kiowa2-sswma2,
               "KIOWA:Humid - CBMA:Humid" = kiowa2-cbma2,
               "SSWMA:Normal - LWMA:Normal" = sswma3-lwma3,
               "CBMA:Normal - LWMA:Normal"  = cbma3 -lwma3,
               "KIOWA:Normal - LWMA:Normal" = kiowa3-lwma3,
               "CBMA:Normal - SSWMA:Normal"  = cbma3 -sswma3,
               "KIOWA:Normal - SSWMA:Normal" = kiowa3-sswma3,
               "KIOWA:Normal - CBMA:Normal" = kiowa3-cbma3,
               "SSWMA:Arid - LWMA:Arid" = sswma4-lwma4,
               "CBMA:Arid - LWMA:Arid"  = cbma4 -lwma4,
               "KIOWA:Arid - LWMA:Arid" = kiowa4-lwma4,
               "CBMA:Arid - SSWMA:Arid"  = cbma4 -sswma4,
               "KIOWA:Arid - SSWMA:Arid" = kiowa4-sswma4,
               "KIOWA:Arid - CBMA:Arid" = kiowa4-cbma4,
               "SSWMA:Extremely Arid - LWMA:Extremely Arid" = sswma5-lwma5,
               "CBMA:Extremely Arid - LWMA:Extremely Arid"  = cbma5 -lwma5,
               "KIOWA:Extremely Arid - LWMA:Extremely Arid" = kiowa5-lwma5,
               "CBMA:Extremely Arid - SSWMA:Extremely Arid"  = cbma5 -sswma5,
               "KIOWA:Extremely Arid - SSWMA:Extremely Arid" = kiowa5-sswma5,
               "KIOWA:Extremely Arid - CBMA:Extremely Arid" = kiowa5-cbma5
               ))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  arid_table = aridity_table_mas(emm_cntrst);arid_table
  my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, arid_table, emm)
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}
 
