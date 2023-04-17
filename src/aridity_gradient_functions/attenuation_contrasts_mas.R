# load("src/cbma_water_table.R")
attenuation_contrasts_mas = function(data,
                                 x1,
                                 x2,
                                 x3,
                                  y){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(y ~ x1*x2 + x3 + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma0  = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma1  = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma2  = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma3  = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma0 = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
  sswma1 = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  sswma2 = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
  sswma3 = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  cbma0  = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  cbma1  = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  cbma2  = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  cbma3  = c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  kiowa0 = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
  kiowa1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  kiowa2 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
  kiowa3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  
  emm = emmeans(m, pairwise ~ x1*x2)
  emm_cntrst = contrast(emm, 
               method = list(
               "SSWMA:Pre Dawn - LWMA:Pre Dawn" = sswma0-lwma0,
               "CBMA:Pre Dawn - LWMA:Pre Dawn"  = cbma0 -lwma0,
               "KIOWA:Pre Dawn - LWMA:Pre Dawn" = kiowa0-lwma0,
               "CBMA:Pre Dawn - SSWMA:Pre Dawn"  = cbma0 -sswma0,
               "KIOWA:Pre Dawn - SSWMA:Pre Dawn" = kiowa0-sswma0,
               "KIOWA:Pre Dawn - CBMA:Pre Dawn" = kiowa0-cbma0,
               "SSWMA:Early - LWMA:Early" = sswma1-lwma1,
               "CBMA:Early - LWMA:Early"  = cbma1 -lwma1,
               "KIOWA:Early - LWMA:Early" = kiowa1-lwma1,
               "CBMA:Early - SSWMA:Early"  = cbma1 -sswma1,
               "KIOWA:Early - SSWMA:Early" = kiowa1-sswma1,
               "KIOWA:Early - CBMA:Early" = kiowa1-cbma1,
               "SSWMA:Mid - LWMA:Mid" = sswma2-lwma2,
               "CBMA:Mid - LWMA:Mid"  = cbma2 -lwma2,
               "KIOWA:Mid - LWMA:Mid" = kiowa2-lwma2,
               "CBMA:Mid - SSWMA:Mid"  = cbma2 -sswma2,
               "KIOWA:Mid - SSWMA:Mid" = kiowa2-sswma2,
               "KIOWA:Mid - CBMA:Mid" = kiowa2-cbma2,
               "SSWMA:Late - LWMA:Late" = sswma3-lwma3,
               "CBMA:Late - LWMA:Late"  = cbma3 -lwma3,
               "KIOWA:Late - LWMA:Late" = kiowa3-lwma3,
               "CBMA:Late - SSWMA:Late"  = cbma3 -sswma3,
               "KIOWA:Late - SSWMA:Late" = kiowa3-sswma3,
               "KIOWA:Late - CBMA:Late" = kiowa3-cbma3
               ))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  arid_table = attenuation_table_mas(emm_cntrst);arid_table
  my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, arid_table, emm)
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}
 
