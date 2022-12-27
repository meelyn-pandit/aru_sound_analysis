ece_contrast_mas = function(data,pc){

  m = lm(pc ~ site + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma  = c(1,0,0,0)
  sswma = c(0,1,0,0)
  cbma  = c(0,0,1,0)
  kiowa = c(0,0,0,1)
  
  emm = emmeans(m, ~ site)
  emm_cntrst = contrast(emm,
                        method = list(
                          "SSWMA - LWMA" = sswma-lwma,
                          "CBMA - LWMA"  = cbma -lwma,
                          "KIOWA - LWMA" = kiowa-lwma,
                          "CBMA - SSWMA" = cbma -sswma,
                          "KIOWA - SSWMA"= kiowa-sswma,
                          "KIOWA - CBMA" = kiowa-cbma
                        ))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  ece_table = ece_table(emm_cntrst)
  my_list = list(summary, 
                 diagnostics, 
                 emm_cntrst_summary,
                 emm_confi_summary,
                 ece_table,
                 emm)
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}
