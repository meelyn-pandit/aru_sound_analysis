
# LMM Analysis ------------------------------------------------------------

ag_lmm = function(data,
                  yvar,
                  xvar,
                  convar,
                  grpvar){
  m = lmer(yvar ~ xvar*mas_bin*site + (1|date/mas_bin), data = data,
           control=lmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun=2e5)),REML = FALSE)
  summary = summary(m)
  diagnostics = assump(m)
  emm_options(lmerTest.limit = 54000,
              pbkrtest.limit = 54000)
  
  
  if(convar == "site"){
    emm = emtrends(m, ~ site|mas_bin, 
                   var = "xvar", 
                   type = 'response',
                   weights = "cells")
    plot(emm)
    lwma  = c(1,0,0,0)
    sswma = c(0,1,0,0)
    cbma  = c(0,0,1,0)
    kiowa = c(0,0,0,1)
    emm_contrast = contrast(emm, 
                            method = list(
                              "SSWMA - LWMA" = sswma-lwma,
                              "CBMA - LWMA"  = cbma -lwma,
                              "KIOWA - LWMA" = kiowa-lwma,
                              "CBMA - SSWMA"  = cbma -sswma,
                              "KIOWA - SSWMA" = kiowa-sswma,
                              "KIOWA - CBMA" = kiowa-cbma),
                            adjust = "bonferroni")
    
  } else if(convar == "mas_bin"){
    emm = emtrends(m, ~ mas_bin|site, 
                   var = "xvar", 
                   type = 'response',
                   weights = "cells")
    
    predawn = c(1,0,0,0)
    early = c(0,1,0,0)
    mid  = c(0,0,1,0)
    late = c(0,0,0,1)
    emm_contrast = contrast(emm, 
                            method = list(
                              "Early - Predawn" = early-predawn,
                              "Mid - Predawn"  = mid-predawn,
                              "Late - Predawn" = late-predawn,
                              "Mid - Early"  = mid-early,
                              "Late - Early" = late-early,
                              "Late - Mid" = late-mid),
                            adjust = "bonferroni")
  }

  slopes_table = summary(emm)
  emm_con_sum = summary(emm_contrast)
  mylist = list(summary,diagnostics,slopes_table,emm_con_sum)
}

