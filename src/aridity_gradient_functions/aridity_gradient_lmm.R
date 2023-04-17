
# LMM Analysis ------------------------------------------------------------

ag_lmm = function(data,
                  yvar,
                  xvar){
  
  m = lmer(yvar ~ xvar*mas_bin + scale(date) + (1|site), data = data,
           control=lmerControl(optimizer="bobyqa", 
                               optCtrl = list(maxfun=2e5)),REML = FALSE)
  summary = summary(m)
  diagnostics = assump(m)
  emm_options(lmerTest.limit = 54000
              # ,
              # pbkrtest.limit = 54000
              )
  
    emm = emtrends(m, ~ mas_bin, 
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

  slopes_table = summary(emm)
  emm_con_sum = summary(emm_contrast)
  mylist = list(summary,diagnostics,slopes_table,emm_con_sum)
}

