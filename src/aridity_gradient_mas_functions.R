# load("src/cbma_water_table.R")
aridity_contrasts_mas = function(pc,
                                 arid_factor){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(pc ~ arid_factor*mas_bin*site + scale(date), data = aw6)
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
  
  emm = emmeans(m, ~ arid_factor*site|mas_bin)
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
  # arid_table = aridity_table_mas2(emm_cntrst_summary);arid_table
  arid_table = aridity_table_mas2(emm_confi_summary);arid_table
  
  my_list = list(summary, 
                 diagnostics, 
                 emm_cntrst_summary,
                 emm_confi_summary,
                 arid_table,
                 summary(emm)) # NEEDS TO BE SUMMARY OF EMM TO BE A DATAFRAME!!!
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}

aridity_table_mas2 = function(contrast_table) {
  
  ### Making good tables in R
  table_predawn <- contrast_table[contrast_table$mas_bin==0,] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = determine_sig(p.value)) %>%
    mutate(p.value = as.factor(p.value)) %>%
    mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
    dplyr::select(-mas_bin)
  
  
  table_early <- contrast_table[contrast_table$mas_bin==1,] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      estimate_e = round(estimate, 3),
      SE_e = round(SE, 3),
      t.ratio_e = round(t.ratio, 3),
      p.value_e = round(p.value, 3)) %>%
    mutate(sig._e = determine_sig(p.value_e)) %>%
    mutate(p.value_e = as.factor(p.value_e)) %>%
    mutate(p.value_e = dplyr::recode(p.value_e, 
                                     "0" = "<0.001")) %>%
    dplyr::select(-mas_bin, -contrast, -estimate, -SE, -t.ratio,-p.value, -df)
  
  
  table_mid <- contrast_table[contrast_table$mas_bin==2,] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      estimate_m = round(estimate, 3),
      SE_m = round(SE, 3),
      t.ratio_m = round(t.ratio, 3),
      p.value_m = round(p.value, 3)) %>%
    mutate(sig._m = determine_sig(p.value_m)) %>%
    mutate(p.value_m = as.factor(p.value_m)) %>%
    mutate(p.value_m = dplyr::recode(p.value_m, 
                                     "0" = "<0.001")) %>%
    dplyr::select(-mas_bin, -contrast, -estimate, -SE, -t.ratio,-p.value, -df)
  
  table_late <- contrast_table[contrast_table$mas_bin==3,] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      estimate_l = round(estimate, 3),
      SE_l = round(SE, 3),
      t.ratio_l = round(t.ratio, 3),
      p.value_l = round(p.value, 3)) %>%
    mutate(sig._l = determine_sig(p.value_l)) %>%
    mutate(p.value_l = as.factor(p.value_l)) %>%
    mutate(p.value_l = dplyr::recode(p.value_l, 
                                     "0" = "<0.001")) %>%
    dplyr::select(-mas_bin, -contrast, -estimate, -SE, -t.ratio,-p.value, -df)
  
  table_combined_horizontal = cbind(table_predawn, 
                                    table_early,
                                    table_mid,
                                    table_late) %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**MAS Bin 0 - Predawn**"),
      columns = c(estimate, SE, df, t.ratio, p.value, sig.)) %>%
    tab_spanner(
      label = md("**MAS Bin 1 - Early**"),
      columns = c(estimate_e, SE_e, t.ratio_e, p.value_e, sig._e)
    ) %>% 
    tab_spanner(
      label = md("**MAS Bin 2 - Mid**"),
      columns = c(estimate_m, SE_m, t.ratio_m, p.value_m, sig._m)
    ) %>%
    tab_spanner(
      label = md("**MAS Bin 3 - Late**"),
      columns = c(estimate_l, SE_l, t.ratio_l, p.value_l, sig._l)
    ) %>%
    cols_align('center') %>%
    cols_label(contrast = md("**Contrast**"),
               estimate = md("**Estimate**"),
               SE = md("**SE**"),
               df = md("**d.f.**"),
               t.ratio = md("**t-ratio**"),
               p.value = md("**p value**"),
               sig. = md("**sig.**")) %>%
    cols_label(estimate_e = md("**Estimate**"),
               SE_e = md("**SE**"),
               # df = md("**d.f.**"),
               t.ratio_e = md("**t-ratio**"),
               p.value_e = md("**p value**"),
               sig._e = md("**sig.**")) %>%
    cols_label(estimate_m = md("**Estimate**"),
               SE_m = md("**SE**"),
               # df = md("**d.f.**"),
               t.ratio_m = md("**t-ratio**"),
               p.value_m = md("**p value**"),
               sig._m = md("**sig.**")) %>%
    cols_label(estimate_l = md("**Estimate**"),
               SE_l = md("**SE**"),
               # df = md("**d.f.**"),
               t.ratio_l = md("**t-ratio**"),
               p.value_l = md("**p value**"),
               sig._l = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman")
  return(table_combined_horizontal)
}

aridity_graph_table = function(emm_summary,
                               y_label,
                               results){
  table_predawn <- emm_summary[emm_summary$mas_bin==0,]
  table_early <- emm_summary[emm_summary$mas_bin==1,]
  table_mid <- emm_summary[emm_summary$mas_bin==2,] 
  table_late <- emm_summary[emm_summary$mas_bin==3,] 
  
  table_combined_graph = rbind(table_predawn,
                               table_early,
                               table_mid,
                               table_late)
  
  ### PC1 - Acoustic Diversity
  ggplot(data = table_combined_graph,
         aes(x=arid_factor, y=emmean, color = site)) +
    geom_point(position = position_dodge(0))+
    # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
    geom_line(aes(group = site, 
                  color = site),
              position = position_dodge(0))+
    geom_errorbar(aes(ymin = emmean-SE, # Standard Error
                      ymax = emmean+SE), width = 0.5,
                  position = position_dodge(0))+
    # geom_errorbar(aes(ymin = lower.CL, # Confidence Interval
    #                   ymax = upper.CL), width = 0.5,
    #               position = position_dodge(0))+
    scale_color_manual(values = cbpalette, 
                       name = "Site",
                       labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
    # scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid")) +
    scale_x_discrete(name = "Aridity - Normalized Within") +
    scale_y_continuous(name = y_label)+
    facet_grid(. ~ mas_bin) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "bottom") +
    # facet_wrap(vars(mas_bin)) + 
    theme(strip.text.y = element_text(angle = 0))
  ggsave(results, dpi = 600, height = 6, width = 8, units = "in")
}
