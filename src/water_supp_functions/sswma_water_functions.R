# load("src/sswma_water_table.R")
sswma_water_contrasts = function(data,
                                 yvar,
                                 xvar){
  # m = lm(pc ~ ws_site*water*arid_withinf+mas_bin + date, data = data)
  m = lm(yvar ~ ws_site*water*xvar*mas_bin + date, data = data)
  
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, ~ ws_site*water|arid_withinf)
  emm = emtrends(m, ~ ws_site*water|mas_bin, var = "xvar", type = 'response',weights = "cells")
  # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0,0,0)
  ws2w0 = c(0,1,0,0,0,0)
  ws3w0 = c(0,0,1,0,0,0)
  ws1w1 = c(0,0,0,1,0,0)
  ws2w1 = c(0,0,0,0,1,0)
  ws3w1 = c(0,0,0,0,0,1)
  emm_cntrst = contrast(emm,
                        method = list("Site1:Open - Site2:Closed" = ws1w1-ws2w0,
                                      "Site2:Open - Site1:Closed" = ws2w1-ws1w0,
                                      "Site1:Open - Site3:Closed" = ws1w1-ws3w0,
                                      "Site2:Open - Site3:Closed" = ws2w1-ws3w0),
                        adjust = "bonferroni")
  
  emm_cntrst_summary = summary(emm_cntrst)
  # emm_cntrst_summary = summary(emm$contrasts)
  
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # sswma_pairwise_pc = sswma_water_table(emm_cntrst);sswma_pairwise_pc
  my_list = list(summary, 
                 diagnostics, 
                 summary(emm),
                 emm_cntrst_summary,
                 emm_confi_summary
                 # , sswma_pairwise_pc
                 )
  return(my_list)
}
 
sswma_water_table = function(contrast_table) {
  ### Making good tables in R
  table <- contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = determine_sig(p.value)) %>%
    mutate(p.value = as.factor(p.value)) %>%
    mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
    # dplyr::select(-c("arid_within")) %>%
    gt(.) %>%
    cols_align('center') %>%
    cols_label(contrast = md("**Contrast**"),
               estimate = md("**Estimate**"),
               SE = md("**SE**"),
               df = md("**d.f.**"),
               t.ratio = md("**t-ratio**"),
               p.value = md("**p value**"),
               sig. = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman") %>%
    tab_row_group(
      label = md("**Extremely Arid**"),
      rows = c(17:20)) %>%
    tab_row_group(
      label = md("**Arid**"),
      rows = c(13:16)) %>%
    tab_row_group(
      label = md("**Normal**"),
      rows = c(9:12)) %>%
    tab_row_group(
      label = md("**Humid**"),
      rows = c(5:8)) %>%
    tab_row_group(
      label = md("**Extremely Humid**"),
      rows = c(1:4)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    ); table
}

sswma_water_table2 = function(pc1_contrast_table,
                              pc2_contrast_table,
                              pc3_contrast_table) {
  
  ### PC1 Table
  table1 <- pc1_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate1 = round(estimate, 3),
           SE1 = round(SE, 3),
           df1 = df,
           t.ratio1 = round(t.ratio, 3),
           p.value1 = round(p.value, 3)) %>%
    mutate(sig.1 = determine_sig(p.value1)) %>%
    mutate(p.value1 = as.factor(p.value1)) %>%
    mutate(p.value1 = dplyr::recode(p.value1, "0" = "<0.001")) %>%
    dplyr::select(-arid_withinf, -estimate, -SE, -t.ratio, -p.value, -df)
  
  ### PC2 Table
  table2 <- pc2_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate2 = round(estimate, 3),
           SE2 = round(SE, 3),
           t.ratio2 = round(t.ratio, 3),
           p.value2 = round(p.value, 3)) %>%
    mutate(sig.2 = determine_sig(p.value2)) %>%
    mutate(p.value2 = as.factor(p.value2)) %>%
    mutate(p.value2 = dplyr::recode(p.value2, "0" = "<0.001")) %>%
    dplyr::select(-contrast, -arid_withinf, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
  ### PC3 Table
  table3<- pc3_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate3 = round(estimate, 3),
           SE3 = round(SE, 3),
           t.ratio3 = round(t.ratio, 3),
           p.value3 = round(p.value, 3)) %>%
    mutate(sig.3 = determine_sig(p.value3)) %>%
    mutate(p.value3 = as.factor(p.value3)) %>%
    mutate(p.value3 = dplyr::recode(p.value3, "0" = "<0.001")) %>%
    dplyr::select(-contrast,-arid_withinf, -estimate, -SE, -t.ratio, -p.value, -df) 
  
  sswma_tables_combined = cbind(table1, table2, table3)
  sswma_tables_combined2 = sswma_tables_combined[1:16,] %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**PC1 - Acoustic Diversity**"),
      columns = c(estimate1, SE1, df1, t.ratio1, p.value1, sig.1)) %>%
    tab_spanner(
      label = md("**PC2 - Avian Abundance**"),
      columns = c(estimate2, SE2, t.ratio2, p.value2, sig.2)
    ) %>% 
    tab_spanner(
      label = md("**PC3 - Acoustic Complexity**"),
      columns = c(estimate3, SE3, t.ratio3, p.value3, sig.3)
    ) %>%
    cols_label(contrast = md("**Contrast**"),
               estimate1 = md("**Estimate**"),
               SE1 = md("**SE**"),
               df1 = md("**d.f.**"),
               t.ratio1 = md("**t-ratio**"),
               p.value1 = md("**p value**"),
               sig.1 = md("**sig.**")) %>%
    cols_label(estimate2 = md("**Estimate**"),
               SE2 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio2 = md("**t-ratio**"),
               p.value2 = md("**p value**"),
               sig.2 = md("**sig.**")) %>%
    cols_label(estimate3 = md("**Estimate**"),
               SE3 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio3 = md("**t-ratio**"),
               p.value3 = md("**p value**"),
               sig.3 = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman") %>%
    # tab_row_group(
    #   label = md("**Extremely Arid**"),
    #   rows = c(17:20)) %>%
    tab_row_group(
      label = md("**Arid**"),
      rows = c(13:16)) %>%
    tab_row_group(
      label = md("**Normal**"),
      rows = c(9:12)) %>%
    tab_row_group(
      label = md("**Humid**"),
      rows = c(5:8)) %>%
    tab_row_group(
      label = md("**Extremely Humid**"),
      rows = c(1:4)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(sswma_tables_combined2)
}

sswma_water_table3 = function(pc1_contrast_table,
                              pc2_contrast_table,
                              pc3_contrast_table) {
  
  ### PC1 Table
  table1 <- pc1_contrast_table %>%
  # table1 = sswma_lag_pc1[[4]] %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate1 = round(estimate, 3),
           SE1 = round(SE, 3),
           df1 = df,
           t.ratio1 = round(t.ratio, 3),
           p.value1 = round(p.value, 3)) %>%
    mutate(sig.1 = determine_sig(p.value1)) %>%
    mutate(p.value1 = as.factor(p.value1)) %>%
    mutate(p.value1 = dplyr::recode(p.value1, "0" = "<0.001")) %>%
    dplyr::select(-mas_bin,-estimate, -SE, -t.ratio, -p.value, -df)
  
  ### PC2 Table
  table2 <- pc2_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate2 = round(estimate, 3),
           SE2 = round(SE, 3),
           t.ratio2 = round(t.ratio, 3),
           p.value2 = round(p.value, 3)) %>%
    mutate(sig.2 = determine_sig(p.value2)) %>%
    mutate(p.value2 = as.factor(p.value2)) %>%
    mutate(p.value2 = dplyr::recode(p.value2, "0" = "<0.001")) %>%
    dplyr::select(-mas_bin,-contrast, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
  ### PC3 Table
  table3<- pc3_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate3 = round(estimate, 3),
           SE3 = round(SE, 3),
           t.ratio3 = round(t.ratio, 3),
           p.value3 = round(p.value, 3)) %>%
    mutate(sig.3 = determine_sig(p.value3)) %>%
    mutate(p.value3 = as.factor(p.value3)) %>%
    mutate(p.value3 = dplyr::recode(p.value3, "0" = "<0.001")) %>%
    dplyr::select(-mas_bin,-contrast, -estimate, -SE, -t.ratio, -p.value, -df) 
  
  sswma_tables_combined = cbind(table1, table2, table3)
  sswma_tables_combined2 = sswma_tables_combined[1:16,] %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**PC1 - Acoustic Diversity**"),
      columns = c(estimate1, SE1, df1, t.ratio1, p.value1, sig.1)) %>%
    tab_spanner(
      label = md("**PC2 - Avian Abundance**"),
      columns = c(estimate2, SE2, t.ratio2, p.value2, sig.2)
    ) %>% 
    tab_spanner(
      label = md("**PC3 - Acoustic Complexity**"),
      columns = c(estimate3, SE3, t.ratio3, p.value3, sig.3)
    ) %>%
    cols_label(contrast = md("**Contrast**"),
               estimate1 = md("**Estimate**"),
               SE1 = md("**SE**"),
               df1 = md("**d.f.**"),
               t.ratio1 = md("**t-ratio**"),
               p.value1 = md("**p value**"),
               sig.1 = md("**sig.**")) %>%
    cols_label(estimate2 = md("**Estimate**"),
               SE2 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio2 = md("**t-ratio**"),
               p.value2 = md("**p value**"),
               sig.2 = md("**sig.**")) %>%
    cols_label(estimate3 = md("**Estimate**"),
               SE3 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio3 = md("**t-ratio**"),
               p.value3 = md("**p value**"),
               sig.3 = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman") %>%
    # tab_row_group(
    #   label = md("**Extremely Arid**"),
    #   rows = c(17:20)) %>%
    tab_row_group(
      label = md("**Late**"),
      rows = c(13:16)) %>%
    tab_row_group(
      label = md("**Mid**"),
      rows = c(9:12)) %>%
    tab_row_group(
      label = md("**Early**"),
      rows = c(5:8)) %>%
    tab_row_group(
      label = md("**Predawn**"),
      rows = c(1:4)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(sswma_tables_combined2)
}

### Paper Graph - Across Sites
sswma_water_site_paper = function(data,
                                  yvar,
                                  xvar,
                                  ylabel,
                                  xlabel){
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  
  ### Creating Labels for Graphs
  # mas labels
  sswma_maslag$mas_labels = factor(sswma_maslag$mas_bin, levels = c("0","1","2","3"),
                                   labels = c("Predawn","Early","Mid","Late"))
  
  # site labels
  sswma_maslag$wssite_labels = factor(sswma_maslag$ws_site, levels = c("1","2","3"),
                                      labels = c("Water Site 1", "Water Site 2", "Water Site 3"))
  
  ggplot(data = data,
         aes(x=xvar, y=yvar, color = ws_site, linetype = water)) +
    # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
    geom_smooth(method = lm) +
    scale_color_manual(values = cbpalette, 
                       name = "Site",
                       labels = c("Water Site 1",
                                  "Water Site 2",
                                  "Water Site 3"))+
    scale_linetype_discrete(name = "Water Presence/Absence") +
    scale_x_continuous(name = xlabel)+
    scale_y_continuous(name = ylabel)+
    # facet_grid(~facet_type) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "right") +
    # facet_wrap(vars(mas_bin)) + 
    ggtitle(label = "Comparisons across Site") +
    facet_grid(~mas_labels)+
    theme(strip.text.y = element_text(angle = 0))
}
