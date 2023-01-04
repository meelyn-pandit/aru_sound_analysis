sswma_water_climate = function(pc){
  m1_sswmawl = lm(pc ~ ws_site*water + mas_bin + date, 
                  data = sswmawl_clmas)
  summary = summary(m1_sswmawl)
  diagnostics = assump(m1_sswmawl)
  emm = emmeans(m1_sswmawl, ~ ws_site*water);emm
  
  # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0,0,0)
  ws2w0 = c(0,1,0,0,0,0)
  ws3w0 = c(0,0,1,0,0,0)
  ws1w1 = c(0,0,0,1,0,0)
  ws2w1 = c(0,0,0,0,1,0)
  ws3w1 = c(0,0,0,0,0,1)
  cntrst_summary = contrast(emm, 
           method = list("Site2:Open - Site1:Closed" = ws2w1 - ws1w0, # not comparing ws1w1 because there's not enough observations of them
                         "Site2:Open - Site3:Closed" = ws2w1 - ws3w0))
  sswma_climate_list = list(summary, emm, cntrst_summary)
  return(sswma_climate_list)
}

sswma_water_climate_table = function(pc1_contrast_table,
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
    dplyr::select(-estimate, -SE, -t.ratio, -p.value, -df)
  
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
    dplyr::select(-contrast, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
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
    dplyr::select(-contrast,-estimate, -SE, -t.ratio, -p.value, -df) 
  
  sswma_tables_combined = cbind(table1, table2, table3) %>%
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
    # tab_row_group(
    #   label = md("**Arid**"),
    #   rows = c(13:16)) %>%
    # tab_row_group(
    #   label = md("**Normal**"),
    #   rows = c(9:12)) %>%
    # tab_row_group(
    #   label = md("**Humid**"),
    #   rows = c(3:4)) %>%
    # tab_row_group(
    #   label = md("**Extremely Humid**"),
    #   rows = c(1:2)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(sswma_tables_combined)
}

sswma_water_impact = function(pc){
  im1_sswmawl = lm(pc ~ ws_site*water + mas_bin + scale(date), 
                   data = sswmawl_thres)
  summary = summary(im1_sswmawl)
  emm = emmeans(im1_sswmawl,~ ws_site*water);emm
  
  # Setting up comparisons for emmeans contrast function
  
  ws1w0 = c(1,0,0,0,0,0)
  ws2w0 = c(0,1,0,0,0,0)
  ws3w0 = c(0,0,1,0,0,0)
  ws1w1 = c(0,0,0,1,0,0)
  ws2w1 = c(0,0,0,0,1,0)
  ws3w1 = c(0,0,0,0,0,1)
  
  cntrst_summary = contrast(emm,
                            method = list("Site1:Open - Site2:Closed" = ws1w1-ws2w0,
                                          "Site1:Open - Site3:Closed" = ws1w1-ws3w0,
                                          "Site2:Open - Site1:Closed" = ws2w1 - ws1w0, 
                                          "Site2:Open - Site3:Closed" = ws2w1 - ws3w0))
  
  sswma_impact_list = list(summary, emm, cntrst_summary)
  return(sswma_impact_list)
}

sswma_water_impact_table = function(pc1_contrast_table,
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
    dplyr::select(-estimate, -SE, -t.ratio, -p.value, -df)
  
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
    dplyr::select(-contrast, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
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
    dplyr::select(-contrast,-estimate, -SE, -t.ratio, -p.value, -df) 
  
  sswma_tables_combined = cbind(table1, table2, table3) %>%
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
    # tab_row_group(
    #   label = md("**Arid**"),
    #   rows = c(13:16)) %>%
    # tab_row_group(
    #   label = md("**Normal**"),
    #   rows = c(9:12)) %>%
    # tab_row_group(
    #   label = md("**Humid**"),
  #   rows = c(3:4)) %>%
  # tab_row_group(
  #   label = md("**Extremely Humid**"),
  #   rows = c(1:2)) %>%
  tab_source_note(
    source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
  )
  return(sswma_tables_combined)
}

sswma_ece_table = function(climate_pc1_table,
                           climate_pc2_table,
                           climate_pc3_table,
                           impact_pc1_table,
                           impact_pc2_table,
                           impact_pc3_table) {
  
  ### PC1 Table - Climate
  table1_climate <- climate_pc1_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate1 = round(estimate, 3),
           SE1 = round(SE, 3),
           df1 = df,
           t.ratio1 = round(t.ratio, 3),
           p.value1 = round(p.value, 3)) %>%
    mutate(sig.1 = determine_sig(p.value1)) %>%
    mutate(p.value1 = as.factor(p.value1)) %>%
    mutate(p.value1 = dplyr::recode(p.value1, "0" = "<0.001")) %>%
    dplyr::select(-estimate, -SE, -t.ratio, -p.value, -df)
  
  ### PC1 Table - Impact
  table1_impact <- impact_pc1_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate1 = round(estimate, 3),
           SE1 = round(SE, 3),
           df1 = df,
           t.ratio1 = round(t.ratio, 3),
           p.value1 = round(p.value, 3)) %>%
    mutate(sig.1 = determine_sig(p.value1)) %>%
    mutate(p.value1 = as.factor(p.value1)) %>%
    mutate(p.value1 = dplyr::recode(p.value1, "0" = "<0.001")) %>%
    dplyr::select(-estimate, -SE, -t.ratio, -p.value, -df)
  
  table1 = rbind(table1_climate,table1_impact)
  
  ### PC2 Table - Climate
  table2_climate <- climate_pc2_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate2 = round(estimate, 3),
           SE2 = round(SE, 3),
           t.ratio2 = round(t.ratio, 3),
           p.value2 = round(p.value, 3)) %>%
    mutate(sig.2 = determine_sig(p.value2)) %>%
    mutate(p.value2 = as.factor(p.value2)) %>%
    mutate(p.value2 = dplyr::recode(p.value2, "0" = "<0.001")) %>%
    dplyr::select(-contrast, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
  ### PC2 Table - Impact
  table2_impact <- impact_pc2_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate2 = round(estimate, 3),
           SE2 = round(SE, 3),
           t.ratio2 = round(t.ratio, 3),
           p.value2 = round(p.value, 3)) %>%
    mutate(sig.2 = determine_sig(p.value2)) %>%
    mutate(p.value2 = as.factor(p.value2)) %>%
    mutate(p.value2 = dplyr::recode(p.value2, "0" = "<0.001")) %>%
    dplyr::select(-contrast, -estimate, -SE, -t.ratio, -p.value,  -df) 
  
  table2 = rbind(table2_climate, table2_impact)
  
  ### PC3 Table - Climate
  table3_climate <- climate_pc3_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate3 = round(estimate, 3),
           SE3 = round(SE, 3),
           t.ratio3 = round(t.ratio, 3),
           p.value3 = round(p.value, 3)) %>%
    mutate(sig.3 = determine_sig(p.value3)) %>%
    mutate(p.value3 = as.factor(p.value3)) %>%
    mutate(p.value3 = dplyr::recode(p.value3, "0" = "<0.001")) %>%
    dplyr::select(-contrast,-estimate, -SE, -t.ratio, -p.value, -df) 
  
  ### PC3 Table - Impact
  table3_impact <- impact_pc3_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate3 = round(estimate, 3),
           SE3 = round(SE, 3),
           t.ratio3 = round(t.ratio, 3),
           p.value3 = round(p.value, 3)) %>%
    mutate(sig.3 = determine_sig(p.value3)) %>%
    mutate(p.value3 = as.factor(p.value3)) %>%
    mutate(p.value3 = dplyr::recode(p.value3, "0" = "<0.001")) %>%
    dplyr::select(-contrast,-estimate, -SE, -t.ratio, -p.value, -df) 
  
  table3 = rbind(table3_climate,table3_impact)
  
  sswma_ece_combined = cbind(table1, table2, table3)
  sswma_ece_combined2 = sswma_ece_combined[-c(3:4),] %>%
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
    # tab_row_group(
    #   label = md("**Arid**"),
    #   rows = c(13:16)) %>%
    # tab_row_group(
    #   label = md("**Normal**"),
    #   rows = c(9:12)) %>%
    tab_row_group(
      label = md("**Impact ECE Definition**"),
      # rows = c(3:6)) %>%
      rows = c(3:4)) %>%
    tab_row_group(
      label = md("**Climate ECE Definition**"),
      rows = c(1:2)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(sswma_ece_combined2)
}
