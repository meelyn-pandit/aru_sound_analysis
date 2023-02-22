cbma_water_climate = function(pc){
  m1_cbmawl = lm(pc ~ ws_site*water*gh + mas_bin + date, 
                  data = cbmawl_clmas)
  summary = summary(m1_cbmawl)
  diagnostics = assump(m1_cbmawl)
  emm = emtrends(m1_cbmawl, ~ ws_site*water, var = "gh", type = 'response',weights = "cells")
  # emm = emmeans(m1_cbmawl, ~ ws_site*water);emm
  
  # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0)
  ws2w0 = c(0,1,0,0)
  ws1w1 = c(0,0,1,0)
  ws2w1 = c(0,0,0,1)
  
  cntrst_summary = contrast(emm,
                            method = list("Site1:Open - Site2:Open" = ws1w1-ws2w1,
                                          "Site1:Closed - Site2:Open" = ws1w0-ws2w1),
                            adjust = "bonferroni")
  
  cbma_climate_list = list(summary, emm, cntrst_summary)
  return(cbma_climate_list)
}

cbma_water_climate_table = function(pc1_contrast_table,
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
  
  cbma_tables_combined = cbind(table1, table2, table3) %>%
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
  tab_source_note(
    source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
  )
  return(cbma_tables_combined)
}


cbma_water_impact = function(data,
                             pc){
  im1_cbmawl = lm(pc ~ ws_site*water*gh + mas_bin + scale(date), 
                   data = data)
  summary = summary(im1_cbmawl)
  # emm = emmeans(im1_cbmawl,~ ws_site*water);emm
  emm = emtrends(im1_cbmawl, ~ ws_site*water, var = "gh", type = 'response',weights = "cells")
  
  
  # Setting up comparisons for emmeans contrast function
  ws1w0 = c(1,0,0,0)
  ws2w0 = c(0,1,0,0)
  ws1w1 = c(0,0,1,0)
  ws2w1 = c(0,0,0,1)
  
  cntrst_summary = contrast(emm,
                  method = list("Site1:Open - Site2:Open" = ws1w1-ws2w1,
                                "Site1:Closed - Site2:Open" = ws1w0-ws2w1),
                  adjust = "bonferroni")
  
  cbma_impact_list = list(summary, emm, cntrst_summary)
  return(cbma_impact_list)
}

cbma_water_impact_table = function(pc1_contrast_table,
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
  
  cbma_tables_combined = cbind(table1, table2, table3) %>%
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
  tab_source_note(
    source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
  )
  return(cbma_tables_combined)
}

cbma_ece_table = function(climate_pc1_table,
                          climate_pc2_table,
                          climate_pc3_table,
                          impact_pc1_table,
                          impact_pc2_table,
                          impact_pc3_table) {
  pcs = list(climate_pc1_table,climate_pc2_table,climate_pc3_table,
             impact_pc1_table,impact_pc2_table,impact_pc3_table)
  climate_pcs = NULL
  impact_pcs = NULL
  # Loop through all the mas bins and create a confidence interval table for the site specific slopes
  for(i in 1:length(pcs)){
    
    table_temp <- pcs[[i]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::mutate(estimate = round(estimate, 3),
                    SE = round(SE, 3),
                    t.ratio = round(t.ratio, 3),
                    p.value = round(p.value, 3)) %>%
      dplyr::mutate(sig = determine_sig(p.value)) %>%
      dplyr::mutate(p.value = as.factor(p.value)) %>%
      dplyr::mutate(p.value = dplyr::recode(p.value, "0" = "<0.001"))
    
    if(i == 1){
      table_temp = table_temp %>% dplyr::relocate(
                                                  contrast,# site,
                                                  estimate,
                                                  SE,
                                                  df,
                                                  t.ratio,
                                                  p.value,
                                                  sig)
      
      names(table_temp) = paste0(names(table_temp),i)
      climate_pcs = table_temp
      
    } else if(i == 2 | i == 3) {
      table_temp = table_temp %>% dplyr::select(
        -contrast,# -site,
        -df) %>%
        dplyr::relocate(estimate,SE,t.ratio,p.value,sig)
      names(table_temp) = paste0(names(table_temp),i)
      climate_pcs = cbind(climate_pcs, table_temp)
    } else if (i == 4) {
      table_temp = table_temp %>% dplyr::relocate(
        contrast,# site,
        estimate,
        SE,
        df,
        t.ratio,
        p.value,
        sig)
      
      names(table_temp) = paste0(names(table_temp),(i-3))
      impact_pcs = table_temp
    } else if(i == 5 | i == 6) {
      table_temp = table_temp %>% dplyr::select(
        -contrast,# -site,
        -df) %>%
        dplyr::relocate(estimate,SE,t.ratio,p.value,sig)
      names(table_temp) = paste0(names(table_temp),(i-3))
      impact_pcs = cbind(impact_pcs, table_temp)
    }
  }
  
  climate_pcs$ece_label = "Climate ECE"
  impact_pcs$ece_label = "Impact ECE"
  
  pcs_all = rbind(climate_pcs, impact_pcs)
  pcs_ag_table = pcs_all %>%
    # gt(groupname_col = "mas_bin1", rowname_col = "site1") %>%
    gt(
      groupname_col = "ece_label",
       rowname_col = "contrast1"
       ) %>%
    tab_options(row_group.as_column = TRUE,
                stub.font.weight = "bold") %>% # makes mas_bin labels bold
    tab_style(style = cell_text(weight = "bold"), # makes site labels bold
              locations = cells_row_groups()) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**PC1 - Acoustic Diversity**"),
      columns = c(estimate1, SE1, df1, t.ratio1, p.value1, sig1)) %>%
    tab_spanner(
      label = md("**PC2 - Avian Abundance**"),
      columns = c(estimate2, SE2, t.ratio2, p.value2, sig2)
    ) %>% 
    tab_spanner(
      label = md("**PC3 - Acoustic Complexity**"),
      columns = c(estimate3, SE3, t.ratio3, p.value3, sig3)) %>%
    cols_label(contrast1 = md("**Contrast**"),
               estimate1 = md("**Estimate**"),
               SE1 = md("**SE**"),
               df1 = md("**d.f.**"),
               t.ratio1 = md("**t-ratio**"),
               p.value1 = md("**p value**"),
               sig1 = md("**sig.**")) %>%
    cols_label(estimate2 = md("**Estimate**"),
               SE2 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio2 = md("**t-ratio**"),
               p.value2 = md("**p value**"),
               sig2 = md("**sig.**")) %>%
    cols_label(estimate3 = md("**Estimate**"),
               SE3 = md("**SE**"),
               # df1 = md("**d.f.**"),
               t.ratio3 = md("**t-ratio**"),
               p.value3 = md("**p value**"),
               sig3 = md("**sig.**")) %>%
    # tab_row_group(
    #   label = md("**Impact ECE Definition**"),
    #   rows = c(3:4)) %>%
    # tab_row_group(
    #   label = md("**Climate ECE Definition**"),
    #   rows = c(1:2)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    ) %>%
    opt_table_font(
      font = "Times New Roman")
  return(pcs_ag_table)
}

