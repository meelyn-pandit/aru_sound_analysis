cbma_water_table = function(contrast_table) {
  ### Making good tables in R
  table <- contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(contrast = dplyr::recode(contrast, 
           "ws_site1 water0 - ws_site2 water1" = "Site1:Closed - Site2:Open",
           "ws_site1 water1 - ws_site2 water1" = "Site1:Open - Site2:Open"),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = determine_sig(p.value)) %>%
    mutate(p.value = as.factor(p.value)) %>%
    mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
    dplyr::select(-c("arid_within")) %>%
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
      rows = c(9:10)) %>%
    tab_row_group(
      label = md("**Arid**"),
      rows = c(7:8)) %>%
    tab_row_group(
      label = md("**Normal**"),
      rows = c(5:6)) %>%
    tab_row_group(
      label = md("**Humid**"),
      rows = c(3:4)) %>%
    tab_row_group(
      label = md("**Extremely Humid**"),
      rows = c(1:2)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    ); table
}


cbma_water_table2 = function(pc1_contrast_table,
                                pc2_contrast_table,
                                pc3_contrast_table) {
  
    
  ### PC1 Table
  table1 <- pc1_contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(contrast = dplyr::recode(contrast, 
                                    "ws_site1 water 1 - ws_site2 water1" = "Site1:Open - Site2:Open",
                                    "ws_site1 water 0 - ws_site2 water1" = "Site1:Closed - Site2:Open"),
           estimate1 = round(estimate, 3),
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
    tab_row_group(
      label = md("**Extremely Arid**"),
      rows = c(9:10)) %>%
    tab_row_group(
      label = md("**Arid**"),
      rows = c(7:8)) %>%
    tab_row_group(
      label = md("**Normal**"),
      rows = c(5:6)) %>%
    tab_row_group(
      label = md("**Humid**"),
      rows = c(3:4)) %>%
    tab_row_group(
      label = md("**Extremely Humid**"),
      rows = c(1:2)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(cbma_tables_combined)
}
