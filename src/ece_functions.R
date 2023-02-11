ece_contrast_mas = function(data,pc){

  m = lm(pc ~ site*mas_bin + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma  = c(1,0,0,0)
  sswma = c(0,1,0,0)
  cbma  = c(0,0,1,0)
  kiowa = c(0,0,0,1)
  
  emm = emmeans(m, ~ site|mas_bin)
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

ece_table = function(contrast_table) {
  
  ### Making good tables in R
  table_ece <- contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = determine_sig(p.value)) %>%
    mutate(p.value = as.factor(p.value)) %>%
    mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) 
  
  # %>%
  #   gt(.) %>%
  #   cols_align('center') %>%
  #   # # tab_spanner(
  #   # #   label = md("**MAS Bin 0 - Predawn**"),
  #   #   columns = c(estimate, SE, df, t.ratio, p.value, sig.)) %>%
  #   # tab_spanner(
  #   #   label = md("**MAS Bin 1 - Early**"),
  #   #   columns = c(estimate_e, SE_e, t.ratio_e, p.value_e, sig._e)
  #   # ) %>% 
  #   # tab_spanner(
  #   #   label = md("**MAS Bin 2 - Mid**"),
  #   #   columns = c(estimate_m, SE_m, t.ratio_m, p.value_m, sig._m)
  #   # ) %>%
  #   # tab_spanner(
  #   #   label = md("**MAS Bin 3 - Late**"),
  #   #   columns = c(estimate_l, SE_l, t.ratio_l, p.value_l, sig._l)
  #   # ) %>%
  #   cols_label(contrast = md("**Contrast**"),
  #              estimate = md("**Estimate**"),
  #              SE = md("**SE**"),
  #              df = md("**d.f.**"),
  #              t.ratio = md("**t-ratio**"),
  #              p.value = md("**p value**"),
  #              sig. = md("**sig.**")) %>%
  #   opt_table_font(
  #     font = "Times New Roman")
  return(table_ece)
}


# Create Single GT Table for each PC --------------------------------------

ece_tables_combined = function(climate_table,
                               impact_table){
  # Climate Table
  ece_pc_climate = climate_table %>%
    # data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      estimate_cl = round(estimate, 3),
      SE_cl = round(SE, 3),
      df_cl = df,
      t.ratio_cl = round(t.ratio, 3),
      p.value_cl = p.value,
      sig._cl = sig.) %>%
    dplyr::select(-mas_bin,-estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  ece_pc_impact = impact_table %>%
    # data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate_im = round(estimate, 3),
           SE_im = round(SE, 3),
           df_im = df,
           t.ratio_im = round(t.ratio, 3),
           p.value_im = p.value,
           sig._im = sig.) %>%
    dplyr::select(-mas_bin,-contrast, -estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  ece_combined = cbind(ece_pc_climate,
                       ece_pc_impact) %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**Climate ECE**"),
      columns = c(estimate_cl, SE_cl, df_cl, t.ratio_cl, p.value_cl, sig._cl)) %>%
    tab_spanner(
      label = md("**Impact ECE**"),
      columns = c(estimate_im, SE_im, df_im, t.ratio_im, p.value_im, sig._im)
    ) %>%
    cols_align('center') %>%
    cols_label(contrast = md("**Contrast**"),
               estimate_cl = md("**Est.**"),
               SE_cl = md("**SE**"),
               df_cl = md("**d.f.**"),
               t.ratio_cl = md("**t-ratio**"),
               p.value_cl = md("**p value**"),
               sig._cl = md("**sig.**")) %>%
    cols_label(estimate_im = md("**Est.**"),
               SE_im = md("**SE**"),
               df_im = md("**d.f.**"),
               t.ratio_im = md("**t-ratio**"),
               p.value_im = md("**p value**"),
               sig._im = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman") %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(ece_combined)
}


# Part 1 in Creating Large Table with All PCs -----------------------------


ece_tables_combined2 = function(climate_table1,
                                impact_table1,
                                climate_table2,
                                impact_table2,
                                climate_table3,
                                impact_table3){
  # Climate Table - PC1
  ece_pc_climate1 = climate_table1 %>%
    mutate(
      estimate_cl = round(estimate, 3),
      SE_cl = round(SE, 3),
      df_cl = df,
      t.ratio_cl = round(t.ratio, 3),
      p.value_cl = p.value,
      sig._cl = sig.) %>%
    dplyr::select(-estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  # Impact ECE - PC1
  ece_pc_impact1 = impact_table1 %>%
    mutate(estimate_im = round(estimate, 3),
           SE_im = round(SE, 3),
           df_im = df,
           t.ratio_im = round(t.ratio, 3),
           p.value_im = p.value,
           sig._im = sig.) %>%
    dplyr::select(-contrast, -estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  ece_combined1 = cbind(ece_pc_climate1,
                        ece_pc_impact1)
  # %>%
  #   dplyr::mutate(pc = 1)
  
  # Climate Table - PC2
  ece_pc_climate2 = climate_table2 %>%
    mutate(
      estimate_cl = round(estimate, 3),
      SE_cl = round(SE, 3),
      df_cl = df,
      t.ratio_cl = round(t.ratio, 3),
      p.value_cl = p.value,
      sig._cl = sig.) %>%
    dplyr::select(-estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  # Impact Table - PC2
  ece_pc_impact2 = impact_table2 %>%
    # data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate_im = round(estimate, 3),
           SE_im = round(SE, 3),
           df_im = df,
           t.ratio_im = round(t.ratio, 3),
           p.value_im = p.value,
           sig._im = sig.) %>%
    dplyr::select(-contrast, -estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  ece_combined2 = cbind(ece_pc_climate2,
                        ece_pc_impact2) 
  # %>%
  #   dplyr::mutate(pc = 2)
  
  # Climate Table - PC3
  ece_pc_climate3 = climate_table3 %>%
    mutate(
      estimate_cl = round(estimate, 3),
      SE_cl = round(SE, 3),
      df_cl = df,
      t.ratio_cl = round(t.ratio, 3),
      p.value_cl = p.value,
      sig._cl = sig.) %>%
    dplyr::select(-estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  # Impact Table - PC3
  ece_pc_impact3 = impact_table3 %>%
    # data.frame(stringsAsFactors = FALSE) %>%
    mutate(estimate_im = round(estimate, 3),
           SE_im = round(SE, 3),
           df_im = df,
           t.ratio_im = round(t.ratio, 3),
           p.value_im = p.value,
           sig._im = sig.) %>%
    dplyr::select(-contrast, -estimate, -SE, -df, -t.ratio,-p.value, -sig.)
  
  ece_combined3 = cbind(ece_pc_climate3,
                        ece_pc_impact3) 
  # %>%
  #   dplyr::mutate(pc = 3)
  
  # Combine all PC tables into one big table
  ece_allpcs = rbind(ece_combined1,
                     ece_combined2,
                     ece_combined3) %>%
    # gt(rowname_col = 'pc') %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**Climate ECE**"),
      columns = c(estimate_cl, SE_cl, df_cl, t.ratio_cl, p.value_cl, sig._cl)) %>%
    tab_spanner(
      label = md("**Impact ECE**"),
      columns = c(estimate_im, SE_im, df_im, t.ratio_im, p.value_im, sig._im)
    ) %>%
    cols_align('center') %>%
    cols_label(contrast = md("**Contrast**"),
               estimate_cl = md("**Est.**"),
               SE_cl = md("**SE**"),
               df_cl = md("**d.f.**"),
               t.ratio_cl = md("**t-ratio**"),
               p.value_cl = md("**p value**"),
               sig._cl = md("**sig.**")) %>%
    cols_label(estimate_im = md("**Est.**"),
               SE_im = md("**SE**"),
               df_im = md("**d.f.**"),
               t.ratio_im = md("**t-ratio**"),
               p.value_im = md("**p value**"),
               sig._im = md("**sig.**")) %>%
    tab_row_group(
      label = md("**PC 3 - Acoustic Complexity**"),
      rows = c(13:18)) %>%
    tab_row_group(
      label = md("**PC 2 - Avian Abundance**"),
      rows = c(7:12)) %>%
    tab_row_group(
      label = md("**PC 1 - Acoustic Diversity**"),
      rows = c(1:6)) %>%
    opt_table_font(
      font = "Times New Roman") %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    )
  return(ece_allpcs)
}
