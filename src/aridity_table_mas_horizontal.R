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
  
  table_combined = cbind(table_predawn, 
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
  return(table_combined)
}

