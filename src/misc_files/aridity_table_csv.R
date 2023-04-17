aridity_table_csv = function(contrast_table) {
  
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
                         table_late)
}
