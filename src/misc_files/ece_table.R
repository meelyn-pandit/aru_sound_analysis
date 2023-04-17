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

