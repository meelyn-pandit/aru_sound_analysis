aridity_table_mas = function(contrast_table) {
  ### Making good tables in R
  table <- contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(
      # contrast = dplyr::recode(contrast, 
      #      "ws_site1 water0 - ws_site2 water1" = "Site1:Closed - Site2:Open",
      #      "ws_site1 water1 - ws_site2 water1" = "Site1:Open - Site2:Open"),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = if_else(p.value < 0.1 & p.value >= 0.05, ".", 
                          if_else(p.value < 0.05 & p.value >= 0.01, "*",
                                  if_else(p.value < 0.01 & p.value >= 0.001, "**",
                                          if_else(p.value < 0.001, "***", " "))))) %>%
    mutate(p.value = as.factor(p.value)) %>%
    mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
    dplyr::select(-x2) %>%
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
      label = md("**MAS Bin 3 - Late**"),
      rows = c(91:120)) %>%
    tab_row_group(
      label = md("**MAS Bin 2 - Mid**"),
      rows = c(61:90)) %>%
    tab_row_group(
      label = md("**MAS Bin 1 - Early**"),
      rows = c(31:60)) %>%
    tab_row_group(
      label = md("**MAS Bin 0 - Pre-Dawn**"),
      rows = c(1:30)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    ); table
}
