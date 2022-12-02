sswma_water_table = function(contrast_table) {
  ### Making good tables in R
  table <- emm1_cntrst %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate(contrast = dplyr::recode(contrast, 
                                    "ws_site1 water 1 - ws_site2 water0" = "Site1:Open - Site2:Closed",
                                    "ws_site2 water 1 - ws_site1 water0" = "Site2:Open - Site1:Closed",
                                    "ws_site1 water 1 - ws_site3 water0" = "Site1:Open - Site3:Closed",
                                    "ws_site2 water 1 - ws_site3 water0" = "Site2:Open - Site3:Closed"),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           t.ratio = round(t.ratio, 3),
           p.value = round(p.value, 3)) %>%
    mutate(sig. = if_else(p.value < 0.1 & p.value > 0.05, ".", 
                          if_else(p.value < 0.05 & p.value > 0.01, "*",
                                  if_else(p.value < 0.01 & p.value > 0.001, "**",
                                          if_else(p.value < 0.001, "***", " "))))) %>%
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
      label = md("**Aridity Factor 5**"),
      rows = c(17:20)) %>%
    tab_row_group(
      label = md("**Aridity Factor 4**"),
      rows = c(13:16)) %>%
    tab_row_group(
      label = md("**Aridity Factor 3**"),
      rows = c(9:12)) %>%
    tab_row_group(
      label = md("**Aridity Factor 2**"),
      rows = c(5:8)) %>%
    tab_row_group(
      label = md("**Aridity Factor 1**"),
      rows = c(1:4)) %>%
    tab_source_note(
      source_note = "P value adjustment: tukey method for comparing a family of 4 estimates."
    ); table
}
