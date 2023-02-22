ece_contrast_mas = function(data,
                            yvar,
                            xvar){

  m = lm(yvar ~ site*xvar*mas_bin + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma  = c(1,0,0,0)
  sswma = c(0,1,0,0)
  cbma  = c(0,0,1,0)
  kiowa = c(0,0,0,1)
  
  # emm = emtrends(m, ~ site|mas_bin)
  emm = emtrends(m, ~ site|mas_bin, var = "xvar", type = 'response',weights = "cells")
  
  emm_cntrst = contrast(emm,
                        method = list(
                          "SSWMA - LWMA" = sswma-lwma,
                          "CBMA - LWMA"  = cbma -lwma,
                          "KIOWA - LWMA" = kiowa-lwma,
                          "CBMA - SSWMA" = cbma -sswma,
                          "KIOWA - SSWMA"= kiowa-sswma,
                          "KIOWA - CBMA" = kiowa-cbma
                        ),
                        adjust = "bonferroni")
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  ece_table = ece_table(emm_cntrst)
  my_list = list(summary, 
                 diagnostics, 
                 emm_cntrst_summary,
                 emm_confi_summary,
                 ece_table,
                 summary(emm))
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

ece_tables_combined3 = function(climate_table1,
                                impact_table1,
                                climate_table2,
                                impact_table2,
                                climate_table3,
                                impact_table3){
  # Combine all tables into a list
  pcs = list(climate_table1,impact_table1,
             climate_table2,impact_table2,
             climate_table3,impact_table3)
  
  pcs_clim = NULL
  pcs_impact = NULL
  
  # Loop through all the mas bins and create a confidence interval table for the site specific slopes
  for(i in 1:length(pcs)){
    table_temp <- pcs[[i]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::rename(estimate = "xvar.trend") %>%
      dplyr::mutate(site = toupper(site),
             estimate = round(estimate, 3),
             SE = round(SE, 3),
             lower.CL = round(lower.CL, 3),
             upper.CL = round(upper.CL, 3),
             sig = if_else(sign(lower.CL) == sign(upper.CL),"*"," "))
    
    if(i == 1 | i == 3 | i == 5){
      table_temp = table_temp %>% dplyr::select(-mas_bin) %>%
        dplyr::relocate(site,
                        estimate,
                        SE,
                        df,
                        lower.CL,
                        upper.CL,
                        sig)
      names(table_temp) = paste0(names(table_temp),i)
      assign(paste0("table_temp",i),table_temp)
      # pcs_clim = cbind(pcs_clim, table_temp)
      
    } else {
      table_temp = table_temp %>% dplyr::select(-site,
                                                -mas_bin) %>%
        dplyr::relocate(estimate,
                        SE,
                        df,
                        lower.CL,
                        upper.CL,
                        sig)
      
      names(table_temp) = paste0(names(table_temp),i)
      # pcs_impact = rbind(pcs_impact, table_temp)
      assign(paste0("table_temp",i),table_temp)
      
    }
  }
  
  pc1_ece = cbind(table_temp1, table_temp2)
  pc2_ece = cbind(table_temp3, table_temp4)
  pc3_ece = cbind(table_temp5, table_temp6)
  
  pc_all_ece = cbind(pc1_ece,pc2_ece,pc3_ece) %>%
    dplyr::select(-site3, -site5)
  
  ece_ag_pcs = pc_all_ece %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**Climate ECE**"),
      id = "pc1_clim",
      columns = c(estimate1, SE1, df1, lower.CL1, upper.CL1, sig1)) %>%
    tab_spanner(
      label = md("**Impact ECE**"),
      id = "pc1_impact",
      columns = c(estimate2, SE2, df2, lower.CL2, upper.CL2, sig2)) %>%
    tab_spanner(
      label = md("**PC1 - Acoustic Diversity**"),
      columns = c(estimate1, SE1, df1, lower.CL1, upper.CL1, sig1,
                  estimate2, SE2, df2, lower.CL2, upper.CL2, sig2)) %>%
    tab_spanner(
      label = md("**Climate ECE**"),
      id = "pc2_clim",
      columns = c(estimate3, SE3, df3, lower.CL3, upper.CL3, sig3)) %>%
    tab_spanner(
      label = md("**Impact ECE**"),
      id = "pc2_impact",
      columns = c(estimate4, SE4, df4, lower.CL4, upper.CL4, sig4)) %>%
    tab_spanner(
      label = md("**PC2 - Avian Abundance**"),
      columns = c(estimate3, SE3, df3, lower.CL3, upper.CL3, sig3,
                  estimate4, SE4, df4, lower.CL4, upper.CL4, sig4)) %>%
    tab_spanner(
      label = md("**Climate ECE**"),
      id = "pc3_clim",
      columns = c(estimate5, SE5, df5, lower.CL5, upper.CL5, sig5)) %>%
    tab_spanner(
      label = md("**Impact ECE**"),
      id = "pc3_impact",
      columns = c(estimate6, SE6, df6, lower.CL6, upper.CL6, sig6)) %>%
    tab_spanner(
      label = md("**PC3 - Acoustic Complexity**"),
      columns = c(estimate5, SE5, df5, lower.CL5, upper.CL5, sig5,
                  estimate6, SE6, df6, lower.CL6, upper.CL6, sig6)) %>%
    # tab_spanner(
    #   label = md("**Climate ECE**"),
    #   columns = c(estimate1, SE1, df1, lower.CL1, upper.CL1, sig1,
    #               estimate3, SE3, df3, lower.CL3, upper.CL3, sig3,
    #               estimate5, SE5, df5, lower.CL5, upper.CL5, sig5)) %>%
    # tab_spanner(
    #   label = md("**Impact ECE**"),
    #   columns = c(estimate2, SE2, df2, lower.CL2, upper.CL2, sig2,
    #               estimate4, SE4, df4, lower.CL4, upper.CL4, sig4,
    #               estimate6, SE6, df6, lower.CL6, upper.CL6, sig6)) %>%
    cols_label(site1 = md("**Site**"),
               estimate1 = md("**Estimate**"),
               SE1 = md("**SE**"),
               df1 = md("**d.f.**"),
               lower.CL1 = md("**Lower CI**"),
               upper.CL1 = md("**Upper CI**"),
               sig1 = md("**Sig.**")) %>%
    cols_label(estimate2 = md("**Estimate**"),
               SE2 = md("**SE**"),
               df2 = md("**d.f.**"),
               lower.CL2 = md("**Lower CI**"),
               upper.CL2 = md("**Upper CI**"),
               sig2 = md("**Sig.**")) %>%
    cols_label(estimate3 = md("**Estimate**"),
               SE3 = md("**SE**"),
               df3 = md("**d.f.**"),
               lower.CL3 = md("**Lower CI**"),
               upper.CL3 = md("**Upper CI**"),
               sig3 = md("**Sig.**")) %>%
    cols_label(estimate4 = md("**Estimate**"),
               SE4 = md("**SE**"),
               df4 = md("**d.f.**"),
               lower.CL4 = md("**Lower CI**"),
               upper.CL4 = md("**Upper CI**"),
               sig4 = md("**Sig.**")) %>%
    cols_label(estimate5 = md("**Estimate**"),
               SE5 = md("**SE**"),
               df5 = md("**d.f.**"),
               lower.CL5 = md("**Lower CI**"),
               upper.CL5 = md("**Upper CI**"),
               sig5 = md("**Sig.**")) %>%
    cols_label(estimate6 = md("**Estimate**"),
               SE6 = md("**SE**"),
               df6 = md("**d.f.**"),
               lower.CL6 = md("**Lower CI**"),
               upper.CL6 = md("**Upper CI**"),
               sig6 = md("**Sig.**")) %>%
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
  return(ece_ag_pcs)
}

