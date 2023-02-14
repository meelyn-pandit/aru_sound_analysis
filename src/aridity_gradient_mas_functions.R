# load("src/cbma_water_table.R")
aridity_contrasts_mas = function(pc,
                                 arid_factor){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(pc ~ arid_factor*mas_bin*site + scale(date), data = aw6)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma1  = c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma2  = c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma3  = c(0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma4  = c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  lwma5  = c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma1 = c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma2 = c(0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma3 = c(0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0)
  sswma4 = c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0)
  sswma5 = c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0)
  cbma1  = c(0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0)
  cbma2  = c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0)
  cbma3  = c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0)
  cbma4  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
  cbma5  = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0)
  kiowa1 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0)
  kiowa2 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0)
  kiowa3 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0)
  kiowa4 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0)
  kiowa5 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)
  
  emm = emmeans(m, ~ arid_factor*site|mas_bin)
  emm_cntrst = contrast(emm,
               method = list(
               "SSWMA:Extremely Humid - LWMA:Extremely Humid" = sswma1-lwma1,
               "CBMA:Extremely Humid - LWMA:Extremely Humid"  = cbma1 -lwma1,
               "KIOWA:Extremely Humid - LWMA:Extremely Humid" = kiowa1-lwma1,
               "CBMA:Extremely Humid - SSWMA:Extremely Humid"  = cbma1 -sswma1,
               "KIOWA:Extremely Humid - SSWMA:Extremely Humid" = kiowa1-sswma1,
               "KIOWA:Extremely Humid - CBMA:Extremely Humid" = kiowa1-cbma1,
               "SSWMA:Humid - LWMA:Humid" = sswma2-lwma2,
               "CBMA:Humid - LWMA:Humid"  = cbma2 -lwma2,
               "KIOWA:Humid - LWMA:Humid" = kiowa2-lwma2,
               "CBMA:Humid - SSWMA:Humid"  = cbma2 -sswma2,
               "KIOWA:Humid - SSWMA:Humid" = kiowa2-sswma2,
               "KIOWA:Humid - CBMA:Humid" = kiowa2-cbma2,
               "SSWMA:Normal - LWMA:Normal" = sswma3-lwma3,
               "CBMA:Normal - LWMA:Normal"  = cbma3 -lwma3,
               "KIOWA:Normal - LWMA:Normal" = kiowa3-lwma3,
               "CBMA:Normal - SSWMA:Normal"  = cbma3 -sswma3,
               "KIOWA:Normal - SSWMA:Normal" = kiowa3-sswma3,
               "KIOWA:Normal - CBMA:Normal" = kiowa3-cbma3,
               "SSWMA:Arid - LWMA:Arid" = sswma4-lwma4,
               "CBMA:Arid - LWMA:Arid"  = cbma4 -lwma4,
               "KIOWA:Arid - LWMA:Arid" = kiowa4-lwma4,
               "CBMA:Arid - SSWMA:Arid"  = cbma4 -sswma4,
               "KIOWA:Arid - SSWMA:Arid" = kiowa4-sswma4,
               "KIOWA:Arid - CBMA:Arid" = kiowa4-cbma4,
               "SSWMA:Extremely Arid - LWMA:Extremely Arid" = sswma5-lwma5,
               "CBMA:Extremely Arid - LWMA:Extremely Arid"  = cbma5 -lwma5,
               "KIOWA:Extremely Arid - LWMA:Extremely Arid" = kiowa5-lwma5,
               "CBMA:Extremely Arid - SSWMA:Extremely Arid"  = cbma5 -sswma5,
               "KIOWA:Extremely Arid - SSWMA:Extremely Arid" = kiowa5-sswma5,
               "KIOWA:Extremely Arid - CBMA:Extremely Arid" = kiowa5-cbma5
               ))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # arid_table = aridity_table_mas2(emm_cntrst_summary);arid_table
  arid_table = aridity_table_mas2(emm_confi_summary);arid_table
  
  my_list = list(summary, 
                 diagnostics, 
                 emm_cntrst_summary,
                 emm_confi_summary,
                 arid_table,
                 summary(emm)) # NEEDS TO BE SUMMARY OF EMM TO BE A DATAFRAME!!!
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}

### Aridity Gradient - Slope table
## Create a table for individual slopes across sites or across mas_bins in the linear model analyses

ind_slopes_site = function(emm_table){
  
  emm_table$mas_bin = factor(emm_table$mas_bin, levels = c(0,1,2,3),
                             labels = c("Predawn","Early","Mid","Late"))
  table_temp <- emm_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(estimate = "xvar.trend") %>%
    mutate(site = toupper(site),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           lower.CL = round(lower.CL, 3),
           upper.CL = round(upper.CL, 3),
           sig = if_else(sign(lower.CL) == sign(upper.CL),"*"," ")) %>%
    # dplyr::group_by(mas_bin) %>%
    gt(groupname_col = "site", rowname_col = "mas_bin") %>%
    # tab_row_group(label = NULL) %>%
    tab_options(row_group.as_column = TRUE,
                stub.font.weight = "bold" # makes mas_bin labels bold
                ) %>%
    tab_style(style = cell_text(weight = "bold"), # makes site labels bold
              locations = cells_row_groups()) %>%
    cols_align('center') %>%
    cols_label(site = md("**Contrast**"),
               estimate = md("**Estimate**"),
               SE = md("**SE**"),
               df = md("**d.f.**"),
               lower.CL = md("**Lower CI**"),
               upper.CL = md("**Upper CI**"),
               sig = md("**Sig.**")) %>%
    opt_table_font(
      font = "Times New Roman")
  return(table_temp)
}

## Create a table for individual slopes across time within sites in the linear model analyses
ind_slopes_time = function(emm_table){

  emm_table$mas_bin = factor(emm_table$mas_bin,
                           levels = c(0,1,2,3),
                           labels = c("Predawn","Early","Mid","Late"))
  table_temp <- emm_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(estimate = "xvar.trend") %>%
    mutate(site = toupper(site),
           estimate = round(estimate, 3),
           SE = round(SE, 3),
           lower.CL = round(lower.CL, 3),
           upper.CL = round(upper.CL, 3),
           sig = if_else(sign(lower.CL) == sign(upper.CL),"*"," ")) %>%
    # dplyr::group_by(mas_bin) %>%
    gt(groupname_col = "mas_bin", rowname_col = "site") %>%
    # tab_row_group(label = NULL) %>%
    tab_options(row_group.as_column = TRUE,
                stub.font.weight = "bold" # makes mas_bin labels bold
    ) %>%
    tab_style(style = cell_text(weight = "bold"), # makes site labels bold
              locations = cells_row_groups()) %>%
    cols_align('center') %>%
    cols_label(site = md("**Contrast**"),
               estimate = md("**Estimate**"),
               SE = md("**SE**"),
               df = md("**d.f.**"),
               lower.CL = md("**Lower CI**"),
               upper.CL = md("**Upper CI**"),
               sig = md("**Sig.**")) %>%
    opt_table_font(
      font = "Times New Roman")
  return(table_temp)
}

### Aridity Gradient - Linear Model analysis with aridity (gh) or sound attenuation at 8kHz as a continuous variable. Comparing across sites and within mas_bins
## Includes functions to make manuscript-level tables

ag_contrasts_convar_site = function(data,
                                    yvar,
                                    xvar){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(yvar ~ xvar*mas_bin*site + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  lwma  = c(1,0,0,0)
  sswma = c(0,1,0,0)
  cbma  = c(0,0,1,0)
  kiowa = c(0,0,0,1)
  
  emm = emtrends(m, ~ site|mas_bin, 
                 var = "xvar", 
                 type = 'response',
                 weights = "cells") # across sites
  emm_summary = summary(emm) # have to save summary to make it a dataframe
  emm_cntrst = contrast(emm,
                        method = list(
                          "SSWMA - LWMA" = sswma-lwma,
                          "CBMA - LWMA"  = cbma -lwma,
                          "KIOWA - LWMA" = kiowa-lwma,
                          "CBMA - SSWMA"  = cbma -sswma,
                          "KIOWA - SSWMA" = kiowa-sswma,
                          "KIOWA - CBMA" = kiowa-cbma))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # arid_table = aridity_table_mas2(emm_cntrst_summary);arid_table
  arid_table = aridity_contrast_table_site(emm_cntrst_summary);arid_table
  
  slope_table = ind_slopes_site(emm_summary)
  
  my_list = list(summary, # summary of linear model
                 diagnostics, # regression diagnostics plots to see if model is good
                 emm_cntrst_summary, # Emmeans contrast table w/ pvalues
                 emm_confi_summary, # Emmeans contrast table with CI 
                 arid_table, # Good emmeans contrast table made with gh()
                 emm_summary, # Emtrends table saved as a dataframe, slopes of each site
                 slope_table) # Emtrends table saved as a gh() object, manuscript table
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}

aridity_contrast_table_site = function(contrast_table) {
  contrast_table$mas_bin = factor(contrast_table$mas_bin, 
                               levels = c(0,1,2,3),
                               labels = c("Predawn","Early","Mid","Late"))
  
  table_temp <- contrast_table %>%
    data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename(Estimate = "estimate") %>%
    dplyr::mutate(Estimate = round(Estimate, 3),
                  SE = round(SE, 3),
                  t.ratio = round(t.ratio, 3),
                  p.value = round(p.value, 3)) %>%
    dplyr::mutate(sig = determine_sig(p.value)) %>%
    dplyr::mutate(p.value = as.factor(p.value)) %>%
    dplyr::mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
    dplyr::group_by(mas_bin) %>%
    gt(groupname_col = "mas_bin", rowname_col = "contrast") %>%
    # tab_row_group(label = NULL) %>%
    tab_options(row_group.as_column = TRUE,
                stub.font.weight = "bold") %>% # makes contrast labels bold (rowname_col)
    tab_style(style = cell_text(weight = "bold"), # makes mas_bin labels bold (groupname_col)
              locations = cells_row_groups()) %>%
    cols_align('center') %>%
    cols_label(contrast = md("**Contrast**"),
               Estimate = md("**Estimate**"),
               SE = md("**SE**"),
               df = md("**d.f.**"),
               t.ratio = md("**t-ratio**"),
               p.value = md("**p value**"),
               sig = md("**sig.**")) %>%
    opt_table_font(
      font = "Times New Roman")
  return(table_temp)
}


### Aridity Gradient - Linear Model analysis with aridity (gh) or sound attenuation at 8kHz as a continuous variable. Comparing across mourning acoustic periods and within sites
## Includes functions to make manuscript-level tables
ag_contrasts_convar_time = function(data,
                                    yvar,
                                    xvar){
  # m = lm(pc ~ arid_within*mas_bin*x3 + scale(date), data = data)
  m = lm(yvar ~ xvar*mas_bin*site + scale(date), data = data)
  summary = summary(m)
  diagnostics = assump(m)
  # emm = emmeans(m, pairwise ~ x3*x2)
  # # Setting up comparisons for emmeans contrast function
  predawn = c(1,0,0,0)
  early = c(0,1,0,0)
  mid  = c(0,0,1,0)
  late = c(0,0,0,1)
  
  emm = emtrends(m, ~ mas_bin|site, 
                 var = "xvar", 
                 type = 'response',
                 weights = "cells") # across morning acoustic period
  emm_summary = summary(emm) # have to save summary to make it a dataframe
  emm_cntrst = contrast(emm,
                        method = list(
                          "Early - Predawn" = early-predawn,
                          "Mid - Predawn"  = mid-predawn,
                          "Late - Predawn" = late-predawn,
                          "Mid - Early"  = mid-early,
                          "Late - Early" = late-early,
                          "Late - Mid" = late-mid))
  # emm_cntrst = contrast(emm)
  emm_cntrst_summary = summary(emm_cntrst)
  emm_confi_summary = confint(emm_cntrst) # run this for confidence intervals, will need to change the table function
  # arid_table = aridity_table_mas2(emm_cntrst_summary);arid_table
  arid_table = aridity_contrast_table_time(emm_cntrst_summary);arid_table
  
  slope_table = ind_slopes_time(emm_summary);slope_table
  
  my_list = list(summary, # summary of linear model
                 diagnostics, # regression diagnostics plots to see if model is good
                 emm_cntrst_summary, # Emmeans contrast table w/ pvalues
                 emm_confi_summary, # Emmeans contrast table with CI
                 arid_table, # Good emmeans contrast table made with gh()
                 emm_summary, # Emtrends table saved as a dataframe, slopes of each site
                 slope_table) # Emtrends table saved as a gh() object, manuscript table
  # my_list = list(summary, diagnostics, emm_cntrst_summary, emm_confi_summary, emm)
  return(my_list)
}

aridity_contrast_table_time = function(contrast_table) {
  contrast_table$site = factor(contrast_table$site, 
                           levels = c("lwma","sswma","cbma","kiowa"),
                           labels = c("LWMA","SSWMA","CBMA","KIOWA"))
    table_temp <- contrast_table %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::rename(Estimate = "estimate") %>%
      dplyr::mutate(Estimate = round(Estimate, 3),
                    SE = round(SE, 3),
                    t.ratio = round(t.ratio, 3),
                    p.value = round(p.value, 3)) %>%
      dplyr::mutate(sig = determine_sig(p.value)) %>%
      dplyr::mutate(p.value = as.factor(p.value)) %>%
      dplyr::mutate(p.value = dplyr::recode(p.value, "0" = "<0.001")) %>%
      dplyr::group_by(site) %>%
      gt(groupname_col = "site", rowname_col = "contrast") %>%
      # tab_row_group(label = NULL) %>%
      tab_options(row_group.as_column = TRUE,
                  stub.font.weight = "bold") %>% # makes mas_bin labels bold (rowname_col)
      tab_style(style = cell_text(weight = "bold"), # makes site labels bold (groupname_col)
                locations = cells_row_groups()) %>%
      cols_align('center') %>%
      cols_label(
        # contrast = md("**Contrast**"),
                 Estimate = md("**Estimate**"),
                 SE = md("**SE**"),
                 df = md("**d.f.**"),
                 t.ratio = md("**t-ratio**"),
                 p.value = md("**p value**"),
                 sig = md("**sig.**")) %>%
      opt_table_font(
        font = "Times New Roman")
  return(table_temp)
}


### Make a manuscript level table of aridity gradient slopes only (not contrasts) 
ag_slopes_table = function(pc1_slopes,
                           pc2_slopes,
                           pc3_slopes) {
  
  pcs = list(pc1_slopes,pc2_slopes,pc3_slopes)
  pcs_all = NULL
  # Loop through all the mas bins and create a confidence interval table for the site specific slopes
  for(i in 1:length(pcs)){
    table_temp <- pcs[[i]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      mutate(site = toupper(site),
             estimate = round(xvar.trend, 3),
             SE = round(SE, 3),
             lower.CL = round(lower.CL, 3),
             upper.CL = round(upper.CL, 3),
             sig = if_else(sign(lower.CL) == sign(upper.CL),"*"," "))
    
    if(i == 1){
      table_temp = table_temp %>% dplyr::select(-xvar.trend,
                                                -mas_bin) %>%
        dplyr::relocate(site,
                        estimate,
                        SE,df,
                        lower.CL,
                        upper.CL,
                        sig)
      names(table_temp) = paste0(names(table_temp),i)
      # assign(paste0("table_temp",i),table_temp) 
      pcs_all = table_temp
      
    } else {
      table_temp = table_temp %>% dplyr::select(-xvar.trend,
                                                -site,
                                                -mas_bin, 
                                                -df) %>%
        dplyr::relocate(estimate,SE,lower.CL,upper.CL,sig)
      names(table_temp) = paste0(names(table_temp),i)
      pcs_all = cbind(pcs_all, table_temp)
      
    }
  }

  # ### PC1 Table
  # 
  # table1 <- pc1_contrast_table %>%
  #   data.frame(stringsAsFactors = FALSE) %>%
  #   mutate(estimate1 = round(estimate, 3),
  #          SE1 = round(SE, 3),
  #          df1 = df,
  #          t.ratio1 = round(t.ratio, 3),
  #          p.value1 = round(p.value, 3)) %>%
  #   mutate(sig.1 = determine_sig(p.value1)) %>%
  #   mutate(p.value1 = as.factor(p.value1)) %>%
  #   mutate(p.value1 = dplyr::recode(p.value1, "0" = "<0.001")) %>%
  #   dplyr::select(-arid_withinf, -estimate, -SE, -t.ratio, -p.value, -df)
  # 
  # ### PC2 Table
  # table2 <- pc2_contrast_table %>%
  #   data.frame(stringsAsFactors = FALSE) %>%
  #   mutate(estimate2 = round(estimate, 3),
  #          SE2 = round(SE, 3),
  #          t.ratio2 = round(t.ratio, 3),
  #          p.value2 = round(p.value, 3)) %>%
  #   mutate(sig.2 = determine_sig(p.value2)) %>%
  #   mutate(p.value2 = as.factor(p.value2)) %>%
  #   mutate(p.value2 = dplyr::recode(p.value2, "0" = "<0.001")) %>%
  #   dplyr::select(-contrast, -arid_withinf, -estimate, -SE, -t.ratio, -p.value,  -df) 
  # 
  # ### PC3 Table
  # table3<- pc3_contrast_table %>%
  #   data.frame(stringsAsFactors = FALSE) %>%
  #   mutate(estimate3 = round(estimate, 3),
  #          SE3 = round(SE, 3),
  #          t.ratio3 = round(t.ratio, 3),
  #          p.value3 = round(p.value, 3)) %>%
  #   mutate(sig.3 = determine_sig(p.value3)) %>%
  #   mutate(p.value3 = as.factor(p.value3)) %>%
  #   mutate(p.value3 = dplyr::recode(p.value3, "0" = "<0.001")) %>%
  #   dplyr::select(-contrast,-arid_withinf, -estimate, -SE, -t.ratio, -p.value, -df) 
  
  pcs_ag_table = pcs_all %>%
    gt(.) %>%
    cols_align('center') %>%
    tab_spanner(
      label = md("**PC1 - Acoustic Diversity**"),
      columns = c(estimate1, SE1, df1, lower.CL1, upper.CL1, sig1)) %>%
    tab_spanner(
      label = md("**PC2 - Avian Abundance**"),
      columns = c(estimate2, SE2, lower.CL2, upper.CL2, sig2)
    ) %>% 
    tab_spanner(
      label = md("**PC3 - Acoustic Complexity**"),
      columns = c(estimate3, SE3, lower.CL3, upper.CL3, sig3)
    ) %>%
    cols_label(site1 = md("**Site**"),
               estimate1 = md("**Estimate**"),
               SE1 = md("**SE**"),
               df1 = md("**d.f.**"),
               lower.CL1 = md("**Lower CI**"),
               upper.CL1 = md("**Upper CI**"),
               sig1 = md("**Sig.**")) %>%
    cols_label(estimate2 = md("**Estimate**"),
               SE2 = md("**SE**"),
               # df1 = md("**d.f.**"),
               lower.CL2 = md("**Lower CI**"),
               upper.CL2 = md("**Upper CI**"),
               sig2 = md("**Sig.**")) %>%
    cols_label(estimate3 = md("**Estimate**"),
               SE3 = md("**SE**"),
               # df1 = md("**d.f.**"),
               lower.CL3 = md("**Lower CI**"),
               upper.CL3 = md("**Upper CI**"),
               sig3 = md("**Sig.**")) %>%
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
  return(pcs_ag_table)
}

# Function to make dot plot of estimates and standard error
aridity_graph_table = function(emm_summary,
                               y_label,
                               results){
  
  # Add mas_bin labels into emm summary table
  table_predawn <- emm_summary[emm_summary$mas_bin==0,]
  table_early <- emm_summary[emm_summary$mas_bin==1,]
  table_mid <- emm_summary[emm_summary$mas_bin==2,] 
  table_late <- emm_summary[emm_summary$mas_bin==3,] 
  
  # combine tables again (could do this in a for loop)
  table_combined_graph = rbind(table_predawn,
                               table_early,
                               table_mid,
                               table_late)
  
  ### PC1 - Acoustic Diversity
  ggplot(data = table_combined_graph,
         aes(x=arid_factor, y=emmean, color = site)) +
    geom_point(position = position_dodge(0))+
    # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
    geom_line(aes(group = site, 
                  color = site),
              position = position_dodge(0))+
    geom_errorbar(aes(ymin = emmean-SE, # Standard Error
                      ymax = emmean+SE), width = 0.5,
                  position = position_dodge(0))+
    # geom_errorbar(aes(ymin = lower.CL, # Confidence Interval
    #                   ymax = upper.CL), width = 0.5,
    #               position = position_dodge(0))+
    scale_color_manual(values = cbpalette, 
                       name = "Site",
                       labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
    # scale_x_discrete(name = "Aridity - Normalized Within", labels = c("Extremely Humid", "Humid", "Normal","Arid","Extremely Arid")) +
    scale_x_discrete(name = "Aridity - Normalized Within") +
    scale_y_continuous(name = y_label)+
    facet_grid(. ~ mas_bin) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "bottom") +
    # facet_wrap(vars(mas_bin)) + 
    theme(strip.text.y = element_text(angle = 0))
  ggsave(results, dpi = 600, height = 6, width = 8, units = "in")
}
