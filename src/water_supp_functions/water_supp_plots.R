###################################################
########### Water Supplementation Dot Graphs #######
###################################################

# Box Plot ----------------------------------------------------------------

### SSWMA
sswma_boxplot = function(data,
                         yvar,
                         xvar,
                         ylabel,
                         xlabel,
                         yangle,
                         title){
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  ## Create group labels
  # mas_bin labels
  # aw6$mas_labels = factor(aw6$mas_bin, levels = c("0","1","2","3"),
  #                         labels = c("Predawn","Early","Mid","Late"))
  
  #Boxplot for Water SSWMA vocals Diversity
  ggplot(data = data,
         aes(x=xvar, 
             y=yvar, 
             color = as.factor(ws_site),
             fill=as.factor(water))) +
    # stat_boxplot(geom ='errorbar', width = 0.6) +
    geom_boxplot(width = 0.6) +
    # stat_summary(fun = "mean",
    #              geom = "point",
    #              aes(group=as.factor(ws_site)),
    #              size = 2,
    #              position = position_dodge(0.5), preserve = "single") +
    scale_y_continuous(name = ylabel)+
    scale_color_manual(name = "Water Site",
                       values = c("#009E73","#D55E00","#CC79A7"))+
    scale_fill_manual(name = "Water Access", 
                      labels = c("Closed","Open"),
                      values = c("#ffffff", "#56B4E9"))+
    xlab(label = xlabel) + 
    # facet_grid(~mas_labels) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 0, vjust = 0.5), # 90 for paper, 0 for presentations
          axis.title.x=element_text(),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.margin=margin(t=-20))
  # sswmaw_vocals_plots = plot_grid(wsvocals_day, boxplot_sswma_vocals,align = "v", ncol = 1, rel_heights = c(0.45, 0.55));sswmaw_vocals_plots

}


# Dot Plots ---------------------------------------------------------------

### Paper Graph - Across Sites
sswma_dotplot = function(data,
                         yvar,
                         xvar,
                         ylabel,
                         xlabel,
                         water,
                         yangle,
                         title,
                         stat_test){
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  
  #Dotplot for Water SSWMA vocals Diversity
  ggplot(data = data,
         aes(x=xvar, 
             y=yvar, 
             group = mas_labels,
             color = as.factor(ws_site))) +
    geom_point(position = position_dodge(0.5),
               shape = 21,
               size = 10,
               stroke = 3,
               aes(fill = as.factor(water)))+
    guides(color = guide_legend(override.aes = list(size = 3)))+
        geom_errorbar(aes(ymin = yvar-SE, 
                      ymax = yvar+SE), 
                  width = 0.5,
                  size = 2,
                  position = position_dodge(0.5))+
    scale_x_discrete(name = xlabel)+
    scale_y_continuous(name = ylabel)+
    scale_color_manual(name = "Water Site",
                       values = c("#009E73",
                                  "#D55E00","#CC79A7"))+
    scale_fill_manual(name = "Water Access", 
                      labels = c("Closed",
                                 "Open"),
                      values = c("#ffffff", 
                                 "#56B4E9"))+
    ggtitle(title)+
    # xlab(label = xlabel) +
    facet_grid(~factor(mas_labels,
                       levels = c(0,1,2,3),
                       labels = c("Predawn\n(Before Sunrise)",
                                  "Early\n(~1 hr After Sunrise)",
                                  "Mid\n(~3 hr After Sunrise)",
                                  "Late\n(~5 hr After Sunrise)")),
               switch = "both") +
    theme_classic(base_size = 25) +
    theme(axis.title.y = element_text(angle = 0, 
                                      vjust = 0.5), # 90 for paper, 0 for presentations
          # axis.title.x=element_text(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_text(size = 17),
          # axis.text.x = element_text(),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold"),
          # plot.subtitle.position = "plot",
          plot.caption.position = "plot",
          # legend.key.size = 10,
          legend.key.width = unit(1, 'in'),
          legend.key.height = unit(0, "in"),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.margin=margin(t=-20))+
    stat_pvalue_manual(
      stat_test,
      y.position = stat_test$y.position,
      step.increase = 0.1,
      tip.length = stat_test$tip.length,
      label = "p.adj.sig",
      size = 10,
      bracket.size = 1,
      hide.ns = TRUE
    )
}

cbma_dotplot = function(data,
                         yvar,
                         xvar,
                         ylabel,
                         xlabel,
                         water,
                         yangle,
                         title,
                         stat_test){
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  
  #Dotplot for Water CBMA vocals Diversity
  ggplot(data = data,
         aes(x=xvar, 
             y=yvar, 
             group = mas_labels,
             color = as.factor(ws_site))) +
    geom_point(position = position_dodge(0.5),
               shape = 21,
               size = 10,
               stroke = 3,
               aes(fill = as.factor(water)))+
    geom_errorbar(aes(ymin = yvar-SE, 
                      ymax = yvar+SE), 
                  width = 0.5,
                  size = 2,
                  position = position_dodge(0.5))+
    scale_x_discrete(name = xlabel)+
    scale_y_continuous(name = ylabel)+
    scale_color_manual(name = "Water Site",
                       values = c("#009E73",
                                  "#D55E00",
                                  "#CC79A7"
                                  ))+
    scale_fill_manual(name = "Water Access", 
                      labels = c("Closed",
                                 "Open"),
                      values = c("#ffffff", 
                                 "#56B4E9"))+
    ggtitle(title)+
    # xlab(label = xlabel) +
    facet_grid(~factor(mas_labels,
                       levels = c(0,1,2,3),
                       labels = c("Predawn\n(Before Sunrise)",
                                  "Early\n(~1 hr After Sunrise)",
                                  "Mid\n(~3 hr After Sunrise)",
                                  "Late\n(~5 hr After Sunrise)")),
               switch = "both") +
    theme_classic(base_size = 25) +
    theme(axis.title.y = element_text(angle = 0, 
                                      vjust = 0.5), # 90 for paper, 0 for presentations
          # axis.title.x=element_text(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          strip.text.x = element_text(size = 17),
          # axis.text.x = element_text(),
          plot.title.position = "plot",
          plot.title = element_text(face = "bold"),
          # plot.subtitle.position = "plot",
          plot.caption.position = "plot",
          legend.key.size = 10,
          legend.key.width = unit(1, 'in'),
          legend.key.height = unit(0, "in"),
          legend.position = "bottom",
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.margin=margin(t=-20))+
    stat_pvalue_manual(
      stat_test,
      y.position = stat_test$y.position,
      # y.position = c(0.5,1,1.5),
      step.increase = 0.1,
      # tip.length = stat_test$tip.length,
      tip.length = 0.05,
      label = "p.adj.sig",
      size = 10,
      bracket.size = 1,
      hide.ns = FALSE
    )
}
