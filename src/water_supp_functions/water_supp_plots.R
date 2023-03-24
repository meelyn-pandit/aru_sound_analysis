###################################################
########### Water Supplementation Dot Graphs #######
###################################################


# Load Data ---------------------------------------------------------------

load("data_clean/sswma_maslag.Rdata")
load("data_clean/cbma_maslag.Rdata")


# Box Plot ----------------------------------------------------------------

### Paper Graph - Across Sites
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
