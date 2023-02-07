#########################################
######### Aridity Gradient Graphs ######
#########################################

### Paper Graph - Across Sites
ag_graph_site_paper = function(yvar,
                               xvar,
                               ylabel,
                               xlabel){
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  ## Create group labels
  # mas_bin labels
  aw6$mas_labels = factor(aw6$mas_bin, levels = c("0","1","2","3"),
                          labels = c("Predawn","Early","Mid","Late"))
  
  # site labels
  aw6$site_labels = factor(aw6$site, levels = c("lwma","sswma","cbma","kiowa"),
                           labels = c("LWMA","SSWMA","CBMA","KIOWA"))
  
  ggplot(data = aw6,
         aes(x=xvar, y=yvar, color = site)) +
    # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
    geom_smooth(method = lm) +
    scale_color_manual(values = cbpalette, 
                       name = "Site",
                       labels = c("LWMA","SSWMA","CBMA","KIOWA"))+
    scale_x_continuous(name = xlabel)+
    scale_y_continuous(name = ylabel)+
    # facet_grid(~facet_type) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "right") +
    # facet_wrap(vars(mas_bin)) + 
    ggtitle(label = "Comparisons across Site") +
    facet_grid(~mas_labels)+
    theme(strip.text.y = element_text(angle = 0))
}
ggsave('results/arid_pc1_dt.png', dpi = 600, height = 11, width = 8, units = "in")

### Paper Graph - Within Sites, Across Time
ag_graph_time_paper = function(yvar,
                               xvar,
                               ylabel,
                               xlabel){
  
  # Set color palette
  cbpalette <- c("#56B4E9", "#009E73", "#E69F00", "#D55E00", "#F0E442", "#0072B2", "#CC79A7","#999999") # Set color palette for graphs
  
  dawn_palette <- c("#4e5978", "#308697", "#d46671", "#e79c63",  "#bb6800", "#CC79A7","#999999") # Set color palette for graphs - dawn colors for time
  ## Create group labels
  # mas_bin labels
  aw6$mas_labels = factor(aw6$mas_bin, levels = c("0","1","2","3"),
                          labels = c("Predawn","Early","Mid","Late"))
  
  # site labels
  aw6$site_labels = factor(aw6$site, levels = c("lwma","sswma","cbma","kiowa"),
                           labels = c("LWMA","SSWMA","CBMA","KIOWA"))
  ggplot(data = aw6,
         # aes(x=gh, y=pc, group = mas_labels, linetype = mas_labels)) +
         aes(x = xvar, y = yvar, color = mas_labels)) +
    # ggtitle("Datetime Summarized - PC1 - Acoustic Diversity")+
    geom_smooth(method = lm) +
    # scale_linetype_discrete(name = "mas_labels") +
    # scale_color_manual(values = dawn_palette, 
    #                    name = "Morning\nAcoustic\nPeriod",
    #                    labels = c("Predawn","Early","Mid","Late")) +
    scale_color_viridis(discrete = TRUE,
                        option = "B",
                        name = "Morning\nAcoustic\nPeriod",
                        labels = c("Predawn","Early","Mid","Late"))+
    # scale_color_brewer(palette = "YlGnBu",
    #                    name = "Morning\nAcoustic\nPeriod",
    #                    labels = c("Predawn","Early","Mid","Late"),
    #                    direction = -1) +
    scale_x_continuous(name = xlabel) +
    scale_y_continuous(name = ylabel) +
    # facet_grid(~facet_type) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5), # change angle to 0 for presentations
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = "right") +
    # facet_wrap(vars(mas_bin)) + 
    facet_grid(~site_labels)+
    ggtitle(label = "Comparisons within Site, Across Morning Singing Period (mas_bin)") +
    theme(strip.text.y = element_text(angle = 0))
}

ag_graph_time_paper(aw6$pc1, 
                    aw6$gh,
                    "PC1 - Acoustic Diversity",
                    "Evaporation Rate (kg of water/h")

ggsave('results/arid_pc1_dt.png', dpi = 600, height = 11, width = 8, units = "in")

