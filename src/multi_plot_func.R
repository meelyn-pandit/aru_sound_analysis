# Completion Rates - Multiple Frequencies ---------------------------------
dotplot = function(data, 
                         xmean, ymean, yse, 
                         group_factor, group_label,
                         xtitle, ytitle, xlabel, ylabel, 
                         facet_type,
                         pd, 
                         palette, 
                         angle,
                         legend_position){
  
  ggplot(data = data,
         aes(x=as.factor(xmean), y=ymean, color = group_factor)) +
    geom_point(position = position_dodge(pd))+
    geom_errorbar(aes(ymin = ymean-yse, ymax = ymean+yse), width = 0.2,
                  position = position_dodge(pd))+
    # geom_smooth(method = "lm")+
    scale_color_manual(values = palette, name = group_label)+
    scale_x_discrete(name = xtitle, labels = xlabel)+
    scale_y_continuous(name = ytitle)+
    # facet_grid(~facet_type) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = angle, vjust = 0.5),
          # axis.title.x=element_blank(),
          # axis.text.x = element_blank(),
          legend.position = legend_position)
  

}
