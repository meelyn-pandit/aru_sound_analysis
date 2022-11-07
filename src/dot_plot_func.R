##################################################################
################ Plots used for ARU Analysis ####################
##################################################################


# DotPlots ----------------------------------------------------------------

dotplot = function(data, 
                         xmean, ymean, yse, 
                         group_factor, group_title, group_label,
                         xtitle, xlabel, ytitle, ylabel, 
                         facet_type,
                         pd, 
                         palette, 
                         angle,
                         title,
                         legend_position){
  
  ggplot(data = data,
         aes(x=as.factor(xmean), y=ymean, color = group_factor)) +
    geom_point(position = position_dodge(pd))+
    ggtitle(title)+
    geom_line(aes(group = group_factor, 
                  color = group_factor),
                  position = position_dodge(pd))+
    geom_errorbar(aes(ymin = ymean-yse, 
                      ymax = ymean+yse), width = 0.2,
                  position = position_dodge(pd))+
    scale_color_manual(values = palette, 
                       name = group_title,
                       labels = group_label
                       )+
    scale_x_discrete(name = xtitle, labels = xlabel)+
    scale_y_continuous(name = ytitle)+
    # facet_grid(~facet_type) +
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_text(angle = angle, vjust = 0.5),
          plot.title = element_text(hjust = 0, vjust = 0),
          legend.position = legend_position)

}


# Scatterplot with Rectangles ---------------------------------------------

sswma_rectangle_plot = function(data, 
                          xvar, yvar, yse, 
                          group_factor, group_title, group_label,
                          xtitle, xlabel, ytitle, ylabel, 
                          facet_type,
                          pd, 
                          palette, 
                          angle,
                          title,
                          legend_position){
  #SSWMA Water Supplementation Graphs - Day-binned date on x-axis and specific species on y-axis
  sswma1_rec1 <- data.frame (xmin=as_date("2021-05-17"), 
                             xmax=as_date("2021-05-30"), 
                             ymin=-Inf, ymax=Inf) #start of water site 1 with water
  sswma2_rec1 <- data.frame (xmin=as_date("2021-05-30"), 
                             xmax=as_date("2021-06-13"), 
                             ymin=-Inf, ymax=Inf) #start of water site 2 with water
  sswma1_rec2 = data.frame (xmin=as_date("2021-06-13"), 
                            xmax=as_date("2021-07-02"), 
                            ymin=-Inf, ymax=Inf) #start of water site 1 with water
  sswma2_rec2 = data.frame (xmin=as_date("2021-07-03"), 
                            xmax=as_date("2021-08-07"), 
                            ymin=-Inf, ymax=Inf) #start of water at water site 2
  
  ### SSWMA Water Supplementation Rectangle Graphs
  ggplot(data = data, aes(x=xvar, y=yvar, 
                              color = as.factor(group_factor))) +
    geom_point(size = 3, position = position_dodge(pd))+
    geom_rect(data=sswma1_rec1, 
              aes(xmin=xmin, 
                  xmax=xmax, 
                  ymin=ymin, 
                  ymax=ymax), 
              fill="#56B4E9", 
              alpha=0.1, 
              inherit.aes = FALSE) +
    geom_rect(data=sswma1_rec2, 
              aes(xmin=xmin, 
                  xmax=xmax, 
                  ymin=ymin, 
                  ymax=ymax), 
              fill="#56B4E9", 
              alpha=0.1, 
              inherit.aes = FALSE) +
    geom_rect(data=sswma2_rec1, 
              aes(xmin=xmin, 
                  xmax=xmax, 
                  ymin=ymin, 
                  ymax=ymax), 
              fill="#E69F00", 
              alpha=0.1, 
              inherit.aes = FALSE) +
    geom_rect(data=sswma2_rec2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="#E69F00", alpha=0.1, inherit.aes = FALSE) +
    geom_smooth(method = "lm")+
    scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),name = group_title)+
    scale_x_date(name = xtitle)+
    scale_y_continuous(name = ytitle)+
    theme_classic(base_size = 10) +
    theme(axis.title.y = element_text(angle = 90, vjust = 0.5),
          # axis.title.x=element_text(),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(),
          legend.position = "right")

}


  # Day bin - CBMA Water Station Rectangle Graphs -------------------------------------
cbma_rectangle_plot = function(data, 
                                xvar, yvar, yse, 
                                group_factor, group_title,
                               group_label,
                                xtitle, xlabel, ytitle, ylabel, 
                                facet_type,
                                pd, 
                                palette, 
                                angle,
                                title,
                                legend_position){
  cbma1_rec1 <- data.frame (xmin=as_date("2021-05-14"), xmax=as_date("2021-06-04"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
  cbma1_rec2 = data.frame (xmin=as_date("2021-06-25"), xmax=as_date("2021-07-19"), ymin=-Inf, ymax=Inf) #start of water site 1 with water
  cbma2_rec2 = data.frame (xmin=as_date("2021-07-03"), xmax=as_date("2021-08-07"), ymin=-Inf, ymax=Inf) #start of water at water site 2
  
  ### CBMA ACI Rectangle Graph
  ggplot(data = data, aes(x=xvar, y= yvar, 
                               color = as.factor(group_factor))) +
    geom_point(size = 3, position = position_dodge())+
    geom_rect(data=cbma1_rec1, 
              aes(xmin=xmin, 
                  xmax=xmax, 
                  ymin=ymin, 
                  ymax=ymax), 
              fill="#56B4E9", 
              alpha=0.1, 
              inherit.aes = FALSE) +
    geom_rect(data=cbma1_rec2, 
              aes(xmin=xmin, 
                  xmax=xmax, 
                  ymin=ymin, 
                  ymax=ymax), 
              fill="#56B4E9", 
              alpha=0.1, 
              inherit.aes = FALSE) +
    geom_smooth(method = "lm")+
    scale_color_manual(values = c("#009E73","#D55E00","#CC79A7"),
                       name = group_title)+
    scale_x_date(name = xtitle)+
    scale_y_continuous(name = ytitle)+
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_text(angle = angle, vjust = 0.5), # change to 0 for presentations
          # axis.title.x=element_text(),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(),
          legend.position = legend_position)

}

