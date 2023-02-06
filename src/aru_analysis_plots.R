
# ARU Analysis Plots ------------------------------------------------------
aci_dot = dotplot(data = graph_acidate,
                  xmean = graph_acidate$arid_within,
                  ymean = graph_acidate$aci_mean,
                  yse = graph_acidate$aci_se,
                  group_factor = graph_acidate$site,
                  group_title = "Site",
                  group_label = c("LWMA","SSWMA","CBMA","KIOWA"),
                  xtitle  = element_blank(),
                  ytitle = "Mean ACI",
                  xlabel = element_blank(),
                  pd = 0.5,
                  palette = cbpalette,
                  angle = 90,
                  title = "A",
                  legend_position = "none");aci_dot

bio_dot = dotplot(data = graph_biodate,
                  xmean = graph_biodate$arid_within,
                  ymean = graph_biodate$bio_mean,
                  yse = graph_biodate$bio_se,
                  group_factor = graph_acidate$site,
                  group_title = "Site",
                  group_label = c("LWMA","SSWMA","CBMA","KIOWA"),
                  xtitle = element_blank(),
                  xlabel = element_blank(),
                  ytitle = "Mean BIO",
                  pd = 0.5,
                  palette = cbpalette,
                  angle = 90,
                  title = "B",
                  legend_position = "none");bio_dot


vocal_dot = dotplot(data = graph_arid_date,
                    xmean = graph_arid_date$arid_within,
                    ymean = graph_arid_date$vocals_mean,
                    yse = graph_arid_date$vocals_se,
                    group_factor = graph_arid_date$site,
                    group_title = "Site",
                    group_label = c("LWMA","SSWMA","CBMA","KIOWA"),
                    xtitle = element_blank(),
                    xlabel = element_blank(),
                    ytitle = "Mean Vocal Num.",
                    pd = 0.5,
                    palette = cbpalette,
                    angle = 90,
                    title = "C",
                    legend_position = "none");vocal_dot

species_dot = dotplot(data = graph_arid_date,
                    xmean = graph_arid_date$arid_within,
                    ymean = graph_arid_date$species_mean,
                    yse = graph_arid_date$species_se,
                    group_factor = graph_arid_date$site,
                    group_title = "Site",
                    group_label = c("LWMA","SSWMA","CBMA","KIOWA"),
                    xtitle = "Aridity",
                    ytitle = "Mea Species Num.",
                    xlabel = c("Extremely\nHumid",
                               "Humid",
                               "Normal",
                               "Arid",
                               "Extremely\nArid"),
                    pd = 0.5,
                    palette = cbpalette,
                    angle = 90,
                    title = "D",
                    legend_position = "bottom");species_dot

arid_plots <- plot_grid(aci_dot,bio_dot,vocal_dot,species_dot, 
                        align = "hv", axis = "l",
                        ncol = 1, nrow = 4,
                        rel_heights = c(0.20,0.20,0.20,0.30));arid_plots

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/aru_sound_analysis/results/")
ggsave("arid_within_plots.jpg", plot = arid_plots, dpi = 600, width = 6, height = 8, units = "in")

# Scatterplot with Rectangles ---------------------------------------------
load("results/aci_water.Rdata")
load("results/bio_water.Rdata")



rectangle
setwd("/home/meelyn/Documents/dissertation/aru_sound_analysis/")
ggsave("results/noca_sswma_water_daybin_results.jpg", width = 8, height = 6, units = "in", dpi = 600)