# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "sf",
    "tigris",
    "factoextra",
    "usmap",
    "viridis",
    "gridExtra",
    "scales"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop(
    "The following required packages are not installed:\n\n  ",
    paste(setdiff(packages, installed.packages()), collapse= "\n  "),
    "\n\nPlease install these and rerun the script."
  )
}

library(tidyverse)
library(viridis)

# GET MAPS   -------------------------------------------------------------------

# Read main sf data
main_sf <- 
  readRDS("./output/datasets/main/main_sf.RDS") %>%
  sf::st_simplify(dTolerance = 1e2) %>%
  tigris::shift_geometry()  %>%
  filter(ZCTA5CE20 != "89412")

# Get state map for outlines
states_sf <-
  tigris::states(year = 2020)  %>%
  filter(!grepl("^PR|^MP|^GU|^VI|^AS", STUSPS)) %>%
  sf::st_simplify(dTolerance = 1e3) %>% 
  tigris::shift_geometry()

# SCI maps
sci_sf <-
  main_sf %>%
  dplyr::filter(data_flag == "valid")  %>%
  dplyr::select(ZCTA5CE20, sci_mean_rel_own, sci_w_geodistance_rel_gnb) %>%
  mutate(sci_w_geodistance_rel_gnb = as.numeric(sci_w_geodistance_rel_gnb)) %>%
  # mutate(sci_w_geodistance = as.numeric(sci_w_geodistance)/1000) %>%
  tidyr::pivot_longer(cols = c("sci_mean_rel_own", "sci_w_geodistance_rel_gnb")) 

sci_sf  <- split(sci_sf ,sci_sf$name)

sci_sf$sci_mean_rel_own$name <- "'\n A. Log. of relative connectedness to social neighbours \n'"
sci_sf$sci_w_geodistance_rel_gnb$name <- "'\n B. Log. of relative distance to social neighbours \n'"


p1 <- ggplot(sci_sf$sci_mean_rel_own) +
  geom_sf(aes(fill = log(value), colour = log(value)), color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) +
  facet_wrap(~name, ncol = 1, labeller = label_parsed) +
  scale_fill_viridis(
    option = "mako",
    direction = -1,
    breaks = c(-7, -5, -3),
    labels = c(-7, -5, -3)
  ) +
  scale_color_viridis(
    option = "mako",
    direction = -1,
    breaks = c(-7, -5, -3),
    labels = c(-7, -5, -3)
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

p2 <- ggplot(sci_sf$sci_w_geodistance) +
  geom_sf(aes(fill = log(value), colour = log(value)), color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) +
  facet_wrap(~name, ncol = 1, labeller = label_parsed) +
  scale_fill_viridis(
    option = "mako",
    direction = -1,
    breaks = c(5, 4, 3, 2),
    labels = c(5, 4, 3, 2)
  ) +
  scale_color_viridis(
    option = "mako",
    direction = -1,
    breaks = c(5, 4, 3, 2),
    labels = c(5, 4, 3, 2)
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

gridExtra::grid.arrange(p1, p2, ncol = 1)

ggsave(filename = "./output/plots/scimaps.pdf", gridExtra::arrangeGrob(p1, p2, ncol = 1),  width = 90, height = 110, dpi = 700, units = "mm", device='pdf')
ggsave(filename = "./output/plots/scimaps.png", gridExtra::arrangeGrob(p1, p2, ncol = 1),  width = 90, height = 110, dpi = 250, units = "mm", device='png')

# Data flag map
ggplot(main_sf) +
  geom_sf(aes(fill = data_flag, colour = data_flag), color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) +
  scale_fill_manual(
    values = c("no sci" = "snow3", "valid" = "black"),
    labels = c("no sci" = "Missing", "valid" = "Available")
  ) +
  scale_color_manual(
    values = c("no sci" = "snow3", "valid" = "black"),
    labels = c("no sci" = "Missing", "valid" = "Available")
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

ggsave(filename = "./output/plots/dataflagmap.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/dataflagmap.png", width = 90, height = 50, dpi = 250, units = "mm", device='png')


# Voting map  (Dem two-party share and indep share)
ggplot(data = filter(main_sf, data_flag == "valid")) + 
  geom_sf(aes(fill = dem_tp_sh*100, colour = dem_tp_sh*100), linewidth = 0, color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) + 
  scale_fill_gradient2(low = "#FF0000", mid = "#eeeeee", high = "#0000FF", midpoint = 50, space = "Lab",aesthetics = "fill") +
  scale_color_gradient2(low = "#FF0000", mid = "#eeeeee", high = "#0000FF", midpoint = 50, space = "Lab",aesthetics = "color") +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

ggsave(filename = "./output/plots/demvotmap.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/demvotmap.png", width = 90, height = 50, dpi = 400, units = "mm", device='png')


ggplot(data = filter(main_sf, data_flag == "valid")) + 
  geom_sf(aes(fill = ind_sh*100, colour = ind_sh*100), linewidth = 0, color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.15) + 
  scale_fill_viridis(
    option = "mako", 
    direction = -1,
    breaks = c(0, 5, 10),
    labels = c(0, 5, 10)
  ) +
  scale_color_viridis(
    option = "mako", 
    direction = -1,
    breaks = c(0, 5, 10),
    labels = c(0, 5, 10)
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

ggsave(filename = "./output/plots/indvotmap.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/indvotmap.png", width = 90, height = 50, dpi = 250, units = "mm", device='png')

# NCES map
ggplot(data = filter(main_sf, data_flag == "valid")) + 
  geom_sf(aes(fill = nces, colour = nces), linewidth = 0, color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) + 
  scale_fill_manual(
    values = c(
      "City" = "darkred",
      "Suburban" = "gold" ,
      "Rural - Fringe" = "darkolivegreen1",
      "Rural - Distant"= "darkolivegreen3",
      "Rural - Remote" = "darkolivegreen"
    ),
    breaks = c("City", "Suburban", "Town", "Rural - Fringe", "Rural - Distant", "Rural - Remote")
  ) +
  scale_color_manual(
    values = c(
      "City" = "darkred",
      "Suburban" = "gold" ,
      "Rural - Fringe" = "darkolivegreen1",
      "Rural - Distant"= "darkolivegreen3",
      "Rural - Remote" = "darkolivegreen"
    ),
    breaks = c("City", "Suburban", "Town", "Rural - Fringe", "Rural - Distant", "Rural - Remote")
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

ggsave(filename = "./output/plots/ncesmap.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/ncesmap.png", width = 90, height = 50, dpi = 250, units = "mm", device='png')


# LISA cluster map
ggplot(data = filter(main_sf, data_flag == "valid")) + 
  geom_sf(aes(fill = lisa_clusters, colour = lisa_clusters), linewidth = 0, color ="transparent") +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) + 
  scale_fill_manual(
    values = c(
      "Not significant" = "snow3",
      "High-High" = "#0000FF" ,
      "Low-Low" = "#FF0000",
      "Low-High" = "#f4ada8",
      "High-Low"= "#a7adf9"
    )
  ) +
  scale_color_manual(
    values = c(
      "Not significant" = "snow3",
      "High-High" = "#0000FF" ,
      "Low-Low" = "#FF0000",
      "Low-High" = "#f4ada8",
      "High-Low"= "#a7adf9"
    )
  ) +
  theme_void() + 
  theme(
    legend.title=element_blank(),
    legend.justification = "center",
    #  plot.margin=unit(c(-5,1,-5,1), "cm"),
    legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
    legend.text=element_text(size = 5),
    legend.key.width = unit(0.1, "cm"),
    legend.key.height = unit(0.3, "cm")
  ) 

ggsave(filename = "./output/plots/morans.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/morans.png", width = 90, height = 50, dpi = 250, units = "mm", device='png')


# Measure difference map

main_sf <-
  main_sf %>%
  mutate(
    sci_w_geodistance_qn = factor(
      ntile(sci_w_geodistance, 5),
      levels = as.character(1:5),
      labels = as.character(1:5)
    ),
    sci_w_geodistance_rel_gnb_qn = factor(
      ntile(sci_w_geodistance_rel_gnb, 5),
      levels = as.character(1:5),
      labels = as.character(1:5)
    ),
    sci_qn_su_diff = as.numeric(sci_w_geodistance_rel_gnb_qn) - as.numeric(sci_w_geodistance_qn)
  ) 

ggplot(filter(main_sf, data_flag == "valid")) +
  geom_sf(aes(fill = sci_qn_su_diff, colour = sci_qn_su_diff, color ="transparent")) +
  geom_sf(data = states_sf, colour = "snow4", fill = NA, linewidth = 0.05) +
  theme_void() +   
  scale_fill_gradient2(low = "#D2B48C", mid = "#eeeeee", high = "#20B2AA", midpoint = 0, space = "Lab",aesthetics = "fill", breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
  scale_color_gradient2(low = "#D2B48C", mid = "#eeeeee", high = "#20B2AA", midpoint = 0, space = "Lab",aesthetics = "color", breaks = c(-2, 0, 2), labels = c(-2, 0, 2)) +
   theme(
    legend.title=element_blank(),
     legend.justification = "center",
     #  plot.margin=unit(c(-5,1,-5,1), "cm"),
       legend.margin = margin(l = -5),
    strip.text = element_text(size = 5),
   legend.text=element_text(size = 5),
       legend.key.width = unit(0.1, "cm"),
     legend.key.height = unit(0.3, "cm")
     ) 

ggsave(filename = "./output/plots/reldiffmap.pdf", width = 90, height = 50, dpi = 600, units = "mm", device='pdf')
ggsave(filename = "./output/plots/reldiffmap.png", width = 90, height = 50, dpi = 250, units = "mm", device='png')

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

rm(list = ls())
gc()


