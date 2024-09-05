# LOAD PACKAGES  ---------------------------------------------------------------
  
  packages <-
    c(
      "tidyverse",
      "effects",
      "sf"
    )
  
  if(length(setdiff(packages, installed.packages())) > 0){
    stop(
      "The following required packages are not installed:\n\n  ",
      paste(setdiff(packages, installed.packages()), collapse= "\n  "),
      "\n\nPlease install these and rerun the script."
    )
  }
  
  library(tidyverse)
  
  
# GET TABLES  ------------------------------------------------------------------

# Read main data,keep only valid, and match ZCTAs to regions
main_df <- 
  readRDS("./output/datasets/main/main_sf.RDS") %>%
  filter(data_flag == "valid") %>%
  sf::st_drop_geometry() %>%
  mutate(
    is_landslide = ifelse(dem_tp_sh > 0.60 | dem_tp_sh < 0.4, 1, 0),
    region = case_when(
      stab %in% c("ME", "VT", "NH", "MA", "CT", "RI", "NY", "PA", "NJ") ~ "Northeast",
      stab %in% c("ND", "MN", "SD", "WI", "MI", "OH", "IN", "IL", "IA", "MO", "NE", "KS") ~ "Midwest", 
      stab %in% c("WA", "MT", "OR", "ID", "WY", "NV", "UT", "CO", "CA", "AZ", "NM", "HI", "AK") ~ "West",
      stab %in% c("DE", "MD", "DC", "WV", "VA", "NC", "KY", "TN", "SC", "GA", "AL", "MS", "FL", "AR", "LA", "OK", "TX") ~ "South"
    )
  )  # %>%
# filter(ZCTA5CE20 != "89412")

# Stack data frame to include entries for "All regions"
main_df <-
  rbind(main_df, mutate(main_df, region = "All regions"))

# Define factors and reference categories
main_df <-
  main_df %>%
  rename(
    cluster = lisa_clusters
  ) %>%
  mutate(
    nces = factor(
      nces, 
      levels = c("City", "Suburban", "Rural - Fringe", "Rural - Distant", "Rural - Remote"),
      labels = c("City", "Suburb", "Rural - fringe", "Rural - distant", "Rural - remote"),
    ),
    region = factor(
      region, 
      levels = c("All regions" , "Northeast", "South", "Midwest", "West"),
      labels = c("All regions", "Northeast", "South", "Midwest", "West"),
    ),
    cluster  = factor(
      cluster,
      levels = c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"),
      labels = c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"),
    ), 
    sci_mean_rel_own_qn = factor(
      ntile(sci_mean_rel_own, 5),
      levels = as.character(1:5),
      labels = as.character(1:5)
    ),
    sci_w_geodistance_qn = factor(
     ntile(sci_w_geodistance, 5),
     levels = as.character(1:5),
     labels = as.character(1:5)
    ),
    sci_w_geodistance_rel_min_qn = factor(
      ntile(sci_w_geodistance_rel_min, 5),
      levels = as.character(1:5),
      labels = as.character(1:5),
    ),
    sci_w_geodistance_rel_gnb_qn = factor(
      ntile(sci_w_geodistance_rel_gnb, 5),
      levels = as.character(1:5),
      labels = as.character(1:5)
    ),
    RDOMESTICMIG_qn = factor(
      ntile(RDOMESTICMIG, 5),
      levels = as.character(1:5),
      labels = as.character(1:5)
    )
  )


# Shares by region*nces with confidence intervals

pop_df <- 
  main_df %>%
  group_by(cluster, region, .drop = F) %>%
  summarise(
    value = 100*sum(tpop)/sum(filter(main_df, region == "All regions")$tpop),
    ci95 = 1.96*100*sqrt((sum(tpop)/sum(filter(main_df, region == "All regions")$tpop))*(1 - sum(tpop)/sum(filter(main_df, region == "All regions")$tpop))/nrow(filter(main_df, region == "All regions")))
  ) %>%
  ungroup() %>%
  mutate(
    var = "Share of total population"
  )

n_df <- 
  main_df %>%
  group_by(cluster, region, .drop = F) %>%
  summarise(
    value = 100*n()/nrow(filter(main_df, region == "All regions")),
    ci95 = 1.96*100*sqrt((n()/nrow(filter(main_df, region == "All regions")))*(1 - n()/nrow(filter(main_df, region == "All regions")))/nrow(filter(main_df, region == "All regions")))
  ) %>%
  ungroup() %>%
  mutate(
    var = "Share of all ZCTAs"
  )

share_plot <-
  rbind(pop_df, n_df)

share_table <- lst()
share_table[["A"]] <- reshape2::acast(share_plot, region + var ~ cluster, value.var = c("value"))
share_table[["B"]] <- reshape2::acast(share_plot, region + var ~ cluster, value.var = c("ci95"))

error_bars <-
  share_plot %>%
  group_by(region, var) %>%
  arrange(fct_rev(cluster)) %>%
  mutate(mean_val_new = cumsum(value)) %>%
  ungroup()

ggplot(share_plot, aes(y = fct_rev(region), x = value, fill = cluster, group = cluster)) +
  geom_bar(stat="identity", colour = "black", linewidth = 0.001) +
  geom_errorbar(data = error_bars, aes(xmin = mean_val_new - ci95, xmax = mean_val_new + ci95), width = 0.2) +
  facet_wrap(
     ~ var,
     ncol = 1
  ) + 
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(vjust = 0.1, hjust=0.95, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    axis.title.y = element_blank(),
    legend.position = "top",
    legend.text=element_text(size= 7),
    legend.key.size = unit(0.6,"line"),
    legend.title = element_blank()
  ) +
  labs(
   x = "\n Share (%)"
  )  +
  scale_fill_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.6),
      "High-High" = alpha("#0000FF", 0.6) ,
      "Low-Low" = alpha("#FF0000", 0.6) ,
      "Low-High" = alpha("#f4ada8", 0.6),
      "High-Low"= alpha("#a7adf9", 0.6) 
    )
  ) + 
  guides(color = guide_legend(nrow = 2)) +
   scale_x_continuous(limits = c(0, 101), breaks = seq(0, 100, 10), expand = c(0, 0))


ggsave(filename = "./output/plots/sharechart.pdf",  width = 130, height = 120, dpi = 700, units = "mm", device='pdf')

# Table

share_tab <- list()

share_tab[[1]] <-
  share_plot %>%
  pivot_wider(
    id_cols = c(var, region),
    names_from = cluster,
    values_from = c(value),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "value"
  )

share_tab[[2]] <-
  share_plot %>%
  mutate(
    ci95 = ci95/1.96 # Convert to SE
  ) %>%
  pivot_wider(
    id_cols = c(var, region),
    names_from = cluster,
    values_from = c(ci95),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "se"
  )

share_tab <- 
  bind_rows(share_tab) %>%
  arrange(var, region) %>%
  mutate(
    across(c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"), ~round(., 2))
  )

write.csv(share_tab, "./output/tables/sharetable.csv") 


# Multinomial regression of clusters on region*settlements, and fitted probabilities

main_df <-
  main_df %>%
  mutate(
    cluster = relevel(cluster, ref = "Low-High"),
    nces = relevel(nces, ref = "Rural - distant"),
    region = relevel(region, ref = "Northeast")
  )

morancl_model_regnces <-
  nnet::multinom(
    cluster ~ region*nces, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_regnces <-
  effects::allEffects(morancl_model_regnces) 

morancl_model_results_regnces <- 
  as.data.frame(morancl_model_results_regnces)[[1]] %>%
  select(nces, region, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_regnces <- pivot_longer(
  morancl_model_results_regnces,
  cols = c(-nces, -region),
  names_to = "var",
  values_to = "value"
  ) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Settlement type, \n without controls"
  ) %>%
  rename(
    value = nces
  )

morancl_model_regnces_ctrl <-
  nnet::multinom(
    cluster ~ region*nces  + medianage + medianhhinc + white_pct + ownoccup_pct + undergrad_pct25ov, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_regnces_ctrl <-
  effects::allEffects(morancl_model_regnces_ctrl) 

morancl_model_results_regnces_ctrl <- 
  as.data.frame(morancl_model_results_regnces_ctrl)[[6]] %>%
  select(nces, region, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_regnces_ctrl <- pivot_longer(
  morancl_model_results_regnces_ctrl,
  cols = c(-nces, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Settlement type, \n demographic controls"
  ) %>%
  rename(
    value = nces
  )

#

morancl_model_regnces_ctrl_int <-
  nnet::multinom(
    cluster ~ region*nces  + medianage + medianhhinc + white_pct + ownoccup_pct + undergrad_pct25ov + + hhsmartphone_pc + AVG_DOWNLOAD_SPEED, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_regnces_ctrl_int <-
  effects::allEffects(morancl_model_regnces_ctrl_int) 

morancl_model_results_regnces_ctrl_int <- 
  as.data.frame(morancl_model_results_regnces_ctrl_int)[[8]] %>%
  select(nces, region, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_regnces_ctrl_int <- pivot_longer(
  morancl_model_results_regnces_ctrl_int,
  cols = c(-nces, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Settlement type, \n + Internet controls"
  ) %>%
  rename(
    value = nces
  )
#

regnces_plot <- rbind(
  morancl_model_results_regnces_ctrl_int,
  morancl_model_results_regnces_ctrl,
  morancl_model_results_regnces
) %>%
  mutate(
    var = factor(var, levels = c("Settlement type, \n without controls", "Settlement type, \n demographic controls", "Settlement type, \n + Internet controls")),
    value = factor(value, levels = c("City", "Suburb", "Rural - fringe", "Rural - distant", "Rural - remote")),
    region = factor(region, levels = c("All regions", "Northeast", "South", "Midwest", "West"))
  )

ggplot(filter(regnces_plot, !is.na(prob)), aes(x = value, y = prob, colour = cluster, fill = cluster, group = cluster)) +
  geom_line() +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = loci95, ymax = upci95, linetype = NA)) +
  facet_grid(
    region ~ var
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(angle = 90,  vjust = 0.1, hjust=0.95, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text=element_text(size= 7),
    legend.key.size = unit(0.6,"line"),
    legend.title = element_blank(),
    panel.spacing.y=unit(0.8, "lines")
  ) +
  labs(
    y = "Fitted probability \n"
  ) +
  scale_colour_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.6),
      "High-High" = alpha("#0000FF", 0.6) ,
      "Low-Low" = alpha("#FF0000", 0.6) ,
      "Low-High" = alpha("#f4ada8", 0.6),
      "High-Low"= alpha("#a7adf9", 0.6) 
    ),
    guide = "none"
  )  +
  scale_fill_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.25),
      "High-High" = alpha("#0000FF", 0.25) ,
      "Low-Low" = alpha("#FF0000", 0.25) ,
      "Low-High" = alpha("#f4ada8", 0.25),
      "High-Low"= alpha("#a7adf9", 0.25) 
    )
  ) + 
  guides(color = guide_legend(nrow = 2)) 


ggsave(filename = "./output/plots/nceschart.pdf",  width = 118, height = 210, dpi = 700, units = "mm", device='pdf')


# Table

regnces_tab <- list()

regnces_tab[[1]] <-
  regnces_plot %>%
  pivot_wider(
    id_cols = c(var, region, value),
    names_from = cluster,
    values_from = c(prob),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "value"
  )

regnces_tab[[2]] <-
  regnces_plot %>%
  mutate(
    se = (prob - loci95)/1.96 # Convert to SE
  ) %>%
  pivot_wider(
    id_cols = c(var, region, value),
    names_from = cluster,
    values_from = c(se),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "se"
  )

regnces_tab <- 
  bind_rows(regnces_tab) %>%
  arrange(var, region, value) %>%
  mutate(
    across(c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"), ~round(., 2))
  )

write.csv(regnces_tab, "./output/tables/ncestable.csv") 


# Multinomial regression of clusters on region*sci_mean_rel_own, and fitted probabilities


main_df <-
  main_df %>%
  mutate(
    sci_mean_rel_own_qn = relevel(sci_mean_rel_own_qn, ref = "1"),
    sci_w_geodistance_qn = relevel(sci_w_geodistance_qn, ref = "5"),
    sci_w_geodistance_rel_gnb_qn = relevel(sci_w_geodistance_rel_gnb_qn, ref = "1"),
    RDOMESTICMIG_qn = relevel(RDOMESTICMIG_qn, ref = "2")
  )

morancl_model_regsci <-
  nnet::multinom(
    cluster ~ region*sci_mean_rel_own_qn, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_regsci <-
  effects::allEffects(morancl_model_regsci) 

morancl_model_results_regsci <- 
  as.data.frame(morancl_model_results_regsci)[[1]] %>%
  select(region, sci_mean_rel_own_qn, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_regsci <- pivot_longer(
  morancl_model_results_regsci,
  cols = c(-sci_mean_rel_own_qn, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Relative connectedness to \n social neighbours"
  ) %>%
  rename(
    value = sci_mean_rel_own_qn
  )

# Multinomial regression of clusters on region*sci_w_geodistanct, and fitted probabilities
morancl_model_reggd <-
  nnet::multinom(
    cluster ~ region*sci_w_geodistance_qn, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_reggd <-
  effects::allEffects(morancl_model_reggd) 

morancl_model_results_reggd <- 
  as.data.frame(morancl_model_results_reggd)[[1]] %>%
  select(region, sci_w_geodistance_qn, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_reggd <- pivot_longer(
  morancl_model_results_reggd,
  cols = c(-sci_w_geodistance_qn, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Relative connectedness to \n social neighbours"
  ) %>%
  rename(
    value = sci_w_geodistance_qn
  )

# Multinomial regression of clusters on region*sci_w_geodistanct_rel_min, and fitted probabilities
morancl_model_reggdrelgnb <-
  nnet::multinom(
    cluster ~ region*sci_w_geodistance_rel_gnb_qn, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_reggdrelgnb <-
  effects::allEffects(morancl_model_reggdrelgnb) 

morancl_model_results_reggdrelgnb <- 
  as.data.frame(morancl_model_results_reggdrelgnb)[[1]] %>%
  select(region, sci_w_geodistance_rel_gnb_qn, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_reggdrelgnb <- pivot_longer(
  morancl_model_results_reggdrelgnb,
  cols = c(-sci_w_geodistance_rel_gnb_qn, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Relative distance to \n social neighbours"
  ) %>%
  rename(
    value = sci_w_geodistance_rel_gnb_qn
  )

# Multinomial regression of clusters on region*rdomesticmig, and fitted probabilities
morancl_model_regmig <-
  nnet::multinom(
    cluster ~ region*RDOMESTICMIG_qn, 
    maxit = 1000,
    data = main_df
  )

morancl_model_results_regmig <-
  effects::allEffects(morancl_model_regmig) 

morancl_model_results_regmig <- 
  as.data.frame(morancl_model_results_regmig)[[1]] %>%
  select(region, RDOMESTICMIG_qn, starts_with("prob"), starts_with("U.prob"), starts_with("L.prob"))

morancl_model_results_regmig <- pivot_longer(
  morancl_model_results_regmig,
  cols = c(-RDOMESTICMIG_qn, -region),
  names_to = "var",
  values_to = "value"
) %>%
  mutate(
    cluster = case_when(
      grepl("High.High", var) ~ "High-High",
      grepl("High.Low", var) ~ "High-Low",
      grepl("Low.High", var) ~ "Low-High",
      grepl("Low.Low", var) ~ "Low-Low",
      grepl("Not.significant", var) ~ "Not significant",
    ),
    var = case_when(
      grepl("^prob", var) ~ "prob",
      grepl("^L.prob", var) ~ "loci95",
      grepl("^U.prob", var) ~ "upci95"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = var,
    values_from = value
  ) %>%
  mutate(
    var = "Net domestic migration rate, \n 2019-2015"
  ) %>%
  rename(
    value = RDOMESTICMIG_qn
  )

prob_plot <- 
  rbind(morancl_model_results_regsci, morancl_model_results_reggdrelgnb) %>%
  mutate(
    value = factor(value, levels = c(1, 2, 3, 4, 5)),
    region = factor(region, levels = c("All regions", "Northeast", "South", "Midwest", "West")),
    var = factor(
      var, 
      levels = c(
        "Relative connectedness to \n social neighbours",
        "Relative distance to \n social neighbours"
      )
    )
  )

prob_plot_nomig <-
  prob_plot %>% filter(var != "Net domestic migration rate, \n 2019-2015")

ggplot(filter(prob_plot, !is.na(prob)), aes(x = value, y = prob, colour = cluster, fill = cluster, group = cluster)) +
  geom_line() +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = loci95, ymax = upci95, linetype = NA)) +
  facet_grid(
    region ~ var
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(vjust = 0.1, hjust=0.95, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text=element_text(size= 7),
    legend.key.size = unit(0.6,"line"),
    legend.title = element_blank(), 
    panel.spacing.y=unit(0.8, "lines")
  ) +
  labs(
    y = "Fitted probability \n",
    x = "\n Quintile"
  ) +
  scale_colour_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.6),
      "High-High" = alpha("#0000FF", 0.6) ,
      "Low-Low" = alpha("#FF0000", 0.6) ,
      "Low-High" = alpha("#f4ada8", 0.6),
      "High-Low"= alpha("#a7adf9", 0.6) 
    ),
    guide = "none"
  )  +
  scale_fill_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.25),
      "High-High" = alpha("#0000FF", 0.25) ,
      "Low-Low" = alpha("#FF0000", 0.25) ,
      "Low-High" = alpha("#f4ada8", 0.25),
      "High-Low"= alpha("#a7adf9", 0.25) 
    )
  ) + 
  guides(color = guide_legend(nrow = 2)) 


ggplot(filter(prob_plot_nomig, !is.na(prob)), aes(x = value, y = prob, colour = cluster, fill = cluster, group = cluster)) +
  geom_line() +
  geom_point(size = 1) +
  geom_ribbon(aes(ymin = loci95, ymax = upci95, linetype = NA)) +
  facet_grid(
    region ~ var
  ) + 
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(vjust = 0.1, hjust=0.95, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.text=element_text(size= 7),
    legend.key.size = unit(0.6,"line"),
    legend.title = element_blank(), 
    panel.spacing.y=unit(0.8, "lines")
  ) +
  labs(
    y = "Fitted probability \n",
    x = "\n Quintile"
  ) +
  scale_colour_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.6),
      "High-High" = alpha("#0000FF", 0.6) ,
      "Low-Low" = alpha("#FF0000", 0.6) ,
      "Low-High" = alpha("#f4ada8", 0.6),
      "High-Low"= alpha("#a7adf9", 0.6) 
    ),
    guide = "none"
  )  +
  scale_fill_manual(
    values = c(
      "Not significant" = alpha("snow4", 0.25),
      "High-High" = alpha("#0000FF", 0.25) ,
      "Low-Low" = alpha("#FF0000", 0.25) ,
      "Low-High" = alpha("#f4ada8", 0.25),
      "High-Low"= alpha("#a7adf9", 0.25) 
    )
  ) + 
  guides(color = guide_legend(nrow = 2)) 


ggsave(filename = "./output/plots/scichart.pdf",  width = 118, height = 210, dpi = 700, units = "mm", device='pdf')

# Table

prob_tab_nomig <- list()

prob_tab_nomig[[1]] <-
  prob_plot_nomig %>%
  pivot_wider(
    id_cols = c(var, region, value),
    names_from = cluster,
    values_from = c(prob),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "value"
  )

prob_tab_nomig[[2]] <-
  prob_plot_nomig %>%
  mutate(
    se = (prob - loci95)/1.96 # Convert to SE
  ) %>%
  pivot_wider(
    id_cols = c(var, region, value),
    names_from = cluster,
    values_from = c(se),
    names_sep = "_"
  ) %>%
  mutate(
    ind = "se"
  )

prob_tab_nomig <- 
  bind_rows(prob_tab_nomig) %>%
  arrange(var, region, value) %>%
  mutate(
    across(c("High-High", "High-Low", "Low-High", "Low-Low", "Not significant"), ~round(., 2))
  )

write.csv(prob_tab_nomig, "./output/tables/scitable.csv") 


# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

rm(list = ls())
gc()
