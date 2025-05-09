require(gcamdata)
#### Main script to analyse the results

library(dplyr)
library(magrittr)
library(ggplot2)

source('R/module_style.R')
source('R/module_data.R')
source('R/module_prj_analysis_SI.R')
source('R/module_sdg0_SI.R')

##### Load food consumption & mortality data ---------------------------------------------------
load_data <- function(dataset_name) {
  datasets_list <- list.files('output/datasets', pattern = dataset_name)
  data <- get(load(file.path('output/datasets',datasets_list[1])))
  for (file in datasets_list[2:length(datasets_list)]) data <- bind_rows(data, get(load(file.path('output/datasets',file))))
  return(data)
}

assign('queries_mort', get(load('output/queries_mort1.RData')))

figures_path <- 'figures'
year_fig <- 2050
year_s <- 2005
year_f <- 2050

#####################################################################################
#####################################################################################
# PROTEIN PERCENTAGE
#####################################################################################
##### PLANT  ===============================================================================
plant_percentage <- load_data('food_consumption_regional') %>%
  dplyr::filter(nestingSector1 == 'Protein') %>%
  dplyr::mutate(is_plant = ifelse(nestingSector2 == 'Plant',TRUE,FALSE)) %>%
  # compute the total and plant Pcal by region
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_plant, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # compute the plant % (plant/total protein consumption)
  tidyr::pivot_wider(names_from = 'is_plant', values_from = 'value') %>%
  dplyr::mutate(value = `TRUE` / (`TRUE` + `FALSE`)) %>%
  dplyr::mutate(Units = 'Percentage') %>%
  dplyr::select(-`TRUE`) %>%
  dplyr::select(-`FALSE`)


# plot
plot_data <- plant_percentage %>% dplyr::filter(scen_type %in% c('spp', 'ref')) %>%
  dplyr::mutate(final_share = ifelse(is.na(final_share), 'REF', final_share)) %>%
  dplyr::mutate(scen_path = ifelse(is.na(scen_path), 'REF', scen_path)) %>%
  dplyr::mutate(peak_year = ifelse(is.na(peak_year), 'REF', peak_year)) %>%
  cut_region_names()
palette_color <- create_color_palette(scenarios = unique(plot_data$final_share), ref_color = 'black')
palette_linetype <- c('2025' = 'dotted', '2035' = 'dashed', '2045' = 'longdash', 'REF' = 'solid')

spp_protein_all <- ggplot(data = plot_data %>%
                            dplyr::filter(scen_path %in% c('all', 'REF')) %>%
                            dplyr::mutate(value = 100*value), # transform to percentage
                          aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('REF')) %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Final Share') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  facet_wrap(. ~ region, nrow = 8) +
  labs(x = '', y = 'Plant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))
ggsave(spp_protein_all, file = file.path(figures_path, paste0('spp_protein_all_',year_fig,'.pdf')),
       width = 600, height = 800, units = 'mm')

spp_protein_plus <- ggplot(data = plot_data %>%
                             dplyr::filter(scen_path %in% c('plus', 'REF')) %>%
                             dplyr::mutate(value = 100*value), # transform to percentage
                           aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('REF')) %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Share\nIncrease') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  facet_wrap(. ~ region, nrow = 8) +
  labs(x = '', y = 'Plant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))
ggsave(spp_protein_plus, file = file.path(figures_path, paste0('spp_protein_plus_',year_fig,'.pdf')),
       width = 600, height = 800, units = 'mm')


## SINGLE REGION - INDIA
spp_protein_all_india <- ggplot(data = plot_data %>%
                                  dplyr::filter(scen_path %in% c('all', 'REF'), region == 'EU-15') %>%
                                  dplyr::mutate(value = 100*value), # transform to percentage
                                aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('REF'), region == 'EU-15') %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Final Share') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  labs(x = '', y = 'Plant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))
ggsave(spp_protein_all_india, file = file.path(figures_path, paste0('spp_protein_all_eu15_',year_fig,'.pdf')),
       width = 500, height = 350, units = 'mm')
spp_protein_plus_india <- ggplot(data = plot_data %>%
                                   dplyr::filter(scen_path %in% c('plus', 'REF'), region == 'EU-15') %>%
                                   dplyr::mutate(value = 100*value), # transform to percentage
                                 aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('REF'), region == 'EU-15') %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Share\nIncrease') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  labs(x = '', y = 'Plant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))
ggsave(spp_protein_plus_india, file = file.path(figures_path, paste0('spp_protein_plus_eu15_',year_fig,'.pdf')),
       width = 500, height = 350, units = 'mm')

pl_example <- cowplot::plot_grid(spp_protein_all_india, spp_protein_plus_india,
                                 ncol = 2,
                                 labels = c("a", "b"),
                                 label_size = 27,
                                 label_fontface = 'bold')
ggsave(pl_example, file = file.path(figures_path, paste0('spp_protein_all_plus_eu15_example_',year_fig,'.pdf')),
       width = 600, height = 350, units = 'mm')

##### RUMINANT ===============================================================================
rumin_percentage <- load_data('food_consumption_regional') %>%
  dplyr::filter(nestingSector2 == 'Animal') %>%
  dplyr::mutate(is_rumin = ifelse(nestingSector3 == 'Ruminant',TRUE,FALSE)) %>%
  # compute the total and ruminant Pcal by region
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_rumin, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # compute the ruminant % (ruminant/total ANIMAL protein consumption)
  tidyr::pivot_wider(names_from = 'is_rumin', values_from = 'value') %>%
  dplyr::mutate(`TRUE` = ifelse(is.na(`TRUE`), 0, `TRUE`)) %>%
  dplyr::mutate(`FALSE` = ifelse(is.na(`FALSE`), 0, `FALSE`)) %>%
  dplyr::mutate(value = `TRUE` / (`TRUE` + `FALSE`)) %>%
  dplyr::mutate(Units = 'Percentage') %>%
  dplyr::select(-`TRUE`) %>%
  dplyr::select(-`FALSE`)

# plot
plot_data <- rumin_percentage %>% dplyr::filter(scen_type %in% c('snr', 'ref')) %>%
  dplyr::mutate(final_share = ifelse(is.na(final_share), 'ref', final_share)) %>%
  dplyr::mutate(scen_path = ifelse(is.na(scen_path), 'ref', scen_path)) %>%
  dplyr::mutate(peak_year = ifelse(is.na(peak_year), 'ref', peak_year)) %>%
  cut_region_names()
palette_color <- create_color_palette(scenarios = unique(plot_data$final_share), ref_color = 'black')
palette_linetype <- c('2025' = 'dotted', '2035' = 'dashed', '2045' = 'longdash', 'ref' = 'solid')

snr_protein_all <- ggplot(data = plot_data %>%
                            dplyr::filter(scen_path %in% c('all', 'ref')) %>%
                            dplyr::mutate(value = 100*value), # transform to percentage
                          aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('ref')) %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Final Share') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  facet_wrap(. ~ region, nrow = 8) +
  labs(x = '', y = 'Ruminant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))

ggsave(snr_protein_all, file = file.path(figures_path, paste0('snr_protein_all_',year_fig,'.pdf')),
       width = 600, height = 800, units = 'mm')

snr_protein_plus <- ggplot(data = plot_data %>%
                             dplyr::filter(scen_path %in% c('plus', 'ref')) %>%
                             dplyr::mutate(value = 100*value), # transform to percentage
                           aes(x = year, y = value, group = scenario, color = final_share, linetype = peak_year)) +
  geom_line() +
  # ref line
  geom_line(data = plot_data %>%
              dplyr::filter(scen_path %in% c('ref')) %>%
              dplyr::mutate(value = 100*value), # transform to percentage
            aes(x = year, y = value, group = scenario, color = final_share),
            linewidth = 1.5) +
  # palettes
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +  # scale_y_continuous(breaks = seq(0.1, 1, by = 0.1)) +
  scale_color_manual(values = palette_color, name = 'Share\nDecrease') +
  scale_linetype_manual(values = palette_linetype, name = 'Peak Year') +
  facet_wrap(. ~ region, nrow = 8) +
  labs(x = '', y = 'Ruminant Protein Consumption [%]') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 27.5),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title = element_text(size = 25),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30))

ggsave(snr_protein_plus, file = file.path(figures_path, paste0('snr_protein_plus_',year_fig,'.pdf')),
       width = 600, height = 800, units = 'mm')


##### SI ==========================================================================
sdg0_ref_probdistrib()
sdg0_scen_path_probdistrib()

#####################################################################################
#####################################################################################
# SDG15 - LAND USE management
#####################################################################################

############## INDICATOR 1: % of re-/aff-forestation ================================
# compute the Land Indicator (Percent of Re/Afforestation)
land_indicator_forestLand = merge(load_data('land_use_regional') %>%
                                    dplyr::filter(scenario != 'ref') %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    aggregate_land_use_type() %>%
                                    dplyr::mutate(value = value * 100) %>% # convert Thous km2 to Mha
                                    dplyr::mutate(forest = ifelse(land_use_type == 'Forest', 'Forest', 'NoForest')) %>%
                                    dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, forest) %>%
                                    dplyr::summarize(value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year) %>%
                                    dplyr::summarize(percent_forest = 100 * sum(value[forest == "Forest"]) / sum(value[forest %in% c("NoForest", "Forest")]),
                                                     total_land = sum(value[forest %in% c("NoForest", "Forest")])) %>%
                                    dplyr::ungroup(),
                                  load_data('land_use_regional') %>%
                                    dplyr::filter(scenario == 'ref') %>%
                                    dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                    aggregate_land_use_type() %>%
                                    dplyr::mutate(value = value * 100) %>% # convert Thous km2 to Mha
                                    dplyr::mutate(forest = ifelse(land_use_type == 'Forest', 'Forest', 'NoForest')) %>%
                                    dplyr::group_by(region, scenario, year, forest) %>%
                                    dplyr::summarize(value = sum(value)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::group_by(region, scenario, year) %>%
                                    dplyr::summarize(percent_forest_ref = 100 * sum(value[forest == "Forest"]) / sum(value[forest %in% c("NoForest", "Forest")]),
                                                     total_land_ref = sum(value[forest %in% c("NoForest", "Forest")])) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::select(-scenario),
                                  by = c('year','region'))

## FIG
# aggregate Global Value with Weighted Average
land_indicator_global_forestLand <- land_indicator_forestLand %>%
  dplyr::mutate(weight = percent_forest * total_land,
                weight_ref = percent_forest_ref * total_land_ref) %>%
  dplyr::group_by(scenario, region, scen_type, scen_path, final_share, peak_year, slope, year) %>%
  dplyr::summarize(percent_forest = sum(weight) / sum(total_land),
                   percent_forest_ref = sum(weight_ref) / sum(total_land_ref)) %>%
  dplyr::ungroup() %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(percent_forest - percent_forest_ref)/percent_forest_ref) %>%
  # compute median by scen
  dplyr::group_by(year, region, scen_type) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::mutate(group = 'Re-/Afforestation area') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

# plot
land_indicator_global_forestLand_map <- land_indicator_global_forestLand %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

land_indicator_global_forestLand_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             land_indicator_global_forestLand_map, by = 'adm0_a3')

# plot
FIG_LANDWATER_land_indicator_global_forestLand_map <- ggplot2::ggplot() +
  # color map by regions
  ggplot2::geom_sf(data = land_indicator_global_forestLand_map, 
                    ggplot2::aes(fill = median_diff)) +
  ggplot2::facet_grid(. ~ scen_type, scales = 'fixed') +
  ggplot2::scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Forest Area difference [%]","\n"))) +
  # theme
  ggplot2::guides(fill = guide_colorbar(title.position = "left")) +
  ggplot2::theme_light() +
  ggplot2::theme(axis.title=ggplot2::element_blank(),
        axis.text=ggplot2::element_blank(),
        axis.ticks=ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.background = ggplot2::element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = ggplot2::element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = ggplot2::element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggplot2::ggsave(FIG_LANDWATER_land_indicator_global_forestLand_map,
       file = file.path(figures_path, paste0('FIG_LANDWATER_sdg15_land_indicator_global_forestLand_map',year_fig,'.pdf')),
       width = 500, height = 300, units = 'mm')


## EXTENDED FIG
land_indicator_global_forestLand <- land_indicator_forestLand %>%
  dplyr::mutate(weight = percent_forest * total_land,
                weight_ref = percent_forest_ref * total_land_ref) %>%
  dplyr::group_by(scenario, region, scen_type, scen_path, final_share, peak_year, slope, year) %>%
  dplyr::summarize(percent_forest = sum(weight) / sum(total_land),
                   percent_forest_ref = sum(weight_ref) / sum(total_land_ref)) %>%
  dplyr::ungroup() %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(percent_forest - percent_forest_ref)/percent_forest_ref) %>%
  # compute median by scen
  dplyr::group_by(year, region, scen_type, scen_path) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::mutate(group = 'Re-/Afforestation area') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


# plot
land_indicator_global_forestLand_map <- land_indicator_global_forestLand %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

land_indicator_global_forestLand_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                               dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                               dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                               dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                             land_indicator_global_forestLand_map, by = 'adm0_a3')

# plot
pl_land_indicator_global_forestLand_map <- ggplot() +
  # color map by regions
  geom_sf(data = land_indicator_global_forestLand_map %>%
            rename_pathways(), aes(fill = median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Forest Area difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_land_indicator_global_forestLand_map,
       file = file.path(figures_path, paste0('ind_sdg15_forest_map_',year_fig,'.pdf')),
       width = 500, height = 300, units = 'mm')

############## INDICATOR 2: % of unmanaged land =====================================
# compute the Land Indicator (Percent of Unmanaged Land)
# assign('tmp', get(load('output/datasets/detailed_land_allocation_regional.RData')))
# land_indicator_managementLand = merge(tmp %>%
#                                         dplyr::filter(scenario != 'ref') %>%
#                                         dplyr::mutate(value = value * 0.1) %>% # convert km2 to Mha
#                                         dplyr::mutate(management = ifelse(grepl("ProtectedUnmanagedForest|UnmanagedForest|Shrubland|ProtectedShrubland|Grassland|ProtectedGrassland|UnmanagedPasture|ProtectedUnmanagedPasture|Tundra|RockIceDesert", landleaf),
#                                                                       'Unmanaged', 'Managed')) %>%
#                                         dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, management) %>%
#                                         dplyr::summarize(value = sum(value)) %>%
#                                         dplyr::ungroup() %>%
#                                         dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year) %>%
#                                         dplyr::summarize(percent_management = 100 * sum(value[management == "Unmanaged"]) / sum(value[management %in% c("Unmanaged", "Managed")]),
#                                                          total_land = sum(value[management %in% c("Unmanaged", "Managed")])) %>%
#                                         dplyr::ungroup(),
#                                       tmp %>%
#                                         dplyr::filter(scenario == 'ref') %>%
#                                         dplyr::mutate(value = value * 0.1) %>% # convert km2 to Mha
#                                         dplyr::mutate(management = ifelse(grepl("ProtectedUnmanagedForest|UnmanagedForest|Shrubland|ProtectedShrubland|Grassland|ProtectedGrassland|UnmanagedPasture|ProtectedUnmanagedPasture|Tundra|RockIceDesert", landleaf),
#                                                                           'Unmanaged', 'Managed')) %>%
#                                         dplyr::group_by(region, scenario, year, management) %>%
#                                         dplyr::summarize(value = sum(value)) %>%
#                                         dplyr::ungroup() %>%
#                                         dplyr::group_by(region, scenario, year) %>%
#                                         dplyr::summarize(percent_management = 100 * sum(value[management == "Unmanaged"]) / sum(value[management %in% c("Unmanaged", "Managed")]),
#                                                          total_land = sum(value[management %in% c("Unmanaged", "Managed")])) %>%
#                                         dplyr::ungroup() %>%
#                                         dplyr::select(-scenario),
#                                       by = c('year','region'))
load('output/datasets/land_indicator_managementLand.RData')

### EXTENDED FIG
# aggregate Global Value with Weighted Average
land_indicator_global_managementLand = land_indicator_managementLand %>%
  dplyr::mutate(weight = percent_management * total_land,
                weight_ref = percent_management_ref * total_land_ref) %>%
  dplyr::group_by(scenario, region, scen_type, scen_path, final_share, peak_year, slope, year) %>%
  dplyr::summarize(percent_management = sum(weight) / sum(total_land),
                   percent_management_ref = sum(weight_ref) / sum(total_land_ref)) %>%
  dplyr::ungroup() %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(percent_management - percent_management_ref)/percent_management_ref) %>%
  # compute median by scen
  dplyr::group_by(year, region, scen_type, scen_path) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::mutate(group = 'Unmanaged area') %>%
  dplyr::ungroup()

# plot
land_indicator_global_managementLand_map <- land_indicator_global_managementLand %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

land_indicator_global_managementLand_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                   dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                   dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                   dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                                 land_indicator_global_managementLand_map, by = 'adm0_a3')
# plot
pl_land_indicator_global_managementLand_map <- ggplot() +
  # color map by regions
  geom_sf(data = land_indicator_global_managementLand_map %>%
            rename_pathways(), aes(fill = median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Unmanaged Area difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_land_indicator_global_managementLand_map,
       file = file.path(figures_path, paste0('ind_sdg15_unmanaged_map_',year_fig,'.pdf')),
       width = 500, height = 300, units = 'mm')


############## AREA ===============================================================================


#### ABSOLUTE
land_use_diffAbs_world_raw = merge(load_data('land_use_world') %>%
                                     dplyr::filter(scenario != 'ref') %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     aggregate_land_use_type(),
                                   load_data('land_use_world') %>%
                                     dplyr::filter(scenario == 'ref') %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     aggregate_land_use_type() %>%
                                     dplyr::select(year, land_use_type, value_ref = value),
                                   by = c('year','land_use_type')) %>%
  # compute total area by land_use_type
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, Units, land_use_type, year) %>%
  dplyr::summarise(value = sum(value),
                   value_ref = sum(value_ref)) %>%
  dplyr::ungroup() %>%
  # compute difference between Reference and runs
  dplyr::mutate(diff = value - value_ref) %>%
  # aesthetics
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::filter(scen_type != 'REF') %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

#### BOXPLOT
pl_land_use_diffAbs_boxplot_world = ggplot(data = land_use_diffAbs_world_raw %>%
                                             dplyr::filter(year %in% c(2030, 2050)) %>%
                                             dplyr::mutate(land_use_type = ifelse(land_use_type == 'Shrubs & Grass', 'Shrub & Grass', land_use_type)) %>%
                                             dplyr::mutate(land_use_type = factor(land_use_type, levels = c("Cropland", "Pasture", "Forest", "Other Natural")))) +
  geom_boxplot(aes(x = as.factor(year), y = diff, fill = land_use_type), alpha = 1) +  # Median area
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(. ~ scen_type) +
  scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                    breaks = land_use_order_palette) +
  # labs
  labs(y = expression(paste('Thous. ', km^2, ' difference with Reference')), x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_land_use_diffAbs_boxplot_world, file = file.path(figures_path,paste0('sdg15_land_use_diffAbs_boxplot_world.pdf')),
       width = 575, height = 400, units = 'mm')

#### SI figures ================================================================
data <- merge(load_data('land_use_world') %>%
                dplyr::filter(scenario != 'ref') %>%
                dplyr::mutate(scen_type = toupper(scen_type)) %>%
                aggregate_land_use_type(),
              load_data('land_use_world') %>%
                dplyr::filter(scenario == 'ref') %>%
                dplyr::mutate(scen_type = toupper(scen_type)) %>%
                aggregate_land_use_type() %>%
                dplyr::select(year, land_use_type, value_ref = value),
              by = c('year','land_use_type')) %>%
  # compute total area by land_use_type
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, Units, land_use_type, year) %>%
  dplyr::summarise(value = sum(value),
                   value_ref = sum(value_ref)) %>%
  dplyr::ungroup() %>%
  # diff with respect to Reference
  dplyr::mutate(diff = value - value_ref) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::filter(scen_type != 'REF') %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR'))) %>%
  dplyr::mutate(land_use_type = ifelse(land_use_type == 'Shrubs & Grass', 'Shrub & Grass', land_use_type)) %>%
  dplyr::mutate(land_use_type = factor(land_use_type, levels = c("Cropland", "Pasture", "Forest", "Other Natural")))
violin_plot_landtype(data, year_fig, type = 'abs')



data <-  merge(load_data('land_use_region') %>%
                 dplyr::filter(scenario != 'ref') %>%
                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                 aggregate_land_use_type(),
               load_data('land_use_region') %>%
                 dplyr::filter(scenario == 'ref') %>%
                 dplyr::mutate(scen_type = toupper(scen_type)) %>%
                 aggregate_land_use_type() %>%
                 dplyr::select(region, year, land_use_type, value_ref = value),
               by = c('year', 'region', 'land_use_type')) %>%
  # compute total area by land_use_type
  dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, Units, land_use_type, year) %>%
  dplyr::summarise(value = sum(value),
                   value_ref = sum(value_ref)) %>%
  dplyr::ungroup() %>%
  # diff with respect to Reference
  dplyr::mutate(diff = value - value_ref) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::filter(scen_type != 'REF') %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR'))) %>%
  dplyr::mutate(land_use_type = ifelse(land_use_type == 'Shrubs & Grass', 'Shrub & Grass', land_use_type)) %>%
  cut_region_names() %>%
  dplyr::mutate(land_use_type = factor(land_use_type, levels = c("Cropland", "Pasture", "Forest", "Other Natural")))
violin_plot_landtype_regional(data, year_fig, type = 'abs')

#####################################################################################
#####################################################################################
# SDG6 - WATER management
#####################################################################################
############## INDICATOR 1: % of avoided water scarcity ================================
# compute the Water Indicator (Water Scarcity Index) -- TODO: update with my queries and compute diff between scen & ref

# # # Get Water Supply Data
# # water_supply = load_data('basin_level_available') %>%
# #   dplyr::select(-region) %>%
# #   bind_rows(
# #     load_data('resource_supply_curves') %>%
# #       dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
# #       dplyr::mutate(subresource = "groundwater") %>%
# #       dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, resource, subresource, Units) %>%
# #       dplyr::summarize(value = sum(value)) %>%
# #       dplyr::ungroup() %>%
# #       dplyr::rename(basin = resource)) %>%
# #   dplyr::rename(value_sup = value)
# # save(water_supply, file = file.path('output','datasets','water_supply.RData'))
# load(file.path('output','datasets','water_supply.RData'))

# # # Get Water Withdrawal Data
# # water_withdrawal = load_data("water_Withdrawals_basin_runoff_regional") %>%
# #   dplyr::select(-region) %>%
# #   dplyr::rename(basin = "runoff water") %>%
# #   bind_rows(
# #     load_data("water_Withdrawals_basin_groundwater_regional") %>%
# #       dplyr::filter(stringr::str_detect(subresource, "groundwater")) %>%
# #       dplyr::mutate(subresource = "groundwater") %>%
# #       dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, groundwater, subresource, Units) %>%
# #       dplyr::summarize(value = sum(value)) %>%
# #       dplyr::ungroup() %>%
# #       dplyr::rename(basin = groundwater)) %>%
# #   dplyr::rename(value_wd = value)
# # save(water_withdrawal, file = file.path('output','datasets','water_withdrawal.RData'))
# load(file.path('output','datasets','water_withdrawal.RData'))

# # Extract values of baseline
# water_withdrawal_2015 = water_withdrawal %>% dplyr::filter(year == 2015) %>% dplyr::rename(value_wd_2015 = value_wd)
# water_supply_2015 = water_supply %>% dplyr::filter(year == 2015) %>% dplyr::rename(value_sup_2015 = value_sup)
#
# # Compute the Weighted Water Scarcity Index (Weighted per Basin both by Supply & by Withdrawal)
# water_scarcity_index = water_supply %>%
#   dplyr::left_join(water_withdrawal) %>%
#   dplyr::mutate(index = value_wd / value_sup)
# water_scarcity_index = merge(water_scarcity_index, water_withdrawal_2015,
#                              by = c("region", "basin", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "subresource"))
# water_scarcity_index = merge(water_scarcity_index, water_supply_2015,
#                              by = c("region", "basin", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "subresource"))
# water_scarcity_index = water_scarcity_index %>%
#   dplyr::mutate(weighted_sup = index * value_sup_2015,
#          weighted_wd = index * value_wd_2015) %>%
#   dplyr::select(-year, -year.y) %>%
#   dplyr::rename(year = year.x) %>%
#   dplyr::group_by(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, resource = if_else(subresource == "runoff", "runoff", "groundwater")) %>%
#   dplyr::summarize(index_sup = sum(weighted_sup) / sum(value_sup_2015),
#             index_wd = sum(weighted_wd) / sum(value_wd_2015)) %>%
#   dplyr::ungroup()
# save(water_scarcity_index, file = file.path('output','datasets','water_scarcity_index.RData'))
load(file.path('output','datasets','water_scarcity_index.RData'))

water_scarcity_index <- water_scarcity_index %>%
  dplyr::filter(resource == "runoff") %>%
  dplyr::select(region, scenario, scen_type, scen_path, final_share, peak_year, slope, year, index = index_wd)

# diff REF vs runs
water_indicator_scarcity = merge(water_scarcity_index %>%
                                   dplyr::filter(scenario != 'ref') %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)),
                                 water_scarcity_index %>%
                                   dplyr::filter(scenario == 'ref') %>%
                                   dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                   dplyr::select(year, index_ref = index, region),
                                 by = c('year','region'))

# aggregate Global Value with Weighted Average
water_indicator_global <- water_indicator_scarcity %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  # dplyr::mutate(diff = (index - index_ref)) %>%
  dplyr::mutate(diff = 100*(index - index_ref)/index_ref) %>%
  # compute median by scen
  dplyr::group_by(year, region, scen_type) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::mutate(group = 'Water scarcity') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

# plot
water_indicator_global_map <- water_indicator_global %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

water_indicator_global_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                     dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                     dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                     dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                   water_indicator_global_map, by = 'adm0_a3')

# plot
FIG_LANDWATER_sdg6_water_index_map <- ggplot() +
  # color map by regions
  geom_sf(data = water_indicator_global_map, aes(fill = median_diff)) +
  facet_grid(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkgreen", high = "darkred",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Water Scarcity index difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(FIG_LANDWATER_sdg6_water_index_map,
       file = file.path(figures_path, paste0('FIG_LANDWATER_sdg6_water_index_map_',year_fig,'.pdf')),
       width = 500, height = 300, units = 'mm')


### SI figure
water_indicator_global <- water_indicator_scarcity %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  # dplyr::mutate(diff = (index - index_ref)) %>%
  dplyr::mutate(diff = 100*(index - index_ref)/index_ref) %>%
  # compute median by scen
  dplyr::group_by(year, region, scen_type, scen_path) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::mutate(group = 'Water scarcity') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

# plot
water_indicator_global_map <- water_indicator_global %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

water_indicator_global_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                     dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                     dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                     dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                   water_indicator_global_map, by = 'adm0_a3')

# plot
FIG_LANDWATER_sdg6_water_index_map_SI <- ggplot() +
  # color map by regions
  geom_sf(data = water_indicator_global_map %>%
            rename_pathways(), aes(fill = median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkgreen", high = "darkred",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Water scarcity index difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(FIG_LANDWATER_sdg6_water_index_map_SI,
       file = file.path(figures_path, paste0('sdg6_SI_water_index_map_',year_fig,'.pdf')),
       width = 500, height = 275, units = 'mm')




############## WATER consumption ===============================================

pl_water_consumption_world <- ggplot(data = load_data('water_withdrawals_world') %>%
                                       dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                       dplyr::group_by(year, scen_type) %>%
                                       dplyr::mutate(median_value = median(value)) %>%
                                       dplyr::mutate(min_value = min(value)) %>%
                                       dplyr::mutate(max_value = max(value))) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 2, alpha = 1, linetype = 'dashed') +  # Median line
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  geom_line(data = load_data('water_withdrawals_world') %>%
              dplyr::filter(scenario == 'ref') %>%
              dplyr::mutate(scen_type = toupper(scen_type)), aes(x = year, y = value, color = scen_type),
            linewidth = 2, alpha = 1, linetype = 'dashed') +  # Median line REF

  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Annual Water flows [",km^3,"]")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_water_consumption_world, file = file.path(figures_path,'sdg6_annual_water_consumption_line.pdf'), width = 500, height = 400, units = 'mm')


ag_water_consumption_world_irr_rfd <- load_data('water_irr_rfd_world') %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  # add water regarldess the crop, etc.
  dplyr::group_by(year, scenario, scen_type, water) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, scenario, scen_type) %>%
  dplyr::mutate(irr_per = 100 * value[water == "RFD"] / (value[water == "IRR"] + value[water == "RFD"])) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(year, scen_type) %>%
  dplyr::mutate(irr_per_median = median(irr_per),
                irr_per_min = min(irr_per),
                irr_per_max = max(irr_per)) %>%
  dplyr::ungroup()


pl_water_consumption_agriculture_world_irr_rfd <- ggplot(data = ag_water_consumption_world_irr_rfd) +
  geom_line(aes(x = year, y = irr_per, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_line(data = ag_water_consumption_world_irr_rfd %>%
              dplyr::filter(scen_type == 'REF'),
            aes(x = year, y = irr_per_median, color = scen_type), linewidth = 2, alpha = 1) +  # Median line
  geom_line(aes(x = year, y = irr_per_median, color = scen_type), linewidth = 2, alpha = 1, linetype = 'dashed') +  # Median line
  geom_ribbon(aes(x = year, ymin = irr_per_min, ymax = irr_per_max, fill = scen_type), alpha = 0.15) +  # Shadow
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Land type using RFD water [%]")), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_water_consumption_agriculture_world_irr_rfd, file = file.path(figures_path,'sdg6_annual_water_agriculture_consumption_line_irr_rfd.pdf'), width = 500, height = 400, units = 'mm')


##### SI figs =================================================================
## total consumption & irr-rfd share by region, scen, and pathway
ag_water_consumption_region_irr_rfd <- load_data('water_irr_rfd_regional') %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::group_by(region, year, scenario, scen_type, scen_path, water) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(region, year, scenario, scen_type, scen_path) %>%
  dplyr::mutate(rfd_per = 100 * value[water == "RFD"] / (value[water == "IRR"] + value[water == "RFD"])) %>%
  dplyr::ungroup()
rfd_index_water(ag_water_consumption_region_irr_rfd, year_fig)

ag_water_consumption_regional <- load_data('water_withdrawals_regional') %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::group_by(region, year, scenario, scen_type, scen_path) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::ungroup()
cum_fun_water(ag_water_consumption_regional, year_fig)

#####################################################################################
#####################################################################################
# FIG - LAND & WATER
#####################################################################################
#####################################################################################
pl = cowplot::ggdraw() +
  cowplot::draw_plot(FIG_LANDWATER_land_indicator_global_forestLand_map, x = 0.01, y = 0.55, width = 0.95, height = 0.6) +
  cowplot::draw_plot(pl_land_use_diffAbs_boxplot_world +
                       labs(y = expression(paste('      Area diff from\nReference [Thous. ', km^2,']'))) + 
                       theme(
                         axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
                       ), x = 0.01, y = 0.30, width = 0.95, height = 0.4) +
  cowplot::draw_plot(FIG_LANDWATER_sdg6_water_index_map, x = 0.01, y = -0.15, width = 0.95, height = 0.6) +
  cowplot::draw_plot_label(label = c("a", "b", "c"), size = 35,
                           x = c(0, 0, 0), y = c(0.99, 0.7, 0.3))
ggsave(file=file.path(figures_path, paste0('FIG_LANDWATER_',year_fig,'.pdf')), plot = pl, width = 800, height = 500, unit = 'mm')



#####################################################################################
#####################################################################################
# SDG13 - EMISSIONS
#####################################################################################
############## INDICATOR 1: % of avoided GHG (global) ===========================
# compute the GHG Indicator (Percent of avoided GHG)
ghg_world_regional_diffPer <- merge(load_data('ghg_regional') %>%
                                      dplyr::filter(scenario != 'ref') %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      # add ghg emissions regardless ghg type
                                      dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                      dplyr::summarise(value = sum(value)) %>%
                                      dplyr::ungroup(),
                                    load_data('ghg_regional') %>%
                                      dplyr::filter(scenario == 'ref') %>%
                                      dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                      # add ghg emissions regardless ghg type
                                      dplyr::group_by(region, year, scenario, scen_type) %>%
                                      dplyr::summarise(ref_value = sum(value)) %>%
                                      dplyr::ungroup() %>%
                                      dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                    by = c('region','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff))

ghg_world_regional_diffPer_map <- ghg_world_regional_diffPer %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


ghg_world_regional_diffPer_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                         dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                         dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                       ghg_world_regional_diffPer_map, by = 'adm0_a3')

# plot
FIG_EMISSHEALTH_sdg13_av_ghg_aggPer_map <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_world_regional_diffPer_map, aes(fill = median_diff)) +
  facet_grid(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided GHG emissions [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(FIG_EMISSHEALTH_sdg13_av_ghg_aggPer_map,
       file = file.path(figures_path, paste0('FIG_EMISSHEALTH_sdg13_av_ghg_aggPer_map_',year_fig,'.pdf')),
       width = 500, height = 300, units = 'mm')

##### GHG emissions TREND ======================================================
dataa <- load_data('ghg_world') %>%
  dplyr::filter(year >= year_s, year <= year_f) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  # add ghg emissions regardless ghg type
  dplyr::group_by(year, scenario) %>%
  dplyr::mutate(value = sum(value)) %>%
  dplyr::ungroup() %>%
  # compute statistics
  dplyr::group_by(year, scen_type) %>%
  dplyr::mutate(median_value = median(value)) %>%
  dplyr::mutate(min_value = min(value)) %>%
  dplyr::mutate(max_value = max(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF','SPP','SNR','SPPNR')))

pl_ghg_emissions_world <- ggplot(data = dataa) +
  geom_line(aes(x = year, y = value, group = scenario, color = scen_type), alpha = 0.3) +  # All runs lines
  geom_ribbon(aes(x = year, ymin = min_value, ymax = max_value, fill = scen_type), alpha = 0.15) +  # Shadow
  geom_line(aes(x = year, y = median_value, color = scen_type), linewidth = 2, alpha = 1, linetype = 'dashed') +  # Median line
  geom_line(data = dataa %>% dplyr::filter(scen_type == 'REF'),
            aes(x = year, y = median_value, color = scen_type), linewidth = 2, alpha = 1, linetype = 'dashed') +  # Median line
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr, name = 'Scenario') +
  # labs
  labs(y = expression(paste("Mt",CO[2])), x = '') +
  # theme
  theme_light() +
  guides(color = guide_legend(keywidth = 3, override.aes = list(linewidth = 2))) +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        axis.title = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40))
ggsave(pl_ghg_emissions_world, file = file.path(figures_path,'sdg13_ghg_emissions_line.pdf'), width = 500, height = 400, units = 'mm')


##### GHG emissions MAPS ==============================================================================

# PERCENT with Path
ghg_world_regional_diffPer_withPath <- merge(load_data('ghg_regional') %>%
                                               dplyr::filter(scenario != 'ref') %>%
                                               dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                               # add ghg emissions regardless ghg type
                                               dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                               dplyr::summarise(value = sum(value)) %>%
                                               dplyr::ungroup(),
                                             load_data('ghg_regional') %>%
                                               dplyr::filter(scenario == 'ref') %>%
                                               dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                               # add ghg emissions regardless ghg type
                                               dplyr::group_by(region, year, scenario, scen_type) %>%
                                               dplyr::summarise(ref_value = sum(value)) %>%
                                               dplyr::ungroup() %>%
                                               dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                             by = c('region','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(ref_value - value)/ref_value) %>%
  # compute median by scen
  dplyr::group_by(region,year,scen_type,scen_path) %>%
  dplyr::summarise(median_diff = median(diff))

ghg_world_regional_diffPer_map_withPath <- ghg_world_regional_diffPer_withPath %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

ghg_world_regional_diffPer_map_withPath = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                  dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                  dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                  dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                                ghg_world_regional_diffPer_map_withPath, by = 'adm0_a3')

pl_ghg_world_regional_diffPer_map_withPath <- ggplot() +
  # color map by regions
  geom_sf(data = ghg_world_regional_diffPer_map_withPath %>%
            rename_pathways(), aes(fill = median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided GHG emissions [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_ghg_world_regional_diffPer_map_withPath, file = file.path(figures_path, paste0('sdg13_ghg_per_map_',year_fig,'_withPath.pdf')),
       width = 500, height = 300, units = 'mm')



#### GHG by ghg world diff BARS =====================================================

### BOXPLOT
ghgType_world_diffAbs_raw <- merge(load_data('ghg_by_ghg_world') %>%
                                     dplyr::filter(scenario != 'ref') %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)),
                                   load_data('ghg_by_ghg_world') %>%
                                     dplyr::filter(scenario == 'ref') %>%
                                     dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                     dplyr::select(year, group, ref_value = value),
                                   by = c('year','group')) %>%
  # compute Abs difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = value - ref_value) %>%
  # kick out FF & CO2 gases
  dplyr::filter(!group %in% c('CO2','F-Gas')) %>%
  # aesthetics
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


pl_ghg_by_ghg_emiss_diffAbs_world_boxplot = ggplot(data = ghgType_world_diffAbs_raw %>%
                                                     dplyr::filter(year %in% c(2030,2050)) %>%
                                                     dplyr::mutate(group = factor(group, levels = c("CH4","CO2","F-Gas","LUC CO2","N2O")))) +
  geom_boxplot(aes(x = as.factor(year), y = diff, fill = group), alpha = 1) +  # Median area
  facet_grid(. ~ scen_type, scales = "free") +
  scale_fill_manual(values = ghg_emiss, name = 'GHG') +
  geom_hline(yintercept = 0, linewidth = 1.2) +
  labs(x = '', y = expression(paste('Mt ', CO[2], ' difference with Reference'))) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40, face = 'bold'),
        title = element_text(size = 40))
ggsave(pl_ghg_by_ghg_emiss_diffAbs_world_boxplot,
       file = file.path(figures_path, paste0('sdg13_ghg_by_ghg_emiss_abs_boxplot_',year_fig,'.pdf')),
       width = 575, height = 400, units = 'mm')

#### SI figs ===================================================================
data <- merge(load_data('ghg_by_ghg_regional') %>%
                dplyr::filter(scenario != 'ref') %>%
                dplyr::mutate(scen_type = toupper(scen_type)),
              load_data('ghg_by_ghg_regional') %>%
                dplyr::filter(scenario == 'ref') %>%
                dplyr::mutate(scen_type = toupper(scen_type)) %>%
                dplyr::select(region, year, group, value_ref = value),
              by = c('region','year','group')) %>%
  dplyr::rename(ghg = group) %>%
  # diff with respect to Reference
  dplyr::mutate(diff = value - value_ref) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::filter(scen_type != 'REF') %>%
  # kick out FF & CO2 gases
  dplyr::filter(!ghg %in% c('CO2','F-Gas')) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR'))) %>%
  dplyr::mutate(ghg = factor(ghg, levels = c("CH4","LUC CO2","N2O")))
violin_plot_ghg_regional(data %>% rename_pathways(), year_fig, type = 'abs')
waterfall_plot_ghg_regional(data, year_fig, type = 'abs')

#####################################################################################
# SDG3 - HEALTH
#####################################################################################
#### PERCENT
deaths_world_regional_diffPer <- merge(queries_mort %>%
                                         dplyr::filter(scenario != 'ref') %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                         dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                         dplyr::group_by(year, region, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
                                         dplyr::summarise(value = sum(mort)) %>%
                                         dplyr::ungroup(),
                                       queries_mort %>%
                                         dplyr::filter(scenario == 'ref') %>%
                                         dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                         dplyr::mutate(pollutant = toupper(pollutant)) %>%
                                         dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
                                         dplyr::group_by(year, region, scenario, scen_type) %>%
                                         dplyr::summarise(ref_value = sum(mort)) %>%
                                         dplyr::ungroup() %>%
                                         dplyr::select(-scenario) %>% dplyr::select(-scen_type),
                                       by = c('region','year')) %>%
  # compute Per difference between Reference and runs
  dplyr::rowwise() %>%
  dplyr::mutate(diff = ifelse(ref_value == 0, 0, 100*(ref_value - value)/ref_value)) %>%
  # compute median by scen
  dplyr::group_by(fasst_region = region,year,scen_type) %>%
  dplyr::summarise(median_diff = median(diff))

deaths_world_regional_diffPer_map <- deaths_world_regional_diffPer %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::left_join(rfasst::fasst_reg %>%
                     dplyr::rename('ISO3' = 'subRegionAlt'), by = 'fasst_region',
                   multiple = 'all') %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO3') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


deaths_world_regional_diffPer_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                            dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                            dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                          deaths_world_regional_diffPer_map, by = 'adm0_a3')

# plot
FIG_EMISSHEALTH_sdg3_deaths_per_map <- ggplot() +
  # color map by regions
  geom_sf(data = deaths_world_regional_diffPer_map, aes(fill = median_diff)) +
  facet_grid(. ~ scen_type) +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided premature deaths [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 25), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 30, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(FIG_EMISSHEALTH_sdg3_deaths_per_map,
       file = file.path(figures_path, paste0('FIG_EMISSHEALTH_sdg3_deaths_per_map_',year_fig,'.pdf')),
       width = 500, height = 200, units = 'mm')


#### SI ========================================================================
weighted_pop_by_iso <- unique(get(load(file.path('input','nutrition','weighted_pop_by_iso.RData'))))

data = merge(queries_mort %>%
               dplyr::filter(scenario != 'ref') %>%
               dplyr::mutate(scen_type = toupper(scen_type)) %>%
               dplyr::mutate(pollutant = toupper(pollutant)) %>%
               dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
               dplyr::group_by(region, year, scenario, scen_type, scen_path, final_share, peak_year, slope, pollutant) %>%
               dplyr::summarise(value = sum(mort)) %>%
               dplyr::ungroup(),
             queries_mort %>%
               dplyr::filter(scenario == 'ref') %>%
               dplyr::mutate(scen_type = toupper(scen_type)) %>%
               dplyr::mutate(pollutant = toupper(pollutant)) %>%
               dplyr::filter(method %in% c('GBD', 'GBD2016')) %>%
               dplyr::group_by(region, year, scenario, scen_type, pollutant) %>%
               dplyr::summarise(ref_value = sum(mort)) %>%
               dplyr::ungroup() %>%
               dplyr::select(-scenario) %>% dplyr::select(-scen_type),
             by = c('pollutant','year','region')) %>%
  dplyr::rename(fasst_region = region) %>%
  dplyr::left_join(rfasst::fasst_reg %>%
                     dplyr::rename('iso' = 'subRegionAlt') %>%
                     dplyr::mutate(iso = tolower(iso)), by = 'fasst_region',
                   multiple = 'all', relationship = "many-to-many") %>%
  dplyr::left_join(read.csv('input/mappings/iso_GCAM_regID.csv', skip = 6) %>%
                     dplyr::select('iso', 'GCAM_region_ID'), by = 'iso') %>%
  dplyr::left_join(read.csv('input/mappings/gcam_id_to_region.csv', skip = 2), by = 'GCAM_region_ID') %>%
  dplyr::mutate(iso = toupper(iso)) %>%
  dplyr::left_join(weighted_pop_by_iso %>%
                     bind_rows(data.frame(
                       region = rep('Taiwan', 4),
                       iso = rep('TWN', 4),
                       weight = rep(1, 4),
                       GCAM_region_ID = rep(30, 4),
                       year = c(2010, 2020, 2030, 2050))),
                   by = c('iso','year','GCAM_region_ID','region'), relationship = "many-to-many") %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::distinct()

data2 <- data %>%
  dplyr::rowwise() %>%
  # apply weights
  dplyr::mutate(ref_value_w = ref_value * weight,
                value_w = value * weight) %>%
  dplyr::mutate(ref_value_w = ifelse(is.na(ref_value_w), 0, ref_value_w),
                value_w = ifelse(is.na(value_w), 0, value_w)) %>%
  # compute Per difference between Reference and runs
  dplyr::mutate(diff = ifelse(ref_value_w != 0, 100*(ref_value_w - value_w)/ref_value_w, 0)) %>%
  # compute by region
  dplyr::group_by(pollutant, year, scenario, scen_type, scen_path, final_share, peak_year, slope, region) %>%
  dplyr::summarise(value = sum(value_w),
                   ref_value = sum(ref_value_w),
                   diff = sum(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(pollutant = factor(pollutant, levels = c('PM25','O3')))

cum_fun_health(data2, year_fig)

#####################################################################################
#####################################################################################
# FIG - GHG & HEALTH
#####################################################################################
#####################################################################################
pl = cowplot::ggdraw() +
  cowplot::draw_plot(FIG_EMISSHEALTH_sdg13_av_ghg_aggPer_map, x = 0.01, y = 0.55, width = 0.95, height = 0.6) +
  cowplot::draw_plot(pl_ghg_by_ghg_emiss_diffAbs_world_boxplot +
                       labs(y = 'GHG diff from\nReference [Mt CO2e]') +
                       theme(
                         axis.title.y = element_text(angle = 90, vjust = 0.5, hjust = 0.5)
                       ), x = 0.01, y = 0.30, width = 0.95, height = 0.4) +
  cowplot::draw_plot(FIG_EMISSHEALTH_sdg3_deaths_per_map, x = 0.01, y = -0.15, width = 0.95, height = 0.6) +
  cowplot::draw_plot_label(label = c("a", "b", "c"), size = 35,
                           x = c(0, 0, 0), y = c(0.99, 0.7, 0.3))
ggsave(file=file.path(figures_path, paste0('FIG_EMISSHEALTH_',year_fig,'.pdf')), plot = pl, width = 800, height = 500, unit = 'mm')


#####################################################################################
#####################################################################################
# SDG 2 - FOOD EXPENDITURE
#####################################################################################
food_subsector <- read.csv('input/nutrition/food_subsector.csv', skip = 3)

#### gdp ---
gdp_regional = load_data('gdp_ppp_pc_regional') %>%
  dplyr::mutate(value = 1e3 * value * gdp_deflator(2005, 1990) ) %>%
  dplyr::mutate(Units = '2005$/capita/day')

#### econ. basket bill ---
food_econ_basket_bill_regional = load_data('food_demand_regional') %>% # units = Pcal/yr
  # Pcal/yr to kcal/capita/day
  dplyr::left_join(load_data('pop_all_regions'),
                   by = c("year", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "region")) %>%
  # convert from Pcal to kcal/day
  dplyr::mutate(value = (value * 1e12) / (population ),
                Units = "kcal/capita/day") %>%
  # total staples and nonstaples kcal consumption
  dplyr::group_by(Units,region,scenario,scen_type,scen_path,final_share,peak_year,slope,year,input) %>%
  dplyr::summarise(consumption = sum(value)) %>%
  # compute the expenditure by supplysector
  dplyr::left_join(load_data('food_demand_prices_regional') %>%
                     dplyr::mutate(price = (value / 1e3),
                                   units_price = '2005$/kcal/day') %>%
                     dplyr::select(-c(Units,value)),
                   by = c('region','year','input','scenario', "scen_type", "scen_path", "final_share", "peak_year", 'slope')) %>%
  dplyr::mutate(expenditure = consumption * price,
                units_expenditure = '2005$/capita/day') %>%
  # total expenditure (staples + nonstaples)
  dplyr::group_by(units_expenditure,region,scenario,scen_type,scen_path,final_share,peak_year,slope,year) %>%
  dplyr::summarise(expenditure = sum(expenditure)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = toupper(scen_type))

#### PERCENT REGIONAL diff
food_econ_basket_bill_regional_ref = food_econ_basket_bill_regional %>%
  dplyr::filter(scenario == 'ref') %>%
  dplyr::select(units_expenditure, region, year, ref_expenditure = expenditure)
food_econ_basket_bill_regional_diff = merge(
  food_econ_basket_bill_regional %>%
    dplyr::filter(scenario != 'ref'),
  food_econ_basket_bill_regional_ref,
  by = c('units_expenditure', 'region', 'year')
) %>%
  dplyr::mutate(diff = 100*(expenditure - ref_expenditure)/ref_expenditure) %>%
  dplyr::group_by(region, year, scen_type) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(region)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR'))) %>%
  cut_region_names(short = T)

pl_food_econ_basket_bill_regional_diff <- ggplot(food_econ_basket_bill_regional_diff %>%
                                                   dplyr::filter(year == year_fig)) +
  geom_bar(aes(x = region, y = median_diff, fill = scen_type), stat = 'identity', alpha = 0.75) +
  geom_errorbar(aes(x = region, ymin = min_diff, ymax = max_diff), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                    labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                    name = 'Scenario') +
  facet_wrap(. ~ scen_type) +
  # labs
  labs(y = 'Daily expenditure per capita difference with Reference [%]', x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=22, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_food_econ_basket_bill_regional_diff, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_regional_diff_',year_fig,'.pdf')),
       width = 950, height = 500, units = 'mm', limitsize = FALSE)

dataa <- merge(
  food_econ_basket_bill_regional %>%
    dplyr::filter(scenario != 'ref'),
  food_econ_basket_bill_regional %>%
    dplyr::filter(scenario == 'ref') %>%
    dplyr::select(units_expenditure, region, year, ref_expenditure = expenditure),
  by = c('units_expenditure', 'region', 'year')
) %>%
  dplyr::mutate(diff = 100*(expenditure - ref_expenditure)/ref_expenditure) %>%
  dplyr::group_by(year, scen_type) %>%
  dplyr::summarise(median_diff = median(diff)) %>%
  dplyr::ungroup()

#### SI ========================================================================


#### PERCENT REGIONAL diff withPath
food_econ_basket_bill_regional_diff_withpath = merge(
  food_econ_basket_bill_regional %>%
    dplyr::filter(scenario != 'ref'),
  food_econ_basket_bill_regional_ref,
  by = c('units_expenditure', 'region', 'year')
) %>%
  dplyr::mutate(diff = 100*(expenditure - ref_expenditure)/ref_expenditure) %>%
  dplyr::group_by(region, year, scen_type, scen_path) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(region)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR'))) %>%
  cut_region_names(short = T)

pl_food_econ_basket_bill_regional_diff_withpath <- ggplot(food_econ_basket_bill_regional_diff_withpath %>%
                                                            rename_pathways() %>%
                                                            dplyr::filter(year == year_fig)) +
  geom_bar(aes(x = region, y = median_diff, fill = scen_type), stat = 'identity', alpha = 0.75) +
  geom_errorbar(aes(x = region, ymin = min_diff, ymax = max_diff), width=0.3, colour="#757575", alpha=1, linewidth=1.2) +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                    labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                    name = 'Scenario') +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  # labs
  labs(y = 'Daily expenditure per capita difference with Reference [%]', x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'right', legend.direction = 'vertical',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=22, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 32),
        legend.title = element_text(size = 40),
        title = element_text(size = 30))
ggsave(pl_food_econ_basket_bill_regional_diff_withpath, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_regional_diff_',year_fig,'_withpath.pdf')),
       width = 950, height = 500, units = 'mm', limitsize = FALSE)



## MAPS
food_econ_basket_bill_regional_diff_map_withpath <- food_econ_basket_bill_regional_diff_withpath %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3') %>%
  # subset scen
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))

food_econ_basket_bill_regional_diff_map_withpath = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                  dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                  dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                  dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                                food_econ_basket_bill_regional_diff_map_withpath, by = 'adm0_a3')

pl_food_econ_basket_bill_world_regional_diffPer_map_withPath <- ggplot() +
  # color map by regions
  geom_sf(data = food_econ_basket_bill_regional_diff_map_withpath %>%
            rename_pathways(), aes(fill = -median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkred", high = "darkgreen",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Avoided daily expenditure per capita [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 35), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_food_econ_basket_bill_world_regional_diffPer_map_withPath, file = file.path(figures_path, paste0('sgd2_food_econ_basket_bill_per_map_',year_fig,'_withPath.pdf')),
       width = 500, height = 300, units = 'mm')




## CUMFUN
food_econ_basket_bill_regional_si <- merge(
  food_econ_basket_bill_regional %>%
    dplyr::filter(scen_type == 'REF') %>%
    dplyr::select(units_expenditure, region, year, ref_expenditure = expenditure),
  food_econ_basket_bill_regional %>%
    dplyr::filter(scen_type != 'REF'),
  by = c('units_expenditure','region','year')
)

cum_fun_foodbasket(food_econ_basket_bill_regional_si, year_fig)

#####################################################################################
# SDG 13 - POLICY COST
#####################################################################################
policyCost <- load_data('policy_cost') %>%
  dplyr::select(-pointset) %>% dplyr::select(-Units) %>%
  distinct() %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_path = ifelse(scen_type == 'REF', 'all', scen_path))


#### PERCENT REGIONAL diff
policyCost_ref = policyCost %>%
  dplyr::filter(scen_type == 'REF') %>%
  dplyr::select(region, year, ref_value = value)
policyCost_regional_diff = merge(
  policyCost %>%
    dplyr::filter(scen_type != 'REF'),
  policyCost_ref,
  by = c('region', 'year')
) %>%
  dplyr::mutate(diff = ifelse(ref_value != 0, 100*(value - ref_value)/ref_value, NA)) %>%
  dplyr::filter(!is.na(diff)) %>%
  dplyr::group_by(region, year, scen_type) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(region)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


policyCost_regional_diff_map <- policyCost_regional_diff %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

policyCost_regional_diff_map = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                       dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                       dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                       dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                     policyCost_regional_diff_map, by = 'adm0_a3')

# plot
pl_policyCost_regional_diff_map <- ggplot() +
  # color map by regions
  geom_sf(data = policyCost_regional_diff_map, aes(fill = median_diff)) +
  facet_grid(. ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkgreen", high = "darkred",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Policy Cost difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 32), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_policyCost_regional_diff_map,
       file = file.path(figures_path, paste0('sdg13_policyCost_regional_diff_map',year_fig,'.pdf')),
       width = 500, height = 200, units = 'mm')

# with Path
policyCost_regional_diff_withPath = merge(
  policyCost %>%
    dplyr::filter(scen_type != 'REF', value != 0),
  policyCost_ref,
  by = c('region', 'year')
) %>%
  dplyr::mutate(diff = ifelse(ref_value != 0, 100*(value - ref_value)/ref_value, 0)) %>%
  dplyr::group_by(region, year, scen_type, scen_path) %>%
  dplyr::summarise(median_diff = median(diff),
                   min_diff = min(diff),
                   max_diff = max(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(region)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))


policyCost_regional_diff_map_withPath <- policyCost_regional_diff_withPath %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  # merge with GCAM regions
  dplyr::mutate('GCAM Region' = region) %>%
  dplyr::inner_join(rfasst::GCAM_reg, by = 'GCAM Region', multiple = "all", relationship = "many-to-many") %>%
  # merge with world data
  dplyr::rename('adm0_a3' = 'ISO 3')

policyCost_regional_diff_map_withPath = merge(rnaturalearth::ne_countries(scale = "small", returnclass = "sf") %>%
                                                dplyr::mutate('adm0_a3' = dplyr::if_else(adm0_a3== 'ROU', 'ROM', adm0_a3)) %>%
                                                dplyr::mutate('adm0_a3' = dplyr::if_else(sovereignt=='South Sudan', 'SSD', adm0_a3)) %>%
                                                dplyr::filter(!adm0_a3 %in% c("ATA","FJI")),
                                              policyCost_regional_diff_map_withPath, by = 'adm0_a3')

# plot
pl_policyCost_regional_diff_map_withPath <- ggplot() +
  # color map by regions
  geom_sf(data = policyCost_regional_diff_map_withPath %>%
            rename_pathways(), aes(fill = median_diff)) +
  facet_grid(scen_path ~ scen_type, scales = 'fixed') +
  scale_fill_gradient2(low = "darkgreen", high = "darkred",
                       mid = '#f7f7f7', midpoint = 0,
                       name = expression(paste("Policy Cost difference [%]","\n"))) +
  # theme
  guides(fill = guide_colorbar(title.position = "left")) +
  theme_light() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#ffffff",
                                        colour = "#ffffff"),
        legend.position = 'bottom',legend.key.height = unit(0.75, 'cm'), legend.key.width = unit(2.5,'cm'),
        legend.text = element_text(size = 32), legend.title = element_text(size = 30, vjust = 0.95),
        strip.text = element_text(size = 40, color = 'black'),
        strip.background =element_rect(fill="white"), title = element_text(size = 30))
ggsave(pl_policyCost_regional_diff_map_withPath,
       file = file.path(figures_path, paste0('sdg13_policyCost_regional_diff_map_withPath',year_fig,'.pdf')),
       width = 500, height = 200, units = 'mm')

#### SI ========================================================================

policyCost_regional_diff_si = merge(
  policyCost %>%
    dplyr::filter(scen_type != 'REF'),
  policyCost_ref,
  by = c('region', 'year')
) %>%
  dplyr::filter(value != 0)

cum_fun_policyCost(policyCost_regional_diff_si, year_fig)
prob_distrib_policyCost(policyCost_regional_diff_si %>%
                          dplyr::filter(year == year_fig) %>%
                          dplyr::group_by(region, year) %>%
                          dplyr::mutate(p95 = quantile(value, 0.95)) %>%
                          dplyr::filter(value <= p95) %>%
                          dplyr::select(-p95),
                        year_fig)



#####################################################################################
#####################################################################################
# FIG - ECON
#####################################################################################
#####################################################################################
pl = cowplot::ggdraw() +
  cowplot::draw_plot(pl_food_econ_basket_bill_regional_diff +
                       theme(legend.position = 'none') +
                       labs(y = 'Daily food expenditure diff\nfrom Reference [%]'), x = 0.01, y = 0.4, width = 0.975, height = 0.6) +
  cowplot::draw_plot(pl_policyCost_regional_diff_map +
                       labs(y = ''), x = 0.01, y = 0.05, width = 0.95, height = 0.4) +
  cowplot::draw_plot_label(label = c("a", "b"), size = 35,
                           x = c(0, 0), y = c(0.99, 0.4))
ggsave(file=file.path(figures_path, paste0('FIG_ECON_',year_fig,'.pdf')), plot = pl, width = 800, height = 500, unit = 'mm')


#####################################################################################
# SDG 3 - NUTRITIONAL VALUES
#####################################################################################
food_subsector <- read.csv('input/nutrition/food_subsector.csv', skip = 3)
data_macronutrient <- read.csv('input/nutrition/gcam_macronutrient.csv', skip = 5)
data_micronutrient <- read.csv('input/nutrition/USDA_data_final.csv')
colnames(data_micronutrient) <- c("Food", "GCAM_commodity", "Calories (kcal)", "Protein (g)",
                                  "Carbohydrate (g)", "Sugars (g)", "Fiber (g)", "Total fat (g)",
                                  "Fatty acids saturated (g)", "Fatty acids monounsaturated (g)",
                                  "Fatty acids polyunsaturated (g)", "Cholesterol (mg)",
                                  "Retinol (mcg)", "Vitamin A (mcg)", "Alpha carotene (mcg)",
                                  "Beta carotene (mcg)", "Cryptoxanthin, beta (mcg)",
                                  "Lycopene (mcg)", "Lutein and zeaxanthin (mcg)", "Thiamin (mg)",
                                  "Riboflavin (mg)", "Niacin (mg)", "Vitamin B6 (mg)",
                                  "Folic acid (mcg)", "Folate (mcg)", "Folate DFE (mcg)", # "Folate food (mcg)" = Folate
                                  "Folate total (mcg)", "Choline (mg)", "Vitamin B12 (mcg)",
                                  "Added vitamin B12 (mcg)", "Vitamin C (mg)",
                                  "Vitamin D (mcg)", "Vitamin E alpha-tocopherol (mg)", # vitamin d2 and d3 = vitamin d
                                  "Added vitamin E (mg)", "Vitamin K (mcg)", "Calcium (mg)", # Vitamin K phylloquinone = Vitamin K
                                  "Phosphorus (mg)", "Magnesium (mg)", "Iron (mg)", "Zinc (mg)",
                                  "Copper (mg)", "Selenium (mcg)", "Potassium (mg)", "Sodium (mg)",
                                  "Caffeine (mg)", "Theobromine (mg)", "Alcohol (g)", "4:0 (g)",
                                  "6:0 (g)", "8:0 (g)", "10:0 (g)", "12:0 (g)", "14:0 (g)",
                                  "16:0 (g)", "18:0 (g)", "16:1 (g)", "18:1 (g)", "20:1 (g)",
                                  "22:1 (g)", "18:2 (g)", "18:3 (g)", "18:4 (g)", "20:4 (g)",
                                  "20:5 n3 (g)", "22:5 n3 (g)", "22:6 n3 (g)", "Water (g)")
mder <- read.csv(paste0("input/nutrition/MDER.csv")) %>%
  dplyr::rename(mder_units = unit) %>%
  dplyr::mutate(mder_units = 'kcal/capita/day')
colnames(mder) = c('variable','mder_units','mder','std','min','max')
GramProteinFatPerKcal <- read.csv("input/nutrition/GramProteinFatPerKcal.csv", skip = 3)
micronutrients <- read.csv('input/nutrition/rni.csv', skip = 3)
weighted_pop_sex_age <- get(load('input/nutrition/weighted_pop_sex_age.RData'))

# compute weight by population
population_weights <- weighted_pop_sex_age %>%
  dplyr::filter(scenario == 'ref', year == 2015) %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(regional_pop = sum(pop_sex_age)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(world_pop = sum(regional_pop)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(weight = regional_pop / world_pop) %>%
  dplyr::select(region, weight) %>%
  dplyr::ungroup()
# save(population_weights, file = file.path('input/nutrition/population_weigths.RData'))

####################### ADESA computation ###########################
#### dietary energy supply (DES) ---
# units: kcal/cap/day
# by region
# get total consumption in calories
dietary_energy_supply <- load_data('food_consumption_regional') %>%
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year) %>%
  # aggregate staple and non-staple calories
  dplyr::summarize(value = sum(value)) %>%
  dplyr::left_join(load_data('pop_all_regions'),
                   by = c("year", "scenario", "scen_type", "scen_path", "final_share", "peak_year", "slope", "region")) %>%
  # convert from Pcal to kcal/cap/day
  dplyr::mutate(value = (value * 1e12) / (population * 365),
                units = "kcal/cap/day") %>%
  dplyr::filter(year <= 2050)

## share of dietary energy supply from staples ---
# find consumption of staple and non-staple Pcal
staples_vs_nonstaples_Pcal <- load_data('food_consumption_regional') %>%
  dplyr::select(-nestingSector1) %>% dplyr::select(-nestingSector2) %>% dplyr::select(-nestingSector3) %>%
  dplyr::rename(subsector = technology) %>%
  left_join_error_no_match(food_subsector, by = 'subsector') %>%
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year, supplysector) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::mutate(Units = 'Pcal') %>%
  dplyr::ungroup()

## SSP2 population data ---
ssp_data <- read.csv(paste0("input/nutrition/SSP2_population_by_demographic.csv"), skip = 1)
iso_gcam_regions <- read.csv(paste0("input/mappings/iso_GCAM_regID.csv"), skip = 6)
id_gcam_regions <- read.csv(paste0("input/mappings/gcam_id_to_region.csv"), skip = 2)
country_gcam_regions <- read.csv(paste0("input/mappings/country_to_gcam_id.csv"))
regions_key <- dplyr::left_join(country_gcam_regions, id_gcam_regions, by = "GCAM_region_ID") %>%
  dplyr::select(-1)

ssp_data_clean <- iso_gcam_regions %>%
  dplyr::select(-region_GCAM3, -GCAM_region_ID) %>%
  dplyr::left_join(ssp_data %>%
                     dplyr::filter(SCENARIO == 'SSP2_v9_130115'),
                   by = "iso", multiple = 'all') %>%
  dplyr::select(-MODEL, -REGION) %>%
  dplyr::rename(scenario = SCENARIO,
                variable = VARIABLE,
                unit = UNIT)
# Remove X from year columns
colnames(ssp_data_clean) <- gsub("X", "", colnames(ssp_data_clean))
# Pivot longer
ssp_data_long <- ssp_data_clean %>%
  tidyr::pivot_longer(cols = 6:24, names_to = "year", values_to = "value") %>%
  dplyr::mutate(value = value * 1e6,
                unit = "total population") %>%
  dplyr::mutate(year = as.integer(year))
# Isolate reference (total) population
reference_pop <- ssp_data_long %>%
  dplyr::filter(variable == "Population") %>%
  dplyr::rename(total_pop = value) %>%
  dplyr::select(iso, year, total_pop)
# Join and calculate demographic shares of population
ssp_data_final <- ssp_data_long %>%
  # Remove total male and total female pop, we want by age/sex
  dplyr::filter(!variable %in% c("Population|Male", "Population|Female", "Population", NA)) %>%
  dplyr::left_join(reference_pop, by = c("iso", "year")) %>%
  dplyr::mutate(demo_share = value / total_pop) %>%
  dplyr::rename(sub_pop = value)  %>%
  dplyr::rename(pop_units = unit)

# Get population by sex and age
# Population weighting
total_regional_pop <- ssp_data_final %>%
  dplyr::select(-scenario,-iso) %>%
  # get GCAM regions instead of country names
  dplyr::left_join(regions_key, by = "country_name") %>%
  # get total regional population
  dplyr::group_by(year, GCAM_region_ID, country_name, region) %>%
  # isolate total population by country
  dplyr::distinct(total_pop) %>%
  dplyr::group_by(year, GCAM_region_ID, region) %>%
  # sum for total regional population
  dplyr::mutate(total_regional_pop = sum(total_pop)) %>%
  dplyr::ungroup()

weighted_pop <- ssp_data_final %>%
  dplyr::select(-scenario) %>%
  # get GCAM regions instead of country names
  dplyr::left_join(regions_key, by = "country_name") %>%
  # get total regional population
  dplyr::left_join(total_regional_pop) %>%
  # weight each country by its population over total regional pop
  dplyr::group_by(country_name, year) %>%
  dplyr::mutate(weight = total_pop / total_regional_pop) %>%
  dplyr::mutate(iso = toupper(iso))
weighted_pop2 <- weighted_pop %>%
  dplyr::select(-variable, -demo_share, -sub_pop)
# save(weighted_pop2, file = file.path('input','nutrition','weighted_pop_by_iso.RData'))

weighted_pop <- weighted_pop %>%
  dplyr::select(-iso) %>%
  # get GCAM population
  dplyr::left_join(load_data('pop_all_regions'),
                   by = c("region", "year"), relationship = "many-to-many") %>%
  # compute GCAM population by sex and age for each country
  dplyr::mutate(weighted_demographics = demo_share * weight * population)

weighted_pop_sex_age <- weighted_pop %>%
  dplyr::select(-pop_units, -sub_pop, -total_pop, -demo_share, -weight) %>%
  dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, variable, year, region) %>%
  # sum the weighted averages for each country into GCAM regions
  dplyr::summarize(pop_sex_age = sum(weighted_demographics))
# save(weighted_pop_sex_age, file = file.path('input','nutrition','weighted_pop_sex_age.RData'))

# # join with MDER data, calculate caloric requirements by sex and age
# adesa_denominator <- weighted_pop_sex_age %>%
#   dplyr::left_join(mder, by = "variable") %>%
#   dplyr::select(-std) %>%
#   dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, variable, year, region) %>%
#   # compute a range because of differing physical activity levels
#   dplyr::summarize(cal_req_x_pop = mder * pop_sex_age,
#             min_cal_req_x_pop = min * pop_sex_age,
#             max_cal_req_x_pop = max * pop_sex_age) %>%
#   # aggregate caloric requirements to get total regional values
#   dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, year) %>%
#   dplyr::summarize(denominator_sum = sum(cal_req_x_pop),
#             min_denominator_sum = sum(min_cal_req_x_pop),
#             max_denominator_sum = sum(max_cal_req_x_pop)) %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   dplyr::filter(year <= 2050)
#
# # add in regional calorie info, calculate ADESA
# adesa <- dplyr::left_join(adesa_denominator, dietary_energy_supply) %>%
#   dplyr::group_by(year, region, scenario, scen_type, scen_path, final_share, peak_year, slope) %>%
#   reframe(adesa = (value / denominator_sum) * population * 100, # convert to unitless and percentage
#           min_adesa = (value / min_denominator_sum) * population * 100,
#           max_adesa = (value / max_denominator_sum) * population * 100,
#           .groups = "keep") %>%
#   dplyr::ungroup()
# save(adesa, file = file.path('output','datasets','adesa.RData'))
#
# adesa_world <- adesa %>%
#   dplyr::left_join(population_weights, by = 'region') %>%
#   rowwise() %>%
#   dplyr::mutate(adesa = weight * adesa,
#          min_adesa = weight * min_adesa,
#          max_adesa = weight * max_adesa) %>%
#   dplyr::group_by(year,scenario,scen_type,scen_path,final_share,peak_year,slope) %>%
#   dplyr::summarise(adesa = sum(adesa),
#             min_adesa = sum(min_adesa),
#             max_adesa = sum(max_adesa)) %>%
#   dplyr::ungroup()
# save(adesa_world, file = file.path('output','datasets','adesa_world.RData'))

load(file.path('output','datasets','adesa.RData'))
load(file.path('output','datasets','adesa_world.RData'))

#### ADESA LOLIPOP PLOT =======================================

adesa_to_plot_withPath <- adesa %>%
  dplyr::filter(year == year_fig) %>%
  dplyr::group_by(region,scen_type,scen_path,year) %>%
  dplyr::summarise(adesa = median(adesa)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR'))) %>%
  dplyr::group_by(region, year, scen_path) %>%
  dplyr::mutate(min_value = min(adesa),
                max_value = max(adesa)) %>%
  dplyr::ungroup() %>%
  cut_region_names() %>%
  dplyr::mutate(region_num = as.numeric(as.factor(region)))

pl_adesa_lolipop_h_withPath <- ggplot() +
  geom_hline(yintercept = 100, linetype = 'dashed', color = 'red') +
  geom_segment(data = adesa_to_plot_withPath %>%
                 dplyr::filter(scen_type != 'REF', scen_path == 'all') %>%
                 dplyr::select(region_num, min_value, max_value) %>%
                 dplyr::distinct(),
               aes(x = region_num-0.225, xend = region_num-0.225, y = min_value, yend = max_value), color="black") +
  geom_segment(data = adesa_to_plot_withPath %>%
                 dplyr::filter(scen_type != 'REF', scen_path == 'plus') %>%
                 dplyr::select(region_num, min_value, max_value) %>%
                 dplyr::distinct(),
               aes(x = region_num+0.225, xend = region_num+0.225, y = min_value, yend = max_value), color="black") +
  geom_point(data = adesa_to_plot_withPath %>%
               dplyr::filter(scen_type == 'SPP'),
             aes(x = region_num, y = adesa, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.85,
             position = position_dodge(width = 0.9)) +
  geom_point(data = adesa_to_plot_withPath %>%
               dplyr::filter(scen_type == 'SNR'),
             aes(x = region_num, y = adesa, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.85,
             position = position_dodge(width = 0.9)) +
  geom_point(data = adesa_to_plot_withPath %>%
               dplyr::filter(scen_type == 'SPPNR'),
             aes(x = region_num, y = adesa, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.85,
             position = position_dodge(width = 0.9)) +
  geom_point(data = adesa_to_plot_withPath %>%
               dplyr::filter(scen_type == 'REF'),
             aes(x = region_num, y = adesa, color = scen_type, fill = scen_type), size = 7, alpha = 0.85, shape = 4, stroke = 2) +
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                     labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                     name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                    labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                    name = 'Scenario') +
  scale_shape_manual(values = scen_path_shape_refVsSppVsSnrVsSppnr2,
                     labels = scen_path_shape_refVsSppVsSnrVsSppnr2.labs,
                     name = 'Pathway') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=27.5, angle = 90, hjust = 1, vjust = 0.25),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40, face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 35)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2), fill = 'none') +
  labs(x = '', y = 'ADESA') +
  scale_x_continuous(breaks = adesa_to_plot_withPath$region_num, labels = adesa_to_plot_withPath$region)
ggsave(pl_adesa_lolipop_h_withPath, filename = file.path(figures_path, 'pl_adesa_lolipop_h_withPath.pdf'),
       width = 750, height = 550, units = 'mm', limitsize = F)


########################### MACRONUTRIENTS analysis ###########################
### compute % of macronutrients in the total energy intake
## Macronutrient by Kcal of food consumption
macronutrients_en_basic = load_data('food_consumption_regional') %>%
  # dplyr::rename columns
  dplyr::rename('GCAM_commodity' = 'technology') %>%
  dplyr::rename('consumption' = 'value') %>%
  # aggregate population data
  dplyr::left_join(load_data('pop_all_regions'),
                   by = c("year", "scenario", "scen_type", "scen_path",
                          "final_share", "peak_year", "slope", "region"),
                   multiple = "all") %>%
  # convert from Pcal to kcal/capita/day
  dplyr::mutate(consumptionPerCapita = (consumption * 1e12) / (population * 365),
                Units = "kcal/capita/day") %>%
  dplyr::left_join(GramProteinFatPerKcal %>%
                     # match regions' id with regions' name
                     left_join_keep_first_only(regions_key %>% dplyr::select(-1), by = 'GCAM_region_ID'),
                   by = c('region','GCAM_commodity'), multiple = "all") %>%
  # compute total Protein and Fat [kcal/capita/day]
  dplyr::mutate(kcalProteinPerCapita = consumptionPerCapita * gProteinPerKcal * Kcalperg,
                kcalFatPerCapita = consumptionPerCapita * gFatPerKcal * Kcalperg) %>%
  # aggregate food commodities
  dplyr::group_by(Units, region, scenario, scen_type, scen_path, final_share, peak_year, slope, year) %>%
  dplyr::summarise(kcalProteinPerCapita = sum(kcalProteinPerCapita),
                   kcalFatPerCapita = sum(kcalFatPerCapita),
                   consumptionPerCapita = sum(consumptionPerCapita)) %>%
  dplyr::ungroup() %>%
  # compute % of protein and fat intake with respect to the kcal (energy) consumption
  dplyr::rowwise() %>%
  dplyr::mutate(perProteinPerCapita = kcalProteinPerCapita / consumptionPerCapita,
                perFatPerCapita = kcalFatPerCapita / consumptionPerCapita) %>%
  dplyr::ungroup() %>%
  dplyr::select(Units, region, scenario, scen_type, scen_path, final_share, peak_year, slope, year,
                perProteinPerCapita, kcalProteinPerCapita, perFatPerCapita, kcalFatPerCapita, consumptionPerCapita)

# world macronutrients intake weighted by pop
macronutrients_en_basic_world <- macronutrients_en_basic %>%
  dplyr::left_join(population_weights, by = 'region') %>%
  dplyr::mutate(perProteinPerCapita = weight * perProteinPerCapita,
                perFatPerCapita = weight * perFatPerCapita) %>%
  dplyr::group_by(Units, scenario, scen_type, scen_path, final_share, peak_year, slope, year) %>%
  dplyr::summarise(perProteinPerCapita = median(perProteinPerCapita, na.rm = T), # remove Taiwan
                   perFatPerCapita = median(perFatPerCapita, na.rm = T)) %>%
  dplyr::ungroup()

########################### MACRONUTRIENTS SI plots ###########################
macronutrients_en_basic_si <- macronutrients_en_basic %>%
  dplyr::select(-'kcalFatPerCapita') %>% dplyr::select(-'kcalProteinPerCapita') %>% dplyr::select(-'consumptionPerCapita') %>%
  tidyr::pivot_longer(cols = c('perProteinPerCapita','perFatPerCapita'),
                      names_to = 'macronutrient', values_to = 'value') %>%
  dplyr::mutate(scen_path = ifelse(scen_type %in% c('REF','ref'), 'REF', scen_path)) %>%
  dplyr::mutate(macronutrient = factor(macronutrient, levels = c("perProteinPerCapita","perFatPerCapita"))) %>%
  dplyr::rename(type = macronutrient)

prob_distrib_nutrients(data.table::as.data.table(macronutrients_en_basic_si), year_fig, type = 'macronutrients')
cum_fun_nutrients(macronutrients_en_basic_si, year_fig, type = 'macronutrients')


########################### MACRONUTRIENTS plots ###########################

# FIG - macronutrients 2050
data_fig1 <- macronutrients_en_basic_world %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  tidyr::pivot_longer(cols = perProteinPerCapita:perFatPerCapita, names_to = 'macronutrient')

data_fig2_raw <- merge(data_fig1 %>% dplyr::filter(scenario == 'ref') %>%
                         dplyr::select(Units, year, macronutrient, value_ref = value),
                       data_fig1 %>% dplyr::filter(scenario != 'ref'),
                       by = c('Units','year','macronutrient')) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100 * (value - value_ref)/value_ref) %>%
  dplyr::mutate(macronutrient = factor(macronutrient, levels = c("perProteinPerCapita","perFatPerCapita"))) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))


## BOX PLOTS
FIG_NUTRITION_macronutrients_boxplot_world = ggplot(data = data_fig2_raw %>%
                                                      dplyr::filter(year %in% c(2030, 2050))) +
  geom_hline(aes(yintercept = 0)) +
  geom_boxplot(aes(x = as.factor(year), y = diff, fill = macronutrient), alpha = 1) +  # Median area
  facet_wrap(. ~ scen_type) +
  scale_fill_manual(values = macronutrients_palette, name = 'Macronutrient',
                    labels = macronutrients.labs) +
  # labs
  labs(y = expression(paste('Per capita intake difference with Reference [%]')), x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=30),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40, face = "bold"),
        title = element_text(size = 40))
ggsave(FIG_NUTRITION_macronutrients_boxplot_world,
       file = file.path(figures_path,paste0('FIG_NUTRITION_macronutrients_boxplot_world.pdf')),
       width = 575, height = 450, units = 'mm')


## REGIONAL lolipop with path
data_fig1_reg <- macronutrients_en_basic %>%
  dplyr::select(-'kcalProteinPerCapita') %>% dplyr::select(-'kcalFatPerCapita') %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  tidyr::pivot_longer(cols = perProteinPerCapita:perFatPerCapita, names_to = 'macronutrient')

data_fig2_reg <- merge(data_fig1_reg %>% dplyr::filter(scenario == 'ref') %>%
                         dplyr::select(region, Units, year, macronutrient, value_ref = value),
                       data_fig1_reg %>% dplyr::filter(scenario != 'ref'),
                       by = c('region', 'Units','year','macronutrient')) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100 * (value - value_ref)/value_ref) %>%
  dplyr::group_by(region, scen_type, scen_path, year, macronutrient) %>%
  dplyr::summarise(min_value = min(diff),
                   max_value = max(diff),
                   median_value = median(diff)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(macronutrient = factor(macronutrient, levels = c("perProteinPerCapita","perFatPerCapita"))) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))

data_fig2_reg_to_plot <- data_fig2_reg %>%
  dplyr::filter(year == year_fig)  %>%
  cut_region_names()%>%
  dplyr::mutate(region = factor(region, levels = rev(sort(unique(region)))))

pl_macronutrients_regional_lolipop_withPath <- ggplot() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red') +
  geom_segment(data = data_fig2_reg_to_plot %>%
                 dplyr::group_by(region, macronutrient, year) %>%
                 dplyr::summarise(min_value = min(median_value),
                                  max_value = max(median_value)) %>%
                 dplyr::ungroup(),
               aes(x = region, xend = region, y = min_value, yend = max_value), color="black") +
  geom_point(data = data_fig2_reg_to_plot %>%
               dplyr::filter(scen_type == 'SPP'),
             aes(x = region, y = median_value, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.95) +
  geom_point(data = data_fig2_reg_to_plot %>%
               dplyr::filter(scen_type == 'SNR'),
             aes(x = region, y = median_value, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.95) +
  geom_point(data = data_fig2_reg_to_plot %>%
               dplyr::filter(scen_type == 'SPPNR'),
             aes(x = region, y = median_value, color = scen_type, fill = scen_type, shape = scen_path), size = 9, alpha = 0.95) +
  scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                     labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                     name = 'Scenario') +
  scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                    labels = scen_palette_refVsSppVsSnrVsSppnr.labs,
                    name = 'Scenario') +
  scale_shape_manual(values = scen_path_shape_refVsSppVsSnrVsSppnr2,
                     labels = scen_path_shape_refVsSppVsSnrVsSppnr2.labs,
                     name = 'Pathway') +
  facet_grid(. ~ macronutrient) +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom',
        legend.direction = 'horizontal', legend.box = "vertical",
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(angle = 0),
        axis.text.x = element_text(size=27.5),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 35)) +
  labs(x = '', y = 'Percentual difference with Reference') +
  coord_flip()
ggsave(pl_macronutrients_regional_lolipop_withPath, filename = file.path(figures_path, 'sdg3_macronutrients_regional_lolipop_withPath.pdf'),
       width = 450, height = 650, units = 'mm', limitsize = F)



########################### MICRONUTRIENTS computation ###########################

# # Average over individual food items to get a representative value for each commodity
# average_data <- data_micronutrient %>%
#   tidyr::pivot_longer(cols = 3:67, names_to = "Nutrient") %>%
#   dplyr::group_by(`GCAM_commodity`, Nutrient) %>%
#   dplyr::summarize(average = median(value)) %>%
#   dplyr::filter_all(all_vars(!stringr::str_detect(., ":"))) %>%
#   tidyr::pivot_wider(names_from = Nutrient, values_from = average) %>%
#   tidyr::pivot_longer(-c('GCAM_commodity','Calories (kcal)'), names_to = 'nutrient', values_to = 'nutrient_value') %>%
#   dplyr::mutate(nutrient_value = nutrient_value/`Calories (kcal)`) %>%
#   dplyr::mutate(
#     nutrient_name = stringr::str_split(nutrient, " \\(") %>%
#       sapply(function(x) x[1]),
#     nutrient_units = stringr::str_split(nutrient, " \\(") %>%
#       sapply(function(x) sub("\\)$", "", x[2])),
#     nutrient_units = paste0(nutrient_units,'/kcal')
#   ) %>%
#   dplyr::select(-c(`Calories (kcal)`,nutrient)) %>%
#   dplyr::ungroup()
#
#
# # Total micronutrients consumption
# micronutrients_consumption <- dplyr::left_join(load_data('food_consumption_regional') %>%
#                                          # TODO: find data of nutritional values of FiberCrop (introduce it in the average_data)
#                                           dplyr::filter(technology != 'FiberCrop') %>%
#                                           dplyr::left_join(load_data('pop_all_regions'),
#                                                    by = c("year", "scenario", "scen_type", "scen_path",
#                                                           "final_share", "peak_year", "slope", "region")) %>%
#                                          # convert from Pcal to kcal/day
#                                           dplyr::mutate(value = (value * 1e12) / (population * 365),
#                                                 Units = "kcal/capita/day") %>%
#                                          # dplyr::rename columns
#                                           dplyr::rename('GCAM_commodity' = 'technology',
#                                                 'consumption' = 'value'),
#                                        average_data,
#                                        by = 'GCAM_commodity') %>%
#   dplyr::mutate('total_micronutrient_intake' = consumption * nutrient_value) %>%
#   dplyr::group_by(region,scenario, scen_type, scen_path, final_share, peak_year, slope,year,nutrient_name,nutrient_units) %>%
#   dplyr::summarise(total_micronutrient_intake = sum(total_micronutrient_intake, na.rm = TRUE)) %>%
#   dplyr::mutate(nutrient_units = stringr::str_replace(nutrient_units, "/kcal", "/capita/day")) %>%
#   dplyr::mutate(year = as.numeric(as.character(year))) %>%
#   dplyr::ungroup()
#
# micronutrients_RNI = merge(micronutrients %>%
#                              dplyr::rename('nutrient_name' = 'micronutrient',
#                                     'units_rni' = 'Units') %>%
#                              dplyr::mutate(nutrient_name = tolower(nutrient_name)),
#                            weighted_pop_sex_age,
#                            by = 'variable') %>%
#   dplyr::mutate(bySocioGroup_rni = as.numeric(mean_requirement * pop_sex_age)) %>%
#   dplyr::group_by(nutrient_name,units_rni,year,region) %>%
#   dplyr::summarise(byReg_rni = sum(bySocioGroup_rni),
#                    pop = sum(pop_sex_age)) %>%
#   dplyr::mutate(byRegC_rni = byReg_rni/pop) %>%
#   dplyr::mutate(units_rni = stringr::str_replace(units_rni, "/day", "/capita/day")) %>%
#   dplyr::mutate(year = as.numeric(as.character(year))) %>%
#   dplyr::ungroup()
#
# micronutrients = merge(micronutrients_RNI %>%
#                          dplyr::mutate(nutrient_name = tolower(nutrient_name)),
#                        micronutrients_consumption %>%
#                          dplyr::mutate(nutrient_name = tolower(nutrient_name)),
#                        by = c('region','year','nutrient_name'))
#
# write.csv(micronutrients, file = 'input/nutrition/micronutrients_computed.csv', row.names = F)
assign('micronutrients', read.csv(file = 'input/nutrition/micronutrients_computed.csv'))


########################### MICRONUTRIENTS plot ###########################

micronutrients_rni <- micronutrients
micronutrients_rni$nutrient_name = factor(micronutrients_rni$nutrient_name,
                                          levels = c("calcium", "iron", "magnesium", "selenium", 'sodium', 'zinc',
                                                     'folate', 'niacin', 'riboflavin','thiamin', 'vitamin a', 'vitamin b6',
                                                     'vitamin b12', 'vitamin c', 'vitamin d', 'vitamin k'))
micronutrients_rni <- micronutrients_rni %>%
  cut_region_names(short = T) %>%
  dplyr::filter(year == year_fig) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(nutrient_item = paste0(nutrient_name, ' [', nutrient_units, ']')) %>%
  dplyr::group_by(region, scen_type, year, nutrient_item, RNI = byRegC_rni) %>%
  dplyr::summarise(total_micronutrient_intake = as.numeric(median(total_micronutrient_intake))) %>%
  tidyr::pivot_wider(names_from = 'scen_type', values_from = 'total_micronutrient_intake') %>%
  tidyr::pivot_longer(cols = 4:8, names_to = 'intake_type')
micronutrients_rni$intake_type = factor(micronutrients_rni$intake_type,
                                        levels = c("RNI", "REF", "SPP", "SNR", 'SPPNR'))


pl_ref_vs_rni <- ggplot(data = micronutrients_rni) +
  #                       aes(region, value, color = intake_type, fill = intake_type, shape = intake_type)) +
  # geom_point() +
  geom_point(data = micronutrients_rni %>%
               dplyr::filter(intake_type == 'SPP'),
             aes(x = region, y = value, color = intake_type, fill = intake_type, shape = intake_type), size = 9, alpha = 0.95) +
  geom_point(data = micronutrients_rni %>%
               dplyr::filter(intake_type == 'SNR'),
             aes(x = region, y = value, color = intake_type, fill = intake_type, shape = intake_type), size = 9, alpha = 0.95) +
  geom_point(data = micronutrients_rni %>%
               dplyr::filter(intake_type == 'SPPNR'),
             aes(x = region, y = value, color = intake_type, fill = intake_type, shape = intake_type), size = 9, alpha = 0.95) +
  geom_point(data = micronutrients_rni %>%
               dplyr::filter(intake_type == 'REF'),
             aes(x = region, y = value, color = intake_type, fill = intake_type, shape = intake_type), size = 9, alpha = 0.95) +
  geom_point(data = micronutrients_rni %>%
               dplyr::filter(intake_type == 'RNI'),
             aes(x = region, y = value, color = intake_type, fill = intake_type, shape = intake_type), size = 11, alpha = 0.95, stroke = 2) +
  scale_shape_manual(values = RNI_shape_palette,
                     labels = RNI_palette.labs,
                     name = 'Data source') +
  scale_color_manual(values = RNI_palette,
                     labels = RNI_palette.labs,
                     name = 'Data source') +
  scale_fill_manual(values = RNI_palette,
                    labels = RNI_palette.labs,
                    name = 'Data source') +
  # facet_grid(. ~ nutrient_name) +
  facet_wrap(~ nutrient_item, scales = "free_y", ncol = 4) +
  labs(y = 'Daily intake per capita', x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        strip.text.y = element_text(color = 'black', size = 40, angle = 0, hjust = 0, vjust = 0.25),
        axis.text.x = element_text(size=25, angle = 45, hjust = 1, vjust = 0.95),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40),
        title = element_text(size = 40),
        panel.spacing.x = unit(1, "cm"))
ggsave(pl_ref_vs_rni, file = file.path(figures_path, paste0('sdg3_micronutrients_rni_',year_fig,'.pdf')),
       width = 1500, height = 1500, units = 'mm', limitsize = F)



## -- bars (per difference)
micronutrients_diffPer_regional_basic = merge(micronutrients %>%
                                                dplyr::filter(scenario != 'ref') %>%
                                                dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                dplyr::select(region, year, nutrient_name, scenario, scen_type, scen_path,
                                                              final_share, peak_year, slope, nutrient_units, intake = total_micronutrient_intake),
                                              micronutrients %>%
                                                dplyr::filter(scenario == 'ref') %>%
                                                dplyr::mutate(scen_type = toupper(scen_type)) %>%
                                                dplyr::select(region, year, nutrient_name, ref_intake = total_micronutrient_intake),
                                              by = c('year','region','nutrient_name'))
micronutrients_diffPer_regional <- micronutrients_diffPer_regional_basic %>%
  # compute diff between intake and ref_intake
  dplyr::mutate(diff = 100*(intake - ref_intake)/ref_intake) %>%
  # compute median by scenario type
  dplyr::group_by(region,scen_type,year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(diff),
                   min_value = min(diff),
                   max_value = max(diff)) %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR'))) %>%
  dplyr::ungroup() %>%
  cut_region_names()

micronutrients_diffPer_regional$nutrient_name = factor(micronutrients_diffPer_regional$nutrient_name,
                                                       levels = c("calcium", "iron", "magnesium", "selenium", 'sodium', 'zinc',
                                                                  'folate', 'niacin', 'riboflavin','thiamin', 'vitamin a', 'vitamin b6', 'vitamin b12', 'vitamin c', 'vitamin d', 'vitamin k'))
# divided by regions
for (n in c(1,2)) {
  if (n == 1) {
    micronutrients_diffPer_regional_subset <- micronutrients_diffPer_regional %>%
      dplyr::filter(region %in% unique(region)[1:length(unique(region))/2])
  } else {
    micronutrients_diffPer_regional_subset <- micronutrients_diffPer_regional %>%
      dplyr::filter(region %in% unique(region)[(length(unique(region))/2 + 1) : length(unique(region))])
  }
  pl_micronutrients_diffPer_regional_bars_subset <- ggplot(data = micronutrients_diffPer_regional_subset %>%
                                                             dplyr::filter(scen_type != 'REF') %>%
                                                             dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))) +
    geom_hline(yintercept = 0, color = 'black') +
    # barchart
    geom_bar(aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
             stat = "identity", color = NA, width = 0.7) +
    geom_errorbar(aes(x=as.factor(nutrient_name), y=median_value, ymin = min_value, ymax = max_value,
                      group = interaction(nutrient_name, scen_type)),
                  position = position_dodge(width = 0.25), width = 0.3, color = '#636363') +
    scale_fill_manual(values = micronutrients_scenario_palette, name = 'Minerals & Vitamins', labels = micronutrients_scenario.labs) +
    facet_grid(region ~ scen_type) +
    # labs
    labs(y = 'Intake diff from Reference [%]', x = '') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(color = 'black', size = 40, angle = 0, hjust = 0, vjust = 0.25),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40),
          panel.spacing.x = unit(1, "cm")) +
    guides(fill = guide_legend(nrow = 2), shape = "none")
  if (n == 1) {
    pl_micronutrients_diffPer_regional_bars_subset <- pl_micronutrients_diffPer_regional_bars_subset + theme(legend.position = 'none')
  }
  ggsave(pl_micronutrients_diffPer_regional_bars_subset, file = file.path(figures_path, paste0('sdg3_micronutrients_reg_subset',n,'.pdf')),
         width = 1000, height = 1500, units = 'mm', limitsize = F)
}



## total diff world
micronutrients_diffPer_world = micronutrients_diffPer_regional_basic %>%
  # compute world using pop weights
  dplyr::left_join(population_weights, by = 'region') %>%
  dplyr::mutate(intake = weight * intake,
                ref_intake = weight * ref_intake) %>%
  # diff
  dplyr::rowwise() %>%
  dplyr::mutate(diff = 100*(intake - ref_intake)/ref_intake) %>%
  # compute median by scenario type
  dplyr::group_by(scen_type,year,nutrient_name,nutrient_units) %>%
  dplyr::summarise(median_value = median(diff),
                   min_value = min(diff),
                   max_value = max(diff)) %>%
  # dplyr::filter desired year
  dplyr::filter(year == year_fig) %>%
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))

micronutrients_diffPer_world$nutrient_name = factor(micronutrients_diffPer_world$nutrient_name,
                                                    levels = c("calcium", "iron", "magnesium", "selenium", 'sodium', 'zinc',
                                                               'folate', 'niacin', 'riboflavin','thiamin', 'vitamin a', 'vitamin b6', 'vitamin b12', 'vitamin c', 'vitamin d', 'vitamin k'))

FIG_NUTRITION_micronutrients_diffPer_clean_world <- ggplot(data = micronutrients_diffPer_world %>%
                                                             dplyr::filter(scen_type != 'REF') %>%
                                                             dplyr::mutate(scen_type = factor(scen_type, levels = c('SPP','SNR','SPPNR')))) +
  geom_hline(yintercept = 0, color = 'black') +
  # barchart
  geom_bar(aes(x = as.factor(nutrient_name), y = median_value, fill = as.factor(nutrient_name)),
           stat = "identity", color = NA, width = 0.5) +
  geom_errorbar(aes(x=as.factor(nutrient_name), y=median_value, ymin = min_value, ymax = max_value,
                    group = interaction(nutrient_name, scen_type)),
                position = position_dodge(width = 0.25), width = 0.3, color = '#636363') +
  scale_fill_manual(values = micronutrients_scenario_palette, name = 'Minerals & Vitamins', labels = micronutrients_scenario.labs) +
  facet_grid(. ~ scen_type) +
  # labs
  labs(y = 'Intake diff from Reference [%]', x = '') +
  # theme
  theme_light() +
  theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
        strip.background = element_blank(),
        strip.text = element_text(color = 'black', size = 40),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=30),
        legend.text = element_text(size = 35),
        legend.title = element_text(size = 40, face = "bold"),
        title = element_text(size = 40),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank()) +
  guides(fill = guide_legend(nrow = 2), shape = "none") +
  # ggbreak::scale_y_break(  breaks = c(-100,0,100, 255, 270)) 
  scale_y_continuous(breaks = c(-100, -50, 0, 50, 100, 255, 270)) +
  ggbreak::scale_y_break(c(100, 255), scales = 'fixed', space = 0.5)#0.15)
ggsave(FIG_NUTRITION_micronutrients_diffPer_clean_world, file = file.path(figures_path, 'FIG_NUTRITION_micronutrients_diffPer_clean_world2.pdf'),
       width = 1000, height = 500, units = 'mm', limitsize = F)


########################### MICRONUTRIENTS SI plots ###########################
micronutrients_si <- micronutrients %>%
  dplyr::select(region, year, nutrient_name, scenario, scen_type, scen_path, total_micronutrient_intake) %>%
  dplyr::mutate(scen_path = ifelse(scen_type == 'ref', 'REF', scen_path))
micronutrients_sii <- micronutrients_si %>%
  # dplyr::filter 3 micronutrients
  dplyr::filter(nutrient_name %in% c('vitamin a','vitamin b12','folate')) %>%
  # reshape
  dplyr::rename('type' = 'nutrient_name',
                'value' = 'total_micronutrient_intake') %>%
  # stylize
  dplyr::mutate(scen_type = toupper(scen_type)) %>%
  dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR'))) %>%
  dplyr::mutate(type = factor(type, levels = c('vitamin a','vitamin b12','folate')))

prob_distrib_nutrients(data.table::as.data.table(micronutrients_sii), year_fig, type = 'micronutrient')
cum_fun_nutrients(micronutrients_sii, year_fig, type = 'micronutrient')

#####################################################################################
# FIG - NUTRITION: MICRO, MACRO & ADESA
#####################################################################################
#####################################################################################
# NOT WORKING WITH ggbreak
# pl = cowplot::ggdraw() +
#   cowplot::draw_plot(FIG_NUTRITION_macronutrients_boxplot_world + labs(y = 'Intake diff from\nReference [%]') +
#                        theme(legend.key.size = unit(1, "cm")), x = 0.01, y = 0.75, width = 0.95, height = 0.25) +
#   cowplot::draw_plot(FIG_NUTRITION_micronutrients_diffPer_clean_world +
#                        labs(y = 'Intake diff from\nReference [%]') +
#                        theme(legend.key.size = unit(1, "cm")),
#                      x = 0.01, y = 0.50, width = 0.95, height = 0.25) 
#   cowplot::draw_plot(pl_adesa_lolipop_h_withPath + labs(y = 'ADESA [index]'), x = 0.01, y = -0.01, width = 0.95, height = 0.5) +
#   cowplot::draw_plot_label(label = c("a", "b", "c"), size = 35,
#                            x = c(0, 0, 0), y = c(0.99, 0.77, 0.50))
# ggsave(file=file.path(figures_path, paste0('FIG_NUTRITION_',year_fig,'.pdf')), plot = pl, width = 800, height = 700, unit = 'mm')


fig_a <- FIG_NUTRITION_macronutrients_boxplot_world +
  labs(y = 'Intake diff from\nReference [%]', tag = 'a') +
  theme(
    plot.tag = element_text(size = 40, face = "bold")
  )+
  theme(legend.key.size = unit(1, "cm"))

fig_b <- FIG_NUTRITION_micronutrients_diffPer_clean_world +
  labs(y = 'Intake diff from\nReference [%]', tag = 'b') +
  theme(
    plot.tag = element_text(size = 40, face = "bold")
  )+
  theme(legend.key.size = unit(1, "cm"))

fig_c <- pl_adesa_lolipop_h_withPath +
  labs(y = 'ADESA [index]', tag = 'c') +
  theme(
    plot.tag = element_text(size = 40, face = "bold")
  )

pl_ab <- fig_a / fig_b
ggsave(
  filename = file.path(figures_path, paste0('FIG_NUTRITION_AB', year_fig, '.png')),
  plot = pl_ab,
  width = 800, height = 350, units = 'mm'
)
ggsave(
  filename = file.path(figures_path, paste0('FIG_NUTRITION_C', year_fig, '.png')),
  plot = fig_c,
  width = 800, height = 350, units = 'mm'
)

library(patchwork)

fig_ab_path <- file.path(figures_path, paste0('FIG_NUTRITION_AB', year_fig, '.png'))
fig_ab_magick <- magick::image_read(fig_ab_path)
fig_ab_plot <- cowplot::ggdraw() + cowplot::draw_image(fig_ab_magick)

fig_c_path <- file.path(figures_path, paste0('FIG_NUTRITION_C', year_fig, '.png'))
fig_c_magick <- magick::image_read(fig_c_path)
fig_c_plot <- cowplot::ggdraw() + cowplot::draw_image(fig_c_magick)

pl = cowplot::ggdraw() + 
  cowplot::draw_plot(fig_ab_plot,
                     x = 0.01, y = 0.5, width = 1, height = 0.5) +
  cowplot::draw_plot(fig_c_plot,
                     x = 0.01, y = -0.01, width = 1, height = 0.5)


ggsave(
  filename = file.path(figures_path, paste0('FIG_NUTRITION_', year_fig, '.pdf')),
  plot = pl,
  width = 800, height = 700, units = 'mm'
)
