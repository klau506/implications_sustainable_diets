library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)


sdg0_scen_path_probdistrib <- function() {
  
  population_weights = get(load(file = file.path('inputs/nutrition/population_weigths.RData')))
  
  ##### PLANT
  df_plot <- plant_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    left_join(population_weights, by = 'region') %>%
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, !scen_type %in% c('ref','snr')) %>%
    dplyr::group_by(scen_type, scen_path) %>%
    dplyr::mutate(median_value = median(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::mutate(is_ref = 'Dietary\nchange') %>%
    dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))
  data_ref <- plant_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    left_join(population_weights, by = 'region') %>%
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::select(value)
  data_reff <- bind_rows(data_ref %>%
                           mutate(scen_type = 'SPP'),
                         data_ref %>%
                           mutate(scen_type = 'SPPNR')) %>%
  dplyr::mutate(is_ref = 'REF')

  # plot
  p_plant <- ggplot(data = df_plot) +
    stat_halfeye(data = df_plot %>% filter(scen_path == 'all'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "right", alpha = 0.3) +
    stat_halfeye(data = df_plot %>% filter(scen_path == 'plus'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "left", alpha = 0.3) +
    geom_point(data = df_plot %>% filter(scen_path == 'all'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = df_plot %>% filter(scen_path == 'plus'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = data_reff,
               aes(x = 1, y = value, shape = is_ref), size = 7, alpha = 0.95, stroke = 2) +
    scale_color_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                       labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                       name = 'Pathway') +
    scale_fill_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                      labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                      name = 'Pathway') +
    scale_shape_manual(values = ref_vs_dietaryChange,
                       labels = ref_vs_dietaryChange.labs,
                       name = 'Scenario') +
    coord_flip() +
    facet_grid(. ~ scen_type) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_text(size = 35)) +
    labs(x = '', y = 'Plant protein intake share [%]')





  ############ RUMINANT
  norumin_percentage <- load_data('food_consumption_regional') %>%
    dplyr::filter(nestingSector2 == 'Animal') %>%
    dplyr::mutate(is_rumin = ifelse(nestingSector3 == 'Ruminant',TRUE,FALSE)) %>%
    # compute the total and no-rumin Pcal by region
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_rumin, year, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute the no-rumin % (no-rumin/total ANIMAL protein consumption)
    tidyr::pivot_wider(names_from = 'is_rumin', values_from = 'value') %>%
    dplyr::mutate(`TRUE` = ifelse(is.na(`TRUE`), 0, `TRUE`)) %>%
    dplyr::mutate(`FALSE` = ifelse(is.na(`FALSE`), 0, `FALSE`)) %>%
    dplyr::mutate(value = `FALSE` / (`TRUE` + `FALSE`)) %>%
    dplyr::mutate(Units = 'Percentage') %>%
    select(-`TRUE`) %>%
    select(-`FALSE`)

  df_plot <- norumin_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    left_join(population_weights, by = 'region') %>%
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, !scen_type %in% c('ref','spp')) %>%
    dplyr::group_by(scen_type, scen_path) %>%
    dplyr::mutate(median_value = median(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::mutate(is_ref = 'Dietary\nchange') %>%
    dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))
  data_ref <- norumin_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    left_join(population_weights, by = 'region') %>%
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, year, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::select(value)
  data_reff <- bind_rows(data_ref %>%
                           mutate(scen_type = 'SNR'),
                         data_ref %>%
                           mutate(scen_type = 'SPPNR')) %>%
    dplyr::mutate(is_ref = 'REF')

  # plot
  p_rumin <- ggplot(data = df_plot) +
    stat_halfeye(data = df_plot %>% filter(scen_path == 'all'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "right", alpha = 0.3) +
    stat_halfeye(data = df_plot %>% filter(scen_path == 'plus'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "left", alpha = 0.3) +
    geom_point(data = df_plot %>% filter(scen_path == 'all'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = df_plot %>% filter(scen_path == 'plus'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = data_reff,
               aes(x = 1, y = value, shape = is_ref), size = 7, alpha = 0.95, stroke = 2) +
    scale_color_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                       labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                       name = 'Pathway') +
    scale_fill_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                      labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                      name = 'Pathway') +
    scale_shape_manual(values = ref_vs_dietaryChange,
                      labels = ref_vs_dietaryChange.labs,
                      name = 'Scenario') +
    coord_flip() +
    facet_grid(. ~ scen_type) +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_text(size = 35)) +
    labs(x = '', y = 'Non-Ruminant protein intake share [%]')




  ##### FINAL PLOT

  # create the dataframe for the legend (inside plot)
  df_for_legend <- df_plot %>%
    filter(scen_type == 'SNR')

  p_legend <- ggplot(df_for_legend) +
    stat_halfeye(data = df_for_legend %>% filter(scen_path == 'all'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "right", alpha = 0.3) +
    stat_halfeye(data = df_for_legend %>% filter(scen_path == 'plus'),
                 aes(x = 1, y = value, fill = scen_path,
                     color = scen_path),
                 side = "left", alpha = 0.3) +
    geom_point(data = df_for_legend %>% filter(scen_path == 'all'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = df_for_legend %>% filter(scen_path == 'plus'),
               aes(x = 1, y = median_value, fill = scen_path, shape = is_ref,
                   color = scen_path), alpha = 1, size = 6) +
    geom_point(data = data_reff,
               aes(x = 1, y = value, shape = is_ref), size = 7, alpha = 0.95, stroke = 2) +
    scale_color_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                       labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                       name = 'Pathway') +
    scale_fill_manual(values = scen_path_palette_col_refVsSppVsSnrVsSppnr,
                      labels = scen_path_palette_col_refVsSppVsSnrVsSppnr.labs,
                      name = 'Pathway') +
    scale_shape_manual(values = ref_vs_dietaryChange,
                       labels = ref_vs_dietaryChange.labs,
                       name = 'Scenario') +
    coord_flip() +
    labs(x = '',y = '') +
    annotate(
      "richtext",
      y = c(78, 84, 62, 93, 53),
      x = c(1.9,0.5,0.6,1.5,0.25),
      label = c("Distribution<br>of protein intake shares<br>following the <i>all</i> pathway",
                "Distribution<br>of protein intake shares<br>following the <i>plus</i> pathway",
                "Median following<br>the <i>all</i> pathway",
                "Median following<br>the <i>plus</i> pathway",
                "Reference<br>value"),
      fill = NA, label.size = NA, size = 7, vjust = 1
    ) +
    geom_curve(
      data = data.frame(
        x = c(1.55,0.5,0.6,1.25,0.25),
        xend = c(1.35, 0.75, 0.975, 1.03, 0.975),
        y = c(73.5, 86, 63.5, 91, 51),
        yend = c(71, 88, 62, 89.75, 51)),
      aes(x = x, xend = xend, y = y, yend = yend),
      stat = "unique", curvature = 0.2, linewidth = 1, color = "grey12",
      arrow = arrow(angle = 20, length = unit(2, "mm"))
    ) +
    # theme_void() +
    theme(legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background =  element_rect(fill = "white", color = NA),
          strip.background = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1))


  # Insert the custom legend into the plot
  blank_p <- patchwork::plot_spacer() + theme_void()
  legend_color = ggpubr::get_legend(p_rumin +
                                      theme(legend.direction = 'vertical') +
                                      guides(shape = 'none'))
  legend_shape = ggpubr::get_legend(p_rumin +
                                      theme(legend.direction = 'vertical') +
                                      guides(color = 'none', fill = 'none'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(cowplot::plot_grid(legend_color,blank_p,ncol=1), x = 0.4615, y = -0.2275, width = 1, height = 1) +
    cowplot::draw_plot(cowplot::plot_grid(legend_shape,blank_p,ncol=1), x = 0.4623, y = -0.3125, width = 1, height = 1) +
    cowplot::draw_plot(p_rumin + theme(legend.position = 'none'),
                       x = 0.0, y = 0.0, width = 0.925, height = 0.485) +
    cowplot::draw_plot(p_plant + theme(legend.position = 'none'),
                       x = 0.0, y = 0.5, width = 0.925, height = 0.485) +
    cowplot::draw_plot(p_legend,
                       x = 0.74, y = 0.79, width = 0.25, height = 0.2)

  ggsave(pl, filename = file.path(figures_path, 'sdg0_path_scen_prot_intake.pdf'),
         width = 800, height = 800, units = 'mm', limitsize = F)
}


sdg0_ref_probdistrib <- function() {

  ##### PLANT
  df_plot <- plant_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR'))) %>%
    as.data.frame()
  df_medi <- plant_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    dplyr::group_by(scen_type, year, Units) %>%
    dplyr::summarise(value = median(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::select(value)

  # plot
  p_plant <- ggplot(df_plot) +
    geom_density(data = df_plot,
                 aes(x = value)) +
    geom_vline(data = df_medi, aes(xintercept = value), linewidth = 1, linetype = 'dashed') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_text(size = 35)) +
    labs(y = 'Probability density function', x = 'Plant protein intake share [%]')





  ############ RUMINANT
  norumin_percentage <- load_data('food_consumption_regional') %>%
    dplyr::filter(nestingSector2 == 'Animal') %>%
    dplyr::mutate(is_rumin = ifelse(nestingSector3 == 'Ruminant',TRUE,FALSE)) %>%
    # compute the total and no-rumin Pcal by region
    dplyr::group_by(scenario, scen_type, scen_path, final_share, peak_year, slope, region, is_rumin, year, Units) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute the no-rumin % (no-rumin/total ANIMAL protein consumption)
    tidyr::pivot_wider(names_from = 'is_rumin', values_from = 'value') %>%
    dplyr::mutate(`TRUE` = ifelse(is.na(`TRUE`), 0, `TRUE`)) %>%
    dplyr::mutate(`FALSE` = ifelse(is.na(`FALSE`), 0, `FALSE`)) %>%
    dplyr::mutate(value = `FALSE` / (`TRUE` + `FALSE`)) %>%
    dplyr::mutate(Units = 'Percentage') %>%
    select(-`TRUE`) %>%
    select(-`FALSE`)

  df_plot <- norumin_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::mutate(scen_type = factor(scen_type, levels = c('REF', 'SPP','SNR','SPPNR')))
  df_medi <- norumin_percentage %>%
    dplyr::mutate(value = 100 * value) %>%
    dplyr::group_by(scen_type, year, Units) %>%
    dplyr::summarise(value = median(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == year_fig, scen_type %in% c('ref')) %>%
    dplyr::select(value)

  # plot
  p_rumin <- ggplot(df_plot) +
    geom_density(data = df_plot,
                 aes(x = value)) +
    geom_vline(data = df_medi, aes(xintercept = value), linewidth = 1, linetype = 'dashed') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          plot.background = element_rect(fill = "white", color = NA),
          axis.title = element_text(size = 35)) +
    labs(y = 'Probability density function', x = 'Non-Ruminant protein intake share [%]')




  ##### FINAL PLOT

  # Side by side plots
  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(p_rumin + theme(legend.position = 'none'),
                       x = 0.0, y = 0.0, width = 0.925, height = 0.485) +
    cowplot::draw_plot(p_plant + theme(legend.position = 'none'),
                       x = 0.0, y = 0.5, width = 0.925, height = 0.485)

  ggsave(pl, filename = file.path(figures_path, 'sdg0_ref_prot_intake.pdf'),
         width = 800, height = 800, units = 'mm', limitsize = F)

}
