require(plyr)
require(vioplot)
require(ggpattern)
require(waterfall)


prob_distrib_nutrients = function(df, y, type) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','type','region',
                     'scen_type','scen_path')]

  df = data.table(df)

  if (type == 'micronutrient') {
    xlab = 'Micronutrient consumption [msg/capita/day]'
  } else {
    xlab = paste0('M',substr(type, 2, nchar(type)),' consumption as a percentage of energy intake [capita/day]')
  }

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    df_reg <- df %>%
      dplyr::filter(region %in% reg)
    df_medi_reg <- df_medi %>%
      dplyr::filter(region %in% reg)

    pl <- ggplot(df_reg) +
      geom_density(data = df_reg %>% filter(scen_type != 'REF'),
                   aes(x = value,color = scen_type,
                       fill = scen_type, linetype = scen_path),
                   linewidth = 0.8, alpha = 0.25) +
      geom_vline(aes(color = scen_type, fill = scen_type, linetype = scen_path, xintercept = medi),
                 data = df_medi_reg, linewidth = 1.5) +
      facet_grid(region ~ type, scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr3,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr3.labs) +
      labs(y = 'Probability density', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_probdistrib_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_probdistrib_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}



cum_fun_nutrients = function(df, y, type) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  dat_tmp <- ddply(df, .(year,type,region,scen_type,scen_path),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  if (type == 'micronutrient') {
    xlab = 'Micronutrient consumption [mcg/capita/day]'
  } else {
    xlab = paste0('M',substr(type, 2, nchar(type)),' consumption as a percentage of energy intake [capita/day]')
  }

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg)

    pl <- ggplot(dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_grid(region ~ type, scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}

#' violin_plot_landtype
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_landtype <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in thous. ', km^2, ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }
  pl_SI_land_area_violin <- ggplot(data = data %>%
                                     dplyr::filter(year == y) %>%
                                     dplyr::select(scen_type, scen_path, land_use_type, diff) %>%
                                     dplyr::distinct() %>%
                                     rename_pathways(),
                                   aes(x = land_use_type,
                                       y = diff,
                                       color = land_use_type,
                                       fill = land_use_type)) +
    geom_violin(trim = FALSE, alpha = 0.5, width = 1.2) +
    geom_jitter(width = 0.2, size = 0.5, alpha = 0.7) +
    geom_boxplot(width=0.2, color="black", alpha=0.5) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(scen_type ~ scen_path) +
    scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                      breaks = land_use_order_palette) +
    scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                       breaks = land_use_order_palette) +
    # labs
    labs(y = ylab, x = '') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_text(size=30, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_SI_land_area_violin, file = file.path(figures_path,paste0('sdg15_SI_land_area_violin_world_',type,'.pdf')),
         width = 500, height = 500, units = 'mm')

}

#' violin_plot_landtype
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_landtype_regional <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in thous. ', km^2, ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }

  data <- data %>%
    dplyr::filter(year == y) %>%
    dplyr::select(region, scen_type, scen_path, land_use_type, diff) %>%
    dplyr::distinct() %>%
    cut_region_names()

  for (i in c(1,2)) {
    reg = unique(data$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    data_reg <- data %>%
      dplyr::filter(region %in% reg)

    pl_SI_land_area_violin <- ggplot(data = data_reg,
                                     aes(x = land_use_type,
                                         y = diff,
                                         color = land_use_type,
                                         fill = land_use_type)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_jitter(width = 0.2, size = 0.5, alpha = 0.7) +
      geom_boxplot(width=0.2, color="black", alpha=0.5) +
      geom_hline(aes(yintercept = 0)) +
      facet_grid(region ~ scen_type + scen_path,scale = 'free') +
      scale_fill_manual(values = land_use_scenario_palette, name = 'Land Type',
                        breaks = land_use_order_palette) +
      scale_color_manual(values = land_use_scenario_palette, name = 'Land Type',
                         breaks = land_use_order_palette) +
      # labs
      labs(y = ylab, x = '') +
      # theme
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 40),
            strip.text.y = element_text(angle = 0),
            axis.text.x = element_blank(), #element_text(size=30, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size=30),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 40),
            title = element_text(size = 40))
    ggsave(pl_SI_land_area_violin, file = file.path(figures_path,paste0('sdg15_SI_land_area_violin_regional_',type,'_',i,'.pdf')),
           width = 1000, height = 1500, units = 'mm', limitsize = FALSE)
  }

}


cum_fun_water = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  dat_tmp <- ddply(df, .(year,region,scen_type,scen_path),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  xlab = 'Water consumption [thous. km2]'

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg) %>%
      dplyr::mutate(scen_path = 'REF')

    pl <- ggplot(dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_grid(region ~ ., scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg6_SI_water_cumfun_reg.pdf')),
         width = 1500, height = 1500, units = 'mm', limitsize = F)

}

rfd_index_water = function(index, y) {

  index = index %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::select(region, year, scenario, scen_type, scen_path, rfd_per) %>%
    dplyr::distinct() %>%
    cut_region_names()
  index_ref = index %>%
    dplyr::filter(scen_type == 'REF') %>%
    dplyr::mutate(scen_path = 'REF')
  index = index %>%
    dplyr::filter(scen_type != 'REF')

  ylab = 'Rainfed water share [%]'

  for (i in c(1,2)) {
    reg = unique(index$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    index_reg <- index %>%
      dplyr::filter(region %in% reg)
    index_ref_reg <- index_ref %>%
      dplyr::filter(region %in% reg)

    pl <- ggplot() +
      geom_boxplot_pattern(data = index_reg,
                           aes(x = scen_type, y = rfd_per, fill = scen_type, linetype = scen_path, pattern = scen_path),
                           width=1, color = 'black') +
      geom_segment(data = index_ref_reg,
                aes(x = 0.5, xend = 3.5, y = rfd_per, yend = rfd_per,
                    linetype = scen_path), color = 'red', linewidth = 3) +
      facet_grid(region ~ ., scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr3,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr3.labs) +
      scale_pattern_manual(values = scen_path_pattern_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway2',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = ylab, x = '') +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
            axis.title = element_text(size=50),
            axis.text = element_text(size=40),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)),
             pattern = guide_legend(override.aes = list(linewidth = 5)))

    # ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_',type,'_cumfun_reg_',i,'.pdf')),
    #        width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend_color = ggpubr::get_legend(ggplot() +
                                geom_col(data = index_reg,
                                                 aes(x = scen_type, y = rfd_per, color = scen_type, fill = scen_type,
                                                     linetype = scen_path),
                                                 width=1) +
                                geom_segment(data = index_ref_reg,
                                             aes(x = 0.25, xend = 3.75, y = rfd_per, yend = rfd_per,
                                                 linetype = scen_path), linewidth = 2) +
                                scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                                                  name = 'Scenario',
                                                  labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
                                scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                                                   name = 'Scenario',
                                                   labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
                                theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom',
                                      legend.direction = 'horizontal',
                                      strip.background = element_blank(),
                                      strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
                                      legend.text = element_text(size = 45),
                                      legend.title = element_text(size = 50)) +
                                guides(color = guide_legend(override.aes = list(linewidth = 5)),
                                       fill = guide_legend(override.aes = list(linewidth = 5)),
                                       linetype = 'none'))

  legend_pattern <- ggpubr::get_legend(ggplot() +
                                         geom_col_pattern(data = index_reg %>% filter(region == unique(index_reg$region)[1]),
                                                          aes(x = scen_type, y = rfd_per, color = scen_type, fill = scen_type,
                                                              pattern = scen_path),
                                                          width=1,fill= 'white',colour= 'black',pattern_aspect_ratio = 1,
                                                          pattern_density= 0.3) +
                                         scale_pattern_manual(values = scen_path_pattern_refVsSppVsSnrVsSppnr2,
                                                              name = '',
                                                              labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
                                         theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom',
                                               legend.direction = 'horizontal',
                                               strip.background = element_blank(),
                                               strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
                                               legend.text = element_text(size = 45),
                                               legend.title = element_text(size = 50)) +
                                         guides(color = 'none',
                                                fill = 'none'))

  legend_linetype = ggpubr::get_legend(ggplot() +
                                geom_segment(data = index_ref_reg,
                                             aes(x = 0.25, xend = 3.75, y = rfd_per, yend = rfd_per,
                                                 linetype = scen_path), linewidth = 2) +
                                scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr3,
                                                      name = 'Pathway',
                                                      labels = scen_path_palette_refVsSppVsSnrVsSppnr3.labs) +
                                scale_pattern_manual(values = scen_path_pattern_refVsSppVsSnrVsSppnr2,
                                                     name = '',
                                                     labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
                                theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom',
                                      legend.direction = 'horizontal',
                                      strip.background = element_blank(),
                                      strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
                                      axis.title = element_text(size=50),
                                      axis.text = element_text(size=45),
                                      legend.text = element_text(size = 45),
                                      legend.title = element_text(size = 50)) +
                                guides(color = 'none',
                                       fill = 'none',
                                       linetype = guide_legend(keywidth = 10, override.aes = list(linewidth = 5))))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend_color,blank_p,nrow=1), x = 0.125, y = -0.485, width = 1, height = 1) +
    cowplot::draw_plot(cowplot::plot_grid(legend_linetype,blank_p,nrow=1), x = 0.275, y = -0.485, width = 1, height = 1) +
    cowplot::draw_plot(cowplot::plot_grid(legend_pattern,blank_p,nrow=1), x = 0.3725, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg6_SI_water_rfd_share_reg.pdf')),
         width = 1500, height = 1500, units = 'mm', limitsize = F)

}



#' violin_plot_ghg
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_ghg <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in Mt ', CO[2], ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }
  pl_SI_ghg_violin <- ggplot(data = data %>%
                                     dplyr::filter(year == y) %>%
                                     dplyr::select(scen_type, scen_path, ghg, diff) %>%
                                     dplyr::distinct(),
                                   aes(x = ghg,
                                       y = diff,
                                       color = ghg,
                                       fill = ghg)) +
    geom_violin(trim = FALSE, alpha = 0.75, width = 1.2) +
    geom_jitter(width = 0.2, size = 0.5, alpha = 0.9) +
    geom_boxplot(width=0.2, color="black", alpha=0.75) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid(scen_type ~ scen_path,scale = 'free') +
    scale_fill_manual(values = ghg_emiss, name = 'GHG Type',
                      breaks = ghg_emiss) +
    scale_color_manual(values = ghg_emiss, name = 'GHG Type',
                       breaks = ghg_emiss) +
    # labs
    labs(y = ylab, x = '') +
    # theme
    theme_light() +
    theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
          strip.background = element_blank(),
          strip.text = element_text(color = 'black', size = 40),
          strip.text.y = element_text(angle = 0),
          axis.text.x = element_blank(), #element_text(size=30, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size=30),
          legend.text = element_text(size = 35),
          legend.title = element_text(size = 40),
          title = element_text(size = 40))
  ggsave(pl_SI_ghg_violin, file = file.path(figures_path,paste0('sdg13_SI_ghg_violin_world_',type,'.pdf')),
         width = 500, height = 500, units = 'mm')

}



#' violin_plot_ghg_regional
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
violin_plot_ghg_regional <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in Mt ', CO[2], ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }

  data <- data %>%
    dplyr::filter(year == y) %>%
    dplyr::select(region, scen_type, scen_path, ghg, diff) %>%
    dplyr::distinct() %>%
    cut_region_names()

  for (i in c(1,2)) {
    reg = unique(data$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    data_reg <- data %>%
      dplyr::filter(region %in% reg)

    pl_SI_ghg_violin <- ggplot(data = data_reg,
                                     aes(x = ghg,
                                         y = diff,
                                         color = ghg,
                                         fill = ghg)) +
      geom_violin(trim = FALSE, alpha = 0.75) +
      geom_jitter(width = 0.2, size = 0.5, alpha = 0.9) +
      geom_boxplot(width=0.2, color="black", alpha=0.75) +
      geom_hline(aes(yintercept = 0)) +
      facet_grid(region ~ scen_type + scen_path,scale = 'free') +
      # facet_wrap(region ~ scen_type + scen_path, scales = 'free', ncol = 6) +
      scale_fill_manual(values = ghg_emiss, name = 'Land Type') +
      scale_color_manual(values = ghg_emiss, name = 'Land Type') +
      # labs
      labs(y = ylab, x = '') +
      # theme
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 40),
            strip.text.y = element_text(angle = 0),
            axis.text.x = element_blank(), #element_text(size=30, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size=30),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 40),
            title = element_text(size = 40))
    ggsave(pl_SI_ghg_violin, file = file.path(figures_path,paste0('sdg13_SI_ghg_violin_regional_',type,'_',i,'.pdf')),
           width = 1000, height = 1500, units = 'mm', limitsize = FALSE)
  }

}


#' waterfall_plot_ghg_regional
#' @param data dataset
#' @param y figure year
#' @param type abs or per
#'
waterfall_plot_ghg_regional <- function(data, y, type) {
  if (type == 'abs') {
    ylab = expression(paste('Change in Mt ', CO[2], ' compared to Reference'))
  } else {
    ylab = 'Percentual change compared to Reference [%]'
  }

  data2 <- data %>% filter(year == y) %>%
    dplyr::mutate(ghg_num = ifelse(ghg == 'CH4', 1,
                                   ifelse(ghg == 'CO2', 2,
                                          ifelse(ghg == 'F-Gas', 3,
                                                 ifelse(ghg == 'LUC CO2', 4, 5))))) %>%
    dplyr::group_by(region, scen_type, scen_path, ghg, ghg_num, year) %>%
    dplyr::summarise(diff_median = median(diff),
                     diff_min = min(diff),
                     diff_max = max(diff)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region_scen_type_path = paste(region, scen_type, scen_path, sep = '-')) %>%
    cut_region_names()

  # compute the waterfall
  data3 <- data.frame()
  for (it in unique(data2$region_scen_type_path)) {
    tmp <- data2 %>%
      dplyr::filter(region_scen_type_path == it) %>%
      dplyr::group_by(region, scen_type, scen_path, year) %>%
      dplyr::mutate(
        end = cumsum(diff_median),
        start = lag(end, default = 0)
      ) %>%
      dplyr::ungroup()

    if (nrow(data3) == 0) {
      data3 <- tmp
    } else {
      data3 <- dplyr::bind_rows(data3, tmp)
    }
  }

  # compute the error intervals
  data4 <- data3 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(error_bar_diff_min = diff_median - diff_min) %>%
    dplyr::mutate(error_bar_diff_max = diff_median - diff_max) %>%
    dplyr::mutate(error_bar_min = end - error_bar_diff_min) %>%
    dplyr::mutate(error_bar_max = end - error_bar_diff_max)


  for (i in c(1,2)) {
    reg = unique(data4$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    data_reg <- data4 %>%
      dplyr::filter(region %in% reg)

    pl_SI_ghg_waterfall <- ggplot(data_reg, aes(x = ghg, fill = ghg, color = ghg)) +
      geom_hline(aes(yintercept = 0), linetype = 'dashed') +
      geom_rect(aes(xmin = ghg_num - 0.4, xmax = ghg_num + 0.4, ymin = start, ymax = end, fill = ghg, color = ghg)) +
      # geom_errorbar(aes(x = ghg, y = end, ymin = error_bar_min, ymax = error_bar_max,
      #                   group = interaction(ghg, scen_type, scen_path, region)),
      #               position = position_dodge(width = 0.25), width = 0.3, color = '#636363') +
      geom_segment(data = data_reg %>%
                     filter(ghg != unique(data_reg$ghg)[5]),
                   aes(x = ghg_num + 0.4, xend = ghg_num + 1 - 0.4, y = end, yend = end),
                   linewidth = 0.2, color = '#636363', linetype = "dotted") +
      labs(y = ylab, x = "GHG") +
      facet_grid(region ~ scen_type + scen_path, scales = 'free') +
      scale_fill_manual(values = ghg_emiss, name = 'Land Type',
                        breaks = ghg_emiss) +
      scale_color_manual(values = ghg_emiss, name = 'Land Type',
                         breaks = ghg_emiss) +
      # theme
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 40),
            strip.text.y = element_text(angle = 0),
            axis.text.x = element_text(size=30, angle = 45, hjust = 1, vjust = 1),
            axis.text.y = element_text(size=30),
            legend.text = element_text(size = 35),
            legend.title = element_text(size = 40),
            title = element_text(size = 40))
    ggsave(pl_SI_ghg_waterfall, file = file.path(figures_path,paste0('sdg13_SI_ghg_waterfall_regional_',type,'_',i,'.pdf')),
           width = 1000, height = 1500, units = 'mm', limitsize = FALSE)
  }

}


#' cum_fun_health
#' @param df data
#' @param y figure year
#'
cum_fun_health = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  dat_tmp <- ddply(df, .(year,region,scen_type,scen_path,pollutant),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  xlab = 'Avoided premature deaths [%]'

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg) %>%
      dplyr::mutate(scen_type = 'REF') %>%
      dplyr::select(pollutant, region, scen_type, scen_path, ref_value) %>%
      dplyr::distinct()

    pl <- ggplot(data = dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = ref_value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_wrap(region ~ pollutant, scales = 'free', ncol = 4) +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text.x = element_text(color = 'black', size = 45),
            strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            axis.text.x = element_text(size=45, angle = 45, vjust = 1, hjust = 1),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_cumfun_reg_',i,'.pdf')),
           width = 2000, height = 2000, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg3_SI_health_cumfun_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}


#' cum_fun_foodbasket
#' @param df data
#' @param y figure year
#'
cum_fun_foodbasket = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names() %>%
    dplyr::rename(value = expenditure,
                  ref_value = ref_expenditure)

  dat_tmp <- ddply(df, .(year,region,scen_type,scen_path),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  xlab = 'Food basket bill [2005$/capita/day]'

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg) %>%
      dplyr::mutate(scen_type = 'REF') %>%
      dplyr::select(region, scen_type, scen_path, ref_value) %>%
      dplyr::distinct()

    pl <- ggplot(data = dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = ref_value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_wrap(region ~ ., scales = 'free', ncol = 4) +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text.x = element_text(color = 'black', size = 45),
            strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            axis.text.x = element_text(size=45, angle = 45, vjust = 1, hjust = 1),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    ggsave(pl, filename = file.path(figures_path, paste0('sdg2_SI_cumfun_reg_',i,'.pdf')),
           width = 2000, height = 2000, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg2_SI_food_basket_cumfun_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}


#' cum_fun_policyCost
#' @param df data
#' @param y figure year
#'
cum_fun_policyCost = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names()

  dat_tmp <- ddply(df, .(year,region,scen_type,scen_path),
                   summarize,
                   value = unique(value),
                   ecdf = ecdf(value)(unique(value)))

  xlab = 'Policy Cost [million 1990$]'

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    dat_tmp_reg <- dat_tmp %>%
      dplyr::filter(region %in% reg)
    df_reg <- df %>%
      dplyr::filter(region %in% reg) %>%
      dplyr::mutate(scen_type = 'REF') %>%
      dplyr::select(region, scen_type, scen_path, ref_value) %>%
      dplyr::distinct()

    pl <- ggplot(data = dat_tmp_reg %>%
                   filter(scen_type != 'REF'),
                 aes(value, ecdf, color = scen_type, fill = scen_type, linetype = scen_path)) +
      geom_line(linewidth = 2) +
      geom_point(data = df_reg %>%
                   filter(scen_type == 'REF'),
                 aes(x = ref_value, y = 1, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_wrap(region ~ ., scales = 'free', ncol = 4) +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Cumulative frequency', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text.x = element_text(color = 'black', size = 45),
            strip.text.y = element_text(color = 'black', size = 45, angle = 0, hjust = 0),
            axis.title = element_text(size=50),
            axis.text = element_text(size=45),
            axis.text.x = element_text(size=45, angle = 45, vjust = 1, hjust = 1),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    ggsave(pl, filename = file.path(figures_path, paste0('sdg13_SI_cumfun_reg_',i,'.pdf')),
           width = 2000, height = 2000, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg13_SI_policyCost_cumfun_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}


#' prob_distrib_policyCost
#' @param df data
#' @param y figure year
#'
prob_distrib_policyCost = function(df, y) {

  df = df %>%
    dplyr::filter(year == y) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    cut_region_names() %>%
    as.data.table()

  df_medi <- df[, .(medi = quantile(value, 0.5)),
                by=c('year','region',
                     'scen_type','scen_path')]

  df = data.table(df)
  xlab = 'Policy Cost [million 1990$]'

  for (i in c(1,2)) {
    reg = unique(df$region)
    if (i == 1) {
      reg = reg[1:(length(reg)/2)]
    } else {
      reg = reg[(length(reg)/2+1):length(reg)]
    }

    df_reg <- df %>%
      dplyr::filter(region %in% reg)
    df_medi_reg <- df_medi %>%
      dplyr::filter(region %in% reg)
    df_ref_reg <- df %>%
      dplyr::filter(region %in% reg) %>%
      dplyr::select(year, region, ref_value, scen_type) %>%
      dplyr::mutate(scen_type = 'REF') %>%
      dplyr::distinct()


    pl <- ggplot(df_reg) +
      geom_density(data = df_reg %>% filter(scen_type != 'REF'),
                   aes(x = value,color = scen_type,
                       fill = scen_type, linetype = scen_path),
                   linewidth = 0.8, alpha = 0.25) +
      geom_vline(aes(color = scen_type, fill = scen_type, linetype = scen_path, xintercept = medi),
                 data = df_medi_reg, linewidth = 1.5) +
      geom_point(data = df_ref_reg,
                 aes(x = ref_value, y = 0, color = scen_type, fill = scen_type),
                 size = 7, alpha = 0.95, shape = 23, stroke = 2) +
      facet_wrap(region ~ ., scales = 'free', nrow = 4) +
      # facet_grid(region ~ ., scales = 'free') +
      scale_fill_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                        name = 'Scenario',
                        labels = scen_palette_refVsSppVsSnrVsSppnr.labs)+
      scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr,
                         name = 'Scenario',
                         labels = scen_palette_refVsSppVsSnrVsSppnr.labs) +
      scale_linetype_manual(values = scen_path_palette_refVsSppVsSnrVsSppnr2,
                            name = 'Pathway',
                            labels = scen_path_palette_refVsSppVsSnrVsSppnr2.labs) +
      labs(y = 'Probability density', x = xlab) +
      theme_light() +
      theme(legend.key.size = unit(2, "cm"), legend.position = 'bottom', legend.direction = 'horizontal',
            strip.background = element_blank(),
            strip.text = element_text(color = 'black', size = 45),
            axis.title = element_text(size=50),
            axis.text.y = element_text(size=45),
            axis.text.x = element_text(size=45, angle = 45, hjust = 1, vjust = 1),
            legend.text = element_text(size = 45),
            legend.title = element_text(size = 50)) +
      guides(color = guide_legend(override.aes = list(linewidth = 5)),
             fill = guide_legend(override.aes = list(linewidth = 5)),
             linetype = guide_legend(keywidth = 10,override.aes = list(linewidth = 5)))

    ggsave(pl, filename = file.path(figures_path, paste0('sdg13_SI_probdistrib_reg_',i,'.pdf')),
           width = 1000, height = 3500, units = 'mm', limitsize = F)

    if (i == 1) { pl1 <- pl } else { pl2 <- pl }

  }

  blank_p <- patchwork::plot_spacer() + theme_void()
  legend = ggpubr::get_legend(pl1 +
                                theme(legend.direction = 'horizontal'))

  pl <- cowplot::ggdraw() +
    cowplot::draw_plot(pl1 + theme(legend.position = 'none'),
                       x = 0.0, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(pl2 + theme(legend.position = 'none',
                                   axis.title.y = element_blank()),
                       x = 0.5, y = 0.025, width = 0.5, height = 0.975) +
    cowplot::draw_plot(cowplot::plot_grid(legend,blank_p,nrow=1), x = 0.225, y = -0.485, width = 1, height = 1)

  ggsave(pl, filename = file.path(figures_path, paste0('sdg13_SI_probdistrib_reg.pdf')),
         width = 2000, height = 2000, units = 'mm', limitsize = F)

}
