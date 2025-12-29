#### PREPROCESS ================================================================
# ==============================================================================

# create folder for the figures
fig_folder = 'results'
if (!dir.exists(fig_folder)) dir.create(fig_folder)

# load style
source('R/module_style.R')
library(magick)


# ==============================================================================
# ===================== Scenarios generation + uncert plot =====================
# ==============================================================================

order_facets = function(data, col_scen_type = 'scen_type') {

  data = data %>%
    dplyr::rename(scen_type = col_scen_type) %>%
    dplyr::mutate(scen_type = toupper(scen_type)) %>%
    dplyr::rename_with(~col_scen_type, 'scen_type')
  data[[col_scen_type]] = factor(data[[col_scen_type]], levels = c('SPP','SNR','SPPNR'))

  return(invisible(data))
}


example_data = read.csv(file.path('data/inputs/mappings', 'db.mapping.complete.csv')) %>%
  dplyr::select(scenario) %>%
  tidyr::separate(scenario, into = c("protein_type", "scenario_pathway", "final_share", "peak_year_title", "peak_year", "slope_title", "slope"), sep = "_", remove = FALSE) %>%
  dplyr::filter(protein_type != 'ref') %>%
  dplyr::mutate(scenario_pathway = ifelse(grepl('all', scenario_pathway), 'GlobT', 'RegG')) %>%
  dplyr::mutate(final_share = as.numeric(gsub("[^0-9]", "", final_share))) %>%
  dplyr::select(-ends_with('title')) %>%
  dplyr::mutate(protein_type = toupper(protein_type)) %>%
  dplyr::select(-scenario) %>%
  dplyr::distinct() %>%
  dplyr::mutate(scen_path_pair = paste0(protein_type,'-',scenario_pathway)) %>%
  dplyr::select(protein_type, scenario_pathway, scen_path_pair, final_share, peak_year, slope)

# map char to numbers
data = example_data %>%
  # mapping col1: scenario_protein_type (between 0.25 and 0.75)
  dplyr::mutate(col1_protein_type = match(protein_type, unique(protein_type)) / (length(unique(protein_type))+1)) %>%
  # mapping col2: scenario_scenario_pathway
  dplyr::mutate(col2_scenario_pathway = match(scenario_pathway, unique(scenario_pathway)) / (length(unique(scenario_pathway))+1)) %>%
  # mapping col3: scen_path_pair
  dplyr::mutate(col3_scen_path_pair = match(scen_path_pair, unique(scen_path_pair)) / (length(unique(scen_path_pair))+1)) %>%
  # mapping col4: final_share
  dplyr::mutate(col4_final_share = match(final_share, unique(final_share)) / (length(unique(final_share))+1)) %>%
  # mapping col5: peak_year
  dplyr::mutate(col5_peak_year = match(peak_year, unique(peak_year)) / (length(unique(peak_year))+1)) %>%
  # mapping col6: slope
  dplyr::mutate(col6_slope = match(slope, unique(slope)) / (length(unique(slope))+1))

# reshape data to plot
reshaped_data = data %>%
  dplyr::select(dplyr::starts_with('col'), protein_type) %>%
  dplyr::mutate(across(dplyr::starts_with("col"), list(prev_value = ~.), .names = "prev_{.col}")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("col"), names_to = "variable", values_to = "value") %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("prev_col"), names_to = "prev_variable", values_to = "prev_value") %>%
  dplyr::filter(variable != 'col1_protein_type', prev_variable != 'prev_col6_slope') %>%
  dplyr::mutate(pairs = paste(variable, '-', prev_variable)) %>%
  dplyr::filter(pairs %in% c('col2_scenario_pathway - prev_col1_protein_type',
                             'col3_scen_path_pair - prev_col2_scenario_pathway',
                             'col4_final_share - prev_col3_scen_path_pair',
                             'col5_peak_year - prev_col4_final_share',
                             'col6_slope - prev_col5_peak_year')) %>%
  dplyr::select(-pairs) %>%
  dplyr::mutate(prev_variable = as.double(stringr::str_sub(prev_variable, 9, 9))) %>%
  dplyr::mutate(variable = as.double(stringr::str_sub(variable, 4, 4))) %>%
  # adjust columns
  dplyr::mutate(prev_variable = ifelse(prev_variable %in% c(1,4:5), prev_variable + 0.1, prev_variable),
                variable = ifelse(variable %in% c(4:6), variable - 0.1, variable)) %>%
  dplyr::mutate(prev_variable = ifelse(prev_variable %in% 1:2, prev_variable + 0.05, prev_variable),
                variable = ifelse(variable %in% c(2), variable - 0.05, variable))

# keep the text for the plot
data = data %>%
  dplyr::rename('col1_protein_type_text' = 'protein_type',
                'col2_scenario_pathway_text' = 'scenario_pathway',
                'col3_scen_path_pair_text' = 'scen_path_pair',
                'col4_final_share_text' = 'final_share',
                'col5_peak_year_text' = 'peak_year',
                'col6_slope_text' = 'slope')
unique_letters = unique(substr(names(data[,7:12]), 1, 4))

# Combine columns by letter into pairs
result <- lapply(unique_letters, function(letter) {
  columns_to_combine <- data %>%
    dplyr::select(dplyr::starts_with(letter))
  data %>%
    dplyr::mutate(!!paste0(letter, "_combined") := do.call(paste, c(dplyr::select(., colnames(columns_to_combine)), sep = " - "))) %>%
    dplyr::select(contains("_combined"))
})

# Combine the results into a single dataframe
text_data2 = dplyr::bind_cols(result) %>%
  tidyr::pivot_longer(cols = everything(), names_to = c(".value", "letter"), names_sep = "_") %>%
  dplyr::select(-letter) %>%
  dplyr::mutate(across(1:6, as.character)) %>%
  tidyr::pivot_longer(cols = 1:6, names_to = 'type', values_to = 'pairs') %>%
  dplyr::distinct() %>%
  tidyr::separate(pairs, into = c("text", "value"), sep = " - ") %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  dplyr::rename(prev_variable = type,
                prev_value = value) %>%
  dplyr::mutate(prev_variable = as.numeric(factor(prev_variable)))

# Curvature factor for controlling the degree of curve
curve_factor = 0.075

# Create the plot with curved lines
desired_labels = c('Protein type\nScenario','Scenario\nPathway','Scenario-Pathway pair',
                   'Target share','Peak year','Slope')
pl_gen = ggplot2::ggplot() +
  # rectangles
  ggplot2::annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 0.05, ymax = 0.975, fill = "red", alpha = 0.1) +
  ggplot2::annotate("rect", xmin = 2.5, xmax = 6.5, ymin = 0.025, ymax = 0.95, fill = "blue", alpha = 0.1) +
  # curves and ggplot2::arrows
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SNR', variable != '3') %>%
               dplyr::mutate(prev_variable = ifelse(prev_variable == '3', prev_variable + 0.325, prev_variable)) %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = curve_factor,
             lineend = "round", linewidth = 0.55, alpha = 0.5) +
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SNR', variable == '3') %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable - 0.34, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = curve_factor,
             lineend = "round", linewidth = 0.55, alpha = 0.5,
             arrow = ggplot2::arrow(type = "closed", length = grid::unit(0.2, "inches"))) +
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SPP', variable != '3') %>%
               dplyr::mutate(prev_variable = ifelse(prev_variable == '3', prev_variable + 0.325, prev_variable)) %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = -curve_factor,
             lineend = "round", linewidth = 0.55, alpha = 0.5) +
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SPP', variable == '3') %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable - 0.34, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = -curve_factor,
             lineend = "round", linewidth = 0.55, alpha = 0.5,
             arrow = ggplot2::arrow(type = "closed", length = grid::unit(0.2, "inches"))) +
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SPPNR', variable != '3') %>%
               dplyr::mutate(prev_variable = ifelse(prev_variable == '3', prev_variable + 0.325, prev_variable)) %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = 0,
             lineend = "round", linewidth = 0.55, alpha = 0.5) +
  ggplot2::geom_curve(data = reshaped_data %>% dplyr::filter(protein_type == 'SPPNR', variable == '3') %>% order_facets('protein_type'),
             ggplot2::aes(x = prev_variable, y = prev_value, xend = variable - 0.34, yend = value,
                 group = interaction(variable, protein_type), color = protein_type),
             curvature = 0,
             lineend = "round", linewidth = 0.55, alpha = 0.5,
             arrow = ggplot2::arrow(type = "closed", length = grid::unit(0.2, "inches"))) +
  ggplot2::scale_color_manual(values = scen_palette_refVsSppVsSnrVsSppnr) +
  # circles/ellipses
  ggforce::geom_ellipse(data = text_data2 %>% dplyr::filter(prev_variable %in% c('1','2')),
                        ggplot2::aes(x0 = prev_variable, y0 = prev_value, a = 0.2, b = 0.035, angle = 0),
                        fill = "white", color = "white", alpha = 1) +
  ggforce::geom_ellipse(data = text_data2 %>% dplyr::filter(prev_variable %in% c('1','2')),
                        ggplot2::aes(x0 = prev_variable, y0 = prev_value, a = 0.2, b = 0.035, angle = 0),
                        fill = "#F2F3CB", color = "grey", alpha = 0.25) +
  ggplot2::geom_point(data = text_data2 %>% dplyr::filter(prev_variable %in% c('4','5','6')),
             ggplot2::aes(x = prev_variable, y = prev_value), shape = 21, size = 33,
             fill = "white", color = "white", alpha = 1) +
  ggplot2::geom_point(data = text_data2 %>% dplyr::filter(prev_variable %in% c('4','5','6')),
             ggplot2::aes(x = prev_variable, y = prev_value), shape = 21, size = 33,
             fill = "#F2F3CB", color = "black", alpha = 0.25) +
  ggforce::geom_ellipse(data = text_data2 %>% dplyr::filter(prev_variable == '3'),
                        ggplot2::aes(x0 = prev_variable, y0 = prev_value, a = 0.33, b = 0.05, angle = 0),
                        fill = "white", color = "white", alpha = 1) +
  ggforce::geom_ellipse(data = text_data2 %>% dplyr::filter(prev_variable == '3'),
                        ggplot2::aes(x0 = prev_variable, y0 = prev_value, a = 0.33, b = 0.05, angle = 0),
                        fill = "#F2F3CB", color = "grey", alpha = 0.25) +
  # text - bullets
  ggplot2::geom_text(data = text_data2,
            ggplot2::aes(label = text, x = prev_variable, y = prev_value), color = 'black', size = 10) +
  # text - rectangles
  ggplot2::annotate('text', label = 'Scenario generation', x = 0.35, y = 0.5, angle = 90, vjust = 1, color = 'red', size = 12.5, alpha = 0.5) +
  ggplot2::annotate('text', label = 'Uncertainty dimensions', x = 6.65, y = 0.5, angle = -90, vjust = 1, color = 'blue', size = 12.5, alpha = 0.5) +
  # theme
  ggplot2::xlab("") + ggplot2::ylab("") +
  ggplot2::scale_x_continuous(breaks = 1:6, labels = desired_labels) +
  ggplot2::theme_light() +
  ggplot2::theme(legend.key.size = grid::unit(2, "cm"), legend.position = 'none', legend.direction = 'horizontal',
        strip.background = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(color = 'black', size = 40),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size=30),
        legend.text = ggplot2::element_text(size = 35),
        legend.title = ggplot2::element_text(size = 40),
        title = ggplot2::element_text(size = 40)) +
  ggplot2::scale_y_reverse()
ggplot2::ggsave(pl_gen, file = file.path(fig_folder, 'fig1_SI_overview_scen_generation_uncert_plot.pdf'), width = 750, height = 500, units = 'mm')


## combination with scenario details table
pl_table <- ggplot2::ggplot() +
  ggplot2::xlim(0, 10) + ggplot2::ylim(0, 8) +
  ggplot2::annotation_custom(grid::rasterGrob(png::readPNG(file.path('data/inputs/figures', 'table_scenarios.png')), interpolate = TRUE),
                    xmin = 0, xmax = 10, ymin = 0, ymax = 8) +
  ggplot2::theme_void()
ggplot2::ggsave(pl_table, file = file.path(fig_folder, 'fig1_SI_overview_scen_generation_uncert_table.pdf'), width = 750, height = 500, units = 'mm')

colored_img <- image_colorize(image_read(file.path('data/inputs/figures', 'arrow.png')), "#FFE5E5", opacity = 100)
temp_file <- tempfile(fileext = ".png")
image_write(colored_img, path = temp_file, format = "png")
pl_arrow <- ggplot2::ggplot() +
  ggplot2::xlim(0, 5) + ggplot2::ylim(0, 5) +
  ggplot2::annotation_custom(grid::rasterGrob(png::readPNG(temp_file), interpolate = TRUE),
                    xmin = 0, xmax = 5, ymin = 0, ymax = 5) +
  ggplot2::theme_void()

pl_complete = cowplot::ggdraw() +
  ggplot2::theme(plot.background = ggplot2::element_rect(fill="white")) +
  cowplot::draw_plot(pl_gen, x = 0, y = -0.005, width = 1, height = 0.65) +
  cowplot::draw_plot(pl_table, x = 0.1, y = 0.425, width = 0.8, height = 0.75) +
  cowplot::draw_plot(pl_arrow, x = -0.075, y = 0.48, width = 0.25, height = 0.25) +
  # Add a black frame around the second plot
  ggplot2::annotation_custom(
    grid::rectGrob(gp = grid::gpar(col = "#FFE5E5", fill = NA, lwd = 20)),
    xmin = 0.11, xmax = 0.9, ymin = 0.635, ymax = 0.975
  ) +
  # remove frame
  ggplot2::theme_void()


ggplot2::ggsave(pl_complete, file = file.path(fig_folder, 'fig1_SI_overview_scen_generation_uncert_complete.pdf'),
       width = 700, height = 850, units = 'mm')

