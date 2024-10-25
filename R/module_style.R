
################################################################################
################################################################################
# PALETTES & LABELS

scen_palette_refVsSppVsSnrVsSppnr = c(
  'SPP' = '#BD08A2',
  'SNR' = '#F6AA1C',
  'SPPNR' = '#0C9425',
  'REF' = '#000000'
)
scen_palette_refVsSppVsSnrVsSppnr.labs <- c('SPP', "SNR", "SPPNR", 'REF')
names(scen_palette_refVsSppVsSnrVsSppnr.labs) = c("SPP", "SNR", "SPPNR", 'REF')

scen_path_palette_refVsSppVsSnrVsSppnr3 = c(
  'all' = 'solid',
  'plus' = 'dashed',
  'REF' = 'dotted'
)
scen_path_palette_refVsSppVsSnrVsSppnr3.labs <- c('GlobT','RegG','REF')
names(scen_path_palette_refVsSppVsSnrVsSppnr3.labs) = c('all','plus','REF')

scen_path_palette_refVsSppVsSnrVsSppnr2 = c(
  'all' = 'solid',
  'plus' = 'dashed'
)
scen_path_pattern_refVsSppVsSnrVsSppnr2 = c(
  'all' = 'none',
  'plus' = 'stripe'
)
scen_path_palette_refVsSppVsSnrVsSppnr2.labs <- c('GlobT','RegG')
names(scen_path_palette_refVsSppVsSnrVsSppnr2.labs) = c('all','plus')

scen_path_shape_refVsSppVsSnrVsSppnr2 = c(
  'all' = 21,
  'plus' = 23
)
scen_path_shape_refVsSppVsSnrVsSppnr2.labs <- c('GlobT','RegG')
names(scen_path_shape_refVsSppVsSnrVsSppnr2.labs) = c('all','plus')

scen_path_palette_col_refVsSppVsSnrVsSppnr <-
  c('all' = '#6b0ead',
    'plus' = '#66A61E')
scen_path_palette_col_refVsSppVsSnrVsSppnr.labs <- c('GlobT','RegG')

ref_vs_dietaryChange <- c('REF' = 18,
                          'Dietary\nchange' = 19)
ref_vs_dietaryChange.labs <- c('Dietary\nchange','REF')

RNI_palette = c(
  'RNI' = '#c40205',
  'SPP' = '#BD08A2',
  'SNR' = '#F6AA1C',
  'SPPNR' = '#0C9425',
  'REF' = '#000000'
)
RNI_shape_palette = c(
  'RNI' = 4,
  'SPP' = 21,
  'SNR' = 21,
  'SPPNR' = 21,
  'REF' = 17
)
RNI_palette.labs <- c('RNI', 'SPP', "SNR", "SPPNR", 'REF')
names(RNI_palette.labs) = c('RNI', 'SPP', "SNR", "SPPNR", 'REF')

land_use_scenario_palette =
  c('Cropland' = 'chocolate4',
    'Pasture' = '#E6AB02',
    'Forest' = 'darkgreen',
    'Shrub & Grass' = '#66A61E',
    'Other Natural' = '#b1e378')
land_use_order_palette =
  c('Cropland',
    'Pasture',
    'Forest',
    'Shrub & Grass',
    'Other Natural')


deaths_scenario_palette =
  c('PM25' = '#6b0ead',
    'O3' = '#E6AB02')
deaths_order_palette =
  c('PM25',
    'O3')

macronutrients_scenario_palette = c(
  'gProteinPerCapita.SNR' = '#500f96',
  'gFatPerCapita.SNR' = '#1558bd',
  'gProteinPerCapita.SPP' = '#2ec74f',
  'gFatPerCapita.SPP' = '#9ac217',
  'gProteinPerCapita.REF' = '#a61230',
  'gFatPerCapita.REF' = '#e85fd1'
)
macronutrients_scenario.labs <- c('SNR - Protein',"SNR - Fat",
                                  'SPP - Protein','SPP - Fat',
                                  'REF - Protein','REF - Fat')
names(macronutrients_scenario.labs) = c("gProteinPerCapita.SNR", "gFatPerCapita.SNR",
                                        "gProteinPerCapita.SPP", "gFatPerCapita.SPP",
                                        'gProteinPerCapita.REF', 'gFatPerCapita.REF')

macronutrients_palette = c(
  'gProteinPerCapita' = '#500f96',
  'gFatPerCapita' = '#a61230',
  'perProteinPerCapita' = '#500f96',
  'perFatPerCapita' = '#a61230'
)
macronutrients.labs <- c('Protein',"Fat",'Protein',"Fat")
names(macronutrients.labs) = c("gProteinPerCapita", "gFatPerCapita", "perProteinPerCapita", "perFatPerCapita")

micronutrients_scenario_palette = c(
  'calcium' = '#a63603',
  'iron' = '#e6550d',
  'magnesium' = '#fd8d3c',
  'selenium' = '#fdae6b',
  'sodium' = '#fdd0a2',
  'zinc' = '#feedde',
  'folate' = '#006d2c',
  'niacin' = '#31a354',
  'riboflavin' = '#74c476',
  'thiamin' = '#bae4b3',
  'vitamin a' = '#a5faed',
  'vitamin b6' = '#52e9f2',
  'vitamin b12' = '#3ebce6',
  'vitamin c' = '#1394bf',
  'vitamin d' = '#002a9c',
  'vitamin k' = '#7293ed'
)
micronutrients_scenario.labs <- c('Calcium',"Iron",
                                  'Magnesium','Selenium',
                                  'Sodium','Zinc',
                                  'Folate', 'Niacin',
                                  'Riboflavin','Thiamin',
                                  'Vitamin A', 'Vitamin B6',
                                  'Vitamin B12', 'Vitamin C',
                                  'Vitamin D', 'Vitamin K')
names(micronutrients_scenario.labs) = c("calcium", "iron",
                                        "magnesium", "selenium",
                                        'sodium', 'zinc',
                                        'folate', 'niacin',
                                        'riboflavin','thiamin',
                                        'vitamin a', 'vitamin b6',
                                        'vitamin b12', 'vitamin c',
                                        'vitamin d', 'vitamin k')


food_scenario_palette = c(
  'Crops|Corn' = '#a63603',
  'Crops|Fiber crops' = '#cc0000',
  'Crops|Other grain crops' = '#ff3333',
  'Crops|Soy bean' = '#ff8080',
  'Crops|Wheat' = '#ffcccc',

  'Crops|Fruits' = '#fc6b03',
  'Crops|Vegetables' = '#feb581',

  'Crops|Oil crops' = '#ffff00',
  'Crops|Palm oil crops' = '#ffff99',

  'Crops|Rice' = '#8e0198',
  'Crops|Root Tubers' = '#be01cb',
  'Crops|Specialty crops and species' = '#f24dfe',
  'Crops|Sugar crops' = '#fab3ff',

  'Crops|Legumes' = '#8000ff',
  'Crops|Nuts and Seeds' = '#bf80ff',

  'Livestock products|Beef meat' = '#006d2c',
  'Livestock products|Dairy' = '#31a354',
  'Livestock products|Pork meat' = '#74c476',
  'Livestock products|Poultry meat' = '#bae4b3',
  'Livestock products|Sheep and Goat meat' = '#edf8e9',

  'Seafood|Fish' = '#3ebce6'
)



ghg_emiss = c(
  'CH4' = '#CD4631',
  'CO2' = '#81ADC8',
  'F-Gas' = '#F8F2DC',
  'LUC CO2' = '#C0DE7E',
  'N2O' = '#9E6240'
)
ghg_emiss.labs <- c('CH4', "CO2", "F-Gas", 'LUC CO2', 'N2O')
names(ghg_emiss.labs) = c('CH4', "CO2", "F-Gas", 'LUC CO2', 'N2O')


# creates color palette: "random" or from palette colors for all scenarios, except for reference one, which is black
create_color_palette <- function(scenarios, reference = "ref", palette = viridis::viridis(length(scenarios), option = 'D'), ref_color = 'black') {
  #  RColorBrewer::brewer.pal(length(scenarios), "Paired")
  palette <- setNames(palette, scenarios)
  palette[[reference]] <- ref_color
  return(palette)
}

# remove ",depth" pattern in columns where it's present
remove_depth <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(`subsector...4` = stringr::str_split(subsector...4, ',depth')[[1]][1]) %>%
    mutate(`subsector...5` = stringr::str_split(subsector...5, ',depth')[[1]][1]) %>%
    mutate(`subsector...6` = stringr::str_split(subsector...6, ',depth')[[1]][1])

  return(data)
}

cut_region_names <- function(data, short = F) {
  if (!short) {
    data <- data %>%
      dplyr::mutate(region = ifelse(region == 'Central America and Caribbean', 'Central America\nand Caribbean',
                                    ifelse(region == 'European Free Trade Association', 'EFTA',
                                           ifelse(region == 'South America_Northern', 'South America\nNorthern',
                                                  ifelse(region == 'South America_Southern', 'South America\nSouthern', region)))))
  } else {
    data <- data %>%
      dplyr::mutate(region = ifelse(region == 'Central America and Caribbean', 'Central America',
                                    ifelse(region == 'European Free Trade Association', 'EFTA',
                                           ifelse(region == 'South America_Northern', 'South America N',
                                                  ifelse(region == 'South America_Southern', 'South America S', region)))))
  }

  return(data)
}
