## food items
food_sector <<- c('Beef','Corn','Dairy','FiberCrop','FodderHerb','Fruits','Legumes',
                  'MiscCrop','NutsSeeds','OilCrop','Pork','Poultry','Rice','RootTuber',
                  'SheepGoat','Soybean','SugarCrop','Vegetables','Wheat','OilPalm',
                  'FodderGrass')
# staple commodities
staples <<- c("Corn", "OtherGrain", "Rice", "RootTuber", "Wheat")
# animal commodities
animal_commodities <<- c("Beef", "Dairy", "OtherMeat_Fish", "Pork", "Poultry", "SheepGoat")
# plant protein commodities
plant_prot_commodities <<- c("Legumes", "NutsSeeds")


# mix a single query data from files
mix_data <- function(query) {
  data <- data.frame()
  for (item in queries_all_list) {
    data <- bind_rows(data, item[[query]])
  }

  return(data)
}

# gather the different land use types from the given landleaf column
aggregate_land_use_type <- function(data) {
  data <- data %>%
    dplyr::mutate(land_use_type = ifelse(landleaf %in% c("UnmanagedHardwood_Forest", "UnmanagedSoftwood_Forest",
                                                  'Softwood_Forest', 'ProtectedUnmanagedHardwood_Forest',
                                                  'Hardwood_Forest', 'ProtectedUnmanagedSoftwood_Forest'), 'Forest',
                                  ifelse(landleaf %in% c('crops','biomass','otherarable'), 'Cropland',
                                         ifelse(landleaf %in% c("pasture (grazed)","pasture (other)"), 'Pasture',
                                                'Other Natural')))) %>%
    dplyr::select(-landleaf)

  return(data)
}

rename_pathways <- function(data) {
  data <- data %>%
    dplyr::mutate(scen_path = ifelse(scen_path == 'all', 'GlobT',
                                     ifelse(scen_path == 'plus', 'RegG', scen_path)))

  return(data)
}
