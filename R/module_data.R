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

#' Compute a left join, taking only the first match.
#'
#' In an ordinary \code{\link{left_join}}, if a row in the left operand has
#' multiple matches in the right operand, you get a copy of the row for each
#' match in the right operand.  Sometimes you want just one arbitrary member of
#' the matching set.  This could be because the right operand is a one-to-many
#' mapping, and you don't care which one you get (but you want only one), or it
#' could be that you're trying to reproduce the behavior of legacy code that
#' uses \code{\link{match}}, which has this behavior.  This function performs
#' such a join.
#'
#' This function performs a left join, except that if the right operand has
#' multiple matches for a row in the left operand, \emph{only} the first match
#' is kept.  \strong{Use this function with caution.}  The results will depend
#' on the order of the rows in the right operand, meaning that seemingly
#' innocuous changes can produce changes in output.  Consider yourself warned.
#'
#' @param x Left table to join
#' @param y Right table to join
#' @param by Vector of id columns.  Unlike in other join variants, these must be
#' supplied explicitly.
#' @return Joined table.  In case of multiple matches, only the first will be
#' included.
#' @importFrom dplyr ungroup left_join
#' @source gcamdata
left_join_keep_first_only <- function(x, y, by) {
  ## Our strategy is to use summarize/first on y for each non-match category,
  ## then join that to x.
  . <- NULL                           # silence notes on package check
  ll <- by
  names(ll) <- NULL
  y %>%
    dplyr::group_by_at(tidyselect::all_of(ll)) %>%
    dplyr::summarize_at(dplyr::vars(-tidyselect::any_of(ll)), first) %>%
    ungroup() %>%
    left_join(x, ., by=by)
}
