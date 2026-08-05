# About -------------------------------------------------------------------

#' Script name: contact_matrices.R
# Author: Aimee Altermatt, aimee.altermatt [at] burnet.edu.au
#' Purpose: Create age-based contact matrices for all of Australia and by
#' state/territory, Cultural and Linguistic Diversity of the Reference person
#' and remoteness of the dwelling.

#' Cleaning data are provided in cleaning-code.R
#' The data used in this script should be a data table of the person-level
#' census, with one row per person, and columns for:
#' SPINE_V6_ID: unique person identifier
#' DWELLING_ID: unique ID to identify dwellings (households)
#' AGE5P: Age in 5-year bands
#' state: the state or territory of the dwelling, a clean version of STEUCD
#'        as defined in cleaning-code.R
#'remoteness_tri: remoteness area of the dwelling, a clean version of RAND,
#'        as defined in cleaning-code.R
#'ref_CALD: CALD status of the reference person, as defined in cleaning-code.R

# Set up  -----------------------------------------------------------------

# Create folders to save output
if(!dir.exists("output/contact-matrices")){
  dir.create("output/contact-matrices", recursive = TRUE)
  dir.create("output/contact-matrices/states")
  dir.create("output/contact-matrices/cald")
  dir.create("output/contact-matrices/remoteness")
}


# Mixing matrices ---------------------------------------------------------

# It will be important to order in the order of the ages
age_orders <- c("0_4", "5_9", "10_14", "15_19" , "20_24", "25_29", "30_34",
                "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69",
                "70_74", "75_79", "80+")

mixing_mat <- function(input_data){
  
  # input-data is the census filtered by grouping of interest
  
  # For each dwelling, get the number of people in each age group
  age_counts <- input_data[, .N, by = c("DWELLING_ID", "AGE5P")] %>% 
    pivot_wider(names_from = AGE5P,
                values_from = N,
                names_prefix = "aged_",
                values_fill = 0)
  
  # Add the number of people per age group per household to the data
  input_data %>% 
    left_join(age_counts, by = "DWELLING_ID") %>% 
    select(SPINE_V6_ID, DWELLING_ID, starts_with("aged_"), AGE5P) %>% 
    # Get the ages of the other people in the household: don't count a person
    # in their own row
    mutate(across(starts_with("aged"),
                  function(cname){
                    # For the column corresponding to the row's age, subtract 
                    # 1 from the count
                    if_else(AGE5P == str_remove(deparse(substitute(cname)), "aged_"),
                            cname - 1, cname)
                  }))  %>%
    # Make the matrix: average number among people in each AGE5P level
    group_by(AGE5P) %>% 
    summarise(across(starts_with("aged_"), mean)) %>% 
    # Order the columns in age order
    select(AGE5P, all_of(paste0("aged_", age_orders))) %>% 
    # Order the rows in age order
    mutate(AGE5P = factor(AGE5P, levels = age_orders)) %>% 
    arrange(AGE5P)
  
}

# Australia ---------------------------------------------------------------

mixing_mat(census_p) %>% 
  write.csv("output/contact-matrices/australia.csv")

# States/territories ------------------------------------------------------

# Order is ACT, WA, SA, NSW, VIC, QLD, TAS, NT
for(state_name in unique(census_p$state)){
  census_p %>% 
    filter(state == state_name) %>% 
    mixing_mat() %>% 
    write.csv(paste0("output/contact-matrices/states/", state_name, ".csv"))
}


# Rural/urban -------------------------------------------------------------

# We are doing remoteness as 3-levels
for(rurality in unique(census_p$remoteness_tri)){
  census_p %>% 
    filter(remoteness_tri == rurality) %>% 
    mixing_mat() %>% 
    write.csv(paste0("output/contact-matrices/remoteness/",  
                     str_replace_all(rurality, "[\\/ ]", "_"), ".csv"))
}

# Born in Aus/overseas ----------------------------------------------------

for(cald_name in unique(census_p$ref_CALD)){
  census_p %>% 
    filter(ref_CALD == cald_name) %>% 
    mixing_mat() %>% 
    write.csv(paste0("output/contact-matrices/cald/", cald_name, ".csv"))
}
