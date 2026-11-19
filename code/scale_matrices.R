# About -------------------------------------------------------------------

#' Script name: scale_matrices.R
#' Author: Aimee Altermatt, aimee.altermatt [at] burnet.edu.au
#' Purpose: Scale contact matrices created in contact_matrices.R so they 
#'          include all households, not just non-single households


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

#' Load the matrices created in contact-matrices.R, which reflect 
#' households of size > 1

#' Entry C_ij, in row i, column j of matrix C, represents the average
#' number of household contacts that an individual in age group i has with
#' people of age group j, among non-single households.

cm_aus <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-australia.csv")
cm_cald <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-CALD.csv")                         
cm_noncald <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Not_CALD.csv")                     
cm_cities <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Major-Cities.csv")                 
cm_region <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Inner-outer_regional.csv")         
cm_remote <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Remote-very_remote.csv")  
cm_act <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Australian-Capital-Territory.csv")
cm_nsw <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-New-South-Wales.csv")             
cm_nt <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Northern-Territory.csv")          
cm_qld <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Queensland.csv")                  
cm_sa <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-South-Australia.csv")             
cm_tas <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Tasmania.csv")                    
cm_vic <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Victoria.csv")                    
cm_wa <- read_csv("data/matrices-nonsingle-hholds/nonsingle_contact-matrix-Western_Australia.csv")   

# Load age size distributions   
ah_all <- read_csv("data/age-vs-household-aggregated.csv")   
ah_cald <- read_csv("data/age-vs-household-by-CALD-aggregated.csv")         
ah_rand <- read_csv("data/age-vs-hhold-by-remoteness-aggregated.csv") 
ah_state <- read_csv("data/age-vs-hhold-by-state-aggregated.csv")   

# Set up vector of ages
ages <- cm_aus$age

# Define order of ages
age_orders <- c("0_4", "5_9", "10_14", "15_19" , "20_24", "25_29", "30_34",
                "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69",
                "70_74", "75_79", "80+")

# Calculate household sizes -----------------------------------------------

#' The matrices summarise the average number of household contacts (co-residents)
#' among people whose age group is represented by the row, for people in non-single
#' households. People in single households have 0 contacts in any age group.
#' To make the matrices reflect the entire population (including single households)
#' the denominator should be the total number of people. The numerator does not change.
#' We therefore need to multiply the entries in row i by the scale factor:

#'           (number of people in non-single households in age group i)
#' fi =   ---------------------------------------------------------------
#'                  (total number of people in age group i)

#' Start by calculating the numerator (total people in non-single households)
#' and the denominator (total people = total in single + medium + large households)

# All of Australia
age_all <- ah_all %>% 
  group_by(AGE5P) %>% 
  summarise(total_ppl = sum(number_of_people),
            total_non_single = sum(number_of_people[hhold_size_cat != "Single"])) %>% 
  ungroup() %>% 
  # Calculate scale factor: non-single / total
  mutate(scale_factor = total_non_single / total_ppl) %>% 
  # Keep age order
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P)

# Cald
age_by_cald <- ah_cald %>% 
  group_by(ref_CALD, AGE5P) %>% 
  summarise(total_ppl = sum(number_of_people),
            total_non_single = sum(number_of_people[hhold_size_cat != "Single"])) %>% 
  ungroup() %>% 
  # Calculate scale factor: non-single / total
  mutate(scale_factor = total_non_single / total_ppl) %>% 
  # Keep age order
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(ref_CALD, AGE5P)

# Remoteness area
age_by_remoteness <- ah_rand %>% 
  group_by(remoteness_tri, AGE5P) %>% 
  summarise(total_ppl = sum(number_of_people),
            total_non_single = sum(number_of_people[hhold_size_cat != "Single"])) %>% 
  ungroup() %>% 
  # Calculate scale factor: non-single / total
  mutate(scale_factor = total_non_single / total_ppl)%>% 
  # Keep age order
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(remoteness_tri, AGE5P)

# State/territory
age_by_state <- ah_state %>% 
  group_by(state, AGE5P) %>% 
  summarise(total_ppl = sum(number_of_people),
            total_non_single = sum(number_of_people[hhold_size_cat != "Single"])) %>% 
  ungroup() %>% 
  # Calculate scale factor: non-single / total
  mutate(scale_factor = total_non_single / total_ppl)%>% 
  # Keep age order
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(state, AGE5P)


# Set up matrices ---------------------------------------------------------

# Convert data frames to matrices

## Contact matrices -------------------------------------------------------

# Function that takes matrix as a dataframe and returns it as a matrix object

make_matrix <- function(input_df){
  output_matrix <- input_df %>% 
  # Remove column 1
    select(-1) %>% 
    as.matrix()
  
}

# List all the matrices
matrix_list <- lst(
  cm_aus, 
  cm_cald, 
  cm_noncald, 
  cm_cities, 
  cm_region, 
  cm_remote, 
  cm_act, 
  cm_nsw, 
  cm_nt, 
  cm_qld, 
  cm_sa, 
  cm_tas, 
  cm_vic, 
  cm_wa, 
)

names(matrix_list) <- str_replace_all(names(matrix_list), "cm", "matrix")

# Apply the function to create matrix objects
matrix_list <- lapply(matrix_list, make_matrix)

list2env(matrix_list, envir = .GlobalEnv)

## Scale factors ----------------------------------------------------------

#' The scale factor matrices should be diagonal so that the entries in row i
#' are multiplied by scale factor fi
#' Need one scale factor per matrix

# All
diag_aus <- age_all %>% 
  pull(scale_factor) %>% 
  diag()

### By CALD ---------------------------------------------------------------

diag_cald <- age_by_cald %>% 
  filter(ref_CALD == "CALD") %>% 
  pull(scale_factor) %>% 
  diag()

diag_noncald <- age_by_cald %>% 
  filter(ref_CALD == "Not CALD") %>% 
  pull(scale_factor) %>% 
  diag()


### By remoteness ---------------------------------------------------------

diag_cities <- age_by_remoteness %>% 
  filter(remoteness_tri == "Major Cities") %>% 
  pull(scale_factor) %>% 
  diag()

diag_region <- age_by_remoteness %>% 
  filter(remoteness_tri == "Inner/outer regional") %>% 
  pull(scale_factor) %>% 
  diag()

diag_remote <- age_by_remoteness %>% 
  filter(remoteness_tri == "Remote/very remote") %>% 
  pull(scale_factor) %>% 
  diag()


### By state --------------------------------------------------------------

diag_act <- age_by_state %>% 
  filter(state == "Australian Capital Territory") %>% 
  pull(scale_factor) %>% 
  diag()

diag_nsw <- age_by_state %>% 
  filter(state == "New South Wales") %>% 
  pull(scale_factor) %>% 
  diag()

diag_nt <- age_by_state %>% 
  filter(state == "Northern Territory") %>% 
  pull(scale_factor) %>% 
  diag()

diag_qld <- age_by_state %>% 
  filter(state == "Queensland") %>% 
  pull(scale_factor) %>% 
  diag()

diag_sa <- age_by_state %>% 
  filter(state == "South Australia") %>% 
  pull(scale_factor) %>% 
  diag()

diag_tas <- age_by_state %>% 
  filter(state == "Tasmania") %>% 
  pull(scale_factor) %>% 
  diag()

diag_vic <- age_by_state %>% 
  filter(state == "Victoria") %>% 
  pull(scale_factor) %>% 
  diag()

diag_wa <- age_by_state %>% 
  filter(state == "Western Australia") %>% 
  pull(scale_factor) %>% 
  diag()



# Perform multiplication --------------------------------------------------

scale_matrices <- function(matrix_name){
  # Perform mulitplication F*C
  # Matrix C, with entries Cij, is scaled by diagonal scale matrix F.
  # Resulting matrix M = FC has Mij = fiCij = average household contacts between
  # individual in age group i and contacts in age group j
  
  # Input: matrix_name is a string identifying the sub-population
  
  # Define matrices
  cm_mat <- eval(parse(text = paste0("matrix_", matrix_name)))
  diag_mat <- eval(parse(text = paste0("diag_", matrix_name)))
  
  # Multiply
  diag_mat %*% cm_mat
}

# Run for all the matrices and save in list
result_matrices <- lapply(
  list( 
        "aus",
        "cald",
        "noncald",
        "cities",
        "region",
        "remote",
        "act",
        "nsw",
        "nt",
        "qld",
        "sa",
        "tas",
        "vic",
        "wa"
      ),
  scale_matrices
)

# Define filenames for saving supplementary tables
names(result_matrices) <- c("01_contact-matrix-Australia", 
                            "02_contact-matrix-CALD", 
                            "03_contact-matrix-Not_CALD", 
                            "04_contact-matrix-Major-Cities", 
                            "05_contact-matrix-Inner-outer_regional", 
                            "06_contact-matrix-Remote-very_remote", 
                            "07_contact-matrix-Australian-Capital-Territory", 
                            "08_contact-matrix-New-South-Wales", 
                            "09_contact-matrix-Northern-Territory", 
                            "10_contact-matrix-Queensland", 
                            "11_contact-matrix-South-Australia", 
                            "12_contact-matrix-Tasmania", 
                            "13_contact-matrix-Victoria", 
                            "14_contact-matrix-Western_Australia")
# Save --------------------------------------------------------------------

if(!dir.exists("output/result-matrices")){
  dir.create("output/result-matrices", recursive = TRUE)
}

# Convert back to dataframes, add first column and save to csv
save_matrix <- function(matrix_name){
  
  # Extract matrix from list
  mat <- result_matrices[[matrix_name]]
  
  mat %>% 
    as.data.frame() %>% 
    mutate(age = ages, .before = 1) %>% 
    write.csv(file = paste0("output/result-matrices/SuppTab",
                             matrix_name, ".csv"),
              row.names = FALSE)
}

lapply(names(result_matrices),
       save_matrix)
