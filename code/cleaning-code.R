
# About -------------------------------------------------------------------

# Script name: cleaning-code.R
# Author: Aimee Altermatt, aimee.altermatt@burnet.edu.au
# Purpose: This code cleans person-level and dwelling-level Census data to 
# be used to construct household contact matrices
# To access Australian Census data, requests should be directed to the Australian Bureau of Statistics

# Load --------------------------------------------------------------------

library(tidyverse)
library(data.table)


# Person level ------------------------------------------------------------

# Read in the census (person level)
census_p <- fread("census_2021_person.csv",
                  columns = c("AGE5P",  # age in 5-year groups
                              "SEXP",
                              # For CALD status
                              'BPLP',   #  country of birth
                              'LANP',   #  language used at home
                              # For linking with dwelling
                              "DWELLING_ID", # to join to dwelling
                              "RPIP"    # whether reference person
                              
))
setnames(census_p, toupper)


# Dwelling level ----------------------------------------------------------

census_d <- fread("census_2021_dwelling.csv",
                  select = c("STEUCD",      # State
                             "DWELLING_ID", # to join to person 
                             "NPDD",        # type of non-private dwelling
                             "DWTD",        # dwelling type
                             "NPRD",        # Number of Persons Usually Resident in Dwelling
                             "RAND"          # Remoteness
                             
                  ))

# Add dwelling info to person-level
census_p <- census_p %>% 
  left_join(census_d, by = "DWELLING_ID")


# Reference person --------------------------------------------------------


# Get the lowest rpip for each dwelling, noting there will be NAs
census_p[, min_rpip := min(as.numeric(RPIP), na.rm=T), by  = 'DWELLING_ID']

# Datasest for the person and dwelling ID of the reference person
ref_ppl <- census_p[order(RPIP), 
                    .SD[{ind = 1}], 
                    by = DWELLING_ID][,
                                      c("SPINE_V6_ID", "DWELLING_ID", "RPIP")]


# CALD status of reference person -----------------------------------------

# Remoteness and state/territory are based on dwelling
# CALD should be one unique value per dwelling so use reference person

# Birthplace: Australia (1101) vs overseas
census_p <- census_p %>% 
  mutate(cob = case_match(BPLP,
                          "1101" ~ "Australia",
                          c( "0000", "0001", "&&&&") ~ "Missing",
                          .default = "Overseas"),
         # Needed for matrices: CALD status
         # CALD, Not CALD, MISSING
         kp_ethnicity = case_when(
           #' CALD: not born in Aus (1101), NZ (1102), UK (2100-2108), Ireland (2201), 
           #' US (8104), Canada (8102) or South Africa (9225)
           (!(BPLP %in% c("1101", "1102", 
                          "2011", "2102", "2103", "2104", "2105", "2106","2107", "2108",
                          "2201", "8102", "8104", "9225",
                          "&&&&")) | # OR
              # Speaks language other than English at home (and not missing language)
              !LANP %in% c("1201", "&&&&")) ~ "CALD",
           # If haven't satisfied any of the above, and any of BPLP, LANP missing, make missing
           (BPLP == "&&&&" | LANP == "&&&&") ~ "MISSING",
           # Otherwise, not CALD
           TRUE ~ "Not CALD"))

# Add CALD status to reference dataset
ref_ppl <- ref_ppl %>% 
  left_join(census_p %>% 
              select(SPINE_V6_ID, 
                     ref_cob = cob, 
                     ref_CALD = kp_ethnicity),
            by = "SPINE_V6_ID")

# Add reference person's (COB) CALD status to each person based on dwelling
census_p <- census_p %>% 
  left_join(ref_ppl %>% 
              select(DWELLING_ID, ref_CALD),
            by = "DWELLING_ID")


# Remove anyone with missing spine-id
census_p <- census_p[SPINE_V6_ID != "",]


# Inclusion criteria ------------------------------------------------------

# remove visitors
census_p <- census_p %>% 
  filter(RPIP != "V")


# Remove overseas territories
census_p <- census_p %>% 
  filter(!STEUCD %in% c("9", 9))


# Remove Migratory, offshore, No usual address
census_p <- census_p %>% 
  filter(!RAND %in% c("5", "9"))

# Remove people from non-private dewllings, or missing reference level
# People removed: dwtd!= 1 or rpip is @ 

# Limit to private dwellings
census_p <- census_p %>% 
  filter(DWTD == '1' )

census_p <- census_p %>% 
  filter(RPIP != "@")

# Remove missing CALD in reference person only
census_p <- census_p %>% 
  filter(ref_CALD != "MISSING")


# Clean census ------------------------------------------------------------

census_p <- census_p %>%  
  mutate(
    # Needed for matrices: hhold size categorical
    # hhold size as categories, and 1, 2, ..., 6+
    # Note there are missings and NAs in NPRD, so will be here too
    hhold_size_num = case_match(NPRD,
                                c("6", "7", "8") ~ "6+",
                                .default = NPRD),
    # household size: single = 1 person, medium = 2-5 people, large = 6+ people
    # Note there are missings and NAs in NPRD, so will be here too
    hhold_size_cat = case_match(NPRD, # there are @s in the num
                                "1" ~ "single",
                                c("2", "3", "4", "5") ~ "medium",
                                c("6", "7", "8") ~ "large"),
    # Needed for matrices: state/territory
    # Recode state
    state = case_match(STEUCD, 
                       1	~ "New South Wales",
                       2	~ "Victoria",
                       3	~ "Queensland",
                       4	~ "South Australia",
                       5	~ "Western Australia",
                       6	~ "Tasmania",
                       7	~ "Northern Territory",
                       8	~ "Australian Capital Territory",
                       .default = as.character(STEUCD)),
    # Recode remoteness, based on ABS website
    remoteness = case_match(RAND,
                            0 ~ "Major Cities",
                            1 ~ "Inner Regional",
                            2 ~ "Outer Regional",
                            3 ~ "Remote",
                            4 ~ "Very Remote"),
    # Remoteness: major vs regional vs remote
    remoteness_tri = case_match(RAND,
                                0 ~ "Major Cities",
                                1:2 ~ "Inner/outer regional",
                                3:4 ~ "Remote/very remote",
                                .default = "CHECK"),
    # Age: write out ranges
    low = as.numeric(AGE5P),
    low = (low - 1)*5,
    hi = low + 4,
    AGE5P = paste0(low, "_", hi),
    # Combine the 80+
    AGE5P = case_match(AGE5P,
                       c("80_84", "85_89", "90_94", "95_99",
                         "100_104") ~ "80+",
                       .default = AGE5P)) %>% 
  select(-c(low, hi)) %>% 
  mutate(SEXP = case_match(SEXP, 1 ~ "Male", 2 ~ "Female"))


# Remove single households ------------------------------------------------


# Remove people in households of size 1
census_p <- census_p %>%
  filter(NPRD != "1")


# Ensure census_p is a data table
census_p <- data.table(census_p)
