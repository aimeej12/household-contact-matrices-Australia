# Script name:check-matrix-symmetry.R
# Study: Household contact matrices for key populations
# Author: Aimee Altermatt, aimee.altermatt@burnet.edu.au
# Purpose: Check matrix symmetry condition
# Date created: 2026-10-27

# Load --------------------------------------------------------------------

library(tidyverse)

# Load matrix for all of Aus
cm_aus <- read_csv("output/result-matrices/SuppTab01_contact-matrix-Australia.csv")

# Load all other matrices
cm_cald <- read_csv("output/result-matrices/SuppTab02_contact-matrix-CALD.csv")
cm_noncald <- read_csv("output/result-matrices/SuppTab03_contact-matrix-Not_CALD.csv")
cm_cities <- read_csv("output/result-matrices/SuppTab04_contact-matrix-Major-Cities.csv")
cm_region <- read_csv("output/result-matrices/SuppTab05_contact-matrix-Inner-outer_regional.csv")
cm_remote <- read_csv("output/result-matrices/SuppTab06_contact-matrix-Remote-very_remote.csv")
cm_act <- read_csv("output/result-matrices/SuppTab07_contact-matrix-Australian-Capital-Territory.csv")
cm_nsw <- read_csv("output/result-matrices/SuppTab08_contact-matrix-New-South-Wales.csv")
cm_nt <- read_csv("output/result-matrices/SuppTab09_contact-matrix-Northern-Territory.csv")
cm_qld <- read_csv("output/result-matrices/SuppTab10_contact-matrix-Queensland.csv")
cm_sa <- read_csv("output/result-matrices/SuppTab11_contact-matrix-South-Australia.csv")
cm_tas <- read_csv("output/result-matrices/SuppTab12_contact-matrix-Tasmania.csv")
cm_vic <- read_csv("output/result-matrices/SuppTab13_contact-matrix-Victoria.csv")
cm_wa <- read_csv("output/result-matrices/SuppTab14_contact-matrix-Western_Australia.csv")

# Load household sizes
hhold_size <- read_csv("data/age-vs-household-aggregated.csv")
hhold_size_cald <- read_csv("data/age-vs-household-by-CALD-aggregated.csv")
hhold_size_region <- read_csv("data/age-vs-hhold-by-remoteness-aggregated.csv")
hhold_size_state <- read_csv("data/age-vs-hhold-by-state-aggregated.csv")


# Australia-wide matrix ---------------------------------------------------


## Calculate total population size ----------------------------------------

# This is disaggregated by hhold size. Combine
popsize <- hhold_size %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup()

# Define order of ages
age_orders <- c("0_4", "5_9", "10_14", "15_19" , "20_24", "25_29", "30_34",
                "35_39", "40_44", "45_49", "50_54", "55_59", "60_64", "65_69",
                "70_74", "75_79", "80+")

popsize <- popsize %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P)

## Set matrices -----------------------------------------------------------

# cm is a 17x17 matrix
# cij is the average household contacts a person in age group i has with people in age group j

# popsize is a 17x17 diagonal matrix
# entry in row i, col i is the number of people in age group i

# cij: average number of household contacts someone in age j has with individuals in group i
cij <- cm_aus %>% 
  select(-age) %>% 
  as.matrix()

# cji: transpose of cij
cji <-  t(cij)

# pop sizes
# Ni = population size in i, diagonal
Ni <- popsize %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()


## Check ------------------------------------------------------------------

# 0-4 with 5-9 is
cij[1,2]
# Number of people 0-4
Ni[1]
(cij[1,2]) *(Ni[1])

# 5-9 with 0-4 is
cij[2,1]
# Number of people aged 5-9 is
Ni[2]
(cij[2,1])*(Ni[2])

# 0-4 with 10-14
(cij[1,3]) *(Ni[1])
(cij[3,1]) *(Ni[3])

# Cij with row=i, col=j has each row multiplied by Ni to get square matrix

Njcij <- Ni %*% cij

# transpose, Cji has colimn i multiplied by Ni, so diag goes on right
cjiNj <- cji %*% Ni

# These should be approximately equal and should be symmetric

# Confirm symmetric
(Njcij- t(Njcij)) %>% round(0)


# Repeat for sub-populations ----------------------------------------------

# Need a popsize matrix for each population size
N_cald <- hhold_size_cald %>% 
  # Get age group totals among sub-population
  filter(ref_CALD == "CALD") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  # Order by age
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  # Convert to diagonal matrix
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_noncald <- hhold_size_cald %>% 
  filter(ref_CALD == "Not CALD") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

# By region
N_cities <- hhold_size_region %>% 
  filter(remoteness_tri == "Major Cities") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_region <- hhold_size_region %>% 
  filter(remoteness_tri == "Inner/outer regional") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_remote <- hhold_size_region %>% 
  filter(remoteness_tri == "Remote/very remote") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

# By state/territory
N_act <- hhold_size_state %>% 
  filter(state == "Australian Capital Territory") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_nsw <- hhold_size_state %>% 
  filter(state == "New South Wales") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_nt <- hhold_size_state %>% 
  filter(state == "Northern Territory") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_qld <- hhold_size_state %>% 
  filter(state == "Queensland") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_sa <- hhold_size_state %>% 
  filter(state == "South Australia") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_tas <- hhold_size_state %>% 
  filter(state == "Tasmania") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_vic <- hhold_size_state %>% 
  filter(state == "Victoria") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()

N_wa <- hhold_size_state %>% 
  filter(state == "Western Australia") %>% 
  group_by(AGE5P) %>% 
  summarise(total_people = sum(number_of_people)) %>% 
  ungroup() %>% 
  mutate(AGE5P = factor(AGE5P,
                        levels = age_orders)) %>% 
  arrange(AGE5P) %>% 
  select(-AGE5P) %>% 
  pull(total_people) %>% 
  diag()


## Perform calculation ----------------------------------------------------

# Write function that takes input matrix Cij and diagonal popsize matrix N
# Performs calculation then checks symmetry by subtracting one matrix from 
# the other

check_symmetry <- function(cij_df, Ni){
  # cij_df is a n*(n+1) dataframe for the contact matrix in a sub-population
  # Ni is the diagonal population size matrix
  
  # Create matrix cij based on input dataframe
  cij <- cij_df %>% 
    select(-age) %>% 
    as.matrix()
  
  # cji: transpose of cij
  cji <-  t(cij)
  
  
  Njcij <- Ni %*% cij
  
  # transpose, Cji has colimn i multiplied by Ni, so diag goes on right
  cjiNj <- cji %*% Ni
  
  # These should be approximately equal and should be symmetric
  
  # Print output to confirm symmetric
  (Njcij- t(Njcij)) %>% round(0)
}

check_symmetry(cm_cald, N_cald)
check_symmetry(cm_noncald, N_noncald)

check_symmetry(cm_cities, N_cities)
check_symmetry(cm_region, N_region)
check_symmetry(cm_remote, N_remote)

check_symmetry(cm_act, N_act)
check_symmetry(cm_qld, N_qld)
check_symmetry(cm_nsw, N_nsw)
check_symmetry(cm_sa, N_sa)
check_symmetry(cm_tas, N_tas)
check_symmetry(cm_vic, N_vic)
check_symmetry(cm_sa, N_sa)
check_symmetry(cm_wa, N_wa)
