# About -------------------------------------------------------------------

#' Script name: compare_to_prem.R
#  Author: Aimee Altermatt, aimee.altermatt@burnet.edu.au
#' Purpose: Compare our contact matrix for Australian to Australian matrix
#'          by Prem et al, 2017. 
#'          Create Figure 3 in main text
#' Date created: 2026-01-22

# Load --------------------------------------------------------------------

# install.packages("contactdata")
library(contactdata)  
library(tidyverse)
library(cowplot)

# Load matrices
ls("package:contactdata")

list_countries()
list_countries(data_source = 2017)

# Country-wide
prem_mat <- contact_matrix(country = "Australia",
               location = "home",
               data_source = 2017)

# Load our matrix - available in supplementary material
australia_df <- read.csv("supplement-matrices/SuppTab01_contact-matrix-australia.csv")


# Clean -------------------------------------------------------------------


# Put in similar format
prem_df <- as.data.frame(prem_mat) 

prem_df <- prem_df %>% 
  rename_with(~paste0("aged_", .)) %>% 
  rownames_to_column("age")


age_levels <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29",
                "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                "60_64", "65_69", "70_74", "75_79", "80+"  )

age_labels <- str_replace_all(age_levels, "_", "-")

age_levels_prem <- c( "00_05", "05_10", "10_15", "15_20", "20_25",
                      "25_30", "30_35", "35_40", "40_45", "45_50",
                      "50_55", "55_60", "60_65","65_70", "70_75", "75_80")

age_labels_prem <- age_labels[age_labels != "80+"]

# Prem only goes to 75-80, no 80+ so remove from ours for comparison

# Need the same labels and same levels
# Write a function to recode
clean_age <- function(input_data){
  input_data %>% 
    mutate(across(ends_with("_age"),
                  ~case_match(.,
                              c("0_4", "00_05") ~ "0-4",
                              c("5_9", "05_10") ~ "5-9",
                              c("10_14", "10_15") ~ "10-14",
                              c("15_19", "15_20") ~ "15-19",
                              c("20_24", "20_25") ~ "20-24" ,
                              c("25_29", "25_30") ~ "25-29",
                              c("30_34", "30_35") ~"30-34",
                              c("35_39", "35_40") ~"35-39", 
                              c("40_44", "40_45") ~"40-44",
                              c("45_49", "45_50") ~"45-49",
                              c("50_54", "50_55") ~"50-54",
                              c("55_59", "55_60") ~"55-59",
                              c("60_64", "60_65") ~"60-64",
                              c("65_69", "65_70") ~"65-69", 
                              c("70_74", "70_75") ~"70-74", 
                              c("75_79", "75_80") ~"75-79", 
                              c("80+", "80.") ~ "80+"
                  ) %>% factor(levels = age_labels)
    ))
}

# Determine min, max val for comparable scales
min_val <- min(
  australia_df %>% select(-c(age, aged_80.)) %>% min(),
  prem_df %>% select(-1) %>% min()
)

max_val <- max(
  australia_df %>% select(-c(age, aged_80.)) %>% max(),
  prem_df %>% select(-1) %>% max()
)

# Check -------------------------------------------------------------------

# Make sure axes are aligned correctly
# Try to replicate plot from supp mat
# https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pcbi.1005697/2/pcbi.1005697.s001.pdf?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20260427%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20260427T015201Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=0d257da9c6d4ce6bcb66445acdc96b77203d0a6bd571b7a71fae2aa72828e2e273f48957af6adc9fdb3b9934ef74033081245ac7e036a575a8d1b70e458acf13db38da83e1d37231c4caf6c0b998258022e69d0ab7c6b9ba4502d851ecd4a12a13f63a392ffd46eb159f50da31843f533c0e98ab6898f63925710ae5052e1e447b2aaf9545127997f67ca854194cd489ae86f2d3a003b5861ce9ded715015bc84b7dec051a57e89e5ff2ac7d65a69a19afee402c14d82f0714b00cc526b395d5de6680bb6e70a0f4818111ee642b165ee955610ef73da3d98d805e5f2c37399277a2d9a19a35959efaa3867f2358355ed9d1ec02bd5eabd07b7a123c86557ed3

head(prem_df)
prem_df %>% 
  rename(from_age = age) %>% 
  pivot_longer(-from_age,
               names_to = "to_age",
               values_to = "value",
               names_prefix = "aged_") %>% 
  ggplot(aes(x = from_age,
             y = to_age,
             fill = value)) +
  geom_tile() +
  theme_bw() +
  scale_fill_distiller(direction = 1) 
# This is essentially the plot from the supp mat, so orientation is correct
# from_age is age of individual, which is rows in the data frame
# to_age is age of contact, which is columns in the data frame

# Compare matrices --------------------------------------------------------
class(prem_mat)

aus_mat <- as.matrix(australia_df %>% 
                       filter(age != "80+") %>% 
                       select(-c(age, aged_80.)))
class(aus_mat)

dim(prem_mat)

dim(aus_mat)

# Calculate difference
diff_mat  <-  aus_mat - prem_mat

# Add the age column to the difference matrix and make it a dataframe
diff_df <- cbind(australia_df %>% 
                   filter(age != "80+") %>% 
                   select(age),
                 diff_mat %>% 
                   as.data.frame)
# Get max and min for plotting
min_diff <- diff_df %>% select(-age) %>% min()
max_diff <- diff_df %>% select(-age) %>% max()


# Plot --------------------------------------------------------------------

census_p <- australia_df %>% 
  rename(from_age = age) %>% 
  pivot_longer(-from_age,
               names_to = "to_age",
               values_to = "value",
               names_prefix = "aged_")  %>% 
  # Clean the col name for 80+
  mutate(to_age = if_else(to_age == "80.", "80+", to_age)) %>% 
  # Actually we don't want 80+ rows or columns because Prem doesn't have
  filter(from_age != "80+" & to_age != "80+") %>% 
  clean_age() %>% 
  droplevels() %>% 
  # Plot
  ggplot(aes(y = fct_rev(from_age),
             x = to_age,
             fill = value)) +
  geom_tile() +
  labs(x = "Age (years) of household contact",
       y = "Age (years) of individual",
       fill = "Average\nnumber\nof contacts",
       title ="Census") +
  # Put them all on the same scale
  scale_fill_continuous(limits = c(min_val, max_val)) +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 14),
        legend.position = 'bottom',
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        aspect.ratio = 1) + 
  # Put the x-axis on the top to look like the matrix
  scale_x_discrete(position= "top")


prem_p <- prem_df %>% 
  rename(from_age = age) %>% 
  pivot_longer(-from_age,
               names_to = "to_age",
               values_to = "value",
               names_prefix = "aged_")  %>% 
  # Reorder age
  clean_age() %>% 
  # Plot
  ggplot(aes(y = fct_rev(from_age),
             x = to_age,
             fill = value)) +
  geom_tile() +
  labs(x = "Age (years) of household contact",
       y = "Age (years) of individual",
       fill = "Average\nnumber\nof contacts",
       title ="Prem") +
  # Put them all on the same scale
  scale_fill_continuous(limits = c(min_val, max_val)) +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 14),
        legend.position = 'bottom',
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        aspect.ratio = 1) + 
  # Put the x-axis on the top to look like the matrix
  scale_x_discrete(position= "top")

# Combine
compare_both <- cowplot::plot_grid(census_p ,
                                   prem_p)

diff_p <-  diff_df %>% 
  rename(from_age = age) %>% 
  pivot_longer(-from_age,
               names_to = "to_age",
               values_to = "value",
               names_prefix = "aged_")  %>% 
  # Reorder age
  clean_age() %>% 
  # Plot
  ggplot(aes(y = fct_rev(from_age),
             x = to_age,
             fill = value)) +
  geom_tile() +
  labs(x = "Age (years) of household contact",
       y = "Age (years) of individual",
       fill = "Difference",
       title ="Difference between Census and Prem") +
  # Put them all on the same scale
  # scale_fill_continuous(limits = c(min_val, max_val)) +
  theme(axis.text = element_text(size = 10),
        legend.text = element_text(size = 14),
        legend.position = 'bottom',
        plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        aspect.ratio = 1) + 
  # Put the x-axis on the top to look like the matrix
  scale_x_discrete(position= "top")+
  # Need diverging scale for negative and positive
  scale_fill_gradient2(low = "#7B3294",
                       mid = "white",
                       high = "#E66101",
                       midpoint = 0,
                       breaks = c(min_diff, 
                                  0, max_diff),
                       labels = c(round(min_diff, 2), 
                                  0, round(max_diff, 2))
                       )

diff_p

# Combine -----------------------------------------------------------------
# Make clean sub plots

# Set theme for axes
clean_theme <- theme(axis.title = element_blank(),
                     plot.title = element_blank(),
                     axis.text.x = element_text(angle = 90,
                                                hjust = 0,
                                                vjust = 1),
                     axis.ticks = element_blank())

prem_clean_p <- prem_p + clean_theme
census_clean_p <- census_p + clean_theme
diff_clean_p <- diff_p + 
  clean_theme +
  theme(legend.text = element_text(vjust = 0),
        legend.title = element_text(vjust = 1))

# cowplot to combine
my_row <- plot_grid(prem_clean_p, 
                    census_clean_p, 
                    diff_clean_p,
                    nrow = 1,
                    align = "hv",
                    axis = "tblr",
                    labels = c("Prem et al (2017)",
                               "Census 2021",
                               "Census − Prem"))
combined_p <-  ggdraw(my_row) +
  theme(plot.margin = margin(l = 40, r = 40))+
  draw_label("Age (years) of household contact", x = 0.5, y = 0.9) +
  draw_label("Age (years) of individual", x = -0.02, y = 0.5, angle = 90)

combined_p

# Save --------------------------------------------------------------------

ggsave(combined_p,
       filename = "output/contact-matrices/plots/Fig_3_compare_to_prem.jpg",
       height = 15, width = 30, units = 'cm', dpi = 300)
