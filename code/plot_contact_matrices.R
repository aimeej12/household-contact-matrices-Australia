# About -------------------------------------------------------------------

#' Script name: plot_contact_matrices.R
#' Author: Aimee Altermatt, aimee.altermatt [at] burnet.edu.au
#' Purpose: Create plots and tables for reporting contact matrices 


# Load --------------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(flextable)

# Save in a new folder
save_path <- "output/plots"
if(!dir.exists(save_path)){
  dir.create(save_path)
}


datapath <- "output/result-matrices"

# Get a list of all the csvs in the contact matrix folder
file_list <- list.files(datapath, recursive = T, full.names = T)

matrix_names <- c()
for(cm_filename in file_list){
  # Extract what to call it
  # Extract the text between the last / and .csv
  description <- str_match(cm_filename, ".*\\/.*contact-matrix-(.*)\\.csv")[2]
  description_clean <- str_replace_all(description, " ", "_")
  description_clean <- str_replace_all(description, "-", "_")
  
  # Load
  
  cm_df <- read_csv(cm_filename)# %>%  select(-1)
  
  assign(description_clean, cm_df)
  # Save the name
  matrix_names <- c(matrix_names, description_clean)
  
}

# Set up ------------------------------------------------------------------


age_levels <- c("0_4", "5_9", "10_14", "15_19", "20_24", "25_29",
                "30_34", "35_39", "40_44", "45_49", "50_54", "55_59",
                "60_64", "65_69", "70_74", "75_79", "80+"  )

age_labels <- str_replace_all(age_levels, "_", "-")

# Set labels for plotting
title_label_df <- data.frame(varname = matrix_names) %>% 
  mutate(label_name = case_match(varname,
                                 "Australia" ~ "whole of Australia",
                                 "CALD" ~ "culturally and linguistically diverse households",
                                 "Not_CALD" ~ "non-culturally and linguistically diverse households",
                                 "Inner_outer_regional" ~ "households in inner, outer and regional areas",
                                 "Major_Cities" ~ "households in major cities",
                                 "Remote_very_remote" ~ "remote and very remote areas",
                                 .default = str_replace_all(varname, "_", " ")
         ))
title_label_df <- title_label_df %>% 
  deframe()

# Define filenames for plotting
# The plot for Australia is figure 2 in the main paper
# The others are supplementary figures
filenames <- data.frame(matrix_name = matrix_names) %>% 
  mutate(filename = case_match(matrix_name,
                               "Australia" ~ "Fig_2_contact-matrix-australia",
                               .default = paste0("Supp_Fig_",
                                                 # number as 2 digits
                                                 str_pad(row_number() - 1, 2, pad = "0"),
                                                 "_contact-matrix-",
                                                 matrix_names
                               )
  )) %>% 
  deframe()

# Get range ---------------------------------------------------------------

# Get the range for plotting
max_val <- lapply(matrix_names,
       function(cm_df_name){
  data.frame(max = eval(parse(text = cm_df_name)) %>%
    select(-1) %>%
    max(),
    table = cm_df_name)
}) %>% 
  bind_rows() %>% 
  pull(max) %>%
  max()

min_val <- lapply(matrix_names,
                  function(cm_df_name){
                    data.frame(min = eval(parse(text = cm_df_name)) %>%
                                 select(-1) %>%
                                 min(),
                               table = cm_df_name)
                  }) %>% 
  bind_rows() %>% 
  pull(min) %>%
  min()

# So we will plot on the range (rounded to something nice close by)
c(min_val, max_val)

# Plot --------------------------------------------------------------------

plot_cm <- function(cm_df_name){
  # Get the filename
  plot_filename <- filenames[cm_df_name]
  
  # Make plot
  cm_p <- eval(parse(text = cm_df_name)) %>% 
  # Need to put in long form
    rename(from_age = age) %>% 
    pivot_longer(-from_age,
                 names_to = "to_age",
                 values_to = "value",
                 names_prefix = "aged_")  %>% 
    # Reorder age
    mutate(across(contains("age"),
                  ~factor(., levels = age_levels,
                          labels = age_labels))) %>% 
    # Plot
    ggplot(aes(y = fct_rev(from_age),
               x = to_age,
               fill = value)) +
    geom_tile() +
    labs(x = "Age (years) of household contact",
         y = "Age (years) of individual",
         fill = "Average\nnumber\nof contacts",
         title = paste0("Household contact matrix for ",
                        title_label_df[cm_df_name])) +
    # Put them all on the same scale
    scale_fill_continuous(limits = c(min_val, max_val)) +
    theme(axis.text = element_text(size = 10),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 16),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          aspect.ratio = 1) + 
    # Put the x-axis on the top to look like the matrix
    scale_x_discrete(position= "top")
  
  # Save
  ggsave(cm_p,
         filename = paste0(save_path, "/", plot_filename,".png"),
         height = 25, width = 25, units = "cm", dpi = 300)
  
  # Output plot
  return(cm_p)
}

plot_list <- lapply(matrix_names, plot_cm)

# Clean table for Australia (T3) ------------------------------------------

# Table 3: contact matrix for Australia in word doc
cm_ft <- Australia %>% 
  # Fix age labels
  rename_with(~str_remove_all(., "aged_") %>% 
                str_replace_all("_", " – ")) %>% 
  mutate(age = factor(age, 
                        levels = age_levels,
                        labels = str_replace(age_levels, "_", " – "))) %>% 
  rename(Age = age) %>% 
  flextable()  %>% 
  # Round to 2 decimal places
  colformat_double(j = 2:18,
                   digits = 2) %>% 
  autofit() %>% 
  # Add some labels
  add_header_row(values = "Age (years) of household contact",
                 colwidths = 18) %>% 
  align(part = "header", align = "center", i=1) %>% 
  set_header_labels(Age = "Age (years) of individual") %>% 
  set_caption("Table 3: Household contact matrix for Australian households")

# Save Table 3 -----------------------------------------------------------

# Table 3
cm_ft %>% 
  save_as_docx(path = "output/table3_contact_mat_aus.docx")


