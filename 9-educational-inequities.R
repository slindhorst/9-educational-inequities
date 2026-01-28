# install packages
library(tidyverse)
library(here)
library(janitor)
library(dataedu)

#install rJava

install.packages("rJava")
library(rJava)

# install tabulapdf
install.packages(
  "tabulapdf",
  repos = c(
    "https://ropensci.r-universe.dev",
    "https://cloud.r-project.org"
  )
)

library(tabulapdf)

# Option 1: Get data using {tabulapdf}
race_list_pdf <-
  extract_tables(here("data", "mps_fall2018_racial_ethnic_by_school_by_grade.pdf"))

race_df <-
  race_list_pdf %>% 
  # Turn each page into a tibble
  map( ~ as_tibble(.x, .name_repair = "unique")) %>%
  # make data frame
  list_rbind() %>% 
  # Remove unnecessary rows
  slice (-1:-2) %>%
  # Use descriptive column names
  set_names(
    c(
      "school_group",
      "school_name",
      "grade",
      "na_num",
      # Native American number of students
      "na_pct",
      # Native American percentage of students
      "aa_num",
      # African American number of students
      "aa_pct",
      # African American percentage
      "as_num",
      # Asian number of students
      "as_pct",
      # Asian percentage
      "hi_num",
      # Hispanic number of students
      "hi_pct",
      # Hispanic percentage
      "wh_num",
      # White number of students
      "wh_pct",
      # White percentage
      "pi_pct",
      # Pacific Islander percentage
      "blank_col",
      # Total number of students (from the Race PDF)
      "tot" 
    )
  )

race_df2 <-
  race_df %>% 
  # remove unnecessary columns
  select(-school_group, -grade, -pi_pct, -blank_col) %>% 
  # Remove the "Grand Total" to get grade-level numbers
  filter(str_detect(school_name, "Total"), school_name != "Grand Total") %>% 
  # Clean up school names
  mutate(school_name = str_replace(school_name, "Total", "")) %>% 
  # Trim white space
  mutate(across(where(is.character), str_trim)) %>% 
  # Turn precentage columns into numeric format
  mutate(across(matches("pct"), ~ as.numeric(str_replace(., "%", ""))/ 100))

#import Free and Reduced_Price Lunch PDF
#Option 1: Get data using tabulapdf

frpl_pdf <-
  extract_tables(here("data", "fall_2018_meal_eligiblity_official.pdf"))

frpl_df <- frpl_pdf %>% 
  # Turn each page into a tibble
  map(~ as_tibble(.x, .name_repair = "unique")) %>% 
  # Make data frame
  list_rbind() %>% 
  # Remove unnecessary rows
  slice(-1) %>% 
  # Use descriptive column names
  set_names(
    c(
      "school_name",
      "not_eligible_num",
      # Number of non-eligible students,
      "reduce_num",
      # Number of students receiving reduced price lunch
      "free_num",
      # Number of students receiving free lunch
      "frpl_num",
      # Total number of students (from the FRPL PDF)
      "frpl_pct" # Free/reduced price lunch percentage
    )
  )

frpl_df2 <-
  frpl_df %>% 
  filter(
    # remove empty rows
    school_name != "",
    # Filter out the rows in this list!
    school_name %in% c(
      "ELM K_08",
      "Mid Schl",
      "High Schl",
      "Alt HS",
      "Spec Ed Total",
      "Cont Alt Total",
      "Hospital Sites Total",
      "Dist Total"
    )
    
  ) %>%
  # Turn percentage columns into numeric and decimal format
  mutate(frpl_pct = as.numeric(str_replace(frpl_pct, "%", "")) / 100)

joined_df <-
  #Create full dataset joined by school name
  left_join(race_df2, frpl_df2, by = c("school_name")) %>% 
  #Convert these columns to numeric
  mutate(across(2:17, as.numeric))

glimpse(joined_df)
