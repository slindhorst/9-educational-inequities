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
    !school_name %in% c(
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

district_merged_df <-
  joined_df %>%
  # Calculate high-poverty numbers
  mutate(
    hi_povnum = case_when(frpl_pct > .75 ~ hi_num),
    aa_povnum = case_when(frpl_pct > .75 ~ aa_num),
    wh_povnum = case_when(frpl_pct > .75 ~ wh_num),
    as_povnum = case_when(frpl_pct > .75 ~ as_num),
    na_povnum = case_when(frpl_pct > .75 ~ na_num)
  ) %>%
    # Calucluate totals of students
  adorn_totals() %>% 
  # Create percentage by demographic
  mutate(
    na_pct = na_num / tot,
    aa_pct = aa_num / tot,
    as_pct = as_num / tot,
    hi_pct = hi_num / tot,
    wh_pct = wh_num / tot,
    frpl_pct = (free_num + reduce_num) / frpl_num,
    # Create precentage by demographic and poverty
    hi_povsch = hi_povnum / hi_num[which(school_name == "Total")],
    aa_povsch = aa_povnum / aa_num[which(school_name == "Total")],
    as_povsch = as_povnum / as_num[which(school_name == "Total")],
    wh_povsch = wh_povnum / wh_num[which(school_name == "Total")],
    na_povsch = na_povnum / na_num[which(school_name == "Total")]
  )

# facilitate creation of plots later on, we use pivot_longer()
district_tidy_df <-
  district_merged_df %>% 
  pivot_longer(
    cols = -matches("school_name"),
    names_to = "category",
    values_to = "value"
)

district_tidy_df %>% 
  # Filter for Total rows, since we want district-level info
  filter(school_name == "Total",
         str_detect(category, "pct"),
         category != "frpl_pct") %>% 
  # Reordering x-axis so bars appear by descending value
  ggplot(aes(x = reorder(category, -value), y = value)) +
  geom_bar(stat = "identity", aes(fill = category)) + 
  labs(title = "Percentage of Population by Subgroup", x = "Subgroup", y = "Percentage of Population") +
  scale_x_discrete(
    labels = c(
      "aa_pct" = "Black",
      "wh_pct" = "White",
      "hi_pct" = "Hispanic",
      "as_pct" = "Asian",
      "na_pct" = "Native Am."
    )
  ) +
  # Makes labels present as percentages
  scale_y_continuous(labels = scales::percent) +
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")

# note those students eligible for FRPL
district_tidy_df %>%
  filter(category == "frpl_pct", school_name == "Total")

district_merged_df %>% 
  # Remove district totals
  filter(school_name != "Total") %>% 
  # x-axis will be the percentage of white students within schools
  ggplot(aes(x = wh_pct)) +
  geom_histogram(breaks = seq(0, 1, by = .1),
                 fill = dataedu_colors("darkblue")) +
  labs(title = "Count of Schools by White Population",
       x = "White Percentage",
       y = "Count") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.position = "none") + 
  theme_dataedu()

# Start to look at the distribution of student race groups in high-poverty schools
district_tidy_df %>% 
  filter(school_name == "Total", str_detect(category, "povsch")) %>% 
  ggplot(aes(x = reorder(category, -value), y = value)) +
  geom_bar(stat = "identity", aes(fill = factor(category))) +
  labs(title = "Distribution of Subgroups in High Poverty Schools",
       x = "Subgroup",
       y = "Percentage in High Poverty Schools") +
  scale_x_discrete(
    labels = c(
      "aa_povsch" = "African Am.",
      "wh_povsch" = "White",
      "hi_povsch" = "Hispanic",
      "as_povsch" = "Asian",
      "na_povsch" = "Native Am."
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_dataedu() +
  theme_dataedu() +
  theme(legend.position = "none")

# Reveal relationships by investigation the correlation between race and FRPL percentage by school
district_merged_df %>% 
  filter(school_name != "Total") %>% 
  ggplot(aes(x = wh_pct, y = frpl_pct)) +
  geom_point(color = dataedu_colors("green")) +
  geom_smooth(method = "lm",
               se = TRUE) +
  labs(title = "FRPL Percentage vs. White Percentage",
       x = "White Percentage",
       y = "FRPL Percentage") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  theme_dataedu() +
  theme(legend.position = "none")
  theme_dataedu()
  