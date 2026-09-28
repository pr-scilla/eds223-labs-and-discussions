# 1. Install packages

packages <- c("here", "janitor", "tidyverse", "sf", "terra", "tmap", "spData", "spDataLarge", "geodata", "kableExtra", "viridisLite")
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(here)
library(janitor)
library(tidyverse)
library(sf)
library(kableExtra)

# 2. Load data

# Read in csv file and name gdw_df. 
# "data" in the here() function points to where to access gdw.csv:

gdw_df <- read_csv(here("data", "gdw.csv")) |>
  clean_names() # convert variable names to lower snake case

# 3. Data exploration

print(nrow(gdw_df)) # returns number of rows
print(ncol(gdw_df)) # returns number of rows
dim(gdw_df) # prints number of rows and number of columns

# Show the first 10 rows of gdw_df and use kable() to make nice HTML tables:
head(gdw_df, n = 10) |> 
  kable() |> 
  kable_styling()

# Show the last 10 rows of gdw_df and use kable() to make nice HTML tables
tail(gdw_df, n = 10) |> 
  kable() |> 
  kable_styling()

# Print column names
names(gdw_df)

# 4. Index, Summarize, Subset Data

# Use indexing brackets to extract the gdw_df column 
# containing country names as a vector and data frame

countries_df <- gdw_df[ , "country"] # index by column named "country"
countries_df # returns a dataframe of country names that appear in the dataframe

countries_vector <- gdw_df[["country"]] # create a vector of the column named "country"
countries_vector # returns a vector of country names that appear in the dataframe

# Use group_by() and summarise() to find the number of dams by dam type in gdw_df
gdw_df |> 
  group_by(dam_type) |> 
  summarize(count=n()) |> # n() identifies that you want the total of dam_type
  ungroup()

# Make a subset called sub_dam that only contains entries for dam_type == "Dam"
sub_dam <- gdw_df |> 
  filter(dam_type == "Dam")
sub_dam

# Re-order gdw_df by ascending order of installation year
gdw_df <- arrange(gdw_df, year_dam)
gdw_df

# 5. Data Visualization
# Make a bar graph of average height of dam/barrier in meters (dam_hgt_m) by country
gdw_df |>
  group_by(country) |>
  summarize(mean_dam_hgt_m = mean(dam_hgt_m, na.rm = TRUE)) |>
  ungroup() |> 
  ggplot(aes(x = country, y = mean_dam_hgt_m)) +
    geom_bar(stat = "identity") +
    labs(x = "Country",
         y = "Average height of dam/barrier in meters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Make a scatterplot of height of dam/barrier versus representative maximum storage capacity 
# of reservoir in million cubic meters (cap_mcm)
ggplot(data = gdw_df,
      aes(x = cap_mcm, y = dam_hgt_m)) +
  geom_point() +
  labs(x = "Storage capacity of reservoir in million cubic meters",
       y = "Height of dam/barrier in meters") +
  theme_minimal()
