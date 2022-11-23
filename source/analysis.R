library(tidyverse)
library(scales)


## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# loading county level data 
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# The following calculated data is to track the ratio of incarcerated individuals of multiple 
# racial groups in Los Angeles County 

# In the most recent year, what was the ratio of incarcerated AAPI individuals who were incarcerated in CA? First, make a column called ratio_aapi, and then pull data/

incarceration_data <- incarceration_data %>%
  drop_na() %>%
  mutate(ratio_aapi = aapi_jail_pop/aapi_pop_15to64)

ca_aapi_ratio <- incarceration_data %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  pull(ratio_aapi) 

mean_ca_aapi_ratio <- mean(ca_aapi_ratio)

# In the most recent year, what was the ratio of incarcerated Black individuals who were incarcerated in CA? First, make a column called ratio_black, and then pull data 
incarceration_data <- incarceration_data %>%
  drop_na() %>%
  mutate(ratio_black = black_jail_pop/black_pop_15to64)

ca_black_ratio <- incarceration_data %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  pull(ratio_black)

mean_ca_black_ratio <- mean(ca_black_ratio)

# In the most recent year, what was the ratio of incarcerated Latinx individuals who were incarcerated in CA?

incarceration_data <- incarceration_data %>%
  drop_na() %>%
  mutate(ratio_latinx = latinx_jail_pop/latinx_pop_15to64)

ca_latinx_ratio <- incarceration_data %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  pull(ratio_latinx)

mean_ca_latinx_ratio <- mean(ca_latinx_ratio)

# In the most recent year, what was the ratio of incarcerated Native individuals who were incarcerated in CA?

incarceration_data <- incarceration_data %>%
  drop_na() %>%
  mutate(ratio_native = native_jail_pop/native_pop_15to64)

ca_native_ratio <- incarceration_data %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  pull(ratio_native)

mean_ca_native_ratio <- mean(ca_native_ratio)

# In the most recent year, what was the ratio of incarcerated White individuals who were incarcerated in CA?

incarceration_data <- incarceration_data %>%
  drop_na() %>%
  mutate(ratio_white = white_jail_pop/white_pop_15to64)

ca_white_ratio <- incarceration_data %>%
  filter(state == "CA") %>%
  filter(year == max(year)) %>%
  pull(ratio_white)

mean_ca_white_ratio <- mean(ca_white_ratio)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#



# This function calculates the total_jail_pop per year in the United States
county_level_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
get_year_jail_pop <- function() {
  county_level_data %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(total_jail_pop_by_year = sum(total_jail_pop)) %>%
return()
}


# This function retruns a chart that graphs the year with the total_jail_pop per year
plot_jail_pop_for_us <- function()  {
  ggplot(get_year_jail_pop(), aes(as.numeric(year), total_jail_pop_by_year)) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") + 
    labs(y = "Total Jail Population", x = "Year") + 
    geom_col() + 
    scale_x_continuous(limits = c(1969, 2020)) + 
    scale_y_continuous(labels = comma)
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#


# returns a data frame with a given state, year, and total_jail population 

get_jail_pop_by_states <- function(states) {
  incarceration_data %>%
    filter(state %in% states) %>%
    group_by(year) %>%
    drop_na() %>%
    mutate(total_jail_pop_by_state = sum(total_jail_pop)) %>%
    select(year, state, total_jail_pop_by_state) %>%
return()
}

# returns a chart graphing the year and total_jail population for each state
plot_jail_pop_by_states <- function (states) {
  ggplot(get_jail_pop_by_states(states), aes(year, total_jail_pop_by_state, color = state)) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018) By State") + 
    labs (y = "Total Jail Population", x = "Year") +
    geom_line()
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# Find the percentage of incarcerated Black people from 2001 - 2013
black_pop_percent_over_time <- function () {
  incarceration_data %>%
  mutate(black_percent = black_pop_15to64 / total_pop_15to64) %>%
  drop_na(black_percent) %>%
  group_by(year) %>%
  summarize(black_percent = sum(black_percent) / length(black_percent)) %>%
  mutate(prop_change = (black_percent - lag(black_percent, n = 1)) * 100) %>%
  drop_na(prop_change)
}

# Find the percentage of incarcerated white people from 2001 - 2013
white_pop_percent_over_time <- function () {
  incarceration_data %>%
    mutate(white_percent = white_pop_15to64 / total_pop_15to64) %>%
    drop_na(white_percent) %>%
    group_by(year) %>%
    summarize(white_percent = sum(white_percent) / length(white_percent)) %>%
    mutate(prop_change = (white_percent - lag(white_percent, n = 1)) * 100) %>%
    drop_na(prop_change)
}

# Find the percentage of incarcerated Latinx people from 2001 - 2013
latinx_pop_percent_over_time <- function () {
  incarceration_data %>%
    mutate(latinx_percent = latinx_pop_15to64 / total_pop_15to64) %>%
    drop_na(latinx_percent) %>%
    group_by(year) %>%
    summarize(latinx_percent = sum(latinx_percent) / length(latinx_percent)) %>%
    mutate(prop_change = (latinx_percent - lag(latinx_percent, n = 1)) * 100) %>%
    drop_na(prop_change)
}


# Find the percentage of incarcerated AAPI people from 2001 - 2013
aapi_pop_percent_over_time <- function () {
  incarceration_data %>%
    mutate(aapi_percent = aapi_pop_15to64 / total_pop_15to64) %>%
    drop_na(aapi_percent) %>%
    group_by(year) %>%
    summarize(aapi_percent = sum(aapi_percent) / length(aapi_percent)) %>%
    mutate(prop_change = (aapi_percent - lag(aapi_percent, n = 1)) * 100) %>%
    drop_na(prop_change)
}

# Find the percentage of incarcerated Native people from 2001 - 2013
native_pop_percent_over_time <- function () {
  incarceration_data %>%
    mutate(native_percent = native_pop_15to64 / total_pop_15to64) %>%
    drop_na(native_percent) %>%
    group_by(year) %>%
    summarize(native_percent = sum(native_percent) / length(native_percent)) %>%
    mutate(prop_change = (native_percent - lag(native_percent, n = 1)) * 100) %>%
    drop_na(prop_change)
}



change_in_percentage_by_year <- function () {
  ggplot() +
  geom_line(
    data = black_pop_percent_over_time(), aes(x = year, y = prop_change),
    color = "blue"
  ) +
  geom_line(
    data = white_pop_percent_over_time(), aes(x = year, y = prop_change),
    color = "red"
  ) +
  geom_line(
      data = latinx_pop_percent_over_time(), aes(x = year, y = prop_change),
      color = "green"
    ) +
  geom_line(
    data = native_pop_percent_over_time(), aes(x = year, y = prop_change),
    color = "purple"
    ) +
  geom_line(
    data = aapi_pop_percent_over_time(), aes(x = year, y = prop_change),
    color = "orange"
    ) +
  labs(
    x = "Year", y = "Percent Change", color = "Race",
    title = "Change in Racial Proportion by Year"
  ) +
  scale_colour_manual("",
                      breaks = c("black", "white"),
                      values = c("white" = "red", "black" = "blue")
  )

}



## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 

black_state <- incarceration_data %>%
  group_by(state) %>%
  drop_na() %>%
  filter(year == max(year)) %>%
  select(black_jail_pop, total_pop) %>%
  mutate(total_pop / black_jail_pop) %>%
  summarize(
    pop = sum(black_jail_pop), total = max(total_pop),
    mutate = sum(total_pop / black_jail_pop)
  )

library(usmap)
map_of_black_incarceration <- plot_usmap(
  data = black_state, values = "pop", color = "black",
  name = "Black Jail Population"
) +
  # coord_fixed() +
  scale_fill_gradientn(
    colours = c("white", "blue"),
    breaks = c(10, 100, 1000, 10000),
    trans = "log10", name = "Black Jail Population"
  ) +
  labs(title = "The United States Black Jail Population in
       2018") +
  theme(legend.position = "right")






