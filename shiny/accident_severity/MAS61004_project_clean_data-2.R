## Purpose: Data cleaning for assesed project
## Authors: Group 7
## Date: 2023-02-19

## load packages and data---------------------------------------------------
#pacman::p_load(janitor, tidyverse, lubridate, finalfit)

library(tidyverse)
library(lubridate)
library(stringr)
library(janitor)
library(finalfit)



# function to replace values in df with labels from reference doc
label_fn <- \(var){
  varname <- deparse(substitute(var))
  v <- labs[labs$field_name == varname,]
  v$label[match({{var}}, v$code_format)]
}

# function to produce table with % for one variable
tbl_fn <- \(x){
  x |>  
    tabyl() |> 
    adorn_totals() |> 
    adorn_pct_formatting() 
}

labs <- readxl::read_xlsx("Road-Safety-Open-Dataset-Data-Guide.xlsx") |> clean_names() |> 
  ## recode NA values if missing 
  mutate(label = case_when(code_format == "-1" ~ NA,
                           str_detect(label, "nknown| self") ~ NA,
                           .default = label),
         code_format = as.numeric(code_format))
accidents_raw <- read.csv("dft-road-casualty-statistics-accident-2020.csv.xls")
vehicles_raw <- read.csv("dft-road-casualty-statistics-vehicle-2020.csv.xls")  
casualties_raw <- read.csv("dft-road-casualty-statistics-casualty-2020.csv.xls")

# clean accidents dataset  --------------------------------------------------------------
accidents_clean <- accidents_raw |> 
  # remove columns unlikely to be of use
  select(-c(accident_index, accident_year, 
            contains("loca"),
            second_road_class, second_road_number,
            first_road_number, police_force,
            did_police_officer_attend_scene_of_accident)) |> 
  mutate(
    # recode missing data as NA
    across(where(is.numeric), ~na_if(.x, -1)),
    # add labels
    across(c(junction_detail:trunk_road_flag, 
             accident_severity, day_of_week,
             first_road_class, road_type), label_fn), 
    # convert  date to date format
    date = dmy(date), 
    # recode fatal and severe as severe - set slight as reference group
    accident_severity = ifelse(accident_severity == "Slight", "Slight", "Severe") |> 
      fct_relevel("Slight"),
    # create categorical variable for time of accident
    time_of_day = case_when(between(hour(hm(time)), 0, 6) ~ "night_off_peak",
                            between(hour(hm(time)), 7,9) ~ "morning_peak", # 7-10AM
                            between(hour(hm(time)), 10,15) ~ "day_off_peak",
                            between(hour(hm(time)), 16,18) ~ "evening_peak",# 4-7PM
                            between(hour(hm(time)), 19,23) ~ "night_off_peak"),
    # should speed be factor or continuous variable?
    speed_limit_cat = factor(speed_limit), 
    road_surface_conditions = case_when(
      str_detect(road_surface_conditions, "Dry|Wet") ~ road_surface_conditions, 
      is.na(road_surface_conditions)~ NA,
      .default = "Other"),
    light_conditions = case_when(str_detect(light_conditions, "Dayl") ~ light_conditions,
                                 is.na(light_conditions) ~ NA,
                                 .default = "Darkness") |> fct_relevel("Daylight"),
    weather_conditions = case_when(str_detect(weather_conditions, "Fine no") ~ "Fine",
                                   str_detect(weather_conditions, "Raining") ~ "Rain", 
                                   str_detect(weather_conditions, "Unkn") ~ NA, 
                                   is.na(weather_conditions) ~ NA,
                                   .default = "Other"), 
    first_road_class = case_when(
      str_detect(first_road_class, "M") ~ "Motorway",
      str_detect(first_road_class, "A") ~ "A",
      str_detect(first_road_class, "B|C") ~ "B/C",
      .default = "Unclassified"), 
    # small number of unallocated road - change to NA
    urban_or_rural_area = na_if(urban_or_rural_area, "Unallocated") |> 
      # set urban as referenec group
      fct_relevel("Urban"))

#glimpse(accidents_clean)
## check counts of variables 
map(accidents_clean[, -c(1:3, 5:7, 9, 12)], tbl_fn)

## remove variable not likely to be useful 
accidents_final <- accidents_clean |> 
  select(-c(pedestrian_crossing_human_control, 
            pedestrian_crossing_physical_facilities, 
            special_conditions_at_site, junction_detail, 
            junction_control, first_road_class, 
            carriageway_hazards))


# clean_vehicles dataset -----------------------------------------------------
vehicles_raw2 <- vehicles_raw |> 
  relocate(c(age_of_driver, age_of_vehicle, 
             generic_make_model,
             engine_capacity_cc), .after = accident_reference) |> 
  mutate(
    # recode missing data as NA
    across(where(is.numeric), ~na_if(.x, -1)),
    across(c(vehicle_reference:driver_home_area_type ), label_fn))

## check if correctly labelled 
#glimpse(vehicles_raw2)

# check which variables are of use
map(vehicles_raw2[, -c(1:7)], tbl_fn)


#library(tidytable) # use for fast group_by operations 
vehicles_clean <- vehicles_raw2 |> 
  # select cols likely to be useful
  select(accident_reference, age_of_driver, age_of_vehicle,
         sex_of_driver, age_band_of_driver, vehicle_type) |> 
  # collapse vehicle type to fewer categories
  mutate(vehicle_type = case_when(
    str_detect(vehicle_type, "car|Car") ~ "car",
    str_detect(vehicle_type, "Van|Goods|Bus|bus") ~ "heavy_vehicle",
    str_detect(vehicle_type, "Motor|motor") ~"motor",
    .default = vehicle_type ) |> 
      # lump to few categories and change to character for next steps
      fct_lump(4) |> as.character(), 
    # change unknown sex of drvier to NA
    sex_of_driver = na_if(sex_of_driver, "Not known")) |> 
  ## create single variable for each accident reference group
  group_by(accident_reference) |> 
  mutate(
    # minimum age of driver involved in accident
    # note: age of driver is NA for some vehicles 
    vehicle_driver_min_age = ifelse(any(!is.na(age_of_driver)), 
                                    min(age_of_driver, na.rm = T), NA), 
    # similarly identify maximum age of vehicle involved in accident
    vehicle_max_age = ifelse(any(!is.na(age_of_vehicle)), 
                             max(age_of_vehicle, na.rm = T), NA),
    # check if male driver involved in accident
    vehicle_male_driver = any(sex_of_driver == "Male"),
    # check if female driver involved in accident
    vehicle_female_driver = any(sex_of_driver == "Female"),
    # check if car involved and similar for other vehicle types
    vehicle_car = any(vehicle_type == "car"),
    vehicle_heavy = any(vehicle_type == "heavy_vehicle"), 
    vehicle_other = any(vehicle_type == "Other"),
    vehicle_motorcyle = any(vehicle_type == "motor"),
    vehicle_pedal_cycle = any(vehicle_type == "Pedal cycle")) |> 
  ungroup() 

# check cleaned vehicles dataframe
#glimpse(vehicles_clean)
# check which variable counts
map(vehicles_clean[, -c(1:3, 7:8)], tbl_fn)

# create df with one row per accident 
vehicles_distinct <- vehicles_clean |> 
  select(-c(age_of_driver:vehicle_type)) |> 
  distinct()


# clean casualties dataset--------------------------------------------------------
#glimpse(casualties_raw)
casualties_clean <- casualties_raw |> 
  select(-accident_index, -accident_year) |> 
  mutate(
    # recode missing data as NA
    across(where(is.numeric), ~na_if(.x, -1)),
    across(c(casualty_class:casualty_imd_decile & !age_of_casualty), label_fn), 
    # identify vehicle of casualty - to five categories
    # car, cycle, pedestrian, motorcycle and all other vehicles
    casualty_type2 = case_when(
      str_detect(casualty_type, "Car|Cycl|Pede") ~ casualty_type, 
      str_detect(casualty_type, "Motor") ~ "Motorcycle",
      str_detect(casualty_type, "car") ~ "Car occupant",
      .default = "All other vehicles")) |> 
  group_by(accident_reference) |> 
  ## identify which category involved in each accident 
  mutate(casualty_cyclist = any(casualty_type2 == "Cyclist"),
         casualty_car_occupant = any(casualty_type2 == "Car occupant"),
         casualty_motorcycle = any(casualty_type2 == "Motorcycle"),
         casualty_pedestrian = any(casualty_type2 == "Pedestrian"),
         casualty_other_vehicle = any(casualty_type2 == "All other vehicles"), 
         casualty_min_age = ifelse(any(!is.na(age_of_casualty)), 
                                   min(age_of_casualty, na.rm = T), NA)) |> 
  ungroup()

#glimpse(casualties_clean)
map(casualties_clean[, -c(1:3, 6, 23)], tbl_fn)

casualties_distinct <- casualties_clean |> 
  select(accident_reference, casualty_cyclist:casualty_min_age) |> 
  distinct()


# final dataframe ---------------------------------------------------------
dat <- left_join(accidents_final, vehicles_distinct) |> 
  left_join(casualties_distinct) |> 
  mutate(across(where(is.character), factor),
         across(where(is.logical), factor),
         driver_u25 = vehicle_driver_min_age <26)

nrow(dat) == nrow(accidents_raw)

# EDA ---------------------------------------------------------------------
#glimpse(dat)
# descriptive table for accidents 
names(dat)

(expl_vars <- names(dat)[c(11,14,15,17,20, 28:32,34)])

table1 <- summary_factorlist(dat, "accident_severity", expl_vars, 
                             include_row_missing_col = TRUE,
                             na_include = TRUE, column = FALSE, 
                             total_col = T, add_row_total = TRUE,
                             cont = "median") 


# MVA ---------------------------------------------------------------------
# remove some vars for MVA 
# drop day of the week as probably not worthwhile 
# keep trunk road instead of road type
# keep speed limit as continuous 
# keep light conditions instead of time of day 
# keep road surface conditions instead of weather conditions
# casualty_min_age probably not useful as children would come under pedestrians

## this take 2-3 minutes to run
#table2 <- finalfit(dat2, "accident_severity", mva_vars) 

# or you can do just MVA using glm function but it still takes 2-3 mins 
#glm(reformulate(expl_vars, "accident_severity"),  
#           family = binomial, dat)



dat%>%
  select( append(expl_vars, "accident_severity"))%>%
  write.csv('./shiny_df.csv')



# https://www.gov.uk/government/statistics/reported-road-casualties-great-britain-annual-report-2020/reported-road-casualties-great-britain-annual-report-2020
