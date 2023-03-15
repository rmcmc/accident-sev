## Purpose: Data cleaning for assesed project
## Authors: Group 7
## Date: 2023-02-19

## load packages and data---------------------------------------------------
pacman::p_load(janitor, tidyverse, lubridate, finalfit, patchwork, kableExtra)

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

labs <- readxl::read_xlsx("data/Road-Safety-Open-Dataset-Data-Guide.xlsx") |> clean_names() |> 
  ## recode NA values if missing 
  mutate(label = case_when(code_format == "-1" ~ NA,
                           str_detect(label, "nknown| self") ~ NA,
                           .default = label),
         code_format = as.numeric(code_format))
accidents_raw <- read_csv("dft-road-casualty-statistics-accident-2020.csv")
vehicles_raw <- read_csv("dft-road-casualty-statistics-vehicle-2020.csv")  
casualties_raw <- read_csv("dft-road-casualty-statistics-casualty-2020.csv")

# clean accidents dataset  --------------------------------------------------------------
accidents_clean <- accidents_raw |> 
  # keep columns likely to be of use
  select(accident_reference, date, time, speed_limit, accident_severity,  
         road_surface_conditions, urban_or_rural_area, trunk_road_flag) |> 
  mutate(
    # recode missing data as NA
    across(where(is.numeric), ~na_if(.x, -1)),
    # add labels
    across(c(accident_severity:trunk_road_flag), label_fn), 
    # convert  date to date format
    date = dmy(date), 
    # recode fatal and severe as severe - set slight as reference group
    accident_severity = ifelse(accident_severity == "Slight", "Slight", "Severe") |> 
      fct_relevel("Slight"),
    # create categorical variable for time of accident
    time_of_day = case_when(
      between(hour(time), 7,9) ~ "7-10am", 
      between(hour(time), 10,15) ~ "10am-4pm",
      between(hour(time), 16,18) ~ "4-7pm",
      between(hour(time), 19,22) ~ "7-11pm",
      .default = "11pm-7am")|> 
      factor(levels =  c("7-10am", "10am-4pm", "4-7pm", "7-11pm", "11pm-7am")),
    # road surface conditions to three categories - dry, wet and other
    road_surface_conditions = case_when(
      str_detect(road_surface_conditions, "Dry|Wet") ~ road_surface_conditions, 
      is.na(road_surface_conditions)~ NA,
      .default = "Other") |>  factor(),
    # small number of unallocated road - change to NA
    urban_or_rural_area = na_if(urban_or_rural_area, "Unallocated") |> 
      # set urban as referenec group
      fct_relevel("Urban"),
    trunk_road_flag = ifelse(trunk_road_flag == "Non-trunk", "Non-trunk", "Trunk") |> 
      fct_rev()) |> 
  rename(trunk_road = trunk_road_flag)

glimpse(accidents_clean)
## check counts of variables 
map(accidents_clean[, -c(1:3)], tbl_fn)

# clean_vehicles dataset -----------------------------------------------------
vehicles_clean <- vehicles_raw |> 
  select(accident_reference,age_of_vehicle, 
         age_of_driver, vehicle_type,  sex_of_driver) |> 
  mutate(
    # recode missing data as NA
    across(where(is.numeric), ~na_if(.x, -1)),
    across(c(vehicle_type:sex_of_driver ), label_fn)) |> 
  # collapse vehicle type to fewer categories
  mutate(vehicle_type = case_when(
    str_detect(vehicle_type, "car|Car") ~ "car",
    str_detect(vehicle_type, "Van|Goods|Bus|bus") ~ "heavy_vehicle",
    str_detect(vehicle_type, "Motor|motor") ~"motor",
    str_detect(vehicle_type, "Pedal") ~"pedal_cycle",
    .default = "other"), 
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
    # check if car involved and similar for other vehicle types
    vehicle_car = any(vehicle_type == "car"),
    vehicle_heavy = any(vehicle_type == "heavy_vehicle"), 
    vehicle_other = any(vehicle_type == "other"),
    vehicle_motorcycle = any(vehicle_type == "motor"),
    vehicle_pedal_cycle = any(vehicle_type == "pedal_cycle"),
    # check if female driver involved in accident but not in pedal cycle or other vehicle
    vehicle_female_driver = !vehicle_pedal_cycle & !vehicle_other & any(sex_of_driver == "Female"),
    ## check if driver under 25 but not in pedal cycle or other vehicle
    vehicle_driver_age_under_25 = !vehicle_pedal_cycle & !vehicle_other & vehicle_driver_min_age <26) |> 
  ungroup() |> 
  select(-c(age_of_vehicle:vehicle_driver_min_age)) |> 
  distinct()

# check cleaned vehicles dataframe
glimpse(vehicles_clean)
# check which variable counts
map(vehicles_clean[, -c(1:2)], tbl_fn)

# clean casualties dataset--------------------------------------------------------
glimpse(casualties_raw)
casualties_clean <- casualties_raw |> 
  select(accident_reference, casualty_type) |> 
  mutate(casualty_type = label_fn(casualty_type), 
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
         casualty_other_vehicle = any(casualty_type2 == "All other vehicles")) |> 
  ungroup() |> 
  select(-casualty_type2, -casualty_type) |> 
  distinct()

glimpse(casualties_clean)
map(casualties_clean[,-1], tbl_fn)

# final dataframe ---------------------------------------------------------
dat <- left_join(accidents_clean, vehicles_clean) |> 
  left_join(casualties_clean) |> 
  mutate(
    across(where(is.logical), ~ifelse(.x, "Yes", "No")),
    across(where(is.character), factor))

nrow(dat) == nrow(accidents_raw)

# EDA ---------------------------------------------------------------------
glimpse(dat)
# descriptive table for accidents 
names(dat)

(expl_vars <- names(dat)[-c(1:3,5)])

table1 <- summary_factorlist(dat, "accident_severity", expl_vars, 
                             na_include = TRUE, column = FALSE, 
                             total_col = T, 
                             cont = "median") |> 
  mutate(label = gsub("_", " ", label) |> str_to_sentence(),
         Total = gsub("(100)", "", Total, fixed = T)) |> 
  rename(Variable = label)

kbl(table1) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) 


## accidents by speed limit 
a <- dat |> 
  drop_na(speed_limit) |> 
  ggplot() + 
  geom_bar(aes(factor(speed_limit), fill = accident_severity), position = "dodge") +
  labs(x = "Speed limit (mph)", y = "Number of accidents", 
       fill = "Accident severity")+
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

b <- dat |> 
  drop_na(speed_limit) |> 
  ggplot() + 
  geom_bar(aes(factor(speed_limit), fill = accident_severity), position = "fill") +
  labs(x = "Speed limit", y = "Proportion of accidents", 
       fill = "Accident severity" ) +
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

a + b+ plot_layout(guides = "collect") & theme(legend.position = 'top')


## vehicles involved in accidents
vehicle_dat <- dat |> 
  select(accident_reference, accident_severity, 
         vehicle_car:vehicle_pedal_cycle) |> 
  pivot_longer(!c(accident_severity, accident_reference), names_to = "vehicle",
               names_pattern = "_(.*)") |> 
  filter(value == "Yes") |> 
  mutate(vehicle = case_match(vehicle,
                              "car" ~ "Car",
                              "heavy" ~ "Heavy vehicle",
                              "motorcycle" ~ "Motorcyle",
                              "other" ~ "Other vehicle",
                              "pedal_cycle" ~ "Pedal cycle") |> 
           fct_infreq()) 

d <- ggplot(vehicle_dat) +
  geom_bar(aes(vehicle, fill = accident_severity), position = "dodge") +
  labs(x = "Vehicle type", y = "Number of accidents", 
       fill = "Accident severity")+
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

e <- ggplot(vehicle_dat) +
  geom_bar(aes(vehicle, fill = accident_severity), position = "fill") +
  labs(x = "Vehicle type", y = "Number of accidents", 
       fill = "Accident severity")+
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

d + e+ plot_layout(guides = "collect") & theme(legend.position = 'top')

## casualties involved in accidents 
casualty_dat <- dat |> 
  select(accident_reference, accident_severity, 
         casualty_cyclist:casualty_other_vehicle) |> 
  pivot_longer(!c(accident_severity, accident_reference), names_to = "casualty",
               names_pattern = "_(.*)") |> 
  filter(value == "Yes") |> 
  mutate(casualty = case_match(casualty,
                               "car_occupant" ~ "Car occupant",
                               "cyclist" ~ "Cyclist",
                               "motorcycle" ~ "Motorcyle rider",
                               "other_vehicle" ~ "Other vehicle occupant",
                               "pedestrian" ~ "Pedestrian") |> 
           fct_infreq()) 

f <- ggplot(casualty_dat) +
  geom_bar(aes(casualty, fill = accident_severity), position = "dodge") +
  labs(x = "Casualty type", y = "Number of accidents", 
       fill = "Accident severity")+
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

g <- ggplot(casualty_dat) +
  geom_bar(aes(casualty, fill = accident_severity), position = "fill") +
  labs(x = "Casualty type", y = "Number of accidents", 
       fill = "Accident severity")+
  scale_fill_manual(values = c( "palegreen3", "violetred4")) +
  theme_classic()

f+ g+ plot_layout(guides = "collect") & theme(legend.position = 'top')

# MVA ---------------------------------------------------------------------
## this take 2-3 minutes to run
mva1 <- finalfit(dat, "accident_severity", expl_vars) 

## model fit 
null_model <- glm(accident_severity ~1, family = binomial, dat)
mod1 <- glm(reformulate(expl_vars, "accident_severity"), family = binomial, dat)

# get lrt p-values
x <- drop1(mod1, test = "Chi") 
lrt_p <- tibble(Variable = rownames(x), 
                lrt = p_tidy(x$`Pr(>Chi)`, digits = 3, prefix = NULL))

mva_clean <- mva1 |> 
  ff_remove_p() |> 
  rename(Variable = `Dependent: accident_severity`) |> 
  select(-Slight,  -Severe) |> 
  left_join(lrt_p, join_by(Variable)) |> 
  mutate(Variable = gsub("_", " ", Variable), 
         `OR (univariable)` =  ifelse(`OR (univariable)` == "-", "Reference", `OR (univariable)`), 
         `OR (multivariable)` =  ifelse(`OR (multivariable)` == "-", "", `OR (multivariable)`),
         lrt = replace_na(lrt, "")) |> 
  #fill(lrt, .direction = "down") |> 
  rename(Levels = " ", 
         "LRT p-value" = lrt) |> 
  mutate(Variable = gsub("_", " ", Variable) |> str_to_sentence())  

kbl(mva_clean) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) 

tidy_mod <-  broom::tidy(mod1, exponentiate = T, conf.int = T) 

## odds ratio plot
tidy_mod |> 
  filter(term != "(Intercept)") |> 
  mutate(term = tolower(term) %>% 
           gsub("_", " ", .) %>%
           gsub("day", "day: ", .) %>%
           gsub("tions", "tions: ", .) %>%
           gsub("alty", "alty: ", .) %>%
           gsub("road", "road: ", .) %>%
           gsub("area", "area: ", .) %>%
           gsub("^vehicle", "vehicle: ", .) %>%
           gsub("yes", "", ., fixed = T) |> 
           str_to_sentence() |> 
           fct_reorder(estimate)) |> 
  ggplot(aes(y = term)) +
  geom_point(aes(x = estimate,color = ifelse(estimate>1,  "violetred4","palegreen4")), size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high, 
                    color = ifelse(conf.low>1, "violetred4","palegreen4")), width = 0.3) +
  scale_x_continuous(trans = 'log2',
                     limits = c(0.125, 16),
                     breaks = c(0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
                     labels = function(x) sprintf("%g", x)) +
  scale_color_identity()+
  labs(x = "Adjusted odds ratio", y = NULL)+
  geom_vline(xintercept = 1, linetype = "solid", color = "blue")+
  theme_minimal()

# predicted probabilities  ------------------------------------------------
dat2 <- dat |> 
  select(all_of(expl_vars), accident_severity) |> 
  drop_na()

dat3 <- cbind(dat2, predict(mod1, dat2, type = "link", se.fit = TRUE)) |> 
  mutate(predicted = plogis(fit), 
         lci = plogis(fit - (qnorm(0.975) * se.fit)),
         uci = plogis(fit + (qnorm(0.975) * se.fit)))

## speed limit - clearly not linear! 
dat3 |>
  summarise(p = mean(predicted), 
            lci = mean(lci),
            uci = mean(uci), .by = speed_limit) |> 
  ggplot() +
  geom_line(aes(speed_limit,p), color =  "violetred4") +
  geom_ribbon(aes(speed_limit, ymin = lci, ymax = uci), fill =  "violetred4", alpha = 0.1) +
  theme_classic() +
  labs(x = "Speed limit (mph)", y = "Predicted probability")

## probabilities for all other variables
gg <- \(var){
  lbl <- gsub("_", " ", var) |> str_to_sentence()
  var <- sym(var)
  dat3 |>
    summarise(p = mean(predicted), 
              lci = mean(lci),
              uci = mean(uci), .by = var) |> 
    ggplot() +
    geom_point(aes_string(var, "p", color = var), size = 3) +
    geom_errorbar(aes_string(var, ymin = "lci", ymax = "uci", color = var), width = 0.05, linewidth = 1) +
    theme_classic() +
    theme(legend.position = "none")+
    labs(x = lbl, y = "Predicted probability")
}


pred_list <- map(expl_vars[-c(1,6)], gg)

## show important ones only in main report 
wrap_plots(pred_list[c(2,4,10, 5, 8:9,12, 14, 15)], nrow = 3, ncol = 3)

## all discrete variables 
wrap_plots(pred_list[1:9], nrow = 3, ncol = 3)
wrap_plots(pred_list[10:16], nrow = 3, ncol = 3)

## check prob for trunk road as looks odd
dat3 |>
  summarise(p = mean(predicted), 
            lci = mean(lci),
            uci = mean(uci), .by = trunk_road) 

# model fit ---------------------------------------------------------------
## pseudo-R squared
(logLik(null_model)-logLik(mod1))/logLik(null_model)

## model fit - ok!
pchisq(deviance(mod1),mod1$df.residual, lower.tail = F)

## quadratic term needed for speed limit
mod2 <- update(mod1, .~. + I(speed_limit^2))
library(lmtest)
lrtest(mod1, mod2)
## model fit is not much better
(logLik(null_model)-logLik(mod1))/logLik(null_model)
(logLik(null_model)-logLik(mod2))/logLik(null_model)

## cubic term not needed 
mod3 <- update(mod2, .~. + I(speed_limit^3))
lrtest(mod2, mod3)

