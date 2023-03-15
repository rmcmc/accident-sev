# Group 7 shiny app

# App code has been checked with styler and linted
# to conform with the tidyverse styleguide

# Note to the examiner - a working version of this app can be found at
# https://shef-uni-projects.shinyapps.io/accident_severity/
# The source script, takes the 4 datafiles and converts them to a single
# clean and reduced file

# Load packages
library(shiny)
library(tidyverse)
library(lubridate)

################################################
##### Load, prep, fit and create baseline ######
################################################

# Generate the code
#source("MAS61004_project_clean_data-2.R")

df <- read.csv("./shiny_df.csv") %>%
  mutate(
    accident_severity = case_when(
      accident_severity == "Slight" ~ 0,
      accident_severity == "Severe" ~ 1
    )
  ) %>%
  select(!X)

mdl <- glm(accident_severity ~ ., data = df, family = binomial())

mdl_summary <- summary(mdl)


# Baseline predictions
speeds <- seq(10, 90, 1)
n_samples <- length(speeds)
zeros_vector <- rep(0, n_samples)

# Baseline dataset
baseline_new_data <- list(
  speed_limit = speeds, #
  speed_limit_squared = speeds^2, #
  road_surface_conditions = rep("Dry", n_samples), #
  trunk_road = rep("Trunk", n_samples), #
  urban_or_rural_area = rep("Urban", n_samples), #
  time_of_day = rep("7-10am", n_samples), #
  vehicle_max_age = rep(5, n_samples), #
  vehicle_car = rep('No', n_samples), #
  vehicle_heavy = rep('No', n_samples),#
  vehicle_other = rep('No', n_samples),#
  vehicle_motorcycle = rep('No', n_samples),#
  vehicle_pedal_cycle = rep('No', n_samples),#
  vehicle_female_driver = rep('No', n_samples),
  vehicle_driver_age_under_25 = rep('No', n_samples),
  casualty_cyclist= rep('No', n_samples),
  casualty_car_occupant= rep('No', n_samples),
  casualty_motorcycle= rep('No', n_samples),
  casualty_pedestrian= rep('No', n_samples),
  casualty_other_vehicle = rep('No', n_samples)
)

# Predict probsba
baseline_pred <- predict(
  mdl,
  type = "response",
  newdata = baseline_new_data,
  se.fit = TRUE
)


baseline_data <-
  tibble(
    speeds = speeds,
    pred = baseline_pred$fit,
    upper_ci = baseline_pred$fit + 1.96 * baseline_pred$se.fit,
    lower_ci = baseline_pred$fit - 1.96 * baseline_pred$se.fit
  )

side_panel_width <- 3

### SIDEBARS
curves_side <-
  sidebarPanel(
    width = side_panel_width,
    checkboxInput("show_baseline", "Show Baseline Model", value = FALSE),
    checkboxInput("show_ci", "Show 95% CI", value = TRUE),
    selectInput("road_conditions",
      "Road Surface Condtions",
      c(
        "Dry" = "Dry",
        "Wet or Damp" = "Wet or damp"
      ),
      multiple = FALSE,
      selected = "Dry"
    ),
    selectInput("urban_or_rural",
      "Urban or Rural",
      c(
        "Urban" = "Urban",
        "Rural" = "Rural"
      ),
      multiple = FALSE,
      selected = "Urban"
    ),
    selectInput("trunk_road",
                "Trunk Road",
                c(
                  "Not Trunk Road" = "Non-trunk",
                  "Trunk Road" = "Trunk"
                ),
                multiple = FALSE,
                selected = "trunk_road"
    ),
    selectInput("time_of_day",
      "Time of Day",
      c(
        "Peak - Morning (7AM - 10AM)" = "7-10am",
        "Peak - Evening (4PM - 7PM)" = "4-7pm",
        "Off Peak - Day (10AM - 4PM)" = "10am-4pm",
        "Off Peak - Evening (11PM - 7AM)"= "7-11pm",
        "Off Peak - Night (11PM - 7AM)" = "11pm-7am"
      ),
      multiple = FALSE,
      selected = "7-10am"
    ),
    sliderInput("max_age",
      "Age of Oldest Vehicle Involved:",
      min = 0, max = 30,
      value = 5
    ),
    selectInput("cyclist_casualty",
                "Cyclist Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("pedestrian_casualty",
                "Pedestrian Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("motorbike_casualty",
                "Motorcycle Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("u25_driver",
                "Driver Under 25 Involved",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    )
  )

################################################
######### Create Siderbars for shiny ###########
################################################

calc_side <-
  sidebarPanel(
    width = side_panel_width,
    numericInput(
      "speed_calc",
      "Speed Limit (MPH)",
      30,
      min = 10,
      max = 100,
      step = 1
    ),
    selectInput("road_conditions_calc",
                "Road Surface Condtions",
                c(
                  "Dry" = "Dry",
                  "Wet or Damp" = "Wet or damp"
                ),
                multiple = FALSE,
                selected = "Dry"
    ),
    selectInput("urban_or_rural_calc",
                "Urban or Rural",
                c(
                  "Urban" = "Urban",
                  "Rural" = "Rural"
                ),
                multiple = FALSE,
                selected = "Urban"
    ),
    selectInput("trunk_road_calc",
                "Trunk Road",
                c(
                  "Not Trunk Road" = "Non-trunk",
                  "Trunk Road" = "Trunk"
                ),
                multiple = FALSE,
                selected = "Urban"
    ),
    selectInput("time_of_day_calc",
                "Time of Day",
                c(
                  "Peak - Morning (7AM - 10AM)" = "7-10am",
                  "Peak - Evening (4PM - 7PM)" = "4-7pm",
                  "Off Peak - Day (10AM - 4PM)" = "10am-4pm",
                  "Off Peak - Evening (11PM - 7AM)"= "7-11pm",
                  "Off Peak - Night (11PM - 7AM)" = "11pm-7am"
                ),
                multiple = FALSE,
                selected = "7-10am"
    ),
    sliderInput("max_age_calc",
                "Age of Oldest Vehicle Involved:",
                min = 0, max = 30,
                value = 5
    ),
    selectInput("cyclist_casualty_calc",
                "Cyclist Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("pedestrian_casualty_calc",
                "Pedestrian Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("motorbike_casualty_calc",
                "Motorcycle Casualty",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    selectInput("u25_driver_calc",
                "Driver Under 25 Involved",
                c(
                  "No" = "No",
                  "Yes" = "Yes"
                ),
                multiple = FALSE,
                selected = "No"
    ),
    actionButton("pred_and_append", "Predict")
  )


################################################
##################  Build UI ###################
################################################


ui <- fluidPage(
  titlePanel("Accident Severity Tool"),
  tabsetPanel(
    tabPanel(
      "Curve Plots",
      fluid = TRUE,
      sidebarLayout(
        curves_side,
        mainPanel(
          plotOutput("logReg", height = "500px"),
          htmlOutput("plot_description")
        )
      )
    ),
    tabPanel(
      "Calulate",
      fluid = TRUE,
      sidebarLayout(
        calc_side,
        mainPanel(
          DT::DTOutput("table", width = "110%")
        )
      )
    ),
    tabPanel(
      "About",
      fluid = TRUE,
      mainPanel(
        htmlOutput("info")
      )
    )
  )
)

################################################
########## Create static plot HTML #############
################################################

html_plot_description <-
  "
      <h2>Plot Details</h2>

      This chart demons

      The baseline model is fixed and represents the probability if the crash:

      <ul>
        	<li>Does not involve a pedestrian</li>
        	<li>Occurs during the hours of 6am-11pm</li>
      </ul>

      "

about_html <-
  "
        <h1>How to Use This App</h1>

        <p>This application contains two tools based on the same statistical model.
        The plot gives a visual indication of how crash severity changes with
        speed limit</p>

        <h2>Background</h2>

        <p>This application has been developed to help understand the 
        a part of the investigation into
        crash severivity in the UK</p>

        <h2>Data Sources</h2>

        <p>The data for this app was taken from the data.gov.uk portal for road
        traffic saftey. 2020 datasets have been amalgamated.</p>


        <h2>Model</h2>


        <h2>Plot A</h2>

        <p>This plot compares the probbaility of a severe 
        crash against speed for various
        different crash conditon.</p>

        <ul>
        	<li>Bullet A</li>
        	<li>Bullet&nbsp;<strong>Bold!</strong></li>
        </ul>


        "

################################################
########### Create plotting server #############
################################################

server <- function(input, output) {
  output$plot_description <- renderText(
    html_plot_description
  )

  output$logReg <- renderPlot({
    newdata <- list(
      speed_limit = speeds, 
      speed_limit_squared = speeds^2,
      road_surface_conditions = rep(input$road_conditions, n_samples),
      trunk_road = rep(input$trunk_road, n_samples),
      urban_or_rural_area = rep(input$urban_or_rural, n_samples),
      time_of_day = rep(input$time_of_day, n_samples),
      vehicle_max_age = rep(input$max_age, n_samples),
      casualty_cyclist = rep(input$cyclist_casualty, n_samples),
      casualty_motorcycle= rep(input$motorbike_casualty, n_samples),
      casualty_pedestrian= rep(input$pedestrian_casualty, n_samples),
      vehicle_driver_age_under_25 = rep(input$u25_driver, n_samples),
      vehicle_car = rep('No', n_samples), 
      vehicle_heavy = rep('No', n_samples),
      vehicle_other = rep('No', n_samples),
      vehicle_motorcycle = rep('No', n_samples),
      vehicle_pedal_cycle = rep('No', n_samples),
      vehicle_female_driver = rep('No', n_samples),
      casualty_other_vehicle = rep('No', n_samples),
      casualty_car_occupant= rep('No', n_samples)
    )
    
    
    
    predicted_vals <- predict(
      mdl,
      type = "response",
      newdata = newdata,
      se.fit = TRUE
    )

    results <-
      tibble(
        speeds = speeds,
        pred = predicted_vals$fit,
        upper_ci = predicted_vals$fit + 1.96 * predicted_vals$se.fit,
        lower_ci = predicted_vals$fit - 1.96 * predicted_vals$se.fit
      )


    main_plot <- ggplot(data = results, aes(x = speeds, y = pred)) +
      geom_line(color = "red") +
      scale_x_continuous(breaks = seq(0, 100, 10), limits = c(10, 90)) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
      labs(
        title = "Probability Plot", x = "Speed Limit (MPH)",
        y = "Probability of Severe Accident"
      ) +
      theme(
        axis.text = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 16, face = "bold")
      )


    if (input$show_ci) {
      main_plot <-
        main_plot +
        geom_ribbon(
          data = results,
          aes(ymin = lower_ci, ymax = upper_ci),
          alpha = 0.5,
          fill = "red"
        )
    }

    if (input$show_baseline) {
      main_plot <-
        main_plot +
        geom_line(data = baseline_data, color = "darkgrey")

      if (input$show_ci) {
        main_plot <-
          main_plot +
          geom_ribbon(
            data = baseline_data,
            aes(ymin = lower_ci, ymax = upper_ci),
            alpha = 0.5,
            fill = "darkgrey"
          )
      }
    }

    main_plot
  })

  reactive_results <-
    reactiveValues(
      table_data = tibble(
        Speed = numeric(),
        `Road Conditions` = character(),
        `Trunk Road` = character(),
        `Urban or Rural` = character(),
        `Time of Day` = character(),
        `Age Oldest Vehicle` = numeric(),
        `Cyclist Casualty` = character(),
        `Pedestrian Casualty` = character(),
        `Motorcycle Casualty` = character(),
        `Under 25 Driver`= character(),
         Probability = numeric(),
        `Prob 95% CI Lower Bound ` = numeric(),
        `Prob 95% CI Upper Bound ` = numeric()
      )
    )

  observeEvent(input$pred_and_append, {
    calc_data <-
      tibble(
        speed_limit = input$speed_calc, 
        speed_limit_squared = input$speed_calc^2,
        road_surface_conditions = input$road_conditions_calc,
        trunk_road = input$trunk_road_calc,
        urban_or_rural_area = input$urban_or_rural_calc, 
        time_of_day = input$time_of_day_calc, 
        vehicle_max_age = input$max_age_calc, 
        casualty_cyclist = input$cyclist_casualty_calc, 
        casualty_motorcycle= input$motorbike_casualty_calc, 
        casualty_pedestrian= input$pedestrian_casualty_calc, 
        vehicle_driver_age_under_25 = input$u25_driver_calc,
        vehicle_car = 'No',
        vehicle_heavy = 'No',
        vehicle_other = 'No', 
        vehicle_motorcycle = 'No', 
        vehicle_pedal_cycle = 'No', 
        vehicle_female_driver = 'No', 
        casualty_other_vehicle = 'No', 
        casualty_car_occupant= 'No'
      )

    prob <- predict(mdl, calc_data, type = "response", se.fit = TRUE)
    prob_se <- prob$se.fit
    lower <- prob$fit - 1.96 * prob_se
    upper <- prob$fit + 1.96 * prob_se


    reactive_results$table_data <-
      reactive_results$table_data %>%
      add_row(
        Speed = input$speed_calc,
        `Road Conditions` = input$road_conditions_calc,
        `Trunk Road` = input$trunk_road_calc,
        `Urban or Rural` = input$urban_or_rural_calc,
        `Time of Day` = input$time_of_day_calc,
        `Age Oldest Vehicle` = input$max_age_calc,
        `Cyclist Casualty` = input$cyclist_casualty_calc,
        `Motorcycle Casualty` = input$motorbike_casualty_calc,
        `Pedestrian Casualty` = input$pedestrian_casualty_calc,
        `Under 25 Driver`= input$u25_driver_calc,
         Probability = round(prob$fit, 3),
        `Prob 95% CI Lower Bound ` = round(lower, 3),
        `Prob 95% CI Upper Bound ` = round(upper, 3)
      )
  })
  


  output$table <- DT::renderDT(
    reactive_results$table_data,
    extensions = c("Buttons", "Scroller"),
    rownames=FALSE,
    options = list(
      dom = "Bfrtip",
      lengthChange = FALSE,
      scroller = TRUE,
      scrollY = 500,
      searchable = FALSE,
      searching = FALSE,
      scrollX = TRUE,
      buttons = c("csv")
    )
  )

  output$info <- renderText({
    about_html
  })
}

# Run the application
shinyApp(ui = ui, server = server)
