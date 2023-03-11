#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

#source('accident_severity/MAS61004_project_clean_data-2.R')

df <- read.csv('./shiny_df.csv')%>%
  mutate(
    accident_severity = case_when(
      accident_severity == 'Slight' ~ 0, 
      accident_severity == 'Severe' ~ 1
    )
  )%>%
  select(!X)

mdl <- glm(accident_severity ~ . ,data = df, family = binomial())

mdl_summary <- summary(mdl)


#Baseline predictions
speeds <- seq(10,90,1)
n_samples <- length(speeds)
zeros_vector <- rep(0,n_samples)

baseline_new_data = list(
  speed_limit = speeds,
  road_surface_conditions = rep('Dry', n_samples),
  urban_or_rural_area = rep('Urban',n_samples),
  time_of_day  = rep('day_off_peak',n_samples),
  vehicle_max_age = rep(5, n_samples),
  casualty_cyclist = rep(FALSE, n_samples),
  casualty_car_occupant = rep(FALSE, n_samples),
  casualty_motorcycle = rep(FALSE, n_samples),
  casualty_pedestrian = rep(FALSE, n_samples),
  casualty_other_vehicle = rep(FALSE, n_samples),
  driver_u25 = rep(FALSE, n_samples)
)
  

baseline_pred <- predict(mdl,type = "response", newdata = baseline_new_data, se.fit=TRUE)


baseline_data <- 
  tibble(
    speeds = speeds, 
    pred = baseline_pred$fit,
    upper_ci =  baseline_pred$fit +1.96*baseline_pred$se.fit,
    lower_ci =  baseline_pred$fit -1.96*baseline_pred$se.fit
    )


### SIDEBARS
curves_side <- 
    sidebarPanel(
      checkboxInput("show_baseline", "Show Baseline Model", value = FALSE),
      checkboxInput("show_ci", "Show 95% CI", value = TRUE),
    
      selectInput("road_conditions",
                "Road Surface Condtions",
                c('Dry' = 'Dry' ,
                  'Wet or Damp' = 'Wet or damp'),
                multiple = FALSE,
                selected='Dry'),
    
    selectInput("urban_or_rural",
                "Urban or Rural",
                c('Urban' = 'Urban' ,
                  'Rural' = 'Rural'),
                multiple = FALSE,
                selected='Urban'),
    
    selectInput("time_of_day",
                "Time of Day",
                c('Peak - Morning (7AM - 10AM)' = 'morning_peak' ,
                  'Peak - Evening (4PM - 7PM)' = 'evening_peak' ,
                  'Off Peak - Day (10AM - 4PM)' = 'day_off_peak',
                  'Off Peak - Night (7PM - 7AM)' = 'night_off_peak'),
                multiple = FALSE,
                selected='day_off_peak'),
    
    sliderInput("max_age",
                "Age of Oldest Vehicle Involved:",
                min = 0, max = 30,
                value = 5),
    
    checkboxInput("cyclist_involved",
                  'Cyclist Involved',
                value= FALSE),
    
    checkboxInput("passenger_involved",
                  'Car Passanger Involved',
                  value= FALSE),
    checkboxInput("motorcycle_involved",
                  'Motorcycle Involved',
                  value= FALSE),
    
    checkboxInput("pedestrian_involved",
                  'Pedestrian Involved',
                  value= FALSE),
    
    checkboxInput("u25_driver",
                  "Driver Under 25 Involved",
                  value= FALSE)
  )

calc_side <- 
  sidebarPanel(
    numericInput(
      "speed_calc",
      'Speed Limit (MPH)',
      30,
      min = 10,
      max = 100,
      step = 1
    ),
    selectInput("road_conditions_calc",
                "Road Surface Condtions",
                c('Dry' = 'Dry' ,
                  'Wet or Damp' = 'Wet or damp'),
                multiple = FALSE,
                selected='Dry'),
    
    selectInput("urban_or_rural_calc",
                "Urban or Rural",
                c('Urban' = 'Urban' ,
                  'Rural' = 'Rural'),
                multiple = FALSE,
                selected='Urban'),
    
    selectInput("time_of_day_calc",
                "Time of Day",
                c('Peak - Morning (7AM - 10AM)' = 'morning_peak' ,
                  'Peak - Evening (4PM - 7PM)' = 'evening_peak' ,
                  'Off Peak - Day (10AM - 4PM)' = 'day_off_peak',
                  'Off Peak - Night (7PM - 7AM)' = 'night_off_peak'),
                multiple = FALSE,
                selected='day_off_peak'),
    
    sliderInput("max_age_calc",
                "Age of Oldest Vehicle Involved:",
                min = 0, max = 30,
                value = 5),
    
    checkboxInput("cyclist_involved_calc",
                  'Cyclist Involved',
                  value= FALSE),
    
    checkboxInput("passenger_involved_calc",
                  'Car Passanger Involved',
                  value= FALSE),
    
    checkboxInput("motorcycle_involved_calc",
                  'Motorcycle Involved',
                  value= FALSE),
    
    checkboxInput("pedestrian_involved_calc",
                  'Pedestrian Involved',
                  value= FALSE),
    
    checkboxInput("u25_driver_calc",
                  "Driver Under 25 Involved",
                  value= FALSE),
    actionButton('pred_and_append', 'Predict')
)

calc_df <- tibble(x=1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel(
      "Curve Plots",
      fluid = TRUE,
      sidebarLayout(
        curves_side,
        mainPanel(
          "Plot",  
          plotOutput("logReg"),
          htmlOutput('plot_description')
          )
        )
      ),
    
    tabPanel(
      "Calulate",
      fluid = TRUE,
      sidebarLayout(
        calc_side,
        mainPanel(
          DT::DTOutput("table")
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$plot_description <- renderText(
      "
        <h2>Plot Details</h2>
      
      This is a regression output of probability of fatality in 
      
      The baseline model is fixed and represents the probability if the crash:
      
      <ul>
        	<li>Does not involve a pedestrian</li>
        	<li>Occurs during the hours of 6am-11pm</li>
      </ul>
      
      "
    )

    output$logReg <- renderPlot({
      
      
      newdata = list(
        speed_limit = speeds,
        road_surface_conditions = rep(input$road_conditions, n_samples),
        urban_or_rural_area = rep(input$urban_or_rural,n_samples),
        time_of_day  = rep(input$time_of_day,n_samples),
        vehicle_max_age = rep(input$max_age, n_samples),
        casualty_cyclist = rep(input$cyclist_involved, n_samples),
        casualty_car_occupant = rep(input$passenger_involved, n_samples),
        casualty_motorcycle = rep(input$motorcycle_involved, n_samples),
        casualty_pedestrian = rep(input$pedestrian_involved, n_samples),
        casualty_other_vehicle = rep(FALSE, n_samples),
        driver_u25 = rep(input$u25_driver, n_samples) 
      )

      
      predicted_vals <- predict(mdl,type = "response", newdata= newdata, se.fit=TRUE)
      
      results <-
        tibble(
          speeds =speeds,
          pred = predicted_vals$fit, 
          upper_ci =  predicted_vals$fit +1.96*predicted_vals$se.fit,
          lower_ci =  predicted_vals$fit -1.96*predicted_vals$se.fit)

        
      #Get UK speeds lims
      uk_speeds_lims <- seq(20,70,10)
      point_prob_baseline <-
        baseline_data%>%
        filter(speeds %in% uk_speeds_lims)
      
      point_prob_results <-
        results%>%
        filter(speeds %in% uk_speeds_lims)
      
      
      main_plot <- ggplot(data=results, aes(x=speeds, y=pred))+
        scale_x_continuous(breaks = seq(0,100, 10))+
        geom_line(color='red')
      
      
      if (input$show_ci) {
        main_plot <-
          main_plot +
          geom_ribbon(
            data=results,
            aes(ymin=lower_ci, ymax=upper_ci), 
            alpha=0.5, 
            fill='red'
          )
      }
      
      if (input$show_baseline){
        
        main_plot<-
          main_plot +
          geom_line(data = baseline_data, color='grey')
        
        if (input$show_ci) {
          main_plot <-
            main_plot +
            geom_ribbon(
              data=baseline_data,
              aes(ymin=lower_ci, ymax=upper_ci), 
              alpha=0.5, 
              fill='grey'
            )
        }
      }

      main_plot
      
      

      # else{
      #   if (input$show_ci) {
      #     main_plot +
      #       geom_ribbon(
      #         data = results, 
      #         aes(ymin=lower_ci, ymax=upper_ci), alpha=0.5, fill='red'
      #       )
      #   }
      #   else{
      #     main_plot
      #   }
      # }
      
      

      

    })
    
    reactive_results <- 
      reactiveValues(
        table_data = tibble(
          Speed = numeric(),
          Prob=numeric(),
          `Prob 95% CI Lower Bound ` = numeric(),
          `Prob 95% CI Upper Bound ` = numeric()
        )
      )
    
    observeEvent(input$pred_and_append, {
      
      calc_data <-
        tibble(
          speed_limit = input$speed_calc,
          road_surface_conditions = input$road_conditions_calc,
          urban_or_rural_area = input$urban_or_rural_calc,
          time_of_day  = input$time_of_day_calc,
          vehicle_max_age = input$max_age_calc,
          casualty_cyclist = input$cyclist_involved_calc,
          casualty_car_occupant = input$passenger_involved_calc,
          casualty_motorcycle = input$motorcycle_involved_calc,
          casualty_pedestrian = input$pedestrian_involved_calc,
          casualty_other_vehicle = FALSE,
          driver_u25 = input$u25_driver_calc
          )
      
      prob <- predict(mdl, calc_data, type='response', se.fit = TRUE)
      prob_se <- prob$se.fit
      lower <- prob$fit - 1.96*prob_se
      upper <- prob$fit + 1.96*prob_se
      
      
      reactive_results$table_data <-
        reactive_results$table_data%>%
        add_row(
          Speed = input$speed_calc,
          Prob=round(prob$fit,3),
          `Prob 95% CI Lower Bound ` = round(lower,3),
        `Prob 95% CI Upper Bound ` = round(upper,3))
      
    })
    
    output$table <-  DT::renderDT(
      reactive_results$table_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        dom = 'Bfrtip',
        lengthChange = FALSE, 
        scroller=TRUE,
        scrollY = 500,
        searchable = FALSE,
        searching=FALSE,
        buttons = c('csv')
        
        )
    )
    
    html<- "
        <h1>How to Use This App</h1>
        
        <h2>Background</h2>
        
        <p>This application has been developed by team XYZ as a part of the investigation into
        crash severivity in the UK</p>
        
        <h2>Plot A</h2>
        
        <p>This plot compares the probbaility of a severe crash against speed for various 
        different crash conditon.</p>
        
        <ul>
        	<li>Bullet A</li>
        	<li>Bullet&nbsp;<strong>Bold!</strong></li>
        </ul>
        "
    
    output$info <-  renderText({html})
    
}

# Run the application 
shinyApp(ui = ui, server = server)




