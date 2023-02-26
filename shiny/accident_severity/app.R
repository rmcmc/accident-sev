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


df <- read.csv('./data/dft-road-casualty-statistics-accident-2020.csv.xls')


high_crash_severity <- c(1,2)

df<-
  df %>%
  mutate(accident_severity = replace(accident_severity,accident_severity ==3 ,0))%>%
  mutate(accident_severity = replace(accident_severity,accident_severity %in% high_crash_severity ,1))%>%
  filter(speed_limit != -1)


df <-
  df%>%
  mutate(date = as.Date(date, "%d/%m/%y"))%>%
  mutate(time = hm(time))%>%
  mutate(hour =hour(time))%>%
  mutate(month = month(date))%>%
  mutate(night = case_when(hour < 6 | hour >= 23  ~ 1, TRUE~0 ))%>%
  mutate(weekend = case_when(hour >= 18 & day_of_week %in% c(6,7)~ 1, TRUE~0 ))%>%
  mutate(season = case_when(
    month %in% c(1,2,12) ~ 'winter',
    month %in% c(3,4,5) ~ 'spring',
    month %in% c(6,7,8) ~ 'summer',
    month %in% c(9,10,11)~'autumn'
  ))%>%
  mutate(season = factor(season, levels = c('winter', 'spring', 'summer', 'autumn')))


mdl <- glm(accident_severity ~ speed_limit +night+weekend,data = df, family = binomial())

mdl_summary <- summary(mdl)


#Baseline predictions
speeds <- seq(10,90,1)
n_samples <- length(speeds)
zeros_vector <- rep(0,n_samples)

baseline_new_data = list(
  speed_limit=speeds, 
  night=zeros_vector, 
  weekend=zeros_vector)

baseline_pred <- predict(mdl,type = "response", newdata= baseline_new_data, se.fit=TRUE)


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
    
      selectInput("night",
                "Night?",
                c('Day' = 0 ,
                  'Night' = 1),
                multiple = FALSE,
                selected=0),
    
    selectInput("weekend",
                "Weekend?",
                c('No' = 0 ,
                  'Yes' = 1),
                multiple = FALSE,
                selected=0)
    
    # selectInput("junction",
    #             "Crash at Junction:",
    #             c('No' = 0 ,
    #               'Yes' = 1),
    #             multiple = FALSE,
    #             selected=0),
    # 
    # selectInput("young_male",
    #             "Were any of the drivers a male under 25?:",
    #             c('No' = 0 ,
    #               'Yes' = 1),
    #             multiple = FALSE,
    #             selected=0),
    # 
    # selectInput("pedestrian",
    #             "Was a pedestrian involved:",
    #             c('No' = 0 ,
    #               'Yes' = 1),
    #             multiple = FALSE,
    #             selected=0),
    # 
    # selectInput("road_conditions",
    #             "Road conditions:",
    #             c('Wet' = 'wet' ,
    #               'Dry' = 'dry',
    #               'Ice/Frost'='frost'),
    #             multiple = FALSE,
    #             selected=0)
  )

calc_side <- 
  sidebarPanel(
    selectInput("night_calc",
                "Night?",
                c('Day' = 0 ,
                  'Night' = 1),
                multiple = FALSE,
                selected=0),
    
    selectInput("weekend_calc",
                "Weekend?",
                c('No' = 0 ,
                  'Yes' = 1),
                multiple = FALSE,
                selected=0),
    
    numericInput(
      "speed_calc",
      'Speed Limit (MPH)',
      30,
      min = 10,
      max = 100,
      step = 1
    ),
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
        speed_limit=speeds, 
        night=as.numeric(rep(input$night, n_samples)),
        weekend=as.numeric(rep(input$weekend,n_samples)))
      
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
          Day=character(), 
          Weekend=character(),
          Prob=numeric(),
          `Prob 95% CI Lower Bound ` = numeric(),
          `Prob 95% CI Upper Bound ` = numeric()
        )
      )
    
    observeEvent(input$pred_and_append, {
      
      calc_data <-
        tibble(
          speed_limit = input$speed_calc,
          night = as.numeric(input$night_calc),
          weekend = as.numeric(input$weekend_calc)
          )
      
      prob <- predict(mdl, calc_data, type='response', se.fit = TRUE)
      prob_se <- prob$se.fit
      lower <- prob$fit - 1.96*prob_se
      upper <- prob$fit + 1.96*prob_se
      
      
      if (input$night_calc == 0){day <- 'Day'} else{day <- 'Night'}
      if (input$weekend_calc == 0){wkend <- 'Weekday'} else{wkend <- 'Weekend'}
      
      reactive_results$table_data <-
        reactive_results$table_data%>%
        add_row(
          Speed = input$speed_calc,
          Day=day, 
          Weekend=wkend, 
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




