  # shiny
  library(ggplot2)
  library(bslib)
  library(thematic)
  library(DT)
  library(shiny)
  library(tidyverse)
  library(greekLetters)
  library(shinyhelper)
  library(shinyBS)
  library(bs4Dash)
  
  # ss calc
  library(rpact)
  library(DT)

  ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "pulse"),
    tabsetPanel(
      tabPanel("Home",
               titlePanel("Characteristics of Trial Design Required to Measure Clinically Meaningful Outcome Measures (Survival)"),
               fluidRow(
                 column(10,
                        htmlOutput("welcome_description")
                 )
               )
      ),
      
          
      
      # Sample size for survival endpoints
      tabPanel("Sample Size",
               titlePanel("Sample Size for Measuring Survival Endpoint"), 
               fluidRow(
                 column(4,
                        numericInput("hr", paste("Hazard Ratio",greeks("Delta")), value = 0.43, min = 0, max = 1, step = 0.01),
                        numericInput("surv_lambdaC", paste("Event Rate Control",greeks("lambda")), value = 0.06, min = 0, max = 1, step = 0.01),
                        numericInput("surv_tT", paste0("Analysis Period (Months) ", greeks("tau")), value = 6, min = 0, max = 1000000, step = 1),
                        numericInput("surv_eta", paste("Dropout Rate",greeks("eta")), value=0.2, min = 0, max = 0.999, step = 0.01), 
                        numericInput("surv_ratio", paste("Ratio",greeks("r")), value=1, min = 1, max = 4, step = 1)
                        
                 ),
                 column(3,
                        numericInput("surv_alpha", label = paste(" Type I Error",greeks("alpha")), value = 0.05, min = 0.001, max = 0.05, step = 0.01),
                        numericInput("surv_power", label = paste("Power","1-",greeks("Beta")), value = 0.8, min = 0.001, max = 0.999, step = 0.01),
                        numericInput("surv_sided", "One- or Two-Sided", value=2, min = 1, max = 2, step = 1),
                        checkboxInput("non_inferiority", "Non-Inferiority", FALSE),
                        numericInput("surv_h0", "Non-Inferiority Margin", value = 0, min = 0, max = 10, step = 0.01)
                        
                        
                 ),
                 column(3,
                        actionButton('calculate_surv', "Calculate")
                 )
               ),
               fluidRow(
                 column(8,
                        wellPanel(
                          textOutput("n_survival")
                        )
                 )
               ), 
               fluidRow(
                 column(10,
                        htmlOutput("surv_description"))
               )
      ),
      
      
      tabPanel("Country Data",
               titlePanel("Number of Live Births in Locations of Interest"), 
               fluidRow(
                 column(12,
                        DTOutput("country_data_table")
                 )
               ),
               fluidRow(
                 column(3,
                        textInput("new_country_name", "Country"),
                        numericInput("new_country_live_births", "Incidence/100000 Live Births", value = 0),
                        numericInput("new_country_prevalence", "Number of All Live Births", value = 0),
                        numericInput("new_country_disease_births", "Number of Live Births for Disease", value = 0),
                        actionButton("add_country", "Add Country")
                 ),
                 column(3,
                        # Moved "Delete Country" button here
                        actionButton("delete_country", "Delete Selected Country")
                 )
               ),
               fluidRow(
                 column(10,
                        htmlOutput("births_description"))
               ),
               br(),  # Added a break here
               
               fluidRow(
                 column(8,
                        wellPanel(
                          textOutput("total_disease_births")
                        )
                 )
               )
               
      ),
      tabPanel("Number of Available Patients",
               titlePanel("Number of Available Patients"), 
               fluidRow(
                 column(2,
                        selectInput("survival_distribution", "Survival Distribution",
                                    choices = c("Exponential"),
                                    selected = "Exponential"),
                        numericInput("lambdaE", label = paste(greeks("lambda"),"Exponential Survival"), value = 0.047, min = 0, step = 0.001)
                 ),
                        
                    
                 column(3,
                        numericInput("age_start", "Minimum Age", value = 5),
                        numericInput("age_end", "Maxmimum Age ", value = 40),
                        numericInput("year_enroll", "Year Enroll", value = 2000)
                        
                        
                 ),
                 
                 column(3,
                        actionButton("calculate_alive", "Calculate")
                 )
                 ),
               fluidRow(
                 column(10,
                        htmlOutput("pts_descr"))
               ),
               br(),  # Added a break here
                 
               fluidRow(
                 column(8,
                        wellPanel(
                          textOutput("alive_summary")
                        )
                 )
               )
               
      )))
  
  
  server <- function(input, output, session) {
    
    #welcome description
    output$welcome_description <- renderText({
      
      "
      <br>

   A framework was developed to explore the feasibility of hypothetical trials measuring long-term endpoints prioritized by payers. The framework focuses on trials conducted in lysosomal storage diseases (LSDs), including mucopolysaccharidosis type I (MPS I), Fabry disease, and Gaucher disease. 

      <br>
      <br>

The overarching steps to implement the framework were determining the required sample size for the hypothetical trial to observe the long-term endpoint and estimating the number of patients available for recruitment. The required sample size was then compared to number of patients available (prevalence) to understand if the required sample could be feasibly and reasonably attained. 
      <br>
      <br>


This tool performs the calculations within the framework. The following tabs are included:    
    <ul>
      <li>Sample Size: to calculate the number of patients needed to measure long-term time-to-event endpoint</li>
      <li>Country Data: to calculate the number of live births in a selected set of locations</li>
      <li>Number of Available Patients: to calculate the number of available patients (prevalence) at the time of trial initiation</li>
    </ul>
    "
    })
  
    
    #survival description
    output$surv_description <- renderText({
      "
      <br>
        
        The following inputs are needed for sample size calculations: 
      <ul>
        <li><strong>Hazard ratio</strong>: hypothesized treatment effect for survival.</li>
        <li><strong>Event rate control</strong>: hazard rate in the control arm</li>
        <li><strong>Analysis period</strong>: time period (in months) to follow patients after starting treatment and measure primary endpoint.</li>
        <li><strong>Dropout rate</strong>: proportion of patients discontinuing from trial.</li>
        <li><strong>Type I error</strong>: probability of false positive finding.</li>
        <li><strong>Power</strong>: 1 - probability of false negative finding.</li>
        <li><strong>One-or two-sided</strong>: one-sided or two-sided hypothesis test.</li>
        <li><strong>Ratio</strong>: allocation ratio between groups, e.g., for 1:1. r=1.</li>
      </ul>
      Note the default calculations are for superiority. To perform calculations for non-inferiority scenario, check box and input the non-inferiority margin. This margin is the upper bound of the 95% confidence interval. 
      "
    })
    
    # birth description
    output$births_description <- renderText({
      "
      <br>
      The number of live births per year across the selected locations is calculated. Countries can be added and deleted. 
            <br>

      To add new country, enter the incidence of live births, the number of total live births, and the number of live births for disease (incidence multiplied by the number of all births).        
      "
    })
    
    # number of pts 
    output$pts_descr <- renderText({
      "
      <br>
      The number of available patients is estimated based on the number of live births from previous tab and survival of patients (currently only expontential survival is supported).     
      <br>
      Input the minimum and maximum age  included in the trial and year of enrollment. The result represents the total number of patients alive at the time of enrollment. 
      "
    })
    
    
    # Set default values for HR and sided when Non-Inferiority checkbox is selected
    observeEvent(input$non_inferiority, {
      if (input$non_inferiority) {
        # Set HR to 1 for non-inferiority
        updateNumericInput(session, "hr", value = 1)
        # Set sided to 1 for one-sided test
        updateNumericInput(session, "surv_sided", value = 1)
        # Set upper bound 
        updateNumericInput(session, "surv_h0", value = 1.3)
        # Set upper bound 
        updateNumericInput(session, "surv_alpha", value = 0.025)
      } else {
        # Set HR back to its default value for superiority
        updateNumericInput(session, "hr", value = 0.43)
        # Set sided back to its default value for superiority
        updateNumericInput(session, "surv_sided", value = 2)
        # Remove 
        updateNumericInput(session, "surv_h0", value = NA)
      }
    })
    
    #survival n output
    output$n_survival <- eventReactive(input$calculate_surv, {
      hr <- input$hr
      surv_lambdaC <- input$surv_lambdaC
      surv_tT <- input$surv_tT
      surv_eta <- input$surv_eta
      surv_power <- input$surv_power
      surv_alpha <- input$surv_alpha
      surv_sided<- input$surv_sided
      surv_ratio<- input$surv_ratio
      surv_h0 <- input$surv_h0  
      
      if (input$non_inferiority) {
        sample_req <- getSampleSizeSurvival(
          typeOfComputation = c("Schoenfeld"),
          lambda1 = hr * surv_lambdaC,
          lambda2 = surv_lambdaC,
          accrualTime = 1E-9, 
          beta = 1 - surv_power,
          alpha = surv_alpha,
          followUpTime = surv_tT / 12,
          sided = 1, allocationRatioPlanned = surv_ratio,
          thetaH0 = surv_h0,
        )
      } else {
        sample_req <- getSampleSizeSurvival(
          typeOfComputation = c("Schoenfeld"),
          lambda1 = hr * surv_lambdaC,
          lambda2 = surv_lambdaC,
          accrualTime = 1E-9, 
          beta = 1 - surv_power,
          alpha = surv_alpha, allocationRatioPlanned = surv_ratio,
          followUpTime = surv_tT / 12,
          sided = surv_sided,
        )
      }
      
      result1 <- paste("Sample Size: ", round(sample_req$numberOfSubjects / (1 - surv_eta), 0))
      result2 <- paste("  Number of Events: ", round(sample_req$eventsFixed, 0))
      return(list(result1, result2))
    })
    
    
    country_data <- reactiveValues(data = data.frame(
      Country = c("United Kingdom", "Germany", "United States", "Canada", "Brazil", "Mexico", "The Netherlands", "Australia"),
      "Incidence/100000 Live Births" = c(1.07, 0.29, 0.34, 0.58, 0.29, 0.19, 1.19, 1.14),
      "Number of All Live Births" = c(700000, 750000, 4000000, 320000, 2700000, 2000000, 170000, 300000),
      "Number of Live Births for Disease" = c(700000, 750000, 4000000, 320000, 2700000, 2000000, 170000, 300000) * c(1.07, 0.29, 0.34, 0.58, 0.29, 0.19, 1.19, 1.14) / 100000,
      stringsAsFactors = FALSE, check.names=FALSE
    ))
    
    
    
    total_disease_births <- reactive({
      sum(country_data$data$`Number of Live Births for Disease`)
    })
    
    # Add a new country to the country_data data frame
    observeEvent(input$add_country, {
      country_name <- input$new_country_name
      live_births <- input$new_country_live_births
      prevalence <- input$new_country_prevalence
      disease_births <- input$new_country_disease_births
      
      new_country_data <- data.frame(
        Country = as.character(country_name),
        "Incidence/100000 Live Births" = 0,  # Placeholder value, you can modify as needed
        "Number of All Live Births" = as.numeric(live_births),
        "Number of Live Births for Disease" = as.numeric(disease_births),
        stringsAsFactors = FALSE, check.names=FALSE
      )
      
      # Update the country_data variable within the reactiveValues object
      country_data$data <- rbind(country_data$data, new_country_data)
      
      # Reset the input fields
      updateTextInput(session, "new_country_name", value = "")
      updateNumericInput(session, "new_country_live_births", value = 0)
      updateNumericInput(session, "new_country_prevalence", value = 0)
      updateNumericInput(session, "new_country_disease_births", value = 0)
    })
    
    # Display country-specific data as a table
    output$country_data_table <- renderDT({
      datatable(
        country_data$data,
        options = list(
          pageLength = 10,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'font-weight':'bold','background-color':'#f7f7f7'});",
            "}"
          )
        ),
        escape = FALSE,
        class = "display",
        colnames = c("Country", "Incidence/100000 Live Births", "Number of All Live Births", "Number of Live Births for Disease")
      )
    })
    
    
    observeEvent(input$delete_country, {
      selected_rows <- input$country_data_table_rows_selected
      if (!is.null(selected_rows)) {
        country_data$data <- country_data$data[-selected_rows, , drop = FALSE]
      }
    })
    
    
    # Calculate and display the total live births for disease
    output$total_disease_births <- renderText({
      paste("Total Live Births for Disease: ", total_disease_births())
    })
    
    # Define reactive variable total_disease_births
    total_disease_births <- reactive({
      # Calculate the total live births for disease based on your data
      sum(country_data$data$`Number of Live Births for Disease`)
    })
    
    output$alive_summary <- renderText({
      if (input$calculate_alive > 0) {
        # Extract relevant inputs
        survival_distribution <- input$survival_distribution
        year_enroll <- input$year_enroll
        age_start <- input$age_start
        age_end <- input$age_end
        
        year_end <- year_enroll-age_start-1
        year_start <- year_enroll-age_end+1
       
        
        # Function to calculate survival probability based on the selected distribution
        surv_function <- switch(
          survival_distribution,
          "Exponential" = function(lambda, t) exp(-lambda * t)
        )
        
        # Calculate the number of patients alive at a certain year for various ages and sum them up
        years <- seq(year_start, year_end, by = 1)
        
        # Calculate total live births for disease
        liveMPSI <- sum(country_data$data$`Number of Live Births for Disease`)
        
        # Create a data frame to store the results
        df_prevl <- data.frame(year = years,  # year
                               liveMPSI = liveMPSI,  # number of live MPS I births
                               alive_year = ifelse(years <= year_enroll, liveMPSI * surv_function(input$lambdaE, year_enroll - years), NA),
                               alive_sum = cumsum(ifelse(years <= year_enroll, liveMPSI * surv_function(input$lambdaE, year_enroll - years), NA)))  # cumulative number alive
        
        total_alive <- df_prevl$alive_sum[length(df_prevl$alive_sum)]
        
        # Construct the output text
        output_text <- paste(
          "Total number of available patients:", round(total_alive, 0)
        )
        
        return(output_text)}
    })
    
    
  }
  
  shinyApp(ui = ui, server = server)
