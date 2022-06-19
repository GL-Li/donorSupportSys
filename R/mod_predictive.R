#' predictive UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predictive_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
          valueBox(value = "Donation Decision Support System",
                   subtitle = "Is the donnor going to donate again?",
                   width = 12,
                   color = "blue")
      ),
      
      tabsetPanel(
          type = "tabs",
          
          ### batch of donors ----
          
          tabPanel(
              "Predict for a batch of donors",
              fluidRow(
                  column(
                      6,
                      fileInput(ns("file_upload"), "Upload Data",
                                multiple = FALSE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv"))
                  )
              ),
              
              fluidRow(
                  DT::dataTableOutput(ns("table")),
                  downloadButton(ns("download"), "Download predicitons")
              ),
              
              br(),
              br(),
              br(),
              
              fluidRow(
                  column(6, plotOutput(ns("age_prob"), height = "200px")),
                  column(6, plotOutput(ns("n_donation_prob"), height = "200px"))
              )
          ),
          
          ### single donor ----
          
          tabPanel(
              "Predict for a single donor",
              fluidRow(
                  valueBoxOutput(ns("prediction"), width = 12)
              ),
              
              fluidRow(
                  box(selectizeInput(ns("state"), "Select a State",
                                     choices = states,
                                     selected = "AK"), 
                      height = "100px",
                      width = 4),
                  box(sliderInput(ns("age"), "Select Age",
                                  value = 50,
                                  min = 0,
                                  max = 100,
                                  step = 1),
                      height = "100px",
                      width = 4),
                  box(sliderInput(ns("n_donation"), 
                                  "Select Number of Previous Donations",
                                  value = 3,
                                  min = 0,
                                  max =100,
                                  step = 1),
                      width = 4,
                      height = "100px")
              ),
              
              fluidRow(
                  box(radioButtons(ns("ses"), "Select a Socioeconomic Status",
                                   choices = 1:3,
                                   selected = 1,
                                   inline = TRUE)),
                  box(radioButtons(ns("income"), "Select a Income Bracket",
                                   choices = 1:7,
                                   selected = 1,
                                   inline = TRUE)),
                  
                  box(radioButtons(ns("gender"), "Select a Gender",
                                   choices = c("F", "M"),
                                   selected = "F",
                                   inline = TRUE),
                      width = 6),
                  box(radioButtons(ns("edu"), "College Educated?",
                                   choices = c("Yes", "No"),
                                   selected = "Yes",
                                   inline = TRUE),
                      width = 6)
              )
          )
      )
 
  )
}
    
#' predictive Server Functions
#'
#' @noRd 
mod_predictive_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    ### singel donor predictions ----
    output$prediction <- renderValueBox({
        req(input$n_donation, input$age)
        
        state <- input$state
        ses <- input$ses
        age <- input$age
        income <- input$income
        gender <- input$gender
        college <- ifelse(input$edu == "Yes", 1, 0)
        n_donatiion <- input$n_donation
        
        new_dat <- data.frame(
            state = state,
            ses = as.integer(ses),
            income = as.integer(income),
            age = age,
            income = as.integer(income),
            gender = gender,
            college = as.integer(college),
            n_donation = as.integer(n_donatiion)
        )
        
        prob <- predict(mod, new_dat, type = "prob")[2] %>%
            round(4)
        
        valueBox(value = ifelse(prob > 0.1522, 
                                "More likely to donate again than an average donor", 
                                "Less likely to donate again than an average donor"),
                 subtitle = paste0("Predicted Probability = ", prob),
                 icon = icon("heart"),
                 color = ifelse(prob > 0.1522, "fuchsia", "light-blue"))
    })
    
    ### batch donnor prediction ----
    
    # dat0 <- mod_upload_file_general_server("uploadfile_pred_1")
    dat0 <- reactive({
        req(input$file_upload)
        read.csv(input$file_upload$datapath, 
                 stringsAsFactors = FALSE)
    })
    
    dat_pred <- reactive({
        dat0() %>%
            bind_cols(predict(mod, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class)
    })
    
    output$table <- DT::renderDataTable({
        dat_pred()
    })
    
    output$download <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat_pred(), file, row.names = FALSE)
        }
    )
    
    output$age_prob <- renderPlot({
        plot_pred_bar(mod, dat0(), "age") +
            labs(x = "Age")
    })
    
    output$n_donation_prob <- renderPlot({
        plot_pred_bar(mod, dat0(), "n_donation") +
            scale_x_continuous(limits = c(1, 70)) +
            labs(x = "Number of Previous Donations")
    })
 
  })
}
    
## To be copied in the UI
# mod_predictive_ui("predictive_1")
    
## To be copied in the server
# mod_predictive_server("predictive_1")
