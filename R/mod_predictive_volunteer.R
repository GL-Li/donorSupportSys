#' predictive_volunteer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_predictive_volunteer_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
          valueBox(value = "Donation Decision Support System",
                   subtitle = "Is the volunteer going to volunteer again?",
                   width = 12,
                   color = "blue")
      ),
      
      
      tabsetPanel(
          type = "tabs",
          
          ### batch of donors ----
          tabPanel(
              "Predict for a batch of volunteers",
              
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
                  DT::dataTableOutput(ns("table_2")),
                  downloadButton(ns("download_2"), "Download predicitons")
              ),
              
              br(),
              br(),
              br(),
              
              fluidRow(
                  column(6, plotOutput(ns("age_prob_2"), height = "200px")),
                  column(6, plotOutput(ns("n_donation_prob_2"), height = "200px"))
              )
          ),
          
          
          ### single donor ----
          tabPanel(
              "Predict for a single volunteer",
              fluidRow(
                  valueBoxOutput(ns("prediction_2"), width = 12)
              ),
              
              fluidRow(
                  box(selectizeInput(ns("state_2"), "Select a State",
                                     choices = states,
                                     selected = "AK"),
                      height = "100px",
                      width = 4),
                  box(sliderInput(ns("age_2"), "Select Age",
                                  value = 50,
                                  min = 0,
                                  max = 100,
                                  step = 1),
                      height = "100px",
                      width = 4),
                  box(sliderInput(ns("n_volunteer"), 
                                  "Select Times of Previous Volunteerings",
                                  value = 3,
                                  min = 0,
                                  max = 100,
                                  step = 1),
                      width = 4,
                      height = "100px")
              ),
              
              fluidRow(
                  box(radioButtons(ns("ses_2"), "Select a Socioeconomic Status",
                                   choices = 1:3,
                                   selected = 1,
                                   inline = TRUE)),
                  box(radioButtons(ns("income_2"), "Select a Income Bracket",
                                   choices = 1:7,
                                   selected = 1,
                                   inline = TRUE)),
                  
                  box(radioButtons(ns("gender_2"), "Select a Gender",
                                   choices = c("F", "M"),
                                   selected = "F",
                                   inline = TRUE),
                      width = 6),
                  box(radioButtons(ns("edu_2"), "College Educated?",
                                   choices = c("Yes", "No"),
                                   selected = "Yes",
                                   inline = TRUE),
                      width = 6)
              )
          )
      )
  )
}
    
#' predictive_volunteer Server Functions
#'
#' @noRd 
mod_predictive_volunteer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    ### single donor prediction ----
    
    output$prediction_2 <- renderValueBox({
        req(input$n_volunteer, input$age_2)
        
        state <- input$state_2
        ses <- input$ses_2
        age <- input$age_2
        income <- input$income_2
        gender <- input$gender_2
        college <- ifelse(input$edu_2 == "Yes", 1, 0)
        n_volunteer <- input$n_volunteer
        
        new_dat <- data.frame(
            state = state,
            ses = as.integer(ses),
            income = as.integer(income),
            age = age,
            income = as.integer(income),
            gender = gender,
            college = as.integer(college),
            n_volunteering = as.integer(n_volunteer)
        )
        
        prob <- predict(mod_2, new_dat, type = "prob")[2] %>%
            round(4)
        
        valueBox(value = ifelse(prob > 0.1495, 
                                "More likely to volunteer again than an average volunteer", 
                                "Less likely to volunteer again than an average volunteer"),
                 subtitle = paste0("Predicted Probability = ", prob),
                 icon = icon("heart"),
                 color = ifelse(prob > 0.1495, "fuchsia", "light-blue"))
    })
    
    ### batch donnor prediction ----
    
    dat2 <- reactive({
        req(input$file_upload)
        read.csv(input$file_upload$datapath, 
                 stringsAsFactors = FALSE)
    })
    
    dat_pred_2 <- reactive({
        dat2() %>%
            bind_cols(predict(mod_2, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod_2, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class)
    })
    
    output$table_2 <- DT::renderDataTable({
        dat_pred_2()
    })
    
    output$download_2 <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat_pred_2(), file, row.names = FALSE)
        }
    )
    
    output$age_prob_2 <- renderPlot({
        plot_pred_bar(mod_2, dat2(), "age") +
            labs(x = "Age")
    })
    
    output$n_donation_prob_2 <- renderPlot({
        plot_pred_bar(mod_2, dat2(), "n_volunteering") +
            scale_x_continuous(limits = c(1, 70)) +
            labs(x = "Number of Previous Donations")
    })
    
  })
}
    
## To be copied in the UI
# mod_predictive_volunteer_ui("predictive_volunteer_1")
    
## To be copied in the server
# mod_predictive_volunteer_server("predictive_volunteer_1")
