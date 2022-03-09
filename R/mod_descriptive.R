#' descriptive UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descriptive_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("donor_data"), "Select a dataset",
                   choices = c("donor_data_group_1",
                               "donor_data_group_2"),
                   selected = "donor_data_group_1"),
    
    fluidRow(
      valueBoxOutput(ns("donor_info"), width = 3),
      valueBoxOutput(ns("avg_dollar"), width = 3),
      valueBoxOutput(ns("again_donor"), width = 3),
      valueBoxOutput(ns("pct_again"), width = 3)
      
    ),
    hr(),
    
    fluidRow(
      
    ),
    
    fluidRow(
      plotOutput(ns("by_state"), height = plot_height),
    ),
    hr(),
    
    fluidRow(
      column(6, plotOutput(ns("by_age"), height = plot_height)),
      column(6, plotOutput(ns("by_n_donation"), height = plot_height))
    ),
    hr(),
    
    fluidRow(
      column(3, plotOutput(ns("pie_gender"), height = plot_height_pie)),
      column(3, plotOutput(ns("pie_ses"), height = plot_height_pie)),
      column(3, plotOutput(ns("pie_college"), height = plot_height_pie)),
      column(3, plotOutput(ns("pie_income"), height = plot_height_pie))
    )
  )
}
    
#' descriptive Server Functions
#'
#' @noRd 
mod_descriptive_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    dat_donor <- reactive({
      get(input$donor_data)
    })
    
    ### numbers ----
    output$donor_info <- renderValueBox({
      n_donors <- nrow(dat_donor())
      
      valueBox(value = n_donors,
               subtitle = "Number of Donors")
    })
    
    output$again_donor <- renderValueBox({
      n_again <- sum(dat_donor()$donated_within_last_year)
      
      valueBox(value = n_again,
               subtitle = "Number of Donors Donated Again Last Year")
    })
    
    output$pct_again <- renderValueBox({
      prob_again <- round(100 * mean(dat_donor()$donated_within_last_year), 2)
      
      valueBox(value = paste0(prob_again, "%"),
               subtitle = "Percent of Donors Donated Again Last Year")
    })
    
    
    output$avg_dollar <- renderValueBox({
      avg <- mean(dat_donor()$total_dollar)
      
      valueBox(value = paste0("$", round(avg, 2)),
               subtitle = "Average Lifetime Donation by a Donor")
    })
    
    
    ### numbers of donors ----
    output$by_state <- renderPlot({
      plot_state(.data = dat_donor())
    })
    
    output$by_age <- renderPlot({
      plot_age(.data = dat_donor())
    })
    
    output$by_n_donation <- renderPlot({
      plot_n_donation(.data = dat_donor())
    })
    
    output$pie_gender <- renderPlot({
      plot_pie("gender", "Gender", .data = dat_donor())
    })
    
    
    output$pie_ses <- renderPlot({
      plot_pie("ses", "Socioeconomic Status", .data = dat_donor())
    })
    
    output$pie_college <- renderPlot({
      plot_pie("college", "College Degree", .data = dat_donor())
    })
    
    # output$pie_gender <- renderPlot({
    #     plot_pie("gender")
    # })
    
    output$pie_income <- renderPlot({
      plot_pie("income", "Income Level", .data = dat_donor())
    })
    
    
    ### percent donated again ----
    
    output$plot_ses <- renderPlot({
      plot_binary(ses, .data = dat_donor()) +
        labs(x = "Socioeconomic Status")
    })
    
    output$plot_gender <- renderPlot({
      plot_binary(gender, .data = dat_donor()) +
        labs(x = "Gender")
    })
    
    output$plot_college <- renderPlot({
      plot_binary(college, .data = dat_donor()) +
        labs(x = "College Degree")
    })
    
    output$plot_income <- renderPlot({
      plot_binary(income, .data = dat_donor()) +
        labs(x = "Income Bracket")
    })
    
    output$plot_age <- renderPlot({
      plot_binary(age, x_type = "continuous", .data = dat_donor()) +
        labs(x = "Age Bracket")
    })
    
    output$plot_n_donations <- renderPlot({
      plot_binary(n_donation, x_type = "continuous", .data = dat_donor()) +
        labs(x = "Number of Past Donations Bracket")
    })
    
  })
}
    
## To be copied in the UI
# mod_descriptive_ui("descriptive_ui_1")
    
## To be copied in the server
# mod_descriptive_server("descriptive_ui_1")
