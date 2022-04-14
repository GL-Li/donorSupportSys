#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id, tooltip_desc, tooltip_pred){
  ns <- NS(id)
  tagList(
    column(12,
           img(src = "www/logo.png"),
           align = "center"),
    fluidRow(
      column(2),
      column(8,
             # valueBox("Welcome to AI-enabled DSS for analysing donor behaviour in nonprofit organisations",
             #          subtitle = "",
             #          width = 12,
             #          color = "orange"),
             
             h1("Welcome to AI-enabled DSS for analysing donor behaviour in nonprofit organisations"),
             
             h2("This system is machine learning techniques based, and has two parts:"),
             
             h3("1. Analysing donor behaviour: descriptive and predictive analysis of donors."),
             
             h3("2. Analysing volunteer behaviour: descriptive and predeictive analysis of volunteers."),
             align = "left"),
      column(2)
    )
    
    # fluidRow(
    #   column(2),
      # column(
      #   4,
      #   div(
      #     id = ns("div_des"),
      #     valueBoxOutput(ns("click_descriptive"), width = 12),
      #     bsTooltip(ns("click_descriptive"),  
      #               tooltip_desc, 
      #               placement = "bottom", 
      #               trigger = "hover",
      #               options = NULL),
      #     align = "center"
      #   ),
      #   
      # ),
      # 
      # column(
      #   4,
      #   div(
      #     id = ns("div_pred"),
      #     valueBoxOutput(ns("click_predictive"), width = 12),
      #     bsTooltip(ns("click_predictive"), 
      #               tooltip_pred, 
      #               placement = "bottom", 
      #               trigger = "hover",
      #               options = NULL),
      #     align = "center"
      #   )
      # ),
    #   column(2)
    # )
  )
}
    
#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$click_descriptive <- renderValueBox({
      valueBox(
        value = actionLink(
          inputId = ns("link_descriptive"),
          label = div("Descriptive analysis", 
                      style = "color: white; font-size: 30px")
        ),
        subtitle = "",
        color = "light-blue"
      )
    })
    
    observeEvent(input$link_descriptive, {
      updateTabItems(session, inputId = "tabs", selected = "descriptive")
    })
    
    output$click_predictive <- renderValueBox({
      valueBox(
        value = actionLink(
          inputId = ns("link_predictive"),
          label = div("Predictive analysis", 
                      style = "color: white; font-size: 30px")
        ),
        subtitle = "",
        color = "light-blue"
      )
    })
    
    observeEvent(input$link_predictive, {
      updateTabItems(session, inputId = "tabs", selected = "predictive")
    })
 
  })
}
    
## To be copied in the UI
# mod_home_ui("home_ui_1")
    
## To be copied in the server
# mod_home_server("home_ui_1")
