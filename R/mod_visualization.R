#' predictive visualization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualization_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    mod_upload_file_general_ui("upload_file_general_1"),
    
    tableOutput(ns("aaa")),
    
    plotOutput(ns("ses"))
  )
}
    
#' visualization Server Functions
#'
#' @noRd 
mod_visualization_server <- function(id, mdl){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    dat <- mod_upload_file_general_server("upload_file_general_1")
    
    output$aaa <- renderTable({
      head(dat())
    })

    # output$ses <- renderPlot({
    #   print(head(dat()))
    #   plot_pred_bar(mdl, dat(), "ses")
    # })
  })
}
    
## To be copied in the UI
# mod_visualization_ui("visualization_ui_1")
    
## To be copied in the server
# mod_visualization_server("visualization_ui_1")
