#' uploadfile UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label label of the file upload box
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_uploadfile_ui <- function(id, label = "Choose a file"){
  ns <- NS(id)
  tagList(
    fileInput(ns("file_upload"), label,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  )
}
    
#' uploadfile Server Functions
#'
#' @noRd 
mod_uploadfile_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    reactive({
      req(input$file_upload)
      read.csv(input$file_upload$datapath, 
               stringsAsFactors = FALSE) 
    })
  })
}
    
## To be copied in the UI
# mod_uploadfile_ui("uploadfile_ui_1")
    
## To be copied in the server
# mod_uploadfile_server("uploadfile_ui_1")
