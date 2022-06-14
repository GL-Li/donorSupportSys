#' upload_file_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_upload_file_general_ui <- function(id, label = "Upload a file"){
  ns <- NS(id)
  tagList(
 
    fileInput(ns("file_upload"), label,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"))
  )
}
    
#' upload_file_general Server Functions
#'
#' @noRd 
mod_upload_file_general_server <- function(id){
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
# mod_upload_file_general_ui("upload_file_general_ui_1")
    
## To be copied in the server
# mod_upload_file_general_server("upload_file_general_ui_1")
