#' about_volunteer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_about_volunteer_ui <- function(id){
  ns <- NS(id)
  tagList(
      # add your content here
      h2("header2"),
      p("your paragraphs"),
      h3("header 3"),
      p("more paragraph")
  )
}
    
#' about_volunteer Server Functions
#'
#' @noRd 
mod_about_volunteer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # leave it as is
  })
}
    
## To be copied in the UI
# mod_about_volunteer_ui("about_volunteer_1")
    
## To be copied in the server
# mod_about_volunteer_server("about_volunteer_1")
