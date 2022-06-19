#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    
    # Your application server logic 
    # donor ====================================================================
    
    ## home ----
    mod_home_server("home_1")
    
    
    ## descriptive analysis ----
    
    mod_descriptive_server("descriptive_1")
    
    
    ## Predictive analysis ----
    
    mod_predictive_server("predictive_1")
    
    
    
    # volunteer ===============================================================
    
    ## home ----
    mod_home_server("home_2")
    
    
    ## descriptive analysis ====================================================
    
    mod_descriptive_volunteer_server("descriptive_volunteer_1")
    
    
    ## predictive analysis =====================================================
    mod_predictive_volunteer_server("predictive_volunteer_1")

    
    # switch between donor and volunteer ======================================
    
    # shinyjs::show("volunteer_dashboard")
    shinyjs::show("donor_dashboard")
    
    observeEvent(input$donor_switch, {
        shinyjs::hide("donor_dashboard")
        shinyjs::show("volunteer_dashboard")
        updateTabItems(session, inputId = "tabs_2", selected = "home_2")
    })
    
    observeEvent(input$volunteer_switch, {
        shinyjs::hide("volunteer_dashboard")
        shinyjs::show("donor_dashboard")
        updateTabItems(session, inputId = "tabs", selected = "home")
    })
}
