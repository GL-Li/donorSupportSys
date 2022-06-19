#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
    tagList(
        # Leave this function for adding external resources
        golem_add_external_resources(),
        # Your application UI logic 
        shinyjs::useShinyjs(),
        
        
        
        # donor dashboard ======================================================
        
        div(id = "donor_dashboard",
            style = "display:none",
            
            dashboardPage(
                title = "Donor Support System",
                # use dashboard but disable header and sidebar. allow for future expansion
                # dashboardHeader(title = "AI-enabled DSS"),
                dashboardHeader(title = tags$img(src = "www/logo_small.png")),
                
                dashboardSidebar(
                    sidebarMenu(
                        id = "tabs",
                        menuItem("Donor Analysis",
                                 tabName = "home",
                                 icon = icon("home")),
                        menuItem("Descriptive analysis",
                                 tabName = "descriptive",
                                 icon = icon("chart-bar")),
                        menuItem("About",
                                 tabName = "about",
                                 icon = icon("list-alt")),
                        menuItem("Predictive analysis",
                                 tabName = "predictive",
                                 icon = icon("brain"))
                    ),
                    
                    br(),
                    br(),
                    
                    actionButton("donor_switch", "Go to Volunteer",
                                 style="color: #fff; background-color: rgb(50, 60, 50); border-color: rgb(50, 60, 50)")
                ),
                
                ## Body content
                dashboardBody(
                    
                    tabItems(
                        
                        ## home ----
                        
                        tabItem(
                            tabName = "home",
                            
                            mod_home_ui("home_1",
                                        "View the descriptive statistics of the data.",
                                        "To predict who is going to donate again.")
                        ),
                        
                        
                        ## descriptive analysis ----
                        
                        tabItem(
                            tabName = "descriptive",
                            
                            mod_descriptive_ui("descriptive_1")
                        ),
                        
                        
                        ## about ----
                        
                        tabItem(
                            tabName = "about",
                            h1("to be filled")
                        ),
                        
                        ## predictive analysis ----
                        tabItem(
                            tabName = "predictive",
                            mod_predictive_ui("predictive_1")
                        )
                    )
                )
            )
        ),
        
        
        
        # volunteer dashboard ======================================================
        
        div(id = "volunteer_dashboard",
            style = "display:none",
            
            dashboardPage(
                title = "Volunteer Support System",
                # use dashboard but disable header and sidebar. allow for future expansion
                # dashboardHeader(title = "AI-enabled DSS"),
                dashboardHeader(title = tags$img(src = "www/logo_small.png")),
                
                dashboardSidebar(
                    # sidebarMenuOutput("sidebar_menu_2")
                    sidebarMenu(
                        id = "tabs_2",
                        menuItem("Volunteer Analysis",
                                 tabName = "home_2",
                                 icon = icon("home")),
                        menuItem("Descriptive analysis",
                                 tabName = "descriptive_2",
                                 icon = icon("chart-bar")),
                        menuItem("About",
                                 tabName = "about_2",
                                 icon = icon("list-alt")),
                        menuItem("Predictive analysis",
                                 tabName = "predictive_2",
                                 icon = icon("brain"))
                    ),
                    
                    br(),
                    br(),
                    
                    actionButton("volunteer_switch", "Go to Donor",
                                 style="color: #fff; background-color: rgb(50, 50, 60); border-color: rgb(50, 50, 60)")
                ),
                
                ## Body content
                dashboardBody(
                    
                    tabItems(
                        
                        ## home ----
                        
                        tabItem(
                            tabName = "home_2",
                            
                            mod_home_ui("home_2",
                                        "View the descriptive statistics of the data.",
                                        "To predict who is going to volunteer again."),
                        ),
                        
                        
                        ## descriptive analysis ----
                        tabItem(
                            tabName = "descriptive_2",
                            mod_descriptive_volunteer_ui("descriptive_volunteer_1")
                        ),
                        
                        
                        ## about ----
                        
                        tabItem(
                            tabName = "about_2",
                            h1("to be filled")
                        ),
                        
                        
                        ## predictive analysis ----
                        tabItem(
                            tabName = "predictive_2",
                            mod_predictive_volunteer_ui("predictive_volunteer_1")
                        )
                    )
                )
            )
        )
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
    
    add_resource_path(
        'www', app_sys('app/www')
    )
    
    add_resource_path(
        "sbs", system.file("www", package = "shinyBS")
    )
    
    tags$head(
        favicon(),
        bundle_resources(
            path = app_sys('app/www'),
            app_title = 'donorSupportSys'
        )
        # Add here other external resources
        # for example, you can add shinyalert::useShinyalert() 
    )
}

