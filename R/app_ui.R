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
                                            mod_upload_file_general_ui("uploadfile_pred_1")
                                        )
                                    ),
                                    
                                    fluidRow(
                                        DT::dataTableOutput("table"),
                                        downloadButton("download", "Download predicitons")
                                    ),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                        column(6, plotOutput("age_prob", height = plot_height)),
                                        column(6, plotOutput("n_donation_prob", height = plot_height))
                                    )
                                ),
                                
                                ### single donor ----
                                
                                tabPanel(
                                    "Predict for a single donor",
                                    fluidRow(
                                        valueBoxOutput("prediction", width = 12)
                                    ),
                                    
                                    fluidRow(
                                        box(selectizeInput("state", "Select a State",
                                                           choices = states,
                                                           selected = "AK"), 
                                            height = "100px",
                                            width = 4),
                                        box(sliderInput("age", "Select Age",
                                                        value = 50,
                                                        min = 0,
                                                        max = 100,
                                                        step = 1),
                                            height = "100px",
                                            width = 4),
                                        box(sliderInput("n_donation", 
                                                        "Select Number of Previous Donations",
                                                        value = 3,
                                                        min = 0,
                                                        max =100,
                                                        step = 1),
                                            width = 4,
                                            height = "100px")
                                    ),
                                    
                                    fluidRow(
                                        box(radioButtons("ses", "Select a Socioeconomic Status",
                                                         choices = 1:3,
                                                         selected = 1,
                                                         inline = TRUE)),
                                        box(radioButtons("income", "Select a Income Bracket",
                                                         choices = 1:7,
                                                         selected = 1,
                                                         inline = TRUE)),
                                        
                                        box(radioButtons("gender", "Select a Gender",
                                                         choices = c("F", "M"),
                                                         selected = "F",
                                                         inline = TRUE),
                                            width = 6),
                                        box(radioButtons("edu", "College Educated?",
                                                         choices = c("Yes", "No"),
                                                         selected = "Yes",
                                                         inline = TRUE),
                                            width = 6)
                                    )
                                )
                            )
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
                                            mod_upload_file_general_ui("uploadfile_pred_2")
                                        )
                                    ),
                                    
                                    fluidRow(
                                        DT::dataTableOutput("table_2"),
                                        downloadButton("download_2", "Download predicitons")
                                    ),
                                    
                                    br(),
                                    br(),
                                    br(),
                                    
                                    fluidRow(
                                        column(6, plotOutput("age_prob_2", height = plot_height)),
                                        column(6, plotOutput("n_donation_prob_2", height = plot_height))
                                    )
                                ),
                                
                                
                                ### single donor ----
                                tabPanel(
                                    "Predict for a single volunteer",
                                    fluidRow(
                                        valueBoxOutput("prediction_2", width = 12)
                                    ),
                                    
                                    fluidRow(
                                        box(selectizeInput("state_2", "Select a State",
                                                           choices = states,
                                                           selected = "AK"),
                                            height = "100px",
                                            width = 4),
                                        box(sliderInput("age_2", "Select Age",
                                                        value = 50,
                                                        min = 0,
                                                        max = 100,
                                                        step = 1),
                                            height = "100px",
                                            width = 4),
                                        box(sliderInput("n_volunteer", 
                                                        "Select Times of Previous Volunteerings",
                                                        value = 3,
                                                        min = 0,
                                                        max = 100,
                                                        step = 1),
                                            width = 4,
                                            height = "100px")
                                    ),
                                    
                                    fluidRow(
                                        box(radioButtons("ses_2", "Select a Socioeconomic Status",
                                                         choices = 1:3,
                                                         selected = 1,
                                                         inline = TRUE)),
                                        box(radioButtons("income_2", "Select a Income Bracket",
                                                         choices = 1:7,
                                                         selected = 1,
                                                         inline = TRUE)),
                                        
                                        box(radioButtons("gender_2", "Select a Gender",
                                                         choices = c("F", "M"),
                                                         selected = "F",
                                                         inline = TRUE),
                                            width = 6),
                                        box(radioButtons("edu_2", "College Educated?",
                                                         choices = c("Yes", "No"),
                                                         selected = "Yes",
                                                         inline = TRUE),
                                            width = 6)
                                    )
                                )
                            ),
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

