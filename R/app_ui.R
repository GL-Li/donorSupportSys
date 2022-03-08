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
              menuItem("Donor Home",
                       tabName = "home",
                       icon = icon("home")),
              menuItem("Descriptive analysis",
                       tabName = "descriptive",
                       icon = icon("chart-bar")),
              menuItem("Predictive analysis",
                       tabName = "predictive",
                       icon = icon("brain"))
            ),
            
            br(),
            br(),
            
            actionButton("donor_switch", "Go to Volumteer",
                         style="color: #fff; background-color: rgb(50, 60, 50); border-color: rgb(50, 60, 50)")
          ),
          
          ## Body content
          dashboardBody(
            ## modify css  ----
            # tags$head(tags$style(includeCSS("asset/custom.css"))),
            
            
            tabItems(
              
              
              ## home ----
              
              tabItem(
                "home",
                # a("aaa", href = "#shiny-tab-predictive", "data-toggle" = "tab"),
                column(12,
                       img(src = "www/logo.png"),
                       align = "center"),
                fluidRow(
                  column(2),
                  column(8,
                         valueBox("Wellcome to AI-enabled DSS to analyse donnors behaviour",
                                  subtitle = "",
                                  width = 12,
                                  color = "orange"),
                         align = "center"),
                  column(2)
                ),
                
                
                tags$head(tags$style(HTML(
                  "#div_des .tooltip-inner {text-align: left;}",
                  "#div_pred .tooltip-inner {text-align: left;}"
                ))),
                fluidRow(
                  column(2),
                  column(
                    4,
                    div(
                      id = "div_des",
                      valueBoxOutput("click_descriptive", width = 12),
                      bsTooltip("click_descriptive",  
                                "View the descriptive statistics of the data.", 
                                placement = "bottom", 
                                trigger = "hover",
                                options = NULL),
                      align = "center"
                    ),
                    
                  ),
                  
                  column(
                    4,
                    div(
                      id = "div_pred",
                      valueBoxOutput("click_predictive", width = 12),
                      bsTooltip("click_predictive", 
                                "To predict who is going to donate again.", 
                                placement = "bottom", 
                                trigger = "hover",
                                options = NULL),
                      align = "center"
                    )
                  ),
                  column(2)
                ),
                
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                br(),
                
                # fluidRow(
                #     column(2),
                #     column(8, 
                #            actionButton("donor_switch", "Switch to Volumteer"),
                #            align = "center"),
                #     column(2)
                # ),
                
              ),
              
              
              
              
              ## descriptive analysis ----
              tabItem(
                "descriptive",
                
                selectizeInput("donor_data", "Select a dataset",
                               choices = c("donor_data_group_1.csv",
                                           "donor_data_group_2.csv"),
                               selected = "donor_data_group_1.csv"),
                
                fluidRow(
                  valueBoxOutput("donor_info", width = 3),
                  valueBoxOutput("avg_dollar", width = 3),
                  valueBoxOutput("again_donor", width = 3),
                  valueBoxOutput("pct_again", width = 3)
                  
                ),
                hr(),
                
                fluidRow(
                  
                ),
                
                fluidRow(
                  plotOutput("by_state", height = plot_height),
                ),
                hr(),
                
                fluidRow(
                  column(6, plotOutput("by_age", height = plot_height)),
                  column(6, plotOutput("by_n_donation", height = plot_height))
                ),
                hr(),
                
                fluidRow(
                  column(3, plotOutput("pie_gender", height = plot_height_pie)),
                  column(3, plotOutput("pie_ses", height = plot_height_pie)),
                  column(3, plotOutput("pie_college", height = plot_height_pie)),
                  column(3, plotOutput("pie_income", height = plot_height_pie))
                )
                
                
                # fluidRow(
                #     column(4, plotOutput("plot_ses", height = plot_height)),
                #     column(4, plotOutput("plot_gender", height = plot_height)),
                #     column(4, plotOutput("plot_college", height = plot_height))
                # ),
                # hr(), 
                # 
                # fluidRow(
                #     column(4, plotOutput("plot_income", height = plot_height)),
                #     column(4, plotOutput("plot_age", height = plot_height)),
                #     column(4, plotOutput("plot_n_donations", height = plot_height))
                # )
              ),
              
              ## predictive analysis ----
              tabItem(
                "predictive",
                fluidRow(
                  valueBox(value = "Donation Decision Support System",
                           subtitle = "Is the donnor going to donate again?",
                           width = 12,
                           color = "blue")
                ),
                
                ### single donor ----
                
                h1("Predict for a single donor"),
                fluidRow(
                  valueBoxOutput("prediction", width = 6)
                ),
                
                fluidRow(
                  box(selectizeInput("state", "Select a State",
                                     choices = states,
                                     selected = "AK"),
                      height = "100px",
                      width = 4),
                  box(numericInput("age", "Type Age",
                                   value = 50,
                                   min = 0,
                                   max = 100,
                                   step = 1),
                      height = "100px",
                      width = 4),
                  box(numericInput("n_donation", 
                                   "Type Number of Previous Donations",
                                   value = 3,
                                   min = 0,
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
                  # box(radioButtons("wealth", "Select a Wealth Bracket",
                  #                  choices = 1:7,
                  #                  selected = 1,
                  #                  inline = TRUE)),
                  
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
                ),
                
                ### batch of donors ----
                h1("Predict for a batch of donors"),
                fluidRow(
                  column(
                    4,
                    box(fileInput("file_upload", "Choose a file",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        width = 12),
                    box(downloadButton("download", "Download predicitons"),
                        width = 12)
                  ),
                  column(
                    8,
                    DT::dataTableOutput("table")
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
              menuItem("Volunteer Home",
                       tabName = "home_2",
                       icon = icon("home")),
              menuItem("Descriptive analysis",
                       tabName = "descriptive_2",
                       icon = icon("chart-bar")),
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
            ## modify css  ----
            # tags$head(tags$style(includeCSS("asset/custom.css"))),
            
            
            tabItems(
              
              ## home ----
              
              tabItem(
                "home_2",
                # a("aaa", href = "#shiny-tab-predictive", "data-toggle" = "tab"),
                column(12,
                       img(src = "www/logo.png"),
                       align = "center"),
                fluidRow(
                  column(2),
                  column(8,
                         valueBox("Wellcome to AI-enabled DSS to analyse donnors behaviour",
                                  subtitle = "",
                                  width = 12,
                                  color = "orange"),
                         align = "center"),
                  column(2)
                ),
                
                
                tags$head(tags$style(HTML(
                  "#div_des_2 .tooltip-inner {text-align: left;}",
                  "#div_pred_2 .tooltip-inner {text-align: left;}"
                ))),
                fluidRow(
                  column(2),
                  column(
                    4,
                    div(
                      id = "div_des_2",
                      valueBoxOutput("click_descriptive_2", width = 12),
                      bsTooltip("click_descriptive_2",
                                "View the descriptive statistics of the data.",
                                placement = "bottom",
                                trigger = "hover",
                                options = NULL),
                      align = "center"
                    ),
                    
                  ),
                  
                  column(
                    4,
                    div(
                      id = "div_pred_2",
                      valueBoxOutput("click_predictive_2", width = 12),
                      bsTooltip("click_predictive_2",
                                "To predict who is going to volunteer again.",
                                placement = "bottom",
                                trigger = "hover",
                                options = NULL),
                      align = "center"
                    )
                  ),
                  column(2)
                )
                
                # br(),
                # br(),
                # br(),
                # br(),
                # br(),
                # br(),
                # br(),
                # br(),
                # 
                # fluidRow(
                #   column(2),
                #   column(8,
                #          actionButton("volunteer_switch", "Switch to Donor"),
                #          align = "center"),
                #   column(2)
                # )
                
              ),
              
              ## descriptive analysis ----
              tabItem(
                "descriptive_2",
                
                selectizeInput("volunteer_data", "Select a dataset",
                               choices = c("volunteer_data_group_1.csv",
                                           "volunteer_data_group_2.csv"),
                               selected = "volunteer_data_group_1.csv"),
                
                fluidRow(
                  valueBoxOutput("volunteer_info", width = 3),
                  valueBoxOutput("avg_hour", width = 3),
                  valueBoxOutput("again_volunteer", width = 3),
                  valueBoxOutput("pct_again_2", width = 3)
                  
                ),
                hr(),
                
                fluidRow(
                  
                ),
                
                fluidRow(
                  plotOutput("by_state_2", height = plot_height),
                ),
                
                hr(),
                
                fluidRow(
                  column(3, plotOutput("pie_gender_2", height = plot_height_pie)),
                  column(3, plotOutput("pie_ses_2", height = plot_height_pie)),
                  column(3, plotOutput("pie_college_2", height = plot_height_pie)),
                  column(3, plotOutput("pie_income_2", height = plot_height_pie))
                ),
                
                hr(),
                
                fluidRow(
                  column(6, plotOutput("by_age_2", height = plot_height)),
                  column(6, plotOutput("by_n_volunteer", height = plot_height))
                )
              ),
              
              
              ## predictive analysis ----
              tabItem(
                "predictive_2",
                fluidRow(
                  valueBox(value = "Donation Decision Support System",
                           subtitle = "Is the volunteer going to volunteer again?",
                           width = 12,
                           color = "blue")
                ),
                
                ### single donor ----
                
                h1("Predict for a single volunteer"),
                fluidRow(
                  valueBoxOutput("prediction_2", width = 6)
                ),
                
                fluidRow(
                  box(selectizeInput("state_2", "Select a State",
                                     choices = states,
                                     selected = "AK"),
                      height = "100px",
                      width = 4),
                  box(numericInput("age_2", "Type Age",
                                   value = 50,
                                   min = 0,
                                   max = 100,
                                   step = 1),
                      height = "100px",
                      width = 4),
                  box(numericInput("n_volunteer", 
                                   "Type Times of Previous Volunteerings",
                                   value = 3,
                                   min = 0,
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
                  # box(radioButtons("wealth", "Select a Wealth Bracket",
                  #                  choices = 1:7,
                  #                  selected = 1,
                  #                  inline = TRUE)),
                  
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
                ),
                
                ### batch of donors ----
                h1("Predict for a batch of volunteers"),
                fluidRow(
                  column(
                    4,
                    box(fileInput("file_upload_2", "Choose a file",
                                  multiple = FALSE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        width = 12),
                    box(downloadButton("download_2", "Download predicitons"),
                        width = 12)
                  ),
                  column(
                    8,
                    DT::dataTableOutput("table_2")
                  )
                  
                )
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

