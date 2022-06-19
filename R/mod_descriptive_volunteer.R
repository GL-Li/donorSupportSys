#' descriptive_volunteer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_descriptive_volunteer_ui <- function(id){
  ns <- NS(id)
  tagList(
      fileInput(ns("volunteer_upload_descriptive"), "Upload Data",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
    
    fluidRow(
      valueBoxOutput(ns("volunteer_info"), width = 3),
      valueBoxOutput(ns("avg_hour"), width = 3),
      valueBoxOutput(ns("again_volunteer"), width = 3),
      valueBoxOutput(ns("pct_again_2"), width = 3)
      
    ),
    hr(),
    
    fluidRow(
      
    ),
    
    fluidRow(
      leafletOutput(ns("map"), height = "600px")
    ),
    
    hr(),

    h2("Volunteering distribution by age and time"),
    
    fluidRow(
      column(4, plotOutput(ns("by_age_2"), height = "200px")),
      column(4, plotOutput(ns("by_n_volunteer"), height = "200px")),
      column(4, plotOutput(ns("volunteer_age_density"), height = "200px"))
    ),
    
    hr(),
    
    h2("Other indicators"),
    
    fluidRow(
      column(6, plotOutput(ns("pie_gender_2"), height = "500px")),
      column(6, plotOutput(ns("pie_ses_2"), height = "500px"))
    ),
    
    fluidRow(
      column(6, plotOutput(ns("pie_college_2"), height = "500px")),
      column(6, plotOutput(ns("pie_income_2"), height = "500px"))
    )
  )
}
    
#' descriptive_volunteer Server Functions
#'
#' @noRd 
mod_descriptive_volunteer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    dat_volunteer <- reactive({
        if (is.null(input$volunteer_upload_descriptive)) {
            dat <- volunteer_data_group_1
        } else {
            dat <- read.csv(input$volunteer_upload_descriptive$datapath, 
                            stringsAsFactors = FALSE)
        }
        
        dat
    })

    
    ### numbers ----
    output$volunteer_info <- renderValueBox({
      n_volunteers <- nrow(dat_volunteer())
      
      valueBox(value = n_volunteers,
               subtitle = "Number of Volunteers")
    })
    
    output$again_volunteer <- renderValueBox({
      n_again <- sum(dat_volunteer()$volunteered_within_last_year)
      
      valueBox(value = n_again,
               subtitle = "Number of Volunteers Volunteered Again Last Year")
    })
    
    output$pct_again_2 <- renderValueBox({
      prob_again <- round(100 * mean(dat_volunteer()$volunteered_within_last_year), 2)
      
      valueBox(value = paste0(prob_again, "%"),
               subtitle = "Percent of Volunteers Volunteered Again Last Year")
    })
    
    
    output$avg_hour <- renderValueBox({
      avg <- mean(dat_volunteer()$total_hour)
      
      valueBox(value = paste0(round(avg, 2), " hours"),
               subtitle = "Average Volunteering Hour by a Volunteer")
    })
    
    
    ### numbers of donors ----
    output$by_state_2 <- renderPlot({
      plot_state(.data = dat_volunteer(), ylab = "Number of Volunteers")
    })
    
    output$by_age_2 <- renderPlot({
      plot_age(.data = dat_volunteer(), ylab = "Number of Volunteers")
    })
    
    output$by_n_volunteer <- renderPlot({
      plot_n_volunteer(.data = dat_volunteer())
    })
    
    output$volunteer_age_density <- renderPlot({
        plot_volunteer_age_density(.data = dat_volunteer(),
                               color = volunteer_type,
                               color_title = "Number of\nVolunteering")
    })
    
    output$pie_gender_2 <- renderPlot({
      plot_pie("gender", "Gender", .data = dat_volunteer())
    })
    
    
    output$pie_ses_2 <- renderPlot({
      plot_pie("ses", "Socioeconomic Status", .data = dat_volunteer())
    })
    
    output$pie_college_2 <- renderPlot({
      plot_pie("college", "College Degree", .data = dat_volunteer())
    })
    
    # output$pie_gender <- renderPlot({
    #     plot_pie("gender")
    # })
    
    output$pie_income_2 <- renderPlot({
      plot_pie("income", "Income Level", 
               .data = dat_volunteer() %>% mutate(income = case_when(
                   income == 1 ~ "< $10000",
                   income == 2 ~ "$10000 - 20000",
                   income == 3 ~ "$20000 - 40000",
                   income == 4 ~ "$40000 - 70000",
                   income == 5 ~ "$70000 - 100000",
                   income == 6 ~ "$100000 - 150000",
                   income == 7 ~ "> $150000")))
    })
    
    ### leaflet map ----
    output$map <- renderLeaflet({
      r_colors <- rgb(t(col2rgb(colors()) / 255))
      names(r_colors) <- colors()
      plot_leaflet(dat_volunteer(), title = "Number of Volunteers", type = "volunteers")
    })
    
  })
}
    
## To be copied in the UI
# mod_descriptive_volunteer_ui("descriptive_volunteer_ui_1")
    
## To be copied in the server
# mod_descriptive_volunteer_server("descriptive_volunteer_ui_1")
