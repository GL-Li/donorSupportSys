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
  
  
  ## descriptive analysis ====================================================
  
  mod_descriptive_server("descriptive_1")
  
  # dat_donor <- reactive({
  #     get(input$donor_data)
  # })
  # 
  # ### numbers ----
  # output$donor_info <- renderValueBox({
  #     n_donors <- nrow(dat_donor())
  #     
  #     valueBox(value = n_donors,
  #              subtitle = "Number of Donors")
  # })
  # 
  # output$again_donor <- renderValueBox({
  #     n_again <- sum(dat_donor()$donated_within_last_year)
  #     
  #     valueBox(value = n_again,
  #              subtitle = "Number of Donors Donated Again Last Year")
  # })
  # 
  # output$pct_again <- renderValueBox({
  #     prob_again <- round(100 * mean(dat_donor()$donated_within_last_year), 2)
  #     
  #     valueBox(value = paste0(prob_again, "%"),
  #              subtitle = "Percent of Donors Donated Again Last Year")
  # })
  # 
  # 
  # output$avg_dollar <- renderValueBox({
  #     avg <- mean(dat_donor()$total_dollar)
  # 
  #     valueBox(value = paste0("$", round(avg, 2)),
  #              subtitle = "Average Lifetime Donation by a Donor")
  # })
  # 
  # 
  # ### numbers of donors ----
  # output$by_state <- renderPlot({
  #     plot_state(.data = dat_donor())
  # })
  # 
  # output$by_age <- renderPlot({
  #     plot_age(.data = dat_donor())
  # })
  # 
  # output$by_n_donation <- renderPlot({
  #     plot_n_donation(.data = dat_donor())
  # })
  # 
  # output$pie_gender <- renderPlot({
  #     plot_pie("gender", "Gender", .data = dat_donor())
  # })
  # 
  # 
  # output$pie_ses <- renderPlot({
  #     plot_pie("ses", "Socioeconomic Status", .data = dat_donor())
  # })
  # 
  # output$pie_college <- renderPlot({
  #     plot_pie("college", "College Degree", .data = dat_donor())
  # })
  # 
  # # output$pie_gender <- renderPlot({
  # #     plot_pie("gender")
  # # })
  # 
  # output$pie_income <- renderPlot({
  #     plot_pie("income", "Income Level", .data = dat_donor())
  # })
  # 
  # 
  # ### percent donated again ----
  # 
  # output$plot_ses <- renderPlot({
  #     plot_binary(ses, .data = dat_donor()) +
  #         labs(x = "Socioeconomic Status")
  # })
  # 
  # output$plot_gender <- renderPlot({
  #     plot_binary(gender, .data = dat_donor()) +
  #         labs(x = "Gender")
  # })
  # 
  # output$plot_college <- renderPlot({
  #     plot_binary(college, .data = dat_donor()) +
  #         labs(x = "College Degree")
  # })
  # 
  # output$plot_income <- renderPlot({
  #     plot_binary(income, .data = dat_donor()) +
  #         labs(x = "Income Bracket")
  # })
  # 
  # output$plot_age <- renderPlot({
  #     plot_binary(age, x_type = "continuous", .data = dat_donor()) +
  #         labs(x = "Age Bracket")
  # })
  # 
  # output$plot_n_donations <- renderPlot({
  #     plot_binary(n_donation, x_type = "continuous", .data = dat_donor()) +
  #         labs(x = "Number of Past Donations Bracket")
  # })
  # 
  ## predictive analysis =====================================================
  ### single donor prediction ----
  
  output$prediction <- renderValueBox({
    req(input$n_donation, input$age)
    
    state <- input$state
    ses <- input$ses
    age <- input$age
    income <- input$income
    gender <- input$gender
    college <- ifelse(input$edu == "Yes", 1, 0)
    n_donatiion <- input$n_donation
    
    new_dat <- data.frame(
      state = state,
      ses = as.integer(ses),
      income = as.integer(income),
      age = age,
      income = as.integer(income),
      gender = gender,
      college = as.integer(college),
      n_donation = as.integer(n_donatiion)
    )
    
    prob <- predict(mod, new_dat, type = "prob")[2] %>%
      round(4)
    
    valueBox(value = ifelse(prob > 0.5, 
                            "Likely to donate again", 
                            "Unlikely to donate again"),
             subtitle = paste0("Predicted Probability = ", prob),
             icon = icon("heart"),
             color = ifelse(prob > 0.5, "fuchsia", "light-blue"))
  })
  
  ### batch donnor prediction ----
  

  
  dat_0 <- mod_uploadfile_server("uploadfile_pred_1", mdl = mod)
  
  output$table <- DT::renderDataTable({
    dat_0()
  })
  
  output$download <- downloadHandler(
    filename = function() {"prediction.csv"},
    content = function(file) {
      write.csv(dat_0(), file, row.names = FALSE)
    }
  )
  
  
  ## predictive visualization ----
  # mod_visualization_server("visualization_1", mod)
  
  dat1 <- mod_upload_file_general_server("upload_file_general_1")
  
  output$state_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "state") +
      labs(x = NULL)
  })
  
  output$age_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "age") +
      labs(x = "Age")
  })
  
  output$n_donation_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "n_donation") +
      scale_x_continuous(limits = c(1, 70)) +
      labs(x = "Number of Previous Donations")
  })
  
  output$ses_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "ses") +
      scale_x_continuous(breaks = 1:3, 
                         labels = c("High", "Average", "Low")) +
      labs(x = "Socioeconomic Status")
  })
  
  output$income_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "income") +
      scale_x_continuous(breaks = 1:7) +
      labs(x = "Income Group")
  })
  
  output$gender_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "gender") +
      scale_x_discrete(breaks = c("M", "F"), 
                       labels = c("Male", "Female")) +
      labs(x = "Gender")
  })
  
  output$college_prob <- renderPlot({
    plot_pred_bar(mod, dat1(), "college") +
      scale_x_continuous(breaks = 0:1, 
                         labels = c("No College", "College or Above")) +
      labs(x = "Education")
  })
  
  # volunteer ===============================================================
  
  ## home ----
  mod_home_server("home_2")
  
  
  ## descriptive analysis ====================================================
  
  mod_descriptive_volunteer_server("descriptive_volunteer_1")
  
  
  ## predictive analysis =====================================================
  ### single donor prediction ----
  
  output$prediction_2 <- renderValueBox({
    req(input$n_volunteer, input$age_2)
    
    state <- input$state_2
    ses <- input$ses_2
    age <- input$age_2
    income <- input$income_2
    gender <- input$gender_2
    college <- ifelse(input$edu_2 == "Yes", 1, 0)
    n_volunteer <- input$n_volunteer
    
    new_dat <- data.frame(
      state = state,
      ses = as.integer(ses),
      income = as.integer(income),
      age = age,
      income = as.integer(income),
      gender = gender,
      college = as.integer(college),
      n_volunteering = as.integer(n_volunteer)
    )
    
    prob <- predict(mod_2, new_dat, type = "prob")[2] %>%
      round(4)
    
    valueBox(value = ifelse(prob > 0.5, 
                            "Likely to volunteer again", 
                            "Unlikely to volunteer again"),
             subtitle = paste0("Predicted Probability = ", prob),
             icon = icon("heart"),
             color = ifelse(prob > 0.5, "fuchsia", "light-blue"))
  })
  
  ### batch donnor prediction ----
  
  dat_2 <- dat <- mod_uploadfile_server("uploadfile_pred_2", mdl = mod_2)
  
  output$table_2 <- DT::renderDataTable({
    dat_2()
  })
  
  output$download_2 <- downloadHandler(
    filename = function() {"prediction.csv"},
    content = function(file) {
      write.csv(dat_2(), file, row.names = FALSE)
    }
  )
  
  
  
  ## predictive visualization ----
  # mod_visualization_server("visualization_1", mod)
  
  dat2 <- mod_upload_file_general_server("upload_file_general_2")
  
  output$state_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "state") +
      labs(x = NULL)
  })
  
  output$age_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "age") +
      labs(x = "Age")
  })
  
  output$n_donation_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "n_volunteering") +
      scale_x_continuous(limits = c(1, 70)) +
      labs(x = "Number of Previous Volunteering")
  })
  
  output$ses_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "ses") +
      scale_x_continuous(breaks = 1:3, 
                         labels = c("High", "Average", "Low")) +
      labs(x = "Socioeconomic Status")
  })
  
  output$income_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "income") +
      scale_x_continuous(breaks = 1:7) +
      labs(x = "Income Group")
  })
  
  output$gender_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "gender") +
      scale_x_discrete(breaks = c("M", "F"), 
                       labels = c("Male", "Female")) +
      labs(x = "Gender")
  })
  
  output$college_prob_2 <- renderPlot({
    plot_pred_bar(mod_2, dat2(), "college") +
      scale_x_continuous(breaks = 0:1, 
                         labels = c("No College", "College or Above")) +
      labs(x = "Education")
  })
  
  
  
  
  
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
