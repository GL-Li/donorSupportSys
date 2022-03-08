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
    output$click_descriptive <- renderValueBox({
        valueBox(
            value = actionLink(
                inputId = "link_descriptive",
                label = div("Descriptive analysis", 
                            style = "color: white; font-size: 30px")
            ),
            subtitle = "",
            color = "light-blue"
        )
    })
    
    observeEvent(input$link_descriptive, {
        updateTabItems(session, inputId = "tabs", selected = "descriptive")
    })
    
    output$click_predictive <- renderValueBox({
        valueBox(
            value = actionLink(
                inputId = "link_predictive",
                label = div("Predictive analysis", 
                            style = "color: white; font-size: 30px")
            ),
            subtitle = "",
            color = "light-blue"
        )
    })
    
    observeEvent(input$link_predictive, {
        updateTabItems(session, inputId = "tabs", selected = "predictive")
    })
    
    ## descriptive analysis ====================================================
    
    dat_donor <- reactive({
        read_csv(input$donor_data) %>%
            filter(state %in% states) %>%
            # mutate(ses = as.character(ses)) %>%
            mutate(ses = case_when(ses == "1" ~ "high",
                                   ses == "2" ~ "average",
                                   ses == "3" ~ "low"),
                   college = case_when(college == 0 ~ "yes",
                                       college == 1 ~ "no"),
                   gender = ifelse(gender == "M", "male", "female"))
    }) 
    
    ### numbers ----
    output$donor_info <- renderValueBox({
        n_donors <- nrow(dat_donor())
        
        valueBox(value = n_donors,
                 subtitle = "Number of Donors")
    })
    
    output$again_donor <- renderValueBox({
        n_again <- sum(dat_donor()$donated_within_last_year)
        
        valueBox(value = n_again,
                 subtitle = "Number of Donors Donated Again Last Year")
    })
    
    output$pct_again <- renderValueBox({
        prob_again <- round(100 * mean(dat_donor()$donated_within_last_year), 2)
        
        valueBox(value = paste0(prob_again, "%"),
                 subtitle = "Percent of Donors Donated Again Last Year")
    })
    
    
    output$avg_dollar <- renderValueBox({
        avg <- mean(dat_donor()$total_dollar)
        print("===========+++++++++++=========")
        print(head(dat_donor()))
        
        valueBox(value = paste0("$", round(avg, 2)),
                 subtitle = "Average Lifetime Donation by a Donor")
    })
    
    
    ### numbers of donors ----
    output$by_state <- renderPlot({
        plot_state(.data = dat_donor())
    })
    
    output$by_age <- renderPlot({
        plot_age(.data = dat_donor())
    })
    
    output$by_n_donation <- renderPlot({
        plot_n_donation(.data = dat_donor())
    })
    
    output$pie_gender <- renderPlot({
        plot_pie("gender", "Gender", .data = dat_donor())
    })
    
    
    output$pie_ses <- renderPlot({
        plot_pie("ses", "Socioeconomic Status", .data = dat_donor())
    })
    
    output$pie_college <- renderPlot({
        plot_pie("college", "College Degree", .data = dat_donor())
    })
    
    # output$pie_gender <- renderPlot({
    #     plot_pie("gender")
    # })
    
    output$pie_income <- renderPlot({
        plot_pie("income", "Income Level", .data = dat_donor())
    })
    
    
    ### percent donated again ----
    
    output$plot_ses <- renderPlot({
        plot_binary(ses, .data = dat_donor()) +
            labs(x = "Socioeconomic Status")
    })
    
    output$plot_gender <- renderPlot({
        plot_binary(gender, .data = dat_donor()) +
            labs(x = "Gender")
    })
    
    output$plot_college <- renderPlot({
        plot_binary(college, .data = dat_donor()) +
            labs(x = "College Degree")
    })
    
    output$plot_income <- renderPlot({
        plot_binary(income, .data = dat_donor()) +
            labs(x = "Income Bracket")
    })
    
    output$plot_age <- renderPlot({
        plot_binary(age, x_type = "continuous", .data = dat_donor()) +
            labs(x = "Age Bracket")
    })
    
    output$plot_n_donations <- renderPlot({
        plot_binary(n_donation, x_type = "continuous", .data = dat_donor()) +
            labs(x = "Number of Past Donations Bracket")
    })
    
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
    
    dat <- reactive({
        req(input$file_upload)
        read.csv(input$file_upload$datapath, 
                 stringsAsFactors = FALSE) %>%
            bind_cols(predict(mod, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class) 
    })
    
    output$table <- DT::renderDataTable({
        dat()
    })
    
    output$download <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat(), file, row.names = FALSE)
        }
    )
    
    
    # volunteer ===============================================================
    
    ## home ----
    
    output$click_descriptive_2 <- renderValueBox({
        valueBox(
            value = actionLink(
                inputId = "link_descriptive_2",
                label = div("Descriptive analysis", 
                            style = "color: white; font-size: 30px")
            ),
            subtitle = "",
            color = "light-blue"
        )
    })
    
    observeEvent(input$link_descriptive_2, {
        updateTabItems(session, inputId = "tabs_2", selected = "descriptive_2")
    })
    
    output$click_predictive_2 <- renderValueBox({
        valueBox(
            value = actionLink(
                inputId = "link_predictive_2",
                label = div("Predictive analysis", 
                            style = "color: white; font-size: 30px")
            ),
            subtitle = "",
            color = "light-blue"
        )
    })
    
    observeEvent(input$link_predictive_2, {
        updateTabItems(session, inputId = "tabs_2", selected = "predictive_2")
    })
    
    
    
    ## descriptive analysis ====================================================
    
    dat_volunteer <- reactive({
        read_csv(input$volunteer_data) %>%
            filter(state %in% states) %>%
            # mutate(ses = as.character(ses)) %>%
            mutate(ses = case_when(ses == "1" ~ "high",
                                   ses == "2" ~ "average",
                                   ses == "3" ~ "low"),
                   college = case_when(college == 0 ~ "yes",
                                       college == 1 ~ "no"),
                   gender = ifelse(gender == "M", "male", "female"))
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
        print("===========+++++++++++=========")
        print(head(dat_donor()))
        
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
        plot_pie("income", "Income Level", .data = dat_volunteer())
    })
    
    
    ### percent volunteered again ----
    
    # output$plot_ses_2 <- renderPlot({
    #     plot_binary(ses, .data = dat_volunteer()) +
    #         labs(x = "Socioeconomic Status")
    # })
    # 
    # output$plot_gender_2 <- renderPlot({
    #     plot_binary(gender, .data = dat_volunteer()) +
    #         labs(x = "Gender")
    # })
    # 
    # output$plot_college_2 <- renderPlot({
    #     plot_binary(college, .data = dat_volunteer()) +
    #         labs(x = "College Degree")
    # })
    # 
    # output$plot_income_2 <- renderPlot({
    #     plot_binary(income, .data = dat_volunteer()) +
    #         labs(x = "Income Bracket")
    # })
    # 
    # output$plot_age_2 <- renderPlot({
    #     plot_binary(age, x_type = "continuous", .data = dat_volunteer()) +
    #         labs(x = "Age Bracket")
    # })
    # 
    # output$plot_n_donations <- renderPlot({
    #     plot_binary(n_donation, x_type = "continuous", .data = dat_donor()) +
    #         labs(x = "Number of Past Donations Bracket")
    # })
    
    
    
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
    
    dat_2 <- reactive({
        req(input$file_upload_2)
        read.csv(input$file_upload_2$datapath, 
                 stringsAsFactors = FALSE) %>%
            bind_cols(predict(mod_2, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod_2, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class) 
    })
    
    output$table_2 <- DT::renderDataTable({
        dat_2()
    })
    
    output$download_2 <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat_2(), file, row.names = FALSE)
        }
    )
    
    
    
    
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
