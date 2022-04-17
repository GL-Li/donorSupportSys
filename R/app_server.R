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
        
        valueBox(value = ifelse(prob > 0.1522, 
                                "More likely to donate again than an average donor", 
                                "Less likely to donate again than an average donor"),
                 subtitle = paste0("Predicted Probability = ", prob),
                 icon = icon("heart"),
                 color = ifelse(prob > 0.1522, "fuchsia", "light-blue"))
    })
    
    ### batch donnor prediction ----
    
    
    
    dat0 <- mod_upload_file_general_server("uploadfile_pred_1")
    
    dat_pred <- reactive({
        dat0() %>%
            bind_cols(predict(mod, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class)
    })
    
    output$table <- DT::renderDataTable({
        dat_pred()
    })
    
    output$download <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat_pred(), file, row.names = FALSE)
        }
    )
    
    output$age_prob <- renderPlot({
        plot_pred_bar(mod, dat0(), "age") +
            labs(x = "Age")
    })
    
    output$n_donation_prob <- renderPlot({
        plot_pred_bar(mod, dat0(), "n_donation") +
            scale_x_continuous(limits = c(1, 70)) +
            labs(x = "Number of Previous Donations")
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
        
        valueBox(value = ifelse(prob > 0.1495, 
                                "More likely to volunteer again than an average volunteer", 
                                "Less likely to volunteer again than an average volunteer"),
                 subtitle = paste0("Predicted Probability = ", prob),
                 icon = icon("heart"),
                 color = ifelse(prob > 0.1495, "fuchsia", "light-blue"))
    })
    
    ### batch donnor prediction ----
    
    dat2 <- mod_upload_file_general_server("uploadfile_pred_2")
    
    dat_pred_2 <- reactive({
        dat2() %>%
            bind_cols(predict(mod_2, ., type = "prob")[, 2] %>%
                          round(4)) %>%
            bind_cols(predict(mod_2, .)) %>%
            rename(predicted_prob = .pred_1,
                   predicted_result = .pred_class)
    })
    
    output$table_2 <- DT::renderDataTable({
        dat_pred_2()
    })
    
    output$download_2 <- downloadHandler(
        filename = function() {"prediction.csv"},
        content = function(file) {
            write.csv(dat_pred_2(), file, row.names = FALSE)
        }
    )
    
    output$age_prob_2 <- renderPlot({
        plot_pred_bar(mod_2, dat2(), "age") +
            labs(x = "Age")
    })
    
    output$n_donation_prob_2 <- renderPlot({
        plot_pred_bar(mod_2, dat2(), "n_volunteering") +
            scale_x_continuous(limits = c(1, 70)) +
            labs(x = "Number of Previous Donations")
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
