# plot data ===================================================================

# percent donated again from the data. Not used in Shiny app
plot_binary <- function(x, 
                        y = donated_within_last_year, 
                        .data = dat_donor,
                        x_type = "discrete", 
                        y_axis = "Probability of Donating Again"){
    
    if (x_type == "discrete"){
        avg_count <- .data %>%
            group_by({{x}}) %>%
            summarise(avg = mean({{y}}),
                      count = n()) 
    } else if (x_type == "continuous"){
        avg_count <- .data %>%
            mutate(cat = cut_number({{x}}, n = 5)) %>%
            group_by(cat) %>%
            summarise(avg = mean({{y}}),
                      count = n())
    }
    
    count_max <- max(avg_count$count)
    
    if (x_type == "discrete"){
        g <- avg_count %>%
            ggplot(aes({{x}}, avg))
    } else if (x_type == "continuous"){
        g <- avg_count %>%
            ggplot(aes(cat, avg))
    }
    
    g +
        # geom_line(aes(group = 1)) +
        geom_point(aes(size = count)) +
        scale_y_continuous(limits = c(0, 0.4)) +
        scale_size_area(limits = c(0, count_max)) +
        labs(y = y_axis) +
        theme_bw()
}


# pie plot
plot_pie <- function(y, title = "", .data = dat_donor){
    .data %>%
        mutate(college = ifelse(college == 0, "No", "Yes"),
               ses = case_when(ses == 1 ~ "High",
                               ses == 2 ~ "Average",
                               ses == 3 ~ "Low"),
               gender = ifelse(gender == "M", "Male", "Female")) %>%
        count(get(y)) %>%
        # mutate(`get(y)` = factor(`get(y)`)) %>%
        mutate(`get(y)` = reorder(`get(y)`, n)) %>%
        ggplot(aes("", n, fill = `get(y)`)) +
        geom_col() +
        geom_text(aes(label = paste0(`get(y)`, "\n(", n, ")")), 
                  position = position_stack(vjust = 0.5),
                  color = "black") +
        labs(title = title) +
        coord_polar("y", start = 0) +
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_brewer(palette="Set1")
    
}

plot_state <- function(.data = dat_donor,
                       ylab = "Number of Donnors"){
    count <- .data %>%
        count(state) %>%
        mutate(state = reorder(state, -n))
    count %>%
        ggplot(aes(state, n)) +
        geom_col(fill = "lightblue") +
        geom_text(aes(label = n), vjust = -0.2) +
        scale_y_continuous(expand = c(0, 0.07 * max(count$n))) +
        labs(y = ylab,
             x = "State") +
        theme_bw() +
        theme(panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
}

plot_age <- function(.data = dat_donor,
                     ylab = "Number of Donnors"){
    .data %>%
        count(age) %>%
        ggplot(aes(age, n)) +
        geom_line() +
        # geom_point() +
        scale_x_continuous(breaks = c(20, 40, 60, 80, 100)) +
        theme_bw() +
        labs(x = "Age",
             y = ylab)
}

plot_n_donation <- function(.data = dat_donor){
    .data %>%
        filter(n_donation > 0) %>%
        count(n_donation) %>%
        ggplot(aes(n_donation, n)) +
        geom_line(size = 0.3) +
        geom_point(size = 1) +
        scale_x_continuous(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100)) +
        theme_bw() +
        labs(x = "Number of Donations",
             y = "Number of Donnors")
}


plot_n_volunteer <- function(.data = dat_volunteer){
    .data %>%
        filter(n_volunteering > 0) %>%
        count(n_volunteering) %>%
        ggplot(aes(n_volunteering, n)) +
        geom_line(size = 0.3) +
        geom_point(size = 1) +
        scale_x_continuous(limits = c(0, 100), breaks = c(20, 40, 60, 80, 100)) +
        theme_bw() +
        labs(x = "Times of Volunteering",
             y = "Number of Volunteers")
}


plot_donor_age_density <- function(.data, color, color_title) {
    .data %>%
        filter(n_donation > 0) %>%
        mutate(donor_type = cut(n_donation, 
                                breaks = c(0, 1,  10, Inf),
                                labels = c("1", "2-10", ">10"))) %>%
        ggplot(aes(age, color = factor({{color}}))) +
        geom_density() +
        labs(x = "Age",
             y = "Density",
             color = color_title) +
        theme_bw()
}


plot_volunteer_age_density <- function(.data, color, color_title) {
    .data %>%
        filter(n_volunteering > 0) %>%
        mutate(volunteer_type = cut(n_volunteering, 
                                breaks = c(0, 1,  10, Inf),
                                labels = c("1", "2-10", ">10"))) %>%
        ggplot(aes(age, color = factor({{color}}))) +
        geom_density() +
        labs(x = "Age",
             y = "Density",
             color = color_title) +
        theme_bw()
}


# predictive visualization ====================================================
# Given a dataframe of donor attribute, predicted the total number of people 
# going to donate and the percentage of total and by age, ses, ...

# dat = read_csv("data-raw/donor_data_group_1.csv")
# pred_class <- predict(mod, dat, type = "class") 
# pred_prob <- predict(mod, dat, type = "prob")[,2]
# dat_pred <- bind_cols(dat, pred_class) %>%
#     bind_cols(pred_prob)
# 
# dat_pred %>%
#     group_by(across("ses")) %>%
#     summarise(avg = mean(.pred_1)) %>%
#     ggplot(aes(ses, avg)) +
#     geom_col()


plot_pred_bar <- function(mdl, .data, col) {
    pred_class <- predict(mdl, .data, type = "class") 
    pred_prob <- predict(mdl, .data, type = "prob")[,2]
    dat_pred <- bind_cols(.data, pred_class) %>%
        bind_cols(pred_prob) %>%
        filter(state %in% states)
    
    if (col %in% c("age", "n_donation", "n_volunteering")) {
        g <- dat_pred %>%
            group_by(across(col)) %>%
            summarise(`Predicted Probability` = mean(.pred_1),
                      n = n()) %>%
            ggplot(aes(get(col), `Predicted Probability`)) +
            geom_line(alpha = 0.3) +
            geom_point(aes(size = n)) +
            scale_size_area(max_size = 3) +
            theme_bw()
    } else (
        g <- dat_pred %>%
            group_by(across(col)) %>%
            summarise(`Predicted Probability` = mean(.pred_1)) %>%
            ggplot(aes(get(col), `Predicted Probability`)) +
            geom_col() +
            theme_bw()
    )
    return(g)
}
