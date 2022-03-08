# mod <- function(){
#     readRDS("lgr_mod.RDS")
# } 
# mod_2 <- function(){
#     readRDS("lgr_mod_volunteer.RDS")
# } 
# 
# states <- function(){
#     c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
#       "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
#       "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
#       "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
#       "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
# } 

# dat_donor <- read_csv("dat_model.csv") %>%
#     filter(state %in% states)
# 
# plot_height = "200px"
# plot_height_pie = "300px"

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

plot_pie <- function(y, title = "", .data = dat_donor){
    .data %>%
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
