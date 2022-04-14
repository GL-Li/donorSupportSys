## code to prepare `my_dataset` dataset goes here

# model data ----

mod <- readRDS("data-raw/lgr_mod.RDS")
usethis::use_data(mod, overwrite = TRUE)

mod_2 <- readRDS("data-raw/lgr_mod_volunteer.RDS")
usethis::use_data(mod_2, overwrite = TRUE)


# values ----

states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
            "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", 
            "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
            "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
            "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
usethis::use_data(states, overwrite = TRUE)

plot_height = "200px"
usethis::use_data(plot_height)

plot_height_pie = "300px"
usethis::use_data(plot_height_pie)


# sample dataset ----

donor_data_group_1 <- read.csv("data-raw/donor_data_group_1.csv")
usethis::use_data(donor_data_group_1)

donor_data_group_2 <- read.csv("data-raw/donor_data_group_2.csv")
usethis::use_data(donor_data_group_2)


volunteer_data_group_1 <- read.csv("data-raw/volunteer_data_group_1.csv")
usethis::use_data(volunteer_data_group_1)

volunteer_data_group_2 <- read.csv("data-raw/volunteer_data_group_2.csv")
usethis::use_data(volunteer_data_group_2)


# sf object ----

sf_state <- tigris::states(cb = TRUE, resolution = "20m") %>%
    mutate(state_abb = STUSPS) %>%
    select(state_abb)
usethis::use_data(sf_state, overwrite = TRUE)
