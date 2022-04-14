#' plot_leaflet 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plot_leaflet <- function(.data, alpha = 0.7, title = "Number of Donnors"){
    
    # this is sf problem. Have to load sf here. Otherwise error:
    # All columns in a tibble must be vectors. Column `geometry` is a 
    # `sfc_MULTIPOLYGON/sfc` object.
    library(sf)
    
    n_count <- .data %>%
        dplyr::group_by(state) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::mutate(cutted = dplyr::case_when(
            n < 100 ~ "<100",
            n < 1000 ~ "100-999",
            n < 5000 ~ "1000-4999",
            n > 5000 ~ ">5000"
        )) %>%
        dplyr::mutate(cutted = factor(
            cutted,
            levels = c("<100", "100-999", "1000-4999", ">5000")
        ))
    
    sf <- sf_state %>%
        dplyr::rename(state = state_abb) %>%
        dplyr::left_join(n_count)
    
    leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-110.5, 40.5, zoom = 4) %>%
        add_polygons(sf, alpha) %>%
        # add_circles(type) %>%
        # addControl("Data: Decennial Census 2010 and 2020", 
        #            position = "topright") %>%
        addLegend(data = sf,
                  position = "bottomleft",
                  pal = pal(sf),
                  values = ~get("cutted"),
                  title = title)
}

# r_colors <- rgb(t(col2rgb(colors()) / 255))
# names(r_colors) <- colors()

pal <- function(.data){
    # Set color palette for leaflet map legend, which depends on actual data dat,
    # which is a reactive value

    colorFactor(
        palette = c("blue", "green", "orange", "red"),
        domain = .data[["cutted"]]
    )
}


add_polygons <- function(map, .data, alpha){
    
    # n_count <- .data %>%
    #     dplyr::group_by(state) %>%
    #     dplyr::summarise(n = dplyr::n()) %>%
    #     dplyr::mutate(cutted = dplyr::case_when(
    #         n < 100 ~ "1-99",
    #         n < 1000 ~ "100-999",
    #         n < 5000 ~ "1000-4999",
    #         n > 5000 ~ ">5000"
    #     )) %>%
    #     dplyr::mutate(cutted = factor(
    #         cutted,
    #         levels = c("1-99", "100-999", "1000-4999", ">5000")
    #     ))
    # 
    # sf <- sf_state %>%
    #     dplyr::rename(state = state_abb) %>%
    #     dplyr::left_join(n_count)
    
    map %>%
        addPolygons(data = .data,
                    fillColor = ~pal(.data)(get("cutted")),
                    fillOpacity = alpha,
                    weight = 1,
                    layerId = ~state,  # one id for each polygon
                    popup = ~paste0(
                        "<b>", state, "</b>", "<br>",
                        "<b>", n, "</b>", " donors"
                    ))
}
