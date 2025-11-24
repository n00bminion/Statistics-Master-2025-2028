library(tidyverse)
library(knitr)
library(ggplot2)
library(dplyr)
library(imputeTS)
library(bslib)
library(shiny)

build_str_path <- function(base, station_name, suffix = "data.txt") {
    station_name <- tolower(gsub(" ", "", gsub("-", "", station_name)))
    str_paths <- paste0(base, station_name, suffix)
    return(str_paths)
}

# build key value mappings to help convert between name and path/url
build_path_to_name_dict <- function(base, station_names) {
    return(
        setNames(
            # values
            station_names,
            # name or keys
            unlist(
                lapply(base, build_str_path, station_names)
            )
        )
    )
}

# create storage dir and download relevant files
download_raw_files <- function(
    output_dir,
    file_paths,
    urls) {
    dir.create(output_dir, showWarnings = FALSE)
    download.file(urls, file_paths, method = "libcurl", mode = "wb")
}

# convert and clean raw txt data into historical data
extract_historical_data <- function(file_path, txt_file_dict) {
    # remove row 1-5 and row 7 # nolint
    txt_data <- readr::read_lines(file_path, skip = 5)[-c(2)]
    # clean the text, row by row
    cleaned_txt_data <- txt_data %>%
        stringr::str_replace_all("---", "NA") %>%
        stringr::str_remove_all("\\*") %>%
        stringr::str_remove_all("#") %>%
        stringr::str_remove_all("Provisional") %>%
        stringr::str_trim()
    # convert into df with the station name
    return(
        readr::read_table(cleaned_txt_data)
        %>% mutate(
                name = txt_file_dict[file_path] # nolint
            )
    )
}

# convert and clean raw text data into location data
extract_geo_data <- function(file_path, txt_file_dict) {
    txt_data <- readr::read_lines(file_path, skip = 1, n_max = 1)
    matched_attr <- gregexpr("\\(?[-0-9.]+", txt_data)
    df <- data.frame(
        matrix(
            as.numeric(
                unlist(regmatches(txt_data, matched_attr))
            ),
            ncol = 5,
            byrow = TRUE
        )
    )
    colnames(df) <- c("E", "W", "lat", "lon", "alt")
    return(
        df
        %>% mutate(
                name = txt_file_dict[file_path] # nolint
            )
    )
}

save_process_data <- function(data, output_dir, file_name) {
    readr::write_csv(
        dplyr::bind_rows(data), build_str_path(output_dir, file_name, ".csv")
    )
}

get_data <- function(output_dir = "MPS4110/Case Study 2/MetOfficeData/", file_name) {
    return(readr::read_csv(
        build_str_path(output_dir, file_name, ".csv")
    ))
}


process_and_save_all_data <- function(
    station_names = c(
        "Sheffield",
        "Yeovilton",
        "Durham",
        "Heathrow",
        "Newton Rigg",
        "Cambridge",
        "Bradford",
        "Oxford",
        "Sutton Bonington",
        "Waddington",
        "Manston",
        "Shawbury",
        "Ross-on-Wye"
    ),
    output_dir = "MPS4110/Case Study 2/MetOfficeData/",
    url = "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/" # nolint
    ) { # nolint

    txt_file_dict <- build_path_to_name_dict(output_dir, station_names)

    # download data to local folder...
    download_raw_files(
        output_dir = output_dir,
        file_paths = names(txt_file_dict),
        urls = build_str_path(url, station_names)
    )

    # load the data into memory...
    hist_data <- lapply(
        names(txt_file_dict),
        extract_historical_data,
        txt_file_dict = txt_file_dict
    )
    geo_data <- lapply(
        names(txt_file_dict),
        extract_geo_data,
        txt_file_dict = txt_file_dict
    )

    # save the data...
    save_process_data(hist_data, output_dir, "hist_data")
    save_process_data(geo_data, output_dir, "geo_data")
}


build_shiny_ui <- function() {
    ui <- bslib::page_sidebar(
        title = "Case Study App Title",
        sidebar = bslib::sidebar(
            sliderInput(
                inputId = "longitudeSlider",
                label = "Longitude",
                min = -180,
                max = 180,
                value = 0
            )
        ),
        plotOutput(outputId = "plotSlider")
    )
    return(ui)
}

build_shiny_server <- function(input, output) {
    # output$plotSlider <- renderPlot({
    #     x <- geo_data$waiting
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)

    #     hist(x,
    #         breaks = bins, col = "#007bc2", border = "white",
    #         xlab = "Waiting time to next eruption (in mins)",
    #         main = "Histogram of waiting times"
    #     )
    # })
}

# MetOfficeData folder & data files should appear when this runs
process_and_save_all_data()

# grab the processed data and load to global
hist_data <- get_data(file_name = "hist_data")
geo_data <- get_data(file_name = "geo_data")
combined_data <- dplyr::inner_join(geo_data, hist_data, by = "name")
combined_data

# build the app and run
shinyApp(
    # ui takes result from func
    ui = build_shiny_ui(),
    # server takes func obj
    server = build_shiny_server
)
