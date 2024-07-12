# Load required libraries
library(shinythemes)
library(shiny)
library(httr)
library(jsonlite)
library(glue)
library(tidyverse)
library(ggplot2)
library(see)
library(easystats)

# Define a function to create bins for numeric data
binned <- function(x){
    x <- sort(as.vector(x))
    x <- base::seq(from= min(x),to = max(x),by= round(length(x)/10))
    return(x)
}

# Function to generate URL for frequency table for human data
human_contig_table_url <- function(start_date, end_date, patient_sex = NULL, age_min = NULL, age_max = NULL, cat_var) {
    base_url <- "https://api.fda.gov/drug/event.json?search="
    date_range <- paste0("receivedate:[", start_date, "+TO+", end_date, "]")
    query <- date_range
    
    # Handle optional parameters (patient sex, age range)
    if (!is.null(patient_sex)) {
        sex <- paste0("patient.patientsex:", patient_sex)
        query <- paste0(query, "+AND+", sex)
    }
    
    if (!is.null(age_min) && !is.null(age_max)) {
        age_range <- paste0("patient.patientonsetage:[", age_min, "+TO+", age_max, "]")
        query <- paste0(query, "+AND+", age_range)
    }
    
    full_url <- paste0(base_url, query, "&count=", cat_var)
    
    return(full_url)
}

# Function to generate URL for frequency table for animal data
animal_contig_table_url <- function(animal_species = NULL, cat_var) {
    base_url <- "https://api.fda.gov/animalandveterinary/event.json?search="
    
    if (!is.null(animal_species)) {
        animal_query <- paste0("animal.species:", animal_species)
        query <- paste0(animal_query)
    } else {
        query <- ""
    }
    
    full_url <- paste0(base_url, query, "&count=", cat_var)
    
    return(full_url)
}

# Function to generate URL for subset data for human data
human_subset_url <- function(start_date, end_date, patient_sex = NULL, age_min = NULL, age_max = NULL) {
    base_url <- "https://api.fda.gov/drug/event.json?search="
    date_range <- paste0("receivedate:[", start_date, "+TO+", end_date, "]")
    query <- date_range
    
    if (!is.null(patient_sex)) {
        sex <- paste0("patient.patientsex:", patient_sex)
        query <- paste0(query, "+AND+", sex)
    }
    
    if (!is.null(age_min) && !is.null(age_max)) {
        age_range <- paste0("patient.patientonsetage:[", age_min, "+TO+", age_max, "]")
        query <- paste0(query, "+AND+", age_range)
    }
    
    full_url <- paste0(base_url, query, "&limit=1000")
    
    return(full_url)
}

# Function to generate URL for subset data for animal data
animal_subset_url <- function(animal_species = NULL) {
    base_url <- "https://api.fda.gov/animalandveterinary/event.json?search="
    
    if (!is.null(animal_species)) {
        animal_query <- paste0("animal.species:", animal_species)
        query <- paste0(animal_query)
    } else {
        query <- ""
    }
    
    full_url <- paste0(base_url, query, "&limit=1000")
    
    return(full_url)
}

# Function to query API and return data as dataframe
api_query <- function(query_url) {
    response <- GET(query_url)
    data <- rawToChar(response$content)
    result <- fromJSON(data)
    return(result$results)
}

# Define the Shiny server function
shinyServer(function(input, output, session) {
    
    # Function to generate query URL based on user inputs
    generate_query_url <- function() {
        data_type <- input$data_type
        first_year <- switch(data_type,
                             "human" = input$first_year_human,
                             "animal" = input$first_year_animal)
        last_year <- switch(data_type,
                            "human" = input$last_year_human,
                            "animal" = input$last_year_animal)
        analysis_type <- input$analysis_type
        
        # Convert selected years to a specific date format
        first_year <- format(first_year, "%Y%m%d")
        last_year <- format(last_year, "%Y%m%d")
        
        # Determine which type of data (human or animal) and which analysis type (frequency table or subset)
        if (data_type == "human") {
            if (analysis_type == "Frequency Table") {
                cat_var <- "patient.reaction.reactionmeddrapt.exact"
                age_min <- ifelse(input$age_min_human == 0, NULL, input$age_min_human)
                age_max <- ifelse(input$age_max_human == 100, NULL, input$age_max_human)
                gender <- ifelse(input$gender_human == "Male", 1,
                                 ifelse(input$gender_human == "Female", 2, NULL))
                query_url <- human_contig_table_url(first_year, last_year, gender, age_min, age_max, cat_var)
            } else if (analysis_type == "Subset") {
                query_url <- human_subset_url(first_year, last_year, input$gender_human, input$age_min_human, input$age_max_human)
            }
        } else {
            if (analysis_type == "Frequency Table") {
                cat_var <- "reaction.veddra_term_name.exact"
                query_url <- animal_contig_table_url(input$species_animal, cat_var)
            } else if (analysis_type == "Subset") {
                query_url <- animal_subset_url(input$species_animal)
            }
        }
        
        return(query_url)
    }
    
    # Reactive expression to query API and get data based on user input
    data <- reactive({
        req(input$submit_btn)  # Ensure the submit button is pressed
        query_url <- generate_query_url()  # Generate the query URL
        api_query(query_url)  # Query the API and retrieve data
    })
    
    # Render the output as a table in the UI
    output$query_table <- renderTable({
        data()[1:10,]  # Display the first 10 rows of the queried data
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE)
    
    # Render the query URL output in the UI
    output$query_url_output <- renderText({
        generate_query_url()  # Display the current query URL being used
    })
    
    # Download handler for the download button in the UI
    output$download_btn <- downloadHandler(
        filename = function() {
            paste("data_download_", Sys.Date(), ".csv", sep = "")  # Create a CSV file name with today's date
        },
        content = function(file) {
            write.csv(data(), file, row.names = FALSE)  # Write the queried data to a CSV file
        }
    )
    
    # Reactive expression to generate query URL for animal subset data based on user input
    animal_subset_query_url <- reactive({
        animal_species <- input$species
        animal_subset_url(animal_species)
    })
    
    # Reactive expression to query API and get animal subset data based on user input
    animal_subset_data <- reactive({
        query_url_sum <- animal_subset_query_url()
        api_query(query_url_sum)
    })
    
    # Reactive expression to process animal subset data for plotting
    animal_processed_data <- reactive({
        subset_raw <- animal_subset_data()
        subset_raw <- as.data.frame(subset_raw)  # Convert queried data to a dataframe
        subset_raw$number_of_animals_treated <- as.numeric(subset_raw$number_of_animals_treated)
        subset_raw$number_of_animals_affected <- as.numeric(subset_raw$number_of_animals_affected)
        subset_raw$treatment_duration <- as.numeric(subset_raw$duration$value)
        subset_raw$animal_type <- subset_raw$animal$species
        subset_raw$event <- 1  # Add a new column 'event'
        return(subset_raw)
    })
    
    # Reactive expression to summarize quantitative variables for animal subset data
    summary_tabl <- reactive({
        pre_summary_tabl <- animal_processed_data()
        pre_summary_tabl <- pre_summary_tabl |>
            filter(animal_type == input$species)  # Filter data based on selected animal species
        
        # Calculate summary statistics for selected quantitative variable
        summary_stats <- pre_summary_tabl |>
            summarise(
                min = min(!!sym(input$quant_type), na.rm = TRUE),
                q1 = quantile(!!sym(input$quant_type), 0.25, na.rm = TRUE),
                median = median(!!sym(input$quant_type), na.rm = TRUE),
                q3 = quantile(!!sym(input$quant_type), 0.75, na.rm = TRUE),
                max = max(!!sym(input$quant_type), na.rm = TRUE),
                mean = mean(!!sym(input$quant_type), na.rm = TRUE),
                sd = sd(!!sym(input$quant_type), na.rm = TRUE)
            )
        
        return(summary_stats)
    })
    
    # Render the summary statistics as a table in the UI
    output$quant_summary <- renderTable({
        summary_tabl()
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Reactive expression to generate different types of plots based on user selection
    infographic <- reactive({
        plot_choices <- c("Top 10 Reported Animal Reactions (BarChart)", "Treatment Duration by Animal Type (ViolinDensity)","Adverse Reactions Over Time (Facetted Time Series)", "Adverse Reaction Frequency: Month and Year (HeatMap)")
        
        if (input$plot_type == plot_choices[1]){
            
            ##Query
            animal_reaction_url <- "https://api.fda.gov/animalandveterinary/event.json?search=&count=reaction.veddra_term_name.exact"
            bar_chart_data <- api_query(animal_reaction_url)
            
            # Create a bar chart of the top 10 reported animal reactions
            ggplot(bar_chart_data[1:10,], aes(x = term, y = count, fill = term)) +
                geom_bar( stat = "identity") +
                theme_modern() +
                xlab("Adverse Events") +
                ylab("Reactions") +
                labs(fill = "Reactions") +
                theme(
                    legend.position = "right",
                    legend.key.size = unit(.5, 'cm'), 
                    legend.key.height = unit(.5, 'cm'), 
                    legend.key.width = unit(.5, 'cm'), 
                    legend.title = element_text(size=7), 
                    legend.text = element_text(size=5),
                    axis.text.x = element_blank()) + 
                labs(title = input$plot_type)
            
        } else if (input$plot_type == plot_choices[2]) {
            
            animal_violin_data <- animal_processed_data()
            
            # Create a violin plot of treatment duration by animal type for selected animals
            ggplot(animal_violin_data[animal_violin_data$animal_type %in% c("Cat","Dog","Horse","Cattle"),], aes(x = animal_type, y = treatment_duration, fill = animal_type)) +
                geom_violindot(fill_dots = "black") +
                labs(
                    x = "Animal Type",
                    y = "Treatment Duration"
                ) +
                labs(title = input$plot_type)
            
            # Create a faceted time series of events over time by animal type 
        } else if (input$plot_type == plot_choices[3]) {
            
            ##Transform
            animal_timeseries_data <- animal_processed_data()
            animal_timeseries_data$year <- substr(animal_timeseries_data$original_receive_date,start = 1,stop = 4)
            
            animal_timeseries_data<-animal_timeseries_data |>
                select(animal_type,year,event) |>
                group_by(animal_type,year)|>
                summarise(Adverse_event_reports = n())
            
            ##plot
            ggplot(data = animal_timeseries_data[animal_timeseries_data$animal_type %in% c("Cat","Dog","Horse","Cattle"),], aes(x = year, y = Adverse_event_reports, group = 1)) +
                geom_line(color = "blue", size = 1.2) + 
                facet_wrap(~ animal_type) +
                scale_x_discrete(breaks= binned(animal_timeseries_data$year)) +
                theme_radar() +
                labs(title = plot_choices[3],
                     x = "Year",
                     y = "Number of Adverse Event Reports") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
            
            # Create a heatmap of adverse event frequency by Month and Year
            
        } else {
            
            
            ##Transform
            animal_hm_data <- animal_processed_data()
            
            animal_hm_data$month <- as.numeric(substr(animal_hm_data$original_receive_date,start = 5,stop = 6))
            
            animal_hm_data$month <- month.name[animal_hm_data$month]
            
            hm_data <- animal_hm_data |>
                select(month,year,event) |>
                group_by(year,month)|>
                summarise(Adverse_event_reports = n())
            
            
            ##Plot
            ggplot(data = animal_hm_data, aes(x = year, y = month, fill = Adverse_event_reports)) +
                geom_tile(interpolate = TRUE) +
                scale_fill_gradient(low = "white", high = "purple") +
                scale_x_discrete(breaks= binned(animal_hm_data$year)) +
                labs(title = plot_choices[4],
                     x = "Year",
                     y = "Month",
                     fill = "Adverse Event Reports") +
                theme_classic() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
        } 
    })
    
    # Return the generated plot based on user selection.
    output$display_plot <- renderPlot({
        infographic()
    })
})

