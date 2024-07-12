fluidPage(
    
    titlePanel("FDAex"),  # App title panel
    
    tabsetPanel(
        id = "tabs",  # Tabset panel for different views
        
        # Tab: About FDAex
        tabPanel("About FDAex", 
                 
                 #Header
                 h4("About the App"),
                 
                 ## Description of the app
                 p("This app provides infographics and subsets of raw datasets of adverse events reported in humans and animals during drug test clinical trials. The data is reported by the Food and Drug Administration (FDA)."),  
                 
                 # Link to openFDA API
                 p("Data Source: ", a("openFDA API", href = "https://open.fda.gov/apis/")),  
                 # Description of tabs
                 p("Tabs Description:"),  
                 tags$ul(  
                     tags$li("About Tab: This page gives insight into the purpose and source of FDAex."),
                     tags$li("Data Query & Download: This page allows users to query the API and download the subsets directly from openFDA."),
                     tags$li("Data Exploration: This page allows the user to pull up numerical summaries and infographics created using the openFDA API.")
                 ),
                 
                 # FDA openFDA logo image
                 img(src = "https://open.fda.gov/img/l_openFDA.png", height = "100px",)  
        ),
        
        # Tab: Data Query & Download
        tabPanel("Data Query & Download",
                 h4("Select Variable for Querying and Download"), 
                 sidebarLayout(
                     sidebarPanel(
                         # Radio buttons to select data type (human or animal)
                         radioButtons("data_type", "Select Data Type:",  #
                                      choices = c("human", "animal"),
                                      selected = "human"),
                         
                         # Conditional panel for human data inputs
                         conditionalPanel(
                             
                             ## Show if human data type is selected
                             condition = "input.data_type == 'human'",  
                             
                             # Date input for first year
                             dateInput("first_year_human", "First Year:", value = as.Date("2004-01-01")),  
                             # Date input for last year
                             dateInput("last_year_human", "Last Year:", value = Sys.Date()),  
                             # Slider input for minimum and maximum age
                             sliderInput("age_min_human", "Minimum Age:", min = 0, max = 100, value = 1),  
                             sliderInput("age_max_human", "Maximum Age:", min = 0, max = 100, value = 99),  
                             #Select input for gender
                             selectInput("gender_human", "Select Gender:",  #
                                         choices = c("Male", "Female"),
                                         selected = "Male")
                         ),
                         
                         # Conditional panel for animal data inputs
                         conditionalPanel(
                             condition = "input.data_type == 'animal'",  
                             dateInput("first_year_animal", "First Year:", value = as.Date("2004-01-01")),
                             dateInput("last_year_animal", "Last Year:", value = Sys.Date()),
                             #Input for Species
                             selectInput("species_animal", "Select Species:",  # 
                                         choices = c("Cat", "Dog", "Horse", "Cattle"),
                                         selected = "Cat")
                         ),
                         
                         # Select input for Data type (frequency table or subset) 
                         selectInput("analysis_type", "Select Analysis Type:",  
                                     choices = c("Frequency Table", "Subset"),
                                     selected = "Frequency Table"),
                         
                         # Submit button to trigger data query
                         actionButton("submit_btn", "Submit"),  
                         
                         # Download button to download queried data
                         downloadButton("download_btn", "Download")  
                     ),
                     
                     mainPanel(
                         # Output for displaying queried data table
                         tableOutput("query_table"),  
                         
                         # Output for displaying query URL
                         verbatimTextOutput("query_url_output")  
                     )
                 )),
        
        # Tab: Data Exploration
        tabPanel("Data Exploration",
                 
                 # Nested tabset panel for summary and plots
                 tabsetPanel(id = "tabs",  
                             tabPanel("Summary",  #Subtab for Numerical Summaries
                                      sidebarLayout(
                                          sidebarPanel(
                                              
                                              # Radio buttons to select quantitative variable
                                              radioButtons("quant_type", "Select Variable:",  
                                                           choices = c("number_of_animals_affected", "number_of_animals_treated","treatment_duration"),
                                                           selected = "treatment_duration"),
                                              # Radio buttons to select animal species
                                              radioButtons("species", "Select Species:",  
                                                           choices = c("Cat", "Dog", "Horse", "Cattle"),
                                                           selected = character(0))
                                          ),
                                          mainPanel(
                                              # Output for displaying quantitative variable summary
                                              tableOutput("quant_summary")  
                                          )
                                      )
                             ),
                             
                             tabPanel("Plots", h4("Corresponding Plots"),  # Subtab: Plots
                                      sidebarLayout(
                                          sidebarPanel(
                                              
                                              # Radio buttons to select plot type
                                              radioButtons("plot_type", "Select Plot Type:",  
                                                           choices = c("Top 10 Reported Animal Reactions (BarChart)", "Treatment Duration by Animal Type (ViolinDensity)","Adverse Reactions Over Time (Facetted Time Series)", "Adverse Reaction Frequency: Month and Year (HeatMap)"),
                                                           selected = character(0))
                                          ),
                                          mainPanel(
                                              
                                              # Output for displaying selected plot 
                                              plotOutput("display_plot")  
                                          )
                                      )
                             )
                 )
        )
    )
)
