# load required packages
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")


## Load in the data
fipsData = read.csv("fipsData.csv", stringsAsFactors = F,  colClasses = "character") %>% 
  mutate(POPESTIMATE2019 = as.integer(POPESTIMATE2019))

covidData = read.csv('us-counties3.31.csv', stringsAsFactors = F) %>% 
  mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
         date = as.Date(date)) %>% select(-county, - state)

#Set some paratmeters
popByMetro = fipsData %>% group_by(CSA.Title) %>% summarise(population = sum(POPESTIMATE2019))
update.time <- max(covidData$date, na.rm = T)

#Function used to generate the doubleing rate guide
doubleRate = function(x, startCases = 10, daysToDouble = 3, population = 10000, perHowMany = 10000) {
  (startCases*2^(x/daysToDouble))/(population / perHowMany)
}


# Define UI for application
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 Tracker", id="nav",

        tabPanel("Plots",
               sidebarLayout(  
                sidebarPanel(
                  selectInput(inputId = "region",
                              label = "Select one or more metro areas:",
                              choices = sort(unique(fipsData$CSA.Title)),
                              selected = c('Cincinnati-Wilmington-Maysville, OH-KY-IN',"Cleveland-Akron-Canton, OH","Columbus-Marion-Zanesville, OH"),
                              multiple = T, 
                              selectize = T,
                              width = "400px"
                  ),
                  helpText('Tip: type the city name for easy searching.'),hr(),
                  radioButtons("outcome", "Outcome", list("Confirmed cases" = 1, "Deaths" = 2), inline = T),
                  radioButtons("timeComp", "Time comparison", list("By date" = 1, "Moment of first x" = 2), inline = T),
                  conditionalPanel(
                    condition = "input.timeComp == 1",
                    dateRangeInput("dateSlider", "Date range", start = Sys.Date() - 21, end = Sys.Date())
                  ),
                  conditionalPanel(
                    condition = "input.timeComp == 2",
                    numericInput("startCases", "Start cases/deaths", min = 1, max = 10000, value = 10, step = 1),
                    numericInput("doublingRate", "Reference line day to double", min = 1, max = 21, value = 3, step = 1)
                  ),
                  radioButtons("yScale", "Scale", list("Linear" = 1, "Logarithmic" = 2), inline = T),
                  checkboxInput("relPop", "Adjust for population size (per 10,000 people)"),
                  tags$div(textOutput("filterWarnings"), style = "color: red;")
                ),
                mainPanel(
                  plotOutput(outputId = 'plot1', width = "800px", height = "700px")
                )
              )
        ),
        tabPanel("About this site",
                 tags$div(
                   tags$h4("Last data update"), 
                   h6(paste0(update.time)),
                   "Updated once daily.",
                   tags$br(),tags$br(),tags$h4("Summary"),
                   'This tool allows users to compare COVID-19 cases and growth rate across U.S. cities.',
                   tags$br(),tags$br(),tags$h4("Code"),
                   "Will release on Github soon.",
                   tags$br(),tags$br(),tags$h4("Sources"),
                   tags$b("COVID-19 cases: "), tags$a(href='https://www.nytimes.com/article/coronavirus-county-data-us.html','The New York Times'), ", based on reports from state and local health agencies.", 
                   tags$br(),tags$b("U.S. metropolitan area definitions: "), tags$a(href='https://www.census.gov/programs-surveys/metro-micro.html','The United States Office of Management and Budget'), ".", 
                   tags$br(),tags$b("Population estimates: "), tags$a(href='https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902','The United States Census Bureau'), ".", 
                   tags$br(),tags$br(),'Inspiration for the design of these charts and this dashboard was derived from', tags$a(href='https://twitter.com/jburnmurdoch','John Burn-Murdoch'),' and', tags$a(href='https://github.com/eparker12/nCoV_tracker','Dr. Edward Parker'),', respectively.',
                   tags$br(),tags$br(),tags$h4("Authors"),
                   'Benjamin Wissel, PJ Van Camp', tags$br(), 'Department of Biomedical Informatics',tags$br(),"Cincinnati Children's Hospital Medical Center",tags$br(),'University of Cincinnati College of Medicine',
                   tags$br(),tags$br(),tags$h4("Contact"),
                   "benjamin.wissel@cchmc.org",tags$br(),
                   tags$a(href="https://twitter.com/BDWissel", "@bdwissel"),
                   tags$br(),tags$br(),tags$h4("Acknowledgements"),
                   'Thank you to', tags$a(href='https://www.cincinnatichildrens.org/bio/w/danny-wu','Dr. Danny Wu'), 'and ', tags$a(href='https://scholar.google.com/citations?user=NmQIjpAAAAAJ&hl=en','Sander Su'), 'for hosting this website on their server.',tags$br()
                 )
        )
)
# Define server logic
server <- function(input, output, session) {
  
  
  filterWarning = reactiveVal("")
  
  #Calculate the data to be displayed
   plot.data = reactive({
     
     #Make sure the plot only gets generated if there is at least one region selected
     req(!is.null(input$region))

     #This will be used later when we can select different region levels (e.g. county, state, ...)
     groupByRegion = sym("CSA.Title")
     outcome = sym(ifelse(input$outcome == 1, "cases", "deaths"))
     
     #Build the data for the plot
     plotData = fipsData %>% 
       filter(!!groupByRegion %in% input$region) %>% #Only use data needed (user selected regions)
       select(region = !!groupByRegion, FIPS) %>% group_by(region, FIPS) %>% 
       left_join(covidData %>% filter(between(date, input$dateSlider[1], input$dateSlider[2])), by = c("FIPS" = "fips")) %>% #Join the cases per fips
       filter(!is.na(date)) %>% group_by(region, date) %>% #Now group per region
       summarise(cases = sum(cases), deaths = sum(deaths)) %>% 
       left_join(popByMetro, by = c("region" = "CSA.Title")) %>%
       mutate(x = date, y = !!outcome) #add the population per region
     
     if(max(plotData$y) < input$startCases){
       filterWarning("No data met the criteria")
       req(F)
     }
     
     #In case the plot needs to show starting from 10 cases
     if(input$timeComp == 2){
       plotData = plotData %>% 
         filter(cases >= input$startCases) %>% group_by(region) %>% #Filter 10+
         mutate(x = 1:n() - 1) #Assign a number from 0 - n (days after first 10)
       omitted = setdiff(input$region, plotData$region %>% unique()) #Regions that have < 10 cases in total
       if(length(omitted) > 0)
         filterWarning(paste("The following have a total less than", input$startCases, "cases and were omitted:", 
                             paste(omitted, collapse = "; "))) #displayed as warning
     }
     
     #When cases per 10,000
     if(input$relPop){
       plotData = plotData %>% 
         mutate(y = y / (population / 10000))
     }
     
     plotData
  })
   
   
  #THis is the plot itself
  output$plot1 <- renderPlot({
    
    #Labels depend on selections
    xLabel = ifelse(input$timeComp == 1, "Date", 
                    sprintf("Days from first %.0f %s", 
                            input$startCases, 
                            ifelse(input$outcome == 1, "cases", "deaths")))
    yLabel = ifelse(input$relPop,ifelse(input$outcome == 1, "Cases per 10,000", "Deaths per 10,000"), 
                    ifelse(input$outcome == 1, "Cases", "Deaths"))

    plot = ggplot(plot.data(), aes(x=x, y=y, color = region)) +
      geom_point() +
      geom_line() +
      theme_bw() +
      labs(title = sprintf('COVID-19 %s in U.S. Metropolitan Areas', 
                           ifelse(input$outcome == 1, "Cases", "Deaths"))) +
      xlab(xLabel) + ylab(yLabel) +
      theme(legend.position = 'bottom', legend.direction = "vertical", 
            legend.title = element_blank(), 
            plot.caption = element_text(hjust = 0), 
            text = element_text(size=20))
    
    #Guide for doubling time
    if(input$timeComp == 2){ #Only relevant when aligned by number of starting cases
      
      #If the population is relative, make sure the guide is too (is average population of ones shown)
      pop = ifelse(input$relPop, 
                   mean(plot.data() %>% group_by(region) %>% summarise(p = max(population)) %>% pull(p)),
                   10000)
      #Generate the doubline time using the doubleRate function (see at top)
      plot = plot + 
        stat_function(fun = ~doubleRate(.x, input$startCases, input$doublingRate, pop),
                      linetype="dashed", colour = "gray", size = 2) +
        ylim(NA, max(plot.data()$y)) + #Cut the y-limit to that of the data
        annotate("text",x=max(plot.data()$x)/2, #Add a label to the line
                 y=doubleRate(max(plot.data()$x/2), input$startCases, input$doublingRate, pop),
                 color = "#474a4f", size = 6,
                 label=sprintf("Double every %.0f days", input$doublingRate))
    }
    
    #In case log-scale, add this to y-axis
    if(input$yScale == 2){
      plot = plot + scale_y_log10()
    } 


    plot
    
  })
  
  #Warning when display after first 10 cases and regions don't have 10+
  output$filterWarnings = renderText({
    filterWarning()
  })

}

shinyApp(ui = ui, server = server)

#rsconnect::deployApp('/Users/wis2lp/Documents/COVID-NYT/R_Scripts/Publish/', appName = 'COVID')
