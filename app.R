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

options(scipen=10000)
Sys.setenv(TZ='America/New_York')

## Load in the data
fipsData = read.csv("fipsData.csv", stringsAsFactors = F,  colClasses = "character") %>% 
  mutate(POPESTIMATE2019 = as.integer(POPESTIMATE2019))

#Link to the NYTimes data on GitHub
sourceDataNYT <- reactiveFileReader(
  intervalMillis = 3.6e+6, #Check every hour
  session = NULL,
  filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  read.csv, stringsAsFactors=FALSE
)

#Set some parameters
popByMetro = fipsData %>% group_by(CSA.Title) %>% summarise(population = sum(POPESTIMATE2019))


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
                              label = "Select one or more metro areas",
                              choices = sort(unique(fipsData$CSA.Title)),
                              selected = c("Seattle-Tacoma, WA","Boston-Worcester-Providence, MA-RI-NH-CT","San Jose-San Francisco-Oakland, CA"),
                              multiple = T, 
                              selectize = T,
                              width = "400px"
                  ),
                  helpText('Tip: type the city name for easy searching.'),hr(),
                  radioButtons("outcome", "Data", list("Confirmed cases" = 1, "Deaths" = 2), inline = T),
                  # radioButtons("yScale", "Time comparison", list("By date" = 1, "By numbers" = 2), inline = T),
                  # conditionalPanel(
                  #   condition = "input.yScale == 1 && input.advanced",
                  #   dateRangeInput("dateSlider", "Date range", start = Sys.Date() - 21, end = Sys.Date())
                  # ),
                  # conditionalPanel(
                  #   condition = "input.yScale == 2 && input.advanced",
                  #   numericInput("startCases", "Start number", min = 1, max = 10000, value = 10, step = 1)
                  #   numericInput("doublingRate", "Reference line days to double", min = 1, max = 21, value = 3, step = 1)
                  # ),
                  radioButtons("yScale", "Scale", list("Linear" = 1, "Logarithmic" = 2), inline = T),
                  conditionalPanel(
                    condition = "input.yScale == 1",  
                    radioButtons("relPop", "Adjust for population size", list("Yes" = 1, "No" = 2), inline = T, selected = 2)
                  ),
                  tags$br(),
                  # tags$div(checkboxInput("advanced", "Show advanced options"), align = 'right', style = "color: gray;"),
                  tags$div(textOutput("filterWarnings"), style = "color: red;")
                ),
                mainPanel(
                  plotOutput(outputId = 'plot1', width = "1000px", height = "600px")
                )
              )
        ),
        tabPanel("About this site",
                 tags$div(
                   tags$h4("Last update"), 
                   textOutput("updateTime"), 'Data updated daily.',
                   tags$br(),tags$br(),tags$h4("Summary"),
                   'This tool allows users to view COVID-19 cases and deaths across U.S. cities.',
                   tags$br(),tags$br(),tags$h4("Code"),
                   "Will release on GitHub soon.",
                   tags$br(),tags$br(),tags$h4("Sources"),
                   tags$b("COVID-19 cases: "), tags$a(href='https://www.nytimes.com/article/coronavirus-county-data-us.html','The New York Times'), ", based on reports from state and local health agencies.", 
                   tags$br(),tags$b("U.S. metropolitan area definitions: "), tags$a(href='https://www.census.gov/programs-surveys/metro-micro.html','The United States Office of Management and Budget'), ".", 
                   tags$br(),tags$b("Population estimates: "), tags$a(href='https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902','The United States Census Bureau'), ".", 
                   tags$br(),tags$br(),'Inspiration for the design of these charts and this dashboard was derived from', tags$a(href='https://twitter.com/jburnmurdoch','John Burn-Murdoch'),' and', tags$a(href='https://github.com/eparker12/nCoV_tracker','Dr. Edward Parker'),', respectively.',
                   tags$br(),tags$br(),tags$h4("Authors"),
                   "Benjamin Wissel, Department of Biomedical Informatics, Cincinnati Children's Hospital Medical Center", tags$br(), "Dr. PJ Van Camp, Department of Biomedical Informatics, Cincinnati Children's Hospital Medical Center",
                   tags$br(),tags$br(),tags$h4("Contact"),
                   "benjamin.wissel@cchmc.org",tags$br(),
                   tags$a(href="https://twitter.com/BDWissel", "@bdwissel"), tags$br(), tags$br(),
                   "vancampn@mail.uc.edu",tags$br(),
                   tags$a(href="https://www.linkedin.com/in/pjvancamp/", "LinkedIn"),
                   tags$br(),tags$br(),tags$h4("Acknowledgements"),
                   'Thank you to', tags$a(href='https://www.cincinnatichildrens.org/bio/w/danny-wu','Dr. Danny Wu'), 'and ', tags$a(href='https://scholar.google.com/citations?user=NmQIjpAAAAAJ&hl=en','Sander Su'), 'for hosting this website on their server.',tags$br()
                 )
        )
)
# Define server logic
server <- function(input, output, session) {
  
  #USE THIS DURING TESTING
  covidData = reactive({
    data = read.csv("us-counties.csv", stringsAsFactors = F) %>% 
      mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
             date = as.Date(date)) %>% select(-county, - state)
    updateTime(as.character(max(data$date, na.rm = T)))
    data
  })
  
  # #USE THIS ONLINE
  # covidData = reactive({
  #   data = sourceDataNYT() %>% 
  #     mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
  #            date = as.Date(date)) %>% select(-county, - state)
  #   updateTime(as.character(max(data$date, na.rm = T)))
  #   data
  # })
  
  updateTime = reactiveVal('April 2, 12:30 PM EST.')
  filterWarning = reactiveVal("")
  
  #Update the time on the page
  output$updateTime = renderText(updateTime())
  
  
  #Calculate the data to be displayed
   plot.data = reactive({
     
     startCases = ifelse(input$outcome == 1, 10, 1)
     # #If not advanced, set the startCases for cases to 10, deaths to 1
     # if(!input$advanced){
     #   startCases = ifelse(input$outcome == 1, 10, 1)
     # } else {
     #   startCases = input$startCases
     # }
     
     #Make sure the plot only gets generated if there is at least one region selected
     req(!is.null(input$region))

     #This will be used later when we can select different region levels (e.g. county, state, ...)
     groupByRegion = sym("CSA.Title")
     outcome = sym(ifelse(input$outcome == 1, "cases", "deaths"))
     
     #If working by date, crop the data
     covidNumbers = covidData() 
     if(input$yScale == 1){
       covidNumbers = covidNumbers%>% filter(between(date, Sys.Date()-21, Sys.Date()))
     }
     
     #Build the data for the plot
     plotData = fipsData %>% 
       filter(!!groupByRegion %in% input$region) %>% #Only use data needed (user selected regions)
       select(region = !!groupByRegion, FIPS) %>% group_by(region, FIPS) %>% 
       left_join(covidNumbers, by = c("FIPS" = "fips")) %>% #Join the cases per fips
       filter(!is.na(date)) %>% group_by(region, date) %>% #Now group per region
       summarise(cases = sum(cases), deaths = sum(deaths)) %>% 
       left_join(popByMetro, by = c("region" = "CSA.Title")) %>%
       mutate(x = date, y = !!outcome) %>% #add the population per region
       filter(y > 0) #Only show cases / deaths more than 0
     
     if(max(plotData$y) < startCases){
       filterWarning("No data met the criteria")
       req(F)
     }
     
     #In case the plot needs to show starting from 10 cases
     omitted = NULL
     if(input$yScale == 2){
       plotData = plotData %>% 
         filter(y >= startCases) %>% group_by(region) %>% #Filter 10+
         mutate(x = 1:n() - 1) #Assign a number from 0 - n (days after first 10)
       omitted = setdiff(input$region, plotData$region %>% unique()) #Regions that have < 10 cases in total
     }
     
     #Update warning if needed
     if(length(omitted) > 0){
       filterWarning(paste("The following have a total less than", startCases, "cases and were omitted:", 
                           paste(omitted, collapse = "; "))) #displayed as warning
     } else {
       filterWarning("")
     }
     
     #When cases per 10,000
     if(input$relPop == 1 && input$yScale == 1){
       plotData = plotData %>% 
         mutate(y = y / (population / 10000))
     }
     
     plotData
  })
   
   
  #THis is the plot itself
  output$plot1 <- renderPlot({
    
    startCases = ifelse(input$outcome == 1, 10, 1)
    # #If not advanced, set the startCases for cases to 10, deaths to 1
    # if(!input$advanced){
    #   startCases = ifelse(input$outcome == 1, 10, 1)
    # } else {
    #   startCases = input$startCases
    # }
    
    plot = ggplot(plot.data(), aes(x=x, y=y, color = region)) +
      # geom_point() +
      geom_line(size = 1.2)
    
    #Guide for doubling time
    if(input$yScale == 2){ #Only relevant when aligned by number of starting cases
      
      #If the population is relative, make sure the guide is too (is average population of ones shown)
      pop = ifelse(F, 
                   mean(plot.data() %>% group_by(region) %>% summarise(p = max(population)) %>% pull(p)),
                   10000)
      
      #Generate the doubline time using the doubleRate function (see at top)
      if(input$yScale == 2){
        plot = plot + 
          stat_function(fun = ~log10(doubleRate(.x, startCases, 2, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~log10(doubleRate(.x, startCases, 3, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3)
      } else {
        plot = plot + 
          stat_function(fun = ~doubleRate(.x, startCases, 2, pop),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~doubleRate(.x, startCases, 3, pop),
                      linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3)
      }
      
      plot = plot + ylim(c(NA, max(plot.data()$y))) 
        # scale_colour_manual(name = "Doubling rates", values = c("gray"), labels = c("Doubling rate per 2 or three days"))
        # annotate("text",x=max(plot.data()$x)/2, #Add a label to the line
        #          y=doubleRate(max(plot.data()$x/2), startCases, input$doublingRate, pop),
        #          color = "#696969", size = 6,
        #          label="Dotted lines are the doubling rate per 2 or 3 days")
        
    }
    
    #In case log-scale, add this to y-axis
    if(input$yScale == 2){
      plot = plot + scale_y_log10(labels = comma)
    } 
    
    #Finalize the plot
    #Labels depend on selections
    xLabel = ifelse(input$yScale == 1, "Date", 
                    paste("Number of Days Since",
                          ifelse(input$outcome == 1, "10th Case", "1st Death")))
    yLabel = ifelse(input$relPop == 1 && input$yScale == 1,ifelse(input$outcome == 1, "Cases per 10,000 Residents", "Deaths per 10,000 Residents"), 
                    ifelse(input$outcome == 1, "Confirmed Cases", "Deaths"))

    plot + theme_bw() +
      labs(title = sprintf('COVID-19 %s in U.S. Metropolitan Areas', 
                           ifelse(input$outcome == 1, "Cases", "Deaths")),
           caption =  paste("Authors: Benjamin Wissel, PJ Van Camp\nData from The New York Times, ",
                            "based on reports from state and local health agencies.\n",
                            "http://bit.ly/covid-cities", sep = ""))  +
      xlab(xLabel) + ylab(yLabel) +
      theme(legend.position = 'right', legend.direction = "vertical", 
            legend.title = element_blank(), 
            plot.caption = element_text(hjust = 0.0), 
            text = element_text(size=20))
    
  })
  
  #Warning when display after first 10 cases and regions don't have 10+
  output$filterWarnings = renderText({
    filterWarning()
  })

}

shinyApp(ui = ui, server = server)

#rsconnect::deployApp('/Users/wis2lp/Documents/COVID-NYT/R_Scripts/Publish/', appName = 'COVID')
