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
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

# This is to prevent the scientific notation in the plot's y-axis
options(scipen=10000)
Sys.setenv(TZ='America/New_York')

## Load in the data
fipsData = read.csv("fipsData.csv", stringsAsFactors = F,  colClasses = "character") %>% 
  mutate(POPESTIMATE2019 = as.integer(POPESTIMATE2019))

#Merge the state and county for search of county
fipsData$stateCounty = paste0(fipsData$State, ": ", fipsData$County)

#Link to the NYTimes data on GitHub
sourceDataNYT <- reactiveFileReader(
  intervalMillis = 3.6e+6, #Check every hour
  session = NULL,
  filePath = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  read.csv, stringsAsFactors=FALSE
)

#Set some parameters
popByMetro = fipsData %>% group_by(CSA.Title) %>% summarise(population = sum(POPESTIMATE2019))
popByCounty = fipsData %>% select(stateCounty, population = POPESTIMATE2019)


#Function used to generate the doubleing rate guide
doubleRate = function(x, startCases = 10, daysToDouble = 3, population = 10000, perHowMany = 10000) {
  (startCases*2^(x/daysToDouble))/(population / perHowMany)
}

#Calculate the x-value for double rate if y is given
getX = function(y, startCases = 10, daysToDouble = 3, population = 10000, perHowMany = 10000){
  log2((y*population) / (startCases * perHowMany)) * daysToDouble
}

#Calulcate the positions of the labels for the doubling rate lines
labelPos = function(maxX, maxY, startCases = 10, daysToDouble = 3, population = 10000, perHowMany = 10000){
  posX = maxX
  posY = doubleRate(posX, startCases = startCases, daysToDouble = daysToDouble, 
                    population = population, perHowMany = perHowMany)
  
  #If the Y for max X falls out of the plot range, recalculate based on the max Y in the data
  if(posY > maxY){
    posX = getX(maxY, startCases = startCases, daysToDouble = daysToDouble, 
                population = population, perHowMany = perHowMany)
    posY = doubleRate(posX, startCases = startCases, daysToDouble = daysToDouble, 
                      population = population, perHowMany = perHowMany)
  }
  
  return(list(posX = posX, posY= posY))
}


# Define UI for application
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 Tracker", id="nav",

        tabPanel("Plots",
               sidebarLayout(  
                sidebarPanel(
                  radioButtons("regionType", "Region detail", list("Metro areas" = "CSA.Title", "Counties" = "stateCounty"), inline = T),
                   selectInput(inputId = "region",
                              label = "Select one or more regions",
                              choices = "",
                              multiple = T, 
                              selectize = T,
                              width = "400px"
                  ),
                  helpText('Tip: type the city name for easy searching.'),hr(),
                  radioButtons("outcome", "Data", list("Confirmed cases" = 1, "Deaths" = 2), inline = T),
                  radioButtons("yScale", "Scale", list("Linear" = 1, "Logarithmic" = 2), inline = T),
                  conditionalPanel(
                    condition = "input.yScale == 1",  
                    radioButtons("relPop", "Adjust for population size", list("Yes" = 1, "No" = 2), inline = T, selected = 2)
                  ),
                  tags$br(),
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
                   tags$br(),tags$br(),
                   HTML("<i>Note on data</i><br>
                        Several areas are not grouped by county or metropolitan area,
                        although you can still search for them:<ul><li><b>New York City</b>: The five boroughs of New York City
                        (New York, Kings, Queens, Bronx and Richmond counties) are assigned to a single area called New York City
                        </li><li><b>Kansas City</b>: Four counties (Cass, Clay, Jackson and Platte) overlap the municipality
                        of Kansas City, Mo. The cases and deaths that we show for these four counties are only for the portions
                        exclusive of Kansas City. Cases and deaths for Kansas City are reported as their own line</li>
                        <li><b>Chicago</b>: All cases and deaths for Chicago are reported as part of Cook County</li></ul>
                        For details, visit the <a href='https://github.com/nytimes/covid-19-data'>New York Times GitHub</a>"
                   ),
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
  
  # showModal(modalDialog(title = "IMPORTANT NOTE", HTML(<h4>NOTE on data</h4>
  #   "Several areas are not grouped by county or metropolitan area,
  #   although you can still search for them:<ul><li><b>New York City</b>: The five boroughs of New York City
  #   (New York, Kings, Queens, Bronx and Richmond counties) are assigned to a single area called New York City
  #   </li><li><b>Kansas City</b>: Four counties (Cass, Clay, Jackson and Platte) overlap the municipality
  #   of Kansas City, Mo. The cases and deaths that we show for these four counties are only for the portions
  #   exclusive of Kansas City. Cases and deaths for Kansas City are reported as their own line</li>
  #   <li><b>Chicago</b>: All cases and deaths for Chicago are reported as part of Cook County</li></ul>
  #   For details, visit the <a href='https://github.com/nytimes/covid-19-data'>New York Times GitHub</a>"
  # )))
  
  #USE THIS DURING TESTING
  covidData = reactive({
    data = read.csv("us-counties.csv", stringsAsFactors = F)
    #Add the special cases
    data[data$county == "New York City" & data$state == "New York","fips"] = "00000" #NYC
    data[data$county == "Kansas City" & data$state == "Missouri","fips"] = "00001" #Kansas City

    data = data %>%
      mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
             date = as.Date(date)) %>% select(-county, - state)
    data[data$county == "New York City", "fips"] = "00000" #They don't provide fips!
    data[data$county == "Kansas City", "fips"] = "00001"
    updateTime(as.character(max(data$date, na.rm = T)))

    data
  })
  
  # # USE THIS ONLINE
  # covidData = reactive({
  #   data = sourceDataNYT()
  #   data[data$county == "New York City" & data$state == "New York","fips"] = "00000" #NYC
  #   data[data$county == "Kansas City" & data$state == "Missouri","fips"] = "00001" #Kansas City
  #   
  #   data = data %>%
  #     mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
  #            date = as.Date(date)) %>% select(-county, - state)
  #   data[data$county == "New York City", "fips"] = "00000" #They don't provide fips!
  #   data[data$county == "Kansas City", "fips"] = "00001"
  #   updateTime(as.character(max(data$date, na.rm = T)))
  #   
  #   data
  # })
  
  updateTime = reactiveVal('April 3, 09:30 AM EST.')
  filterWarning = reactiveVal("")
  regionTest = reactiveVal("")
  
  #Update the time on the page
  output$updateTime = renderText(updateTime())
  
  observeEvent(input$regionType, {
    if(input$regionType == "CSA.Title"){
      updateSelectInput(session, "region", "Select one or more metro areas", choices = sort(unique(fipsData$CSA.Title)),
                        selected = c("Seattle-Tacoma, WA"))
    } else {
      updateSelectInput(session, "region", "Select one or more counties", choices = sort(unique(fipsData$stateCounty)),
                        selected = "CA: Orange County")
    }
  })
  

  #Calculate the data to be displayed
   plot.data = reactive({

     #Make sure the plot only gets generated if there is at least one region selected
     # Also don't update during change of regionType
     req(!is.null(input$region))
     
     startCases = ifelse(input$outcome == 1, 10, 1)

     #This will be used later when we can select different region levels (e.g. county, state, ...)
     groupByRegion = sym(isolate(input$regionType))
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
       summarise(cases = sum(cases), deaths = sum(deaths)) 
     
     if(isolate(input$regionType) == "CSA.Title"){
       plotData = plotData %>% 
         left_join(popByMetro, by = c("region" = "CSA.Title")) 
     } else {
       plotData = plotData %>% 
         left_join(popByCounty, by = c("region" = "stateCounty"))
     }
     
     plotData = plotData %>% ungroup() %>% 
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
     }
     
     omitted = setdiff(input$region, plotData$region %>% unique()) #Regions that have < 10 cases in total
     
     #Update warning if needed
     if(length(omitted) > 0){
       filterWarning(paste("The following have a less than", ifelse(input$yScale == 1, 1, startCases), 
                           ifelse(input$outcome == 1, "cases", "death"), "and were omitted:", 
                           paste(omitted, collapse = "; "))) #displayed as warning
     } else {
       filterWarning("")
     }
     
     #When cases per 10,000
     if(input$relPop == 1 && input$yScale == 1){
       plotData = plotData %>% 
         mutate(y = y / (population / 10000))
     }
     
     #Edit the order of the labels by descending y-value
     myOrder = plotData %>% group_by(region) %>% summarise(y = max(y)) %>% arrange(desc(y))
     plotData$region = factor(plotData$region, levels = c(myOrder$region))
     
     plotData %>% ungroup()
     
  })
   
   
  #THis is the plot itself
  output$plot1 <- renderPlot({
    
    startCases = ifelse(input$outcome == 1, 10, 1)

    plot = ggplot(plot.data(), aes(x=x, y=y, color = region)) +
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
        
        twoDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 2, startCases = startCases)
        threeDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 3, startCases = startCases)
      } else {
        plot = plot + 
          stat_function(fun = ~doubleRate(.x, startCases, 2, pop),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~doubleRate(.x, startCases, 3, pop),
                      linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3)
        
        twoDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 2, startCases = startCases)
        threeDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 3, startCases = startCases)
      }
      
      plot = plot +
        annotate("text", x = twoDayLabel$posX, y = twoDayLabel$posY, label = "Double every\n2 days", color = "#8D8B8B") +
        annotate("text", x = threeDayLabel$posX, y = threeDayLabel$posY, label = "Double every\n3 days", color = "#8D8B8B") + 
        scale_y_log10(labels = comma, limits = c(NA, max(plot$data$y)))
        
    }

    #Finalize the plot
    #Labels depend on selections
    xLabel = ifelse(input$yScale == 1, "Date", 
                    paste("Number of Days Since",
                          ifelse(input$outcome == 1, "10th Case", "1st Death")))

    yLabel = ifelse(input$relPop == 1 && input$yScale == 1,ifelse(input$outcome == 1, "Cases per 10,000 Residents", 
                                                                  "Deaths per 10,000 Residents"), 
                    ifelse(input$outcome == 1, "Confirmed Cases", "Deaths"))

    plot + theme_bw() +
      geom_text(data = plot.data() %>% filter(date == last(date)), 
                aes(label = if(input$relPop == 2){ y}else{ sprintf("%.2f", y)}, 
                    x = x, y = y), size = 6, check_overlap = T,hjust=0, vjust=0.5) +
      labs(title = sprintf('COVID-19 %s in U.S. Metropolitan Areas', 
                           ifelse(input$outcome == 1, "Cases", "Deaths")),
           subtitle = ifelse(input$yScale == 1, "Data shown for last 3 weeks", ""),
           caption =  paste("Authors: Benjamin Wissel, PJ Van Camp\nData from The New York Times, ",
                            "based on reports from state and local health agencies.\n",
                            "http://bit.ly/covid-cities", sep = ""))  +
      xlab(xLabel) + ylab(yLabel) +
      coord_cartesian(clip = 'off') +
      scale_color_discrete(labels = str_trunc(levels(plot.data()$region), 20)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
            legend.position = 'right', legend.direction = "vertical", 
            legend.title = element_blank(), 
            panel.border = element_blank(),
            plot.caption = element_text(hjust = 0.0),
            axis.line = element_line(colour = "black"),
            text = element_text(size=20))
    
  })
  
  #Warning when display after first 10 cases and regions don't have 10+
  output$filterWarnings = renderText({
    filterWarning()
  })
}

shinyApp(ui = ui, server = server)