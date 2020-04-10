#********************************************************
# ---- COVID 19 WATCHER (Ben Wissel and PJ Van Camp) ----
#********************************************************

# ---- Packages and general settings ----
#****************************************
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
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# This is to prevent the scientific notation in the plot's y-axis
options(scipen=10000)
Sys.setenv(TZ='America/New_York')

# ---- TESTING THE SCRIPT WITH LOCAL DATA ONLY ?? ----
#**************************************************
local_data_only = F


# ---- Loading initial data----
#******************************

#FIPS data
fipsData = read.csv("data/fipsData.csv", stringsAsFactors = F,  colClasses = "character") %>% 
  mutate(POPESTIMATE2019 = as.integer(POPESTIMATE2019)) 
unknownCounties = read.csv("data/unknownCounties.csv", stringsAsFactors = F, colClasses = "character")

#Merge the state and county for search of county
fipsData$stateCounty = paste0(fipsData$State, ": ", fipsData$County)

#Get total population by region
popByCountry = fipsData %>% group_by(Country) %>% summarise(Population = sum(POPESTIMATE2019, na.rm = T))
popByState = fipsData %>% group_by(State_name, State) %>% summarise(Population = sum(POPESTIMATE2019, na.rm = T))
popByMetro = fipsData %>% group_by(CSA.Title) %>% summarise(Population = sum(POPESTIMATE2019, na.rm = T))
popByCounty = fipsData %>% select(stateCounty, FIPS, Population = POPESTIMATE2019)

#Refresh the data every hour or used stored one if not possible 
NYTdata = reactivePoll(intervalMillis = 3.6E+6, session = NULL, checkFunc = function() {Sys.time()}, 
                       valueFunc = function() {
                         
                         if(local_data_only){
                           test = 0
                         } else {
                           link = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
                           test = GET(link)
                         }
                         
                         #If local_data_only = T or data not accessible online, use local files
                         if(status_code(test) == 200){
                           data = read.csv(link, stringsAsFactors = F)
                           write.csv(data, "data/us-counties.csv", row.names = F)
                           print("NYT data succesfully refreshed")
                           data
                         } else {
                           print("NYT not accessible online, local data used")
                           read.csv("data/us-counties.csv", stringsAsFactors = F)
                         }
                       })

covidProjectData = reactivePoll(intervalMillis = 3.6E+6, session = NULL, checkFunc = function() {Sys.time()}, 
                       valueFunc = function() {
                         
                         if(local_data_only){
                           data = 0
                         } else {
                           data = GET("https://covidtracking.com/api/v1/states/daily.csv")
                         }
                         
                         #If local_data_only = T or data not accessible online, use local files
                         if(status_code(data) == 200){
                           data = content(data)
                           write.csv(data, "data/hospitalData.csv", row.names = F)
                           print("covidProjectData data succesfully refreshed")
                           data
                         } else {
                           print("covidProjectData not accessible online, local data used")
                           read.csv("data/hospitalData.csv", stringsAsFactors = F)
                         }
                       })


# ---- Functions ----
#********************

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

#Function to detect if a mobile device is used (for plot size)
mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "extraJS.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}


# ---- UI ----
#**************
ui <- navbarPage(theme = shinytheme("paper"), collapsible = TRUE, id="nav",
        title = "COVID-19 Watcher",
        
        tabPanel("Cases/Deaths",
               sidebarLayout(  
                sidebarPanel(width = 4,
                  radioButtons("regionType", "Region", 
                               list("County" = "stateCounty", "City" = "CSA.Title", 
                                    "State" = "State_name", "USA" = "Country"), inline = T, selected = "CSA.Title"),
                  conditionalPanel(
                    condition = "input.regionType != 'Country'", 
                     selectInput(inputId = "region",
                                label = "Select one or more regions",
                                choices = "",
                                multiple = T, 
                                selectize = T
                    ),
                    helpText('Tip: type the name for easy searching'),hr()
                  ),
                  radioButtons("outcome", "Data", list("Cases" = 1, "Deaths" = 2), inline = T),
                  radioButtons("yScale", "Scale", list("Linear" = 1, "Logarithmic" = 2), inline = T),
                  conditionalPanel(
                    condition = "input.yScale == 1",  
                    radioButtons("relPop", "Adjust for population size", list("Yes" = 1, "No" = 2), inline = T, selected = 2)
                  ),
                  tags$br(),
                  downloadButton("downloadcasesPlot", "Download plot"),
                  tags$div(textOutput("filterWarnings"), style = "color: red;"),
                  mobileDetect('isMobile')

                ),
                mainPanel(
                  plotOutput(outputId = 'casesPlot')
                )
              )
        ),
        tabPanel("Testing",
                 sidebarLayout(
                   sidebarPanel(
                     width = 4,
                     selectInput(inputId = "testState", label = "Select one or more states",
                                 choices = popByState$State_name, 
                                 multiple = T, selectize = T, selected = "Ohio"
                     ),
                     radioButtons("testCurve", "COVID-19 tests", 
                                  list("Positive" = "positive", "Negative" = "negative", "All" = "totalTestResults"), 
                                  inline = T),
                     tags$div(textOutput("filterWarningsTest"), style = "color: red;"),
                     br(),
                     downloadButton("downloadTestPlot", "Download plot")
                   ),
                   mainPanel(
                     plotOutput(outputId = 'testPlot')
                   )
                 )
        ),
        tabPanel("Rankings",
                 fluidRow(
                   div(wellPanel(
                     radioButtons("rankItem", "Data", inline = T, 
                                  list("Cases" = "Cases", "Deaths"= "Deaths"))
                   ), align = "center")
                 ),
                 fluidRow(column(12, 
                   # div(HTML("<h5>You can sort each column or search for an area of interest</h5>"), 
                   #     align = "center"),
                  tabsetPanel(
                   tabPanel("County",br(),
                            HTML("NOTE: If your county is not listed, there is no data available.<br><br>"),
                            DTOutput("rankingCounty")),
                   tabPanel("City",br(),DTOutput("rankingMetro")),
                   tabPanel("State",br(),DTOutput("rankingState"))
                 )))
                 ),
        tabPanel("About this site",
                 tags$div(
                   tags$h4("Last update"), 
                   textOutput("updateTime"), 'Data updated daily.',
                   tags$br(),tags$br(),tags$h4("Summary"),
                   "This tool allows users to view COVID-19 data from across the United States. It works by merging county-level COVID-19 data from The New York Times with sources from the U.S. Census Bureau, mapping the data by metropolitan area.", tags$br(), tags$br(),
                   "As the coronavirus continues to spread throughout the U.S., thousands of people from across the country have used this dashboard to understand how the virus is impacting their community. Users can compare cities to watch the effects of shelter-in-place orders and gain insights on what may come next.",
                   tags$br(),tags$br(),tags$h4("Sources"),
                   tags$b("COVID-19 cases and deaths: "), HTML(paste0(a(href='https://www.nytimes.com/article/coronavirus-county-data-us.html','The New York Times', target="_blank"), ", based on reports from state and local health agencies.", sep = '')), 
                   tags$br(),tags$b("U.S. metropolitan area definitions: "), HTML(paste0(a(href='https://www.census.gov/programs-surveys/metro-micro.html','The United States Office of Management and Budget', target="_blank"), ".", sep = '')),
                   tags$br(),tags$b("Population estimates: "), HTML(paste0(a(href='https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html#par_textimage_70769902','The United States Census Bureau', target="_blank"), ".", sep = '')),
                   tags$br(),tags$b("COVID-19 Testing data: "), HTML(paste0(a(href='https://covidtracking.com/about-project','The COVID Tracking Project', target="_blank"), ".", sep = '')),
                   tags$br(),tags$br(),HTML(paste0('Inspiration for the design of these charts and this dashboard was derived from ', 
                   a(href='https://twitter.com/jburnmurdoch','John Burn-Murdoch', target="_blank"),' and ', 
                   a(href='https://github.com/eparker12/nCoV_tracker','Edward Parker, PhD', target="_blank"),', respectively.', sep = '')),
                   tags$br(),tags$br(),tags$h4("Code"),
                   HTML(paste0("This is an open-source tool and suggestions for improvement are welcomed. Those interested in contributing to this site can access the code on ", 
                               a(href='https://github.com/wisselbd/COVID-Tracker','GitHub', target="_blank"), ". Major contributions will be acknowledged.", sep = '')),
                   tags$br(),tags$br(),tags$h4("Authors"),
                   HTML(paste0("Benjamin Wissel, BS<sup>1,2</sup>, and PJ Van Camp, MD<sup>1,2</sup>", sep = "")), tags$br(),tags$br(), 
                   HTML(paste0("<sup>1</sup>Department of Biomedical Informatics, University of Cincinnati College of Medicine", sep = "")), tags$br(), 
                   HTML(paste0("<sup>2</sup>Division of Biomedical Informatics, Cincinnati Children's Hospital Medical Center", sep = "")),
                   tags$br(),tags$br(),tags$h4("Contact"),
                   #PJ: Can you make this email address a hyperlink that pulls up an email?
                   HTML('<b>Benjamin Wissel</b><br>Email: <a href="mailto:benjamin.wissel@cchmc.org?Subject=About%20COVID19%20WATCHER" target="_top">benjamin.wissel@cchmc.org</a>'),
                   tags$br(),
                   "Twitter: ", tags$a(href="https://twitter.com/BDWissel", "@bdwissel", target="_blank"),
                   HTML("<br><br><b>PJ Van Camp</b><br>LinkedIn: <a href='https://www.linkedin.com/in/pjvancamp/'>pjvancamp</a>"),
                   tags$br(),tags$br(),tags$h4("Acknowledgements"),
                   HTML(paste0("This site would not be possible without the help of our excellent team. Special thanks to Chad Weis and ", 
                    a(href="https://www.cincinnatichildrens.org/bio/k/michal-kouril", "Michal Kouril, PhD", target="_blank"), 
                    " for their help with building the infrastructure for the site; ",
                    "Leighanne Toole for media relations and promotion; and ", 
                   a(href="https://researchdirectory.uc.edu/p/wutz", "Danny Wu, PhD", target="_blank"), 
                   " and Sander Su for their help launching the beta version of this site.", sep = "")),
                   " We have received excellent feedback from the academic community, which we have taken into consideration and used to improve the presentation of the data; ",
                   "we would especially like to acknowledge Samuel Keltner for his suggestions.", tags$br(),tags$br(),tags$br(),tags$br()
                   
                 )
        ),
        #Add the logo
        tags$script(HTML("var header = $('.navbar> .container-fluid > .navbar-collapse');
                       header.append('<div style=\"float:right; margin-top:10px;\"><img src=\"headerLogo.jpg\" height=\"40px\"></div>');"))

)


# ---- SERVER ----
#*****************
server <- function(input, output, session) {
  
  referenceUs1 = reactive(paste("Authors: Benjamin Wissel and PJ Van Camp, MD\n",
                      "Data from The New York Times, based on reports from state and local health agencies.\n",
                      "Plot created: ", str_replace(input$clientTime, ":\\d+\\s", " "), 
                      "\nShowing data through: ", isolate(updateTime()),
                      "\nhttps://covid19watcher.research.cchmc.org", sep = ""))
  
  referenceUs2 = reactive(paste("Authors: Benjamin Wissel and PJ Van Camp, MD\n",
                               "Data from 'The COVID Tracking Project'\n",
                               "Plot created: ", str_replace(input$clientTime, ":\\d+\\s", " "), 
                               "\nShowing data through: ", isolate(updateTimeHospital()),
                               "\nhttps://covid19watcher.research.cchmc.org", sep = ""))
  
  #Load the NYT data
  covidData = reactive({
    
    data = NYTdata()

    #Add the special cases
    data[data$county == "New York City" & data$state == "New York","fips"] = "36124" #NYC
    data[data$county == "Kansas City" & data$state == "Missouri","fips"] = "29511" #Kansas City

    data = data %>%
      mutate(fips = as.character(fips), fips = ifelse(nchar(fips) < 5, paste0(0, fips), fips),
             date = as.Date(date))

    #Add the unknow counties
    data = data %>% left_join(unknownCounties %>% select(-state), by = c("state" = "stateName", "county"))
    data = data %>% mutate(fips = ifelse(is.na(fips), FIPS, fips))%>% select(-FIPS)

    updateTime(gsub("/0","/", gsub("^0","", as.character(max(data$date, na.rm = T) %>% format('%m/%d/%Y')))))

    data  %>% select(-county, - state)
  })

  #Load the COVID project data
  hospitalData = reactive({
    
    data = covidProjectData() %>% 
      mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
      left_join(popByState, by = c("state" = "State"))

    updateTimeHospital(gsub("/0","/", gsub("^0","", as.character(max(data$date, na.rm = T) %>% format('%m/%d/%Y')))))

    data

  })
  

  #updateTime = reactiveVal(gsub("/0","/", gsub("^0","", as.character(Sys.time() %>% format('%m/%d/%Y')))))
  updateTime = reactiveVal(Sys.time())
  updateTimeHospital = reactiveVal(Sys.time())
  filterWarning = reactiveVal("")
  filterWarningTest = reactiveVal("")
  regionTest = reactiveVal("")
  
  #Update the time on the page
  output$updateTime = renderText(updateTime())
  
  observeEvent(input$regionType, {
    if(input$regionType == "CSA.Title"){
      updateSelectInput(session, "region", "Select one or more metro areas", choices = sort(unique(fipsData$CSA.Title)),
                        selected = c("New Orleans-Metairie-Hammond, LA-MS", "San Jose-San Francisco-Oakland, CA"))
    } else if(isolate(input$regionType) == "stateCounty") {
      updateSelectInput(session, "region", "Select one or more counties", 
                        #Do not display unknown counties
                        choices = sort(unique(fipsData %>% filter(County != "Unknown") %>% pull(stateCounty))),
                        selected = "CA: Orange County")
    } else if(isolate(input$regionType) == "State_name") {
      updateSelectInput(session, "region", "Select one or more states", choices = sort(unique(fipsData$State_name)),
                        selected = "Missouri")
    } else {
      updateSelectInput(session, "region", "Select one or more countries", choices = sort(unique(fipsData$Country)),
                        selected = "USA")
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
       select(region = !!groupByRegion, FIPS) %>% 
       left_join(covidNumbers, by = c("FIPS" = "fips")) %>% #Join the cases per fips
       filter(!is.na(date)) %>% group_by(region, date) %>% #Now group per region
       summarise(cases = sum(cases), deaths = sum(deaths)) 

     if(isolate(input$regionType) == "CSA.Title"){
       plotData = plotData %>% 
         left_join(popByMetro, by = c("region" = "CSA.Title")) 
     } else if(isolate(input$regionType) == "stateCounty"){
       plotData = plotData %>% 
         left_join(popByCounty, by = c("region" = "stateCounty"))
     } else if(isolate(input$regionType) == "State_name"){
       plotData = plotData %>% 
         left_join(popByState, by = c("region" = "State_name"))
     } else {
       plotData = plotData %>% 
         left_join(popByCountry, by = c("region" = "Country"))
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
     
     #When normalizing cases or deaths per 10,000 residents
     if(input$relPop == 1 && input$yScale == 1){
       plotData = plotData %>% 
         mutate(y = y / (Population / 10000))
     }
     
     #Edit the order of the labels by descending y-value
     myOrder = plotData %>% group_by(region) %>% summarise(y = max(y)) %>% arrange(desc(y))
     plotData$region = factor(plotData$region, levels = c(myOrder$region))
    
     plotData %>% ungroup()
     
  })
   
  # ---- Generate Cases / Deaths plot ---
  casesPlot = reactive({
    
    startCases = ifelse(input$outcome == 1, 10, 1)
    
    plot = ggplot(plot.data(), aes(x=x, y=y, color = region)) +
      geom_line(size = 1.2)
    
    #Guide for doubling time
    if(input$yScale == 2){ #Only relevant when aligned by number of starting cases
      
      #If the population is relative, make sure the guide is too (is average population of ones shown)
      pop = ifelse(F, 
                   mean(plot.data() %>% group_by(region) %>% summarise(p = max(Population)) %>% pull(p)),
                   10000)
      
      
      #Generate the doubline time using the doubleRate function (see at top)
      if(input$yScale == 2){
        plot = plot + 
          stat_function(fun = ~log10(doubleRate(.x, startCases, 1, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~log10(doubleRate(.x, startCases, 2, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~log10(doubleRate(.x, startCases, 3, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3) +
          stat_function(fun = ~log10(doubleRate(.x, startCases, 7, pop)),
                        linetype="dashed", colour = "#8D8B8B", size = 1.0, alpha = 0.3)
          
        oneDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 1, startCases = startCases)
        twoDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 2, startCases = startCases)
        threeDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 3, startCases = startCases)
        sevenDayLabel = labelPos(max(plot$data$x),max(plot$data$y), daysToDouble = 7, startCases = startCases)
      }
      
      plot = plot +
        annotate("text", x = oneDayLabel$posX, y = oneDayLabel$posY, label = "Doubles every day", color = "#8D8B8B") +
        annotate("text", x = twoDayLabel$posX, y = twoDayLabel$posY, label = "...every 2 days", color = "#8D8B8B") +
        annotate("text", x = threeDayLabel$posX, y = threeDayLabel$posY, label = "...every 3 days", color = "#8D8B8B") + 
        annotate("text", x = sevenDayLabel$posX, y = sevenDayLabel$posY, label = "...every week", color = "#8D8B8B") + 
        scale_y_log10(labels = comma, limits = c(NA, max(plot$data$y)))
      
    } else {
      plot = plot + scale_y_continuous(labels = comma)
    }
    
    #Labels depend on selections
    xLabel = ifelse(input$yScale == 1, "Date", 
                    paste("Number of Days Since",
                          ifelse(input$outcome == 1, "10th Case", "1st Death")))
    
    yLabel = ifelse(input$relPop == 1 && input$yScale == 1,ifelse(input$outcome == 1, "Cases per 10,000 Residents", 
                                                                  "Deaths per 10,000 Residents"), 
                    ifelse(input$outcome == 1, "Confirmed Cases", "Deaths"))
    
    plot + theme_bw() +
      #Add the latest counts at the end of the curve
      geom_text(data = plot.data() %>% filter(date == last(date)), 
                aes(label = if(input$relPop == 1){format(round(y, 2), big.mark = ",", nsmall = 2)} else 
                  {format(round(y, 0), big.mark = ",", nsmall = 0)}, 
                    x = x, y = y), size = 4, check_overlap = T,hjust=0, vjust=0.5) +
      #Update the labs based on the filters
      labs(title = sprintf('COVID-19 %s in %s', 
                           ifelse(input$outcome == 1, "Cases", "Deaths"),
                           case_when(
                             isolate(input$regionType) == "CSA.Title" ~ "U.S. Metropolitan Areas",
                             isolate(input$regionType) == "stateCounty" ~ "U.S. Counties",
                             isolate(input$regionType) == "State_name" ~ "U.S. States",
                             T ~ "the USA"
                           )),
           caption = isolate(referenceUs1()))  +
      xlab(xLabel) + ylab(yLabel) +
      coord_cartesian(clip = 'off') + #prevent clipping off labels
      theme(plot.title = element_text(hjust = 0.0),
            legend.position = 'right', legend.direction = "vertical", 
            legend.title = element_blank(), 
            panel.border = element_blank(),
            plot.caption = element_text(hjust = 0.0, size = 14),
            axis.line = element_line(colour = "black"),
            text = element_text(size=20)) +
      #Set the icons of the legend (looked weird)
      guides(colour = guide_legend(override.aes = list(size=1.5, shape=95)))
  }) 
   
  # ---- Render the plot ----
  output$casesPlot <- renderPlot({
    req(!is.null(input$region))
    casesPlot() + scale_color_discrete(labels = str_trunc(levels(plot.data()$region), 40)) 
     #If mobile, use statix width for plot, else dynamic 
  }, height = 600, width = function(){ifelse(input$isMobile, 1000, "auto")})

  
  # ---- Download Cases/Deaths plot ----
  output$downloadcasesPlot <- downloadHandler(
    filename = function() {
      paste("covid19watcher_Plot_", as.integer(Sys.time()), ".png", sep="")
    },
    content = function(file) {
      myPlot = casesPlot() +
        labs(caption =  isolate(referenceUs1()))
      ggsave(file, myPlot, width = 12, height = 7, device = "png")

    }
  )
  
  # ---- Warning message ----
  #Warning when display after first 10 cases and regions don't have 10+
  output$filterWarnings = renderText({
    filterWarning()
  })
   
   
   # ---- TESTING PLOT ----
   #***********************
  
   testingData = reactive({
     
     req(!is.null(input$testState))
     
     #Filter the hospitalData based on user selections
     myData = hospitalData() %>% filter(State_name %in% input$testState) %>% 
       select(State_name, date, y = !!sym(input$testCurve), hospitalizedCurrently, inIcuCurrently, fips) %>% 
       filter(y > 0)
     
     #If there is no data (e.g. 0 tests) warn the user and don't show line
     noData = setdiff(input$testState, myData$State_name)
     if(length(noData) > 0){
       filterWarningTest(paste("The following states have no data:", paste(noData, collapse = ", ")))
     }
     
     #Generate the plot
     ggplot(myData, aes(x = date, y = y, color = State_name)) + 
       geom_line(size = 1.2) + theme_bw() + scale_y_log10(labels = comma) +
       #Add the latest counts at the end of the curve
       geom_text(data = myData %>% group_by(State_name) %>% filter(date == first(date)), 
                 aes(label = y, x = date, y = y), size = 5, check_overlap = F,hjust=0, vjust=0.5) +
       #Update the labs based on the filters
       labs(title = sprintf('%s COVID-19 tests',
                            case_when(
                              input$testCurve == "positive" ~ "Postive",
                              input$testCurve == "negative" ~ "Negative",
                              T ~ "All"
                            )),
            caption = isolate(referenceUs2()))  +
       xlab("Date") + ylab("Number of tests") +
       coord_cartesian(clip = 'off') + #prevent clipping off labels
       theme(plot.title = element_text(hjust = 0.0),
             legend.position = 'right', legend.direction = "vertical",
             legend.title = element_blank(),
             panel.border = element_blank(),
             plot.caption = element_text(hjust = 0.0, size = 14),
             axis.line = element_line(colour = "black"),
             text = element_text(size=20)) +
       #Set the icons of the legend (looked weird)
       guides(colour = guide_legend(override.aes = list(size=1.5, shape=95)))
   })
   
   output$testPlot = renderPlot({
     testingData()
   }, height = 600, width = function(){ifelse(input$isMobile, 1000, "auto")})
   
   
   # ---- Download testing plot ----
   output$downloadTestPlot <- downloadHandler(
     filename = function() {
       paste("covid19watcher_TestingPlot_", as.integer(Sys.time()), ".png", sep="")
     },
     content = function(file) {
       
       ggsave(file, testingData(), width = 12, height = 7, device = "png")
       
     }
   )
   
   # ---- Warning message ----
   #Warning when no testing data
   output$filterWarningsTest = renderText({
     filterWarningTest()
   })
   
   
   # ---- RANKING TABLES ----
   #*************************
   allRankingData = reactive({
     latestDate = max(covidData()$date)
     data = covidData() %>% filter(date == latestDate) %>% 
       select(fips, cases, deaths)
     colnames(data)[colnames(data) %in% c('cases','deaths')] <- c('Cases', 'Deaths')
     
     fipsData %>% left_join(data, by = c("FIPS" = "fips"))
   })
   
   
   countyTable = reactive({
     rankItem = sym(input$rankItem)
     
     allRankingData() %>% filter(County != "Unknown", !is.na(!!rankItem)) %>%  
      select(County, State, !!rankItem, FIPS) %>% 
      left_join(popByCounty %>% select(-stateCounty), by = "FIPS") %>% 
      mutate(`Per 10,000 Residents` = round(!!rankItem / (Population / 10000), 2)) %>% 
      select(-FIPS) %>% arrange(desc(!!rankItem)) %>% ungroup() %>% 
      mutate(Ranking = 1:n()) %>% select(Ranking, County, State, Population, !!rankItem, `Per 10,000 Residents`)
   })
   
   output$rankingCounty = renderDT({
     datatable(countyTable(), rownames = F, 
               options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
       formatCurrency(4:5, "", digits = 0)
   })
   
   metroTable = reactive({
     rankItem = sym(input$rankItem)
     
     allRankingData() %>% filter(!is.na(CSA.Title), !is.na(!!rankItem)) %>%  
      select(City = CSA.Title, !!rankItem) %>% 
      group_by(City) %>% 
      summarise(!!rankItem := sum(!!rankItem)) %>% 
      left_join(popByMetro, by = c("City" = "CSA.Title")) %>% 
      mutate(`Per 10,000 Residents` = round(!!rankItem / (Population / 10000), 2)) %>% 
      arrange(desc(!!rankItem)) %>% ungroup() %>% 
      mutate(Ranking = 1:n()) %>% select(Ranking, City, Population, !!rankItem, `Per 10,000 Residents`) 
   })
   
   output$rankingMetro = renderDT({
     datatable(metroTable(), rownames = F, 
               options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
       formatCurrency(3:4, "", digits = 0)
   })
   
   stateTable = reactive({
     rankItem = sym(input$rankItem)
     
     allRankingData() %>% filter(!is.na(!!rankItem)) %>%  
      select(State, !!rankItem) %>% 
      group_by(State) %>% 
      summarise(!!rankItem := sum(!!rankItem)) %>% 
      left_join(popByState, by = "State") %>% 
      mutate(`Per 10,000 Residents` = round(!!rankItem / (Population / 10000), 2)) %>% 
      arrange(desc(!!rankItem)) %>% ungroup() %>% 
      mutate(Ranking = 1:n(), State = sprintf("%s (%s)", State_name, State)) %>% 
      select(Ranking, State, Population, !!rankItem, `Per 10,000 Residents`) 
   })
   
   output$rankingState = renderDT({
     datatable(stateTable(), rownames = F, 
               options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>% 
       formatCurrency(3:4, "", digits = 0)
   })
}

shinyApp(ui = ui, server = server)