# load required packages
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
#if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
#if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")

## Load in the data
counties <- read.csv('us-counties3.31.csv') %>% as.data.frame()
counties <- counties %>% mutate_all(as.character)
counties$date <- as.Date(counties$date)
county.pop <- read.csv('est2019-alldata.csv') %>% as.data.frame()
county.pop <- county.pop %>% mutate_all(as.character)
fips.codes <- read.xlsx('metro_fips_codes.xlsx') %>% as.data.frame()
fips.codes$fips <- paste(fips.codes$FIPS.State.Code,fips.codes$FIPS.County.Code, sep = '')
fips.codes$fips <- gsub('^0','',fips.codes$fips)
fips.codes <- left_join(fips.codes, county.pop, by = 'fips')
fips.codes$POPESTIMATE2019 <- as.integer(fips.codes$POPESTIMATE2019)
metro.pop <- group_by(fips.codes, CSA.Title) %>% summarize(pop = sum(POPESTIMATE2019)) %>% as.data.frame()
colnames(metro.pop) <- c('metro','pop')
metro.options <- sort(unique(fips.codes$CSA.Title)[!is.na(unique(fips.codes$CSA.Title))])

## Set some formatting things
min.date <- '2020-03-10'
min.cases.on.log <- 10
#update.time <- paste(Sys.time() %>% format('%b %d'),', ',Sys.time() %>% format('%l:%M %p'),' EST.', sep = '')
update.time <- 'March 31, 10:25 AM EST'
y.labels <- c('Confirmed Cases','Cases per 10,000 Residents')
names(y.labels) <- c('ConfirmedCases','Casesper10000Residents')

## Set up color codes for reference lines on log scale
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Define UI for application
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "COVID-19 Tracker", id="nav",

        tabPanel("Plots",
               sidebarLayout(  
                sidebarPanel(
                  selectInput(inputId = "MetrosChosen",
                              label = "Select one or more metro areas:",
                              choices = metro.options,
                              selected = c('Cincinnati-Wilmington-Maysville, OH-KY-IN',"Cleveland-Akron-Canton, OH","Columbus-Marion-Zanesville, OH"),
                              multiple = T, 
                              selectize = T,
                              width = "400px"
                  ),
                  helpText('Tip: type the city name for easy searching.'),hr(),
                  radioButtons("y.label", "Adjust for population size:",
                               list("Yes" = "Casesper10000Residents",
                                    "No" = "ConfirmedCases"), selected = 'ConfirmedCases'),
                  br(),
                  actionButton("button", "Submit")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Linear Scale", plotOutput(outputId = 'plot1', width = "1000px", height = "500px")),
                    tabPanel("Log Scale", plotOutput(outputId = 'plot2', width = "1000px", height = "500px"))
                  )
                )
              )
        ),
        tabPanel("About this site",
                 tags$div(
                   tags$h4("Last update"), 
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
                   tags$br(),tags$br(),tags$h4("Author"),
                   'Benjamin Wissel', tags$br(), 'MD-PhD Candidate', tags$br(), 'Department of Biomedical Informatics',tags$br(),"Cincinnati Children's Hospital Medical Center",tags$br(),'University of Cincinnati College of Medicine',
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
  
  #GET THE DATA
  plot.linear.data <- eventReactive(input$button, {
    metro.fips <- fips.codes[fips.codes$CSA.Title %in% as.character(input$MetrosChosen), ] %>% as.data.frame()
    metro.fips <- metro.fips[!is.na(metro.fips$CSA.Title), ]
    metro.fips <- paste(metro.fips$FIPS.State.Code, metro.fips$FIPS.County.Code, sep = '')
    metro.fips <- gsub('^0','',metro.fips)
    
    ## Get the case data from the fips codes
    case.data <- counties[counties$fips %in% metro.fips, ] %>% as.data.frame()
    case.data <- case.data[order(case.data$fips), ]
    metro.fips.w.names <- cbind(fips.codes$CSA.Title, paste(paste(fips.codes$FIPS.State.Code, fips.codes$FIPS.County.Code, sep = ''))) %>% as.data.frame()
    metro.fips.w.names <- metro.fips.w.names %>% mutate_all(as.character)
    metro.fips.w.names$V2 <- gsub('^0','',metro.fips.w.names$V2)
    metro.fips.w.names <- metro.fips.w.names[metro.fips.w.names$V2 %in% metro.fips, ]
    metro.fips.w.names <- metro.fips.w.names[order(match(metro.fips.w.names$V2, case.data$fips)), ]
    colnames(metro.fips.w.names) <- c('metro','fips')
    case.data <- left_join(case.data, metro.fips.w.names, by = c('fips')) %>% as.data.frame()
    case.data$cases <- as.integer(case.data$cases)
    case.data <- group_by(case.data, metro, date) %>% summarize(cases = sum(cases))
    case.data <- left_join(case.data,metro.pop , by = 'metro') %>% as.data.frame()
    case.data$cases.per.10000 <- case.data$cases/case.data$pop*10000
    colnames(case.data)[c(3,5)] <- c('ConfirmedCases', 'Casesper10000Residents')
    case.data[case.data$date>= min.date, ] %>% as.data.frame()
  })
  
  case.data.log <- eventReactive(input$button, {
    metro.fips <- fips.codes[fips.codes$CSA.Title %in% as.character(input$MetrosChosen), ] %>% as.data.frame()
    metro.fips <- metro.fips[!is.na(metro.fips$CSA.Title), ]
    metro.fips <- paste(metro.fips$FIPS.State.Code, metro.fips$FIPS.County.Code, sep = '')
    metro.fips <- gsub('^0','',metro.fips)
    
    ## Get the case data from the fips codes
    case.data <- counties[counties$fips %in% metro.fips, ] %>% as.data.frame()
    case.data <- case.data[order(case.data$fips), ]
    metro.fips.w.names <- cbind(fips.codes$CSA.Title, paste(paste(fips.codes$FIPS.State.Code, fips.codes$FIPS.County.Code, sep = ''))) %>% as.data.frame()
    metro.fips.w.names <- metro.fips.w.names %>% mutate_all(as.character)
    metro.fips.w.names$V2 <- gsub('^0','',metro.fips.w.names$V2)
    metro.fips.w.names <- metro.fips.w.names[metro.fips.w.names$V2 %in% metro.fips, ]
    metro.fips.w.names <- metro.fips.w.names[order(match(metro.fips.w.names$V2, case.data$fips)), ]
    colnames(metro.fips.w.names) <- c('metro','fips')
    case.data <- left_join(case.data, metro.fips.w.names, by = c('fips')) %>% as.data.frame()
    case.data$cases <- as.integer(case.data$cases)
    case.data <- group_by(case.data, metro, date) %>% summarize(cases = sum(cases))
    case.data.log <- case.data[case.data$cases>=min.cases.on.log, ]
    day.count <- table(case.data.log$metro) %>% as.integer()
    days <- c()
    for(city in 1:length(day.count)){
      days <- c(days, seq(0,day.count[city]-1))
    }
    case.data.log$days <- days
    as.data.frame(case.data.log)
  })
  
  reference <- eventReactive(input$button, {
    reference <- data.frame(x=case.data.log()[,4],y=(10*2^(case.data.log()[,4]/2)), ref = '2 days: doubling time') %>% unique()
    rbind(reference, data.frame(x=case.data.log()[,4],y=(10*2^(case.data.log()[,4]/3)), ref = '3 days: doubling time') %>% unique())
  })
  
  #OUTPUT PLOT
  output$plot1 <- renderPlot({
    
    ggplot(plot.linear.data(), aes_string(x='date', y=input$y.label, group = 'metro', colour = 'metro')) +
      geom_point() + 
      geom_line() +
      scale_x_date(date_breaks = "3 days", date_labels = "%b %d", limits = c(as.Date(min(plot.linear.data()[,2])), as.Date(max(plot.linear.data()[,2])))) +
      theme_bw() +
      labs(title = 'COVID-19 Cases in U.S. Metropolitan Areas') +
      xlab('Date') +
      ylab(y.labels[names(y.labels) %in% input$y.label]) +
      theme(legend.position = 'right', legend.title = element_blank(), plot.caption = element_text(hjust = 0), text = element_text(size=20))
    
  })

  output$plot2 <- renderPlot({
    
    ggplot(case.data.log(), aes(x=days, y=cases, group = metro, colour = metro)) +
      geom_point() + 
      geom_line() +
      theme_bw() +
      scale_y_log10(labels = comma) +
      scale_x_continuous(limits = c(0,max(case.data.log()[,4])), breaks = seq(0, max(case.data.log()[,4]), by = 2)) +
      labs(title = 'COVID-19 Cases in U.S. Metropolitan Areas') +
      ylab('Confirmed Cases') +
      xlab('Number of Days Since 10th Case') +
      geom_line(data=reference(), aes(x=x,y=y, group = ref, colour = ref), linetype = 'dashed') +
      scale_color_manual(values=c("#000000", "#808080",gg_color_hue(length(unique(case.data.log()[,1]))))) +
      theme(legend.title = element_blank(), plot.caption = element_text(hjust = 0), text = element_text(size=20)) 
  })
}

shinyApp(ui = ui, server = server)

#rsconnect::deployApp('/Users/wis2lp/Documents/COVID-NYT/R_Scripts/Publish/', appName = 'COVID')
