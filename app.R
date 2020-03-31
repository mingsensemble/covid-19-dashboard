rm(list = ls())

# load required pacakges

require(tidyverse)
require(shiny)

# set macro parameters
x0 <- 100 # counting days since x0 confirmed cases
# ==================================
# define exponential growth function
exp_growth <- function(x, k, x0) {
  log10(2)/k * (x) + log10(x0)
}
inverse_exp_growth <- function(y, k, x0) {
  (k/log10(2)) * (log10(y) - log10(x0))
}
# ================================
fileLoc <- 'data/time_series_covid19_confirmed_global.csv'
data <- read_csv(fileLoc, 
                 col_types = cols(
                   .default = col_double(),
                   `Province/State` = col_character(),
                   `Country/Region` = col_character()
                 )
) 
fileLocUS <- 'data/us-states.csv'
dataUS <- read_csv(fileLocUS, 
                   col_types = cols(
                     date = col_date(format = "%Y-%m-%d"),
                     state = col_character(),
                     fips = col_character(),
                     cases = col_double(),
                     deaths = col_double()
                   )
) 
# =================================
disclaimer <- paste0("
  Data are obtained from GitHub: Global data from CSSEGISandData/COVID-19; US data from nytimes/covid-19-data; last update on
", file.info(fileLoc)$ctime)

fineprint <- "The idea of using exponential growth model to benchmark confirmed case trajectories was inpired by https://ourworldindata.org/coronavirus"
devInfo <- "Developed by Ming-Sen Wang (Economist/Data Scientist); first version 03/16/2020; current version 03/30/2020"
# ================================
# World data
# aggregate data at country level
allDf <- data %>% gather(
  date, cases, -`Province/State`, -`Country/Region`, -Lat, -Long
) %>% group_by(`Country/Region`, date) %>% summarize(
  cases = sum(cases)
) %>% mutate(
  date = as.Date(date, format = "%m/%d/%y")
) %>% dplyr::filter(
  cases >= x0 # start monitoring after x0 cases
) %>% ungroup() %>% arrange(date) %>% group_by(`Country/Region`) %>% mutate(
  days = (1:length(cases)) - 1
) %>% dplyr::filter(`Country/Region` != 'Diamond Princess')

keyCountries <- c(
  "US", "Italy", "China", "Korea, South", "United Kingdom",
  "Singapore", "Australia", "Japan", "Peru", "India",
  "Taiwan*", "United Arab Emirates", "France", "Germany"
)
# ==================================
# location parameters
posData <- allDf %>% group_by(`Country/Region`) %>% 
  dplyr::filter(days == max(days)) %>%
  select(days, cases, `Country/Region`)

ylvl <- c(100000, 100000, 10000, 800)
funcLoc <- tibble(
  days = do.call('c', Map(function(y, k) {inverse_exp_growth(y, k , x0 = x0)}, y = as.list(ylvl), k = as.list(c(2, 3, 5, 10)))),
  cases = ylvl,
  `Country/Region` = c("Double every 2 days", "Double every 3 days", "Double every 5 days", "Double every 10 days")
)

posData <- bind_rows(posData, funcLoc)

# =================================
# drop down list
dropdown <- list()
choices <- unique(allDf$`Country/Region`)[order(unique(allDf$`Country/Region`))]
capLetters <- unique(substr(choices,1, 1))

for(l in capLetters) {
  dropdown <- c(dropdown, list(choices[which(substr(choices,1, 1) == l)]))
}

names(dropdown) <- capLetters
# ==================================
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ==================================
# US Data
allDfUS <- dataUS %>% dplyr::filter(
  cases >= x0 # start monitoring after x0 cases
) %>% arrange(date) %>% group_by(state) %>% mutate(
  days = (1:length(cases)) - 1
)
keyStates <- c(
  'California', 'New York', 'New Jersey',
  'Michigan', 'Illinois', 'Washington', 
  'Connecticut', 'Florida')
# ==================================
# location parameters
posDataUS <- allDfUS %>% group_by(state) %>% 
  dplyr::filter(days == max(days)) %>%
  select(days, cases, state)

ylvl <- c(40000, 20000, 10000, 5000)
funcLoc <- tibble(
  days = do.call('c', Map(function(y, k) {inverse_exp_growth(y, k , x0 = x0)}, y = as.list(ylvl), k = as.list(c(2, 3, 5, 10)))),
  cases = ylvl,
  state = c("Double every 2 days", "Double every 3 days", "Double every 5 days", "Double every 10 days")
)

posDataUS <- bind_rows(posDataUS, funcLoc)
# =================================
# drop down list
dropdownUS <- list()
choices <- unique(allDfUS$state)[order(unique(allDfUS$state))]
capLetters <- unique(substr(choices,1, 1))

for(l in capLetters) {
  dropdownUS <- c(dropdownUS, list(choices[which(substr(choices,1, 1) == l)]))
}

names(dropdownUS) <- capLetters
# ==================================
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ==================================
# dashboard structure 
# user interface
ui_world <- fluidPage(
  sidebarPanel(
    selectInput(
      'country', "Select a Country",
      choices = dropdown,
      selected = "Taiwan*"
    ),
    checkboxInput("simple", "Declutter", FALSE),
    hr(),
    p(disclaimer, style = "font-size:16px"),
    hr(),
    p(fineprint, style = "font-size:16px"),
    hr(),
    p(devInfo, style = "font-size:12px")
  ), 
  mainPanel(
    plotOutput("lineChart")
  )
)

ui_us <- fluidPage(
  sidebarPanel(
    selectInput(
      'state', "Select a State",
      choices = dropdownUS,
      selected = "Illinois"
    ),
    checkboxInput("simpleUS", "Declutter", FALSE),
    hr(),
    p(disclaimer, style = "font-size:16px"),
    hr(),
    p(fineprint, style = "font-size:16px"),
    hr(),
    p(devInfo, style = "font-size:12px")
  ), 
  mainPanel(
    plotOutput("lineChartUS")
  )
)

ui <- navbarPage("Trajectory of Total Confirmed Cases of COVID-19",
                 tabPanel("World", ui_world),
                 tabPanel("USA", ui_us)
)
#  ======================================
theme_update(
  panel.background = element_blank(),
  legend.position = "none",
  panel.grid.major = element_line(colour = "light grey"),
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 14)
)
# ======================================
server <- function(input, output) {
  # World Trend Chart   
  output$lineChart <- renderPlot({

    if(input$simple) {
      df <- allDf %>% dplyr::filter(`Country/Region` %in% c(
        keyCountries, input$country
      ))
      keyPosData <- posData %>% 
        dplyr::filter(`Country/Region` %in% c(
          keyCountries, input$country, "Double every 2 days",
          "Double every 3 days", "Double every 5 days", "Double every 10 days"
        ))
    } else {
      df <- allDf
      keyPosData <- posData
    }
    
    usedData <- df %>% mutate(
      selected = ifelse(
        (`Country/Region` == input$country), "Y", 
        ifelse(sub(" .*", "",`Country/Region`) == "Double", "O", "F")
      )
    )
    usedPosData <- keyPosData %>% dplyr::mutate(
      selected = ifelse(
        (`Country/Region` == input$country), "Y", 
        ifelse(sub(" .*", "",`Country/Region`) == "Double", "O", "F")
      )
    )
    
    ggplot(usedData, aes(x = days, y = cases, group = `Country/Region`)) +
      geom_line(aes(colour = selected, size = selected, alpha = as.numeric(as.factor(selected)))) + 
      scale_size_manual(values = c(0.5, 1, 1.5)) +
      geom_point(aes(colour = selected, size = selected, alpha = as.numeric(as.factor(selected)))) +
      scale_colour_manual(values = c("#c3c3c3", "#ee9e64", "#cd4c46")) +
      scale_alpha(range = c(0.80, 1)) +
      geom_text(
        data = usedData %>% dplyr::filter(selected == "Y"),
        aes(label = cases),
        nudge_y = log10(1.25)
      ) + 
      geom_text(
        data = usedPosData,
        aes(x = days, y = cases, label = `Country/Region`, colour = selected, alpha = as.numeric(as.factor(selected))),
        nudge_x = 1,
        nudge_y = log10(1.5)
      ) + scale_y_log10(labels = scales::comma, limits = c(x0, max(df$cases) * 5)) + 
      scale_x_continuous(limits = c(0, max(df$days) + 5)) +
      stat_function(fun = exp_growth, args = list(k = 10, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 5, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 3, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 2, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      xlab(paste0("Days since ", x0,  "th confirmed cases")) + ylab("Total Confirmed Cases")
  }, width = 800, height = 600)
  
  output$lineChartUS <- renderPlot({
    
    if(input$simpleUS) {
      dfUS <- allDfUS %>% dplyr::filter(state %in% c(
        keyStates, input$state
      ))
      keyPosData <- posDataUS %>% 
        dplyr::filter(state %in% c(
          keyStates, input$state, "Double every 2 days",
          "Double every 3 days", "Double every 5 days", "Double every 10 days"
        ))
    } else {
      dfUS <- allDfUS
      keyPosData <- posDataUS
    }
    
    usedData <- dfUS %>% mutate(
      selected = ifelse(
        (state == input$state), "Y", 
        ifelse(sub(" .*", "",state) == "Double", "O", "F")
      )
    )
    usedPosData <- keyPosData %>% dplyr::mutate(
      selected = ifelse(
        (`state` == input$state), "Y", 
        ifelse(sub(" .*", "",state) == "Double", "O", "F")
      )
    )
    
    ggplot(usedData, aes(x = days, y = cases, group = state)) +
      geom_line(aes(colour = selected, size = selected, alpha = as.numeric(as.factor(selected)))) + 
      scale_size_manual(values = c(0.5, 1, 1.5)) +
      geom_point(aes(colour = selected, size = selected, alpha = as.numeric(as.factor(selected)))) +
      scale_colour_manual(values = c("#c3c3c3", "#ee9e64", "#cd4c46")) +
      scale_alpha(range = c(0.80, 1)) +
      geom_text(
        data = usedData %>% dplyr::filter(selected == "Y"),
        aes(label = cases),
        nudge_y = log10(1.25)
      ) + 
      geom_text(
        data = usedPosData,
        aes(x = days, y = cases, label = state, colour = selected, alpha = as.numeric(as.factor(selected))),
        nudge_x = 1,
        nudge_y = log10(1.5)
      ) + scale_y_log10(labels = scales::comma, limits = c(x0, max(dfUS$cases) * 5)) + 
      scale_x_continuous(limits = c(0, max(dfUS$days) + 5)) +
      stat_function(fun = exp_growth, args = list(k = 10, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 5, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 3, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 2, x0 = x0), colour = "#ee9e64", alpha = 0.5) +
      xlab(paste0("Days since ", x0,  "th confirmed cases")) + ylab("Total Confirmed Cases")
  }, width = 800, height = 600)

}
shinyApp(ui = ui, server = server)