rm(list = ls())

# load required pacakges

require(tidyverse)
require(shiny)

# ================================
fileLoc <- 'data/time_series_19-covid-Confirmed.csv'
data <- read_csv(fileLoc, 
                 col_types = cols(
                   .default = col_double(),
                   `Province/State` = col_character(),
                   `Country/Region` = col_character()
                 )
) 

# ================================
# World data
# aggregate data at country level
df <- data %>% gather(
  date, cases, -`Province/State`, -`Country/Region`, -Lat, -Long
) %>% group_by(`Country/Region`, date) %>% summarize(
  cases = sum(cases)
) %>% mutate(
  date = as.Date(date, format = "%m/%d/%y")
) %>% dplyr::filter(
  cases >= 10 # start monitoring after 10 cases
) %>% ungroup() %>% arrange(date) %>% group_by(`Country/Region`) %>% mutate(
  days = (1:length(cases)) - 1
)
# ==================================
# define exponential growth function
exp_growth <- function(x, k) {
  log10(2)/k * (x) + 1
}
# ==================================
# location parameters
posData <- df %>% group_by(`Country/Region`) %>% 
  dplyr::filter(days == max(days)) %>%
  select(days, cases, `Country/Region`)

ylvl <- c(100000, 100000, 10000, 400)
funcLoc <- tibble(
  days = do.call('c', Map(function(y, k) {(log10(y) - 1) * k/log10(2)}, y = as.list(ylvl), k = as.list(c(2, 3, 5, 10)))),
  cases = ylvl,
  `Country/Region` = c("Double every 2 days", "Double every 3 days", "Double every 5 days", "Double every 10 days")
)

posData <- bind_rows(posData, funcLoc)

# =================================
# drop down list
dropdown <- list()
choices <- unique(df$`Country/Region`)[order(unique(df$`Country/Region`))]
capLetters <- unique(substr(choices,1, 1))

for(l in capLetters) {
  dropdown <- c(dropdown, list(choices[which(substr(choices,1, 1) == l)]))
}

names(dropdown) <- capLetters
# ==================================
# prepare US state level data
dfUS <- data %>% dplyr::filter(
  `Country/Region` == "US" & !grepl(",", `Province/State`) & !grepl("Princess", `Province/State`)
) %>% gather(
  date, cases, -`Province/State`, -`Country/Region`, -Lat, -Long
) %>% mutate(
  date = as.Date(date, format = "%m/%d/%y")
) %>% dplyr::filter(
  cases >= 10 # start monitoring after 10 cases
) %>% ungroup() %>% arrange(date) %>% group_by(`Province/State`) %>% mutate(
  days = (1:length(cases)) - 1
)

# location parameters
posDataUS <- dfUS %>% group_by(`Province/State`) %>% 
  dplyr::filter(days == max(days)) %>%
  select(days, cases, `Province/State`)

ylvl <- c(120, 60, 30)
funcLoc <- tibble(
  days = do.call('c', Map(function(y, k) {(log10(y) - 1) * k/log10(2)}, y = as.list(ylvl), k = as.list(c(2, 3, 5)))),
  cases = ylvl,
  `Province/State` = c("Double every 2 days", "Double every 3 days", "Double every 5 days")
)

posDataUS <- bind_rows(posDataUS, funcLoc)

# drop down list
dropdownUS <- list()
choices <- unique(dfUS$`Province/State`)[order(unique(dfUS$`Province/State`))]
capLetters <- unique(substr(choices,1, 1))

for(l in capLetters) {
  dropdownUS <- c(dropdownUS, list(choices[which(substr(choices,1, 1) == l)]))
}

names(dropdownUS) <- capLetters
# ================================
disclaimer <- paste0("
  Data are obtained from CSSEGISandData/COVID-19; last updated on
", file.info(fileLoc)$ctime)

fineprint <- "The idea of using exponential growth model to benchmark confirmed case trajectories was inpired by https://ourworldindata.org/coronavirus"
devInfo <- "Developed by Ming-Sen Wang (Economist/Data Scientist); current version 1.0; 03/16/2020"
# =================================
# dashboard structure 
# user interface
ui_world <- fluidPage(
  #titlePanel(paste0("Total Confirmed Cases of Chinese Virus Worldwide: Data Updated on ", file.info(fileLoc)$ctime)),
  
  sidebarPanel(
    selectInput(
      'country', "Select a Country",
      choices = dropdown,
      selected = "Taiwan*"
    ),
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
  #titlePanel(paste0("Total Confirmed Cases of Chinese Virus Worldwide: Data Updated on ", file.info(fileLoc)$ctime)),
  
  sidebarPanel(
    selectInput(
      'state', "Select a State",
      choices = dropdownUS,
      selected = "Illinois"
    ),
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
    print(Sys.time())
    
    usedData <- df %>% mutate(
      selected = ifelse(
        (`Country/Region` == input$country), "Y", 
        ifelse(sub(" .*", "",`Country/Region`) == "Double", "O", "F")
      )
    )
    usedPosData <- posData %>% dplyr::mutate(
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
      ) + scale_y_log10(labels = scales::comma, limits = c(10, max(df$cases) * 5)) + 
      scale_x_continuous(limits = c(0, max(df$days) + 5)) +
      stat_function(fun = exp_growth, args = list(k = 10), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 5), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 3), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 2), colour = "#ee9e64", alpha = 0.5) +
      xlab("Days since 10th confirmed cases") + ylab("Total Confirmed Cases")
  }, width = 800, height = 600)
  
  # US Trend Chart   
  output$lineChartUS <- renderPlot({
    #print(Sys.time())
    
    usedData <- dfUS %>% mutate(
      selected = ifelse(
        (`Province/State` == input$state), "Y", 
        ifelse(sub(" .*", "", `Province/State`) == "Double", "O", "F")
      )
    )
    
    usedPosData <- posDataUS %>% dplyr::mutate(
      selected = ifelse(
        (`Province/State` == input$state), "Y", 
        ifelse(sub(" .*", "",`Province/State`) == "Double", "O", "F")
      )
    )
    
    ggplot(usedData, aes(x = days, y = cases, group = `Province/State`)) +
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
        aes(
          x = days, y = cases, 
          label = `Province/State`, 
          colour = selected, 
          alpha = as.numeric(as.factor(selected))
          ),
        nudge_x = 0.5,
        nudge_y = log10(1)
      ) + scale_y_log10(labels = scales::comma, limits = c(10, max(dfUS$cases) * 2)) +
      scale_x_continuous(limits = c(0, max(dfUS$days) + 3), breaks = seq(0, max(dfUS$days) + 3, by = 2)) +
      stat_function(fun = exp_growth, args = list(k = 5), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 3), colour = "#ee9e64", alpha = 0.5) +
      stat_function(fun = exp_growth, args = list(k = 2), colour = "#ee9e64", alpha = 0.5) +
      xlab("Days since 10th confirmed cases") + ylab("Total Confirmed Cases")
  }, width = 800, height = 600)
}
shinyApp(ui = ui, server = server)