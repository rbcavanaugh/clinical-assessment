# next:

# fix calendar issue
# figure out effect sizes with different items
# layout
# cores for rstan
# make stan model post processing faster
# waiter
# upload individual or all files

library(shiny)
library(tibble)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(tidyr)
library(report)
library(ggdist)
library(keys)
library(DT)
library(rstanarm)

# This file holds the instructions. I've organized the text like this so that
# translating to another language is easy and finding text is easy.
source('R/english_structure.R')
files = list.files('www/items')

# These indicate errors (1) and correct responses (2)
response_keys <- c(
    "1","2"
)

# The next button
enter <- "enter"


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(
      '
      img {
      height: 100%;
    }
    '
    )
  ),
                # imports javascript for hotkeys
                useKeys(),
                keysInput("keys", response_keys),
                keysInput("enter_key", enter),
        # layout starts here
        navbarPage(title = pagetitle, id = "mainpage",
        # page 1 instructions
         tabPanel(tabtitle0,
              fluidRow(
                  column(width = 7,
                    tabsetPanel(
                      tabPanel("Background",br(),
                               intro1,br(),br(),
                               intro2,br(),br(),
                               intro3
                              ),
                      tabPanel("Instructions",br(),
                               instructions1, br(),br(),
                               instructions2, br(),br(),
                               instructions3, br(),br(),
                               instructions4, br(),br(),
                               instructions5, br(),br(),
                               instructions6, br(),br(),
                               instructions7, br(), br(),
                               note1
                               ),
                      tabPanel("FAQ", br(),
                               faq1,br(),br(),
                               faq2,br(),br(),
                               faq3,br(),br(),
                               faq4,br(),br(),
                               faq5,br(),br(),
                               faq6,br(),br(),
                               faq7
                               
                               )
                      )
                    ),
                  column(width= 1),
                  column(width = 3, align = "left", style = "background-color: #f1f1f1; border-radius: 10px;",
                         h3("Get Started"),
                         textInput("name", nameinput),
                         airDatepickerInput(
                           inputId = "date",
                           position = "top right",
                           label = dateinput,
                           multiple = FALSE,
                           value = Sys.time(),
                           timepicker = TRUE,
                           timepickerOpts = timepickerOptions(
                             dateTimeSeparator = " at ",
                             minutesStep = 10,
                             hoursStep = 1
                           )
                         ),
                         fileInput(
                           inputId = "image",
                           label = "Upload images",
                           accept = c('image/png', 'image/jpeg','image/jpg'),
                           multiple = T
                         ),
                         checkboxInput("randomize", "Randomize order?"),
                         div(align = "center",
                             actionButton("start", inputstart)
                         ), br()
                  )
              )

         ),
         # Page 2 contains the picture stimuli
         tabPanel(title = tabtitle1,
                  uiOutput("slides_tab")
         ),
        # page 3 contains the results
         tabPanel(title = tabtitle2, 
                  
                  uiOutput("results_tab")
            
         ),
        tabPanel(title = tabtitle3, 
                 
                 uiOutput("results_overtime_tab")
                 
        ),
        tabPanel(title = "Download", br(),
                 downloadButton("downloadData", "Download data for next time")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # reactiveValues is like a list where elements of the list can change based on user input
    values = reactiveValues()
    # default starting values
    values$i = 0 # this is the counter to track what picture to show (the "ith" slide)
    values$keyval = NULL # keeps track of the button press 1 or 2
    values$response = NULL # this list element holds 1-row tibbles of each response for each slide
    values$n = length(files) # the number of slides for the progress bar
    values$order = tibble(name = sample(files, 20, replace = FALSE))
    values$errorbars = NULL
    values$predline = NULL
    
    
    observeEvent(input$image,{
      if(isTruthy(input$randomize)){
      values$order = sample_n(input$image, size = nrow(input$image), replace = F)
      } else {
        values$order = input$image
      }
   })
    
    # start button. sets the i value to 1 corresponding to the first slide
    # switches to the assessment tab
    # updates the progress bar very slightly. 
    observeEvent(input$start, {
        values$i = 1
        updateNavbarPage(session, "mainpage",
                         selected = tabtitle1)
    })
    
    observeEvent(input$gotoprogress, {
      updateNavbarPage(session, "mainpage",
                       selected = "Progress")
    })
    
    observeEvent(input$gotodownload, {
      updateNavbarPage(session, "mainpage",
                       selected = "Download")
    })
    
    # tracks the inputs
    observeEvent(input$keys, {
        values$key_val = input$keys
    })
    
    
    # observe event will take an action if an input changes. here the next button or the enter key
    observeEvent(input$enter_key, {
        # if not an instructions slide, require a key input response
        if(is.null(values$key_val)){
            showNotification("Enter a score", type = "error")
        # as long as there's a response or it's an insturction slide...
        } else {
            
            # if an instruction slide, put an element in values$response for the instructions slide
            if(values$i < length(values$order$name)){
              values$response[[values$i]] = tibble(
                slide_num = values$i,
                slide_id = values$order$name[[values$i]],
                response = ifelse(values$key_val == "1", "0",
                                  ifelse(values$key_val == "2", "1",
                                         "NR")
                ),
                name = input$name,
                date = lubridate::date(input$date),
                time = paste(hour(input$date), minute(input$date), sep = ":")
              )
              #print(dplyr::bind_rows(values$response))
              values$i = values$i+1
              
                   
                # if its the last slide, then go to the results page automatically .
                } else {
                    
                  values$response[[values$i]] = tibble(
                    slide_num = values$i,
                    slide_id = values$order$name[[values$i]],
                    response = ifelse(values$key_val == "1", "0",
                                      ifelse(values$key_val == "2", "1",
                                             "NR")
                    ),
                    name = input$name,
                    date = lubridate::date(input$date),
                    time = paste(hour(input$date), minute(input$date), sep = ":")
                  )
                  #print(dplyr::bind_rows(values$response))
                    updateNavbarPage(session, "mainpage",
                                     selected = tabtitle2)
                }
            # resets the key value AFTER saving the data. 
            values$key_val = NULL
            #print(input$date)
        }
        # don't run this on start up. 
    })
    
    # Probably the back button will be disabled in production. 
    # note right now, if you hit the back button, you will have 
    # to re-enter a response. 
    observeEvent(input$back, {
        values$i = values$i-1
        if(values$i < 1){
            values$i = 1
            updateNavbarPage(session, "mainpage",
                             selected = tabtitle0)
        } else {
        }
    })
    
    ### upload data
    observeEvent(input$showmodal, {
      showModal(modalDialog(
        size = "l",
        title = "Upload Data from last time",
        footer = modalButton("Done"),
        fluidRow(
          column(width = 5,
          fileInput("file1", "",
                    multiple = F,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          
          # Input: Checkbox if file has header ----
          checkboxInput("header", "Header", TRUE),
          
          # Input: Select separator ----
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          
          # Input: Select quotes ----
          radioButtons("quote", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"')
            ),
          column(width = 7,
          tableOutput("contents")
          )
        )
      ))
    })
    
    
    observeEvent(input$calculate,{
      req(input$file1) # change to validate
      df = results_over_time() %>%
        mutate(response = as.numeric(response),
               time = dense_rank((date)),
               target = gsub(".jpeg", "", slide_id)) 
      
      model = stan_glmer(response ~ time + (time|target), data = df, family = "binomial")
     values$model <- model
     error_data <- tidybayes::add_fitted_draws(model,
                                               newdata = df %>% 
                                                 select(time, date) %>%
                                                 distinct(),
                                               re_formula = NA) %>%
       as_tibble() %>%
       group_by(time, date) %>%
       summarize(point_interval(.value, .width = .9))%>%
       rename(accuracy = y)
     
     values$errorbars <- geom_ribbon(data = error_data,
                                     aes(ymin=ymin,ymax=ymax),
                                     alpha = .2, fill = "red")
     values$predline <- geom_line(data = error_data, aes(x = date, y = accuracy), color = "darkred")
     
     effect_size <- tidybayes::add_fitted_draws(model,
                                                newdata = df %>% 
                                                  select(time, date) %>%
                                                  filter(time == min(time) | time == max(time)) %>%
                                                  distinct(),
                                                re_formula = NA) %>%
       as_tibble() %>%
       filter(time == min(time) | time == max(time)) %>%
       select(date, value = .value, draw = .draw) %>%
       mutate(num_items = length(unique(model$data$target)),
              date = ifelse(date == min(date), "entry", "exit")) %>%
       pivot_wider(names_from = date, values_from = value) %>%
       mutate(change = exit*num_items - entry*num_items) %>%
       summarize(point_interval(change, .width = .9)) %>%
       mutate_if(is.numeric, round, 1)
     
     
     values$model_summary <- paste0(as.character(report_parameters(model, width = .9)[2]), ". The data suggest that the the odds of a correct response become ", round(exp(fixef(model)[2]),1), " times more likely with each treatment session.", " In terms of the number of items improved, the data suggest that the client gained approximately ", effect_size$y, "items with a 90% probability that this effect size ranges between ", effect_size$ymin, " and ", effect_size$ymax, ".")
     
     values$effect_size_draws <- tidybayes::add_fitted_draws(model,
                                                newdata = df %>% 
                                                  select(time, date) %>%
                                                  filter(time == min(time) | time == max(time)) %>%
                                                  distinct(),
                                                re_formula = NA) %>%
       as_tibble()  %>%
       select(date, value = .value, draw = .draw) %>%
       mutate(num_items = length(unique(model$data$target)),
              date = ifelse(date == min(date), "entry", "exit")) %>%
       pivot_wider(names_from = date, values_from = value) %>%
       mutate(change = exit*num_items - entry*num_items, .keep = "none")
 
    })
    
    observeEvent(input$results_oneday,{
      req(results_data_long())
      df = results_data_long() %>%
        mutate(response = as.numeric(response),
               target = slide_id) 
      
      model2 = stan_glmer(response ~ 1 + (1|target), data = df, family = "binomial")
      values$model_oneday <- tidybayes::add_fitted_draws(model2,
                                                         newdata = df %>% select(date) %>% distinct(),
                                                         re_formula = NA) %>%
        as_tibble() %>%
        select(value = .value)
      
    })
    
    
    output$accuracy_plot <- renderPlot({
      req(values$model_oneday)
      values$model_oneday %>%
        ggplot(aes(x = value)) +
        stat_halfeye(aes(fill = stat(
          cut_cdf_qi(cdf,.width= c(.02,.66,.9, 1))
        ))) +
        theme_minimal(base_size = 14) +
        ylab("Density") +
        xlab("Estimated accuracy") +
        scale_fill_manual(values = c("black", "royalblue3", "skyblue","gray85")) +
        theme(legend.position = "none") +
        ggtitle(paste0("Estimated accuracy: ", round(median(values$model_oneday$value)*100, 1), "%"))
    })
    
    
    
    
    output$model <- renderUI({
      req(values$model_summary)
      values$model_summary
    })
    
    output$effect_size_plot <- renderPlot({
      req(values$effect_size_draws)
      values$effect_size_draws %>%
        ggplot(aes(x = change)) +
        stat_halfeye(aes(fill = stat(
          cut_cdf_qi(cdf,.width= c(.02,.66,.9, 1))
        ))) +
        theme_minimal(base_size = 14) +
        ylab("Density") +
        xlab("Number of items gained") +
        scale_fill_manual(values = c("black", "royalblue3", "skyblue","gray85")) +
        theme(legend.position = "none")
    })
    
    
#### Outputs ##################################################################
    
    
    
    # this shows the slide for the i'th value
    output$slide <- renderImage({
      if(isTruthy(input$image)){
        item = values$order$name[[values$i]]
        tmp = values$order$datapath[values$i]
        #print(tmp)
        list(
          src = tmp
        ) 
        } else {
        item = values$order$name[[values$i]]
        tmp = paste0("www/items/", item)
        #print(tmp)
        list(
          src = tmp
        ) 
      }
 
    }, deleteFile = F)
    
  # holds the item-level responses. 
  results_data_long <- reactive({
     dplyr::bind_rows(values$response)
  })
  
  # holds the mean accuracy
  results_data_summary <- reactive({
      dplyr::bind_rows(values$response) %>%
          mutate(response = as.numeric(response)) %>%
          summarize(accuracy = mean(response)) %>%
          pull(accuracy)
  })
  
  read_data_in <- reactive({
      req(input$file1)
      df = read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      #print(input$file1$datapath)
      df
  })
  
  results_over_time <- reactive({
    req(input$file1)
    if(isTruthy(values$response)){
    current <- bind_rows(values$response) %>%
      mutate(response = as.numeric(response))
    old <- read_data_in() %>% select(slide_num:time) %>%
      mutate(date = date(date))
    dplyr::bind_rows(current, old)
    } else {
      read_data_in() %>% select(slide_num:time) %>%
        mutate(date = date(date))
    }
  })
  
  # outputs a table of the item level responses
  output$results_long <- renderDT({
      results_data_long() %>% select(1:3)
  }, rownames = F)
  
  #  outputs a summary sentence
  output$results_summary <- renderUI({
      paste0("The total accuracy for this test was ", round(results_data_summary()*100, 1), "%.")
  })
  
  output$contents <- renderTable({
    req(input$file1)
    head(read_data_in())
  })
  
  ### results plot
  
  output$results_plot <- renderPlot({
    results_over_time() %>%
      group_by(date) %>%
      summarize(accuracy = mean(response)) %>%
      ggplot(aes(x = date, y = accuracy)) +
      geom_point() + 
      geom_line() +
      ylim(0,1) +
      theme_minimal(base_size = 14) +
      values$errorbars +
      values$predline
  })
  
  
  # download handler
  
  data_to_download <- reactive({
    if(isTruthy(input$file1)){
      results_over_time()
    } else {
      bind_rows(values$response)
    }
  } )

  output$downloadData <- downloadHandler(
    filename = function() {
      paste(gsub(" ", "-", input$name), as.character(lubridate::date(input$date)), "cpt.csv", sep = "_")
    },
    content = function(file) {
        write.csv(data_to_download(), file, row.names = FALSE)
    }
  )
  
 ##### tab UI ###############################################################
  
  output$slides_tab <- renderUI({
      if(values$i == 0){
          "To start the test, hit 'Start Assessment' on the home page"
      } else {
              column(width = 12, align = "center",
                     fluidRow(imageOutput("slide")),
                     # note the progress bar and next/back buttons are not in the slide image. They
                     # are their  own static area below the slides. 
                     fluidRow(
                         column(width = 12, align = "center",
                                hr(),
                             actionButton("back", backbutton), br(), br(),
                             # actionButton("nxt", nextbutton), br(), br(),
                             div(style = "width:50%;",
                             progressBar(id = "progress_bar", value = ((values$i-1)/length(values$order$name))*100,
                                         display_pct = F, size = "s")
                             )
                         )
                     )
              )
      }
      
  })
  
  output$results_tab <- renderUI({
      if(values$i < 2){
          "Hmmm....No results to show yet. "
      } else {
          div(
            fluidRow(
                    uiOutput("results_summary")
            ),br(),
          fluidRow(
          column(width = 5,
          DTOutput("results_long"), br(),
          actionButton("results_oneday", "Estimate Accuracy"),
          actionButton("gotoprogress", "Go to Effect Sizes page"),
          ),
          column(width = 6,
                 plotOutput("accuracy_plot")
          )
          )
          )
      }
      
  })
  
  
  output$results_overtime_tab <- renderUI({
    # if(values$i < 2){
    #   "Hmmm....No results to show yet. "
    # } else {
      div(
        fluidRow(
          column(width = 6,
                 actionButton("showmodal", "Upload Data from last time"),
                 actionButton("calculate", "Calculate Results"),
                 actionButton("gotodownload", "Go to download page"),
                 br(), br(),
                 plotOutput("results_plot")
          ),
          column(width = 6,
                 br(),
                 uiOutput("model"),
                 br(),
                 plotOutput("effect_size_plot")
          )
        )

      )
   # }
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

