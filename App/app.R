library(shiny)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinydashboardPlus)
library(shinyWidgets)

indu_list <- c("Agriculture, Extractive, or Construction", "Manufacturing, Logistics",
               "Health, Education, Government, or Social and Consumer Services",
               "Wholesale, Retail", "Information, Communications, or Technology",
               "Finance, Real Estate, or Business Services")

sidebar <- dashboardSidebar(
  sidebarMenu(id = 'sidebarmenu',
    menuItem("General information", tabName = "info", icon = icon("info"), badgeColor = "green"),
      menuSubItem("Action Crisis", tabName = "Action"),
      menuSubItem("Exploitation", tabName = "Exploitation"),
      menuSubItem("Exploration", tabName = "Exploration"),
      menuSubItem("Business Errors", tabName = "Errors"),
      menuSubItem("Thriving", tabName = "Thriving"),
    menuItem(numericInput("Participant", label = "Please enter your Participant ID", value =F)),
    menuItem(actionButton("do", "See results")),
    menuItem("Contact Information", tabName = "admin", icon = icon("paper-plane", lib = "font-awesome"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "info",
    fluidRow(
      box(status = "primary",
        height = "70px",
          h5(htmlOutput("sample_N")),
          width = 6, background = NULL),
          ),
            
    fluidRow(
      tabBox(
        title = "Demographic information",
        height = "500px",
        tabPanel("Age distribution",
                 plotOutput("age_dist")),
        tabPanel("Gender distribution",
                 plotlyOutput("sex_dist")),
        tabPanel("Native language",
                 plotlyOutput("lang_dist")),
        tabPanel("Highest education level",
                 plotlyOutput("edu_dist")),
        tabPanel("Current occupation", 
                 plotlyOutput("octyp_dist")),
        tabPanel("Past occupation", 
                 plotlyOutput("octyp_past_dist"))
        ),
      
      tabBox(
        title = "Business information",
        height = "500px",
        tabPanel("Industry",
                 plotlyOutput("dist_indu")),
        tabPanel("Ownership status",
                 plotlyOutput("dist_owner")),
        tabPanel("Shared ownership",
                 plotlyOutput("dist_shared_owner")),
        tabPanel("Time since business foundation",
                 plotOutput("timebuiss"))
      )
    )
    ),
    
    
    # Action Crisis
    tabItem(tabName = "Action",
            fluidRow(
              box(width = 7, status = "primary",
                title = "Action crisis over time", solidHeader = TRUE,
                collapsible = TRUE,
                "How much do you agree that you experience an action crisis?",
                plotOutput("accri_plot", height = 450),
              ),
                box(width = 5, status = "primary",
                  flipBox(
                    align = "left",
                     id = 1,
                     main_img = "accri.jpg",
                     front_btn_text = "More",
                     front_title = "Action Crisis",
                     back_title = "",
                     h4(htmlOutput("accri_mean_person_sample_text")),
                    p(htmlOutput("accri_eval")),
                    back_content = p(htmlOutput("accri_eval2"))
                )
                )
                ),
              
            fluidRow(
                box(
                  title = "Industry", status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "industry_accri", "Choose an industry:",
                              choices = indu_list), width = 7
              ),

                box(
                  h4(htmlOutput("accri_mean_indu_text")),
                  width = 5, status = "warning"
                    )
                ),
            
            fluidRow(
                box(
                  title = "Experience", status = "warning", solidHeader = TRUE,
                  selectInput(inputId = "time_accri", "Choose time in business:",
                              choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                          "over 6 years")
                  ), width = 7
                ),
       
                
                box(
                  h4(htmlOutput("accri_mean_time_text")),
                  width = 5, status = "warning"
                )
              )
  ),
            
    # Exploitation
  tabItem(tabName = "Exploitation",
          fluidRow(
            box(width = 7, status = "primary",
                title = "Exploitation over time", solidHeader = TRUE,
                collapsible = TRUE,
                "To what extent have you engaged in exploitative activities?",
                plotOutput("exploi_plot", height = 450),
            ),
            box(width = 5, status = "primary",
                flipBox(
                  align = "left",
                  id = 2,
                  main_img = "exploi.jpg",
                  front_btn_text = "More",
                  front_title = "Exploitation",
                  back_title = "",
                  h4(htmlOutput("exploi_mean_person_sample_text")),
                  p(htmlOutput("exploi_eval")),
                  back_content = p(htmlOutput("exploi_eval2"))
                )
            )
          ),
          
          fluidRow(
            box(
              title = "Industry", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "industry_exploi", "Choose an industry:",
                          choices = indu_list),
              width = 7
            ),
            
            box(
              h4(htmlOutput("exploi_mean_indu_text")),
              status = "warning",
              width = 5,
              height = 70
            )
          ),
          fluidRow(
            box(
              title = "Experience", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "time_exploi", "Choose time in business:",
                          choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                      "over 6 years")),
              width = 7
            ),
            
            box(
              h4(htmlOutput("exploi_mean_time_text")),
              status = "warning",
              width = 5, 
              height = 70
            )
          )
  ),
  
  # Exploration
  tabItem(tabName = "Exploration",
          fluidRow(
            box(width = 7, status = "primary",
                title = "Exploration over time", solidHeader = TRUE,
                collapsible = TRUE,
                "To what extent have you engaged in exploratory activities?",
                plotOutput("explor_plot", height = 450),
            ),
            box(width = 5, status = "primary",
                flipBox(
                  align = "left",
                  id = 3,
                  main_img = "explor.jpg",
                  front_btn_text = "More",
                  front_title = "Exploration",
                  back_title = "",
                  h4(htmlOutput("explor_mean_person_sample_text")),
                  p(htmlOutput("explor_eval")),
                  back_content = p(htmlOutput("explor_eval2"))
                )
            )
          ),
          
          fluidRow(
            box(
              title = "Industry", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "industry_explor", "Choose an industry:",
                          choices = indu_list
              ), width = 7
            ),
            
            box(
              h4(htmlOutput("explor_mean_indu_text")),
              width = 5, status = "warning"
            )
          ),
          fluidRow(
            box(
              title = "Experience", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "time_explor", "Choose time in business:",
                          choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                      "over 6 years")
              ), width = 7
            ),
            
            
            box(
              h4(htmlOutput("explor_mean_time_text")),
              width = 5, status = "warning"
            )
          )
  ),
    # Errors
  tabItem(tabName = "Errors",
          fluidRow(
            box(width = 7, status = "primary",
                title = "Number of errors over time", solidHeader = TRUE,
                collapsible = TRUE,
                "Number of errors",
                plotOutput("errorcount_plot", height = 450),
            ),
            box(width = 5, status = "primary",
                flipBox(
                  align = "left",
                  id = 4,
                  main_img = "error.jpg",
                  front_btn_text = "More",
                  front_title = "Business Errors",
                  back_title = "",
                  h4(htmlOutput("errorcount_mean_person_sample_text")),
                  h4(htmlOutput("errser_mean_person_sample_text")),
                  back_content = p(htmlOutput("error_eval"))
                )
            )
          ),
          
          fluidRow(
            box(
              title = "Industry", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "industry_errorcount", "Choose an industry:",
                          choices = indu_list
              ), width = 7
            ),
            
            box(
              h4(htmlOutput("errorcount_mean_indu_text")),
              width = 5, status = "warning"
            )
          ),
          fluidRow(
            box(
              title = "Experience", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "time_errorcount", "Choose time in business:",
                          choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                      "over 6 years")
              ), width = 7
            ),
            
            
            box(
              h4(htmlOutput("errorcount_mean_time_text")),
              width = 5, status = "warning"
            )
          ),
          fluidRow(
            box(
              title = "Mean severity of errors over time", status = "primary", solidHeader = TRUE,
              collapsible = TRUE,
              "Error severity",
              plotOutput("errser_plot", height = 450), 
              width = 7
            )
          ),
          
          fluidRow(
            box(
              title = "Industry", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "industry_errser", "Choose an industry:",
                          choices = indu_list
              ), width = 7
            ),
            
            box(
              h4(htmlOutput("errser_mean_indu_text")),
              width = 5, status = "warning"
            )
          ),
          fluidRow(
            box(
              title = "Experience", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "time_errser", "Choose time in business:",
                          choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                      "over 6 years")
              ), width = 7
            ),
            
            box(
              h4(htmlOutput("errser_mean_time_text")),
              width = 5, status = "warning"
            )
          )
  ),
  
  # Thriving
  tabItem(tabName = "Thriving",
          fluidRow(
            box(width = 7, status = "primary",
                title = "Thriving over time", solidHeader = TRUE,
                collapsible = TRUE,
                "How much do you agree that you are thriving at work?",
                plotOutput("thriv_plot", height = 450),
            ),
            box(width = 5, status = "primary",
                flipBox(
                  align = "left",
                  id = 5,
                  main_img = "thriv.jpg",
                  front_btn_text = "More",
                  front_title = "Thriving",
                  back_title = "",
                  h4(htmlOutput("thriv_mean_person_sample_text")),
                  p(htmlOutput("thriv_eval")),
                  back_content = p(htmlOutput("thriv_eval2"))
                )
            )
          ),
          
          fluidRow(
            box(
              title = "Industry", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "industry_thriv", "Choose an industry:",
                          choices = indu_list
              ),
              width = 7
            ),
            
            box(
              h4(htmlOutput("thriv_mean_indu_text")),
              width = 5,
              status = "warning"
            )
          ),
          fluidRow(
            box(
              title = "Experience", status = "warning", solidHeader = TRUE,
              selectInput(inputId = "time_thriv", "Choose time in business:",
                          choices = c("0 to 1.5 year", "1.5 to 3.5 years", "3.5 to 6 years",
                                      "over 6 years")
              ), width = 7
            ),
            
            
            box(
              h4(htmlOutput("thriv_mean_time_text")),
              width = 5,
              status = "warning"
            )
          )
  ),
  tabItem(tabName = "admin",
          widgetUserBox(
            title = "Anne-Kathrin Kleine",
            subtitle = "Researcher - Organizational Psychology - Groningen, The Netherlands",
            type = NULL,
            src = ("me.jpeg"),
            background = TRUE,
            boxToolSize = "md",
            backgroundUrl = ("hagebutte.JPG"),
            footer_padding = F,
            actionButton(inputId = "email", label = "Email", 
                         icon = icon("envelope", lib = "font-awesome"),
                         onclick = "window.open('mailto:a.k.kleine@rug.nl')"),
            actionButton(inputId = "linkedIn", label = "LinkedIn", 
                         icon = icon("linkedin", lib = "font-awesome"),
                         onclick= "window.open('https://www.linkedin.com/in/anne-kathrin-kleine/', '_blank')"),
            width = 12, height = 180
          )
  )
)
)



ui <- dashboardPage(
  dashboardHeader(title = "Feedback Reports"),
  sidebar, 
  body
)

server <- function(input, output, session) {
  long_pan<-read.csv("Data/long_pan.csv")
    
  # Action Crisis
  observeEvent( input$do, {
  output$accri_plot <- renderPlot({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    long_pan %>% 
      mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
      ggplot(aes(x = wave, y = accri)) +
      geom_line(aes(linetype = highlight_flag, color = "red")) +
      scale_linetype_manual(values=c("solid", "blank")) +
      stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
      theme(legend.position = "bottom") +
      labs(title = "",
           x = "Time point in the survey",
           y = "") +
      guides(linetype = FALSE) + 
      scale_y_continuous(breaks = 1:7, labels = c("Strongly \ndisagree", "Disagree", "Somewhat \ndisagree", "Neither agree \nnor disagree", "Somewhat \nagree",
                       "Agree", "Strongly agree")) +
      scale_colour_manual(name = '', 
                          values =c('red'='red','blue'='blue'), labels = c('Sample mean Action Crisis', 'Your Action Crisis'))
  })
  }
  )
  
  observeEvent( input$do, {
  output$accri_mean_person_sample_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    accri_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(accri_av = mean(accri_av)) %>% ungroup(.)
    accri_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(accri_per_indu = mean(accri, na.rm = T)) %>% ungroup(.) 
    paste("Your mean Action Crisis is: \n<B>", round(accri_avi$accri_av[accri_avi$id == input$Participant], 1), "<br/><br/>",
           "</B>The mean Action Crisis for the whole sample is: <B>",
           round(mean(long_pan$accri_av, na.rm =T),1),
           sep="")
  })
  }
  )
  
  output$accri_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    accri_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(accri_av = mean(accri_av)) %>% ungroup(.)
    accri_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(accri_per_indu = mean(accri, na.rm = T)) %>% ungroup(.) 
    paste("The mean Action Crisis for this industry is: <B>",
          round(mean(accri_per$accri_per_indu[which(accri_per$cat_indu == input$industry_accri)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$accri_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    accri_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(accri_av = mean(accri_av)) %>% ungroup(.)
    accri_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(accri_per_time = mean(accri, na.rm = T)) %>% ungroup(.) 
    paste("The mean Action Crisis for entrepreneurs at this stage is: \n<B>", 
          round(mean(accri_per$accri_per_time[which(accri_per$cat_time == input$time_accri)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  # Exploitation
  observeEvent( input$do, {
    output$exploi_plot <- renderPlot({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      long_pan %>% 
        mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
        ggplot(aes(x = wave, y = exploi)) +
        geom_line(aes(linetype = highlight_flag, color = "red")) +
        scale_linetype_manual(values=c("solid", "blank")) +
        stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
        theme(legend.position = "bottom") +
        labs(title = "",
             x = "Time point in the survey",
             y = "Exploitation") +
        guides(linetype = FALSE) + 
        scale_y_continuous(breaks = 1:7, labels = c("To an extremely \n small extent", "To a very \nsmall extent", "To a \nsmall extent", "To a \nmoderate extent", 
                                                    "To a \nlarge extent",
                                                    "To a very \nlarge extent", "To an extremely \nlarge extent")) +
        scale_colour_manual(name = '', 
                            values =c('red'='red','blue'='blue'), labels = c('Sample mean Exploitation', 'Your Exploitation'))
    })
  }
  )
  
  observeEvent( input$do, {
    output$exploi_mean_person_sample_text <- renderText({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      exploi_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(exploi_av = mean(exploi_av)) %>% ungroup(.)
      exploi_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(exploi_per_indu = mean(exploi, na.rm = T)) %>% ungroup(.) 
      paste("Your mean Exploitation is: \n<B>", round(exploi_avi$exploi_av[exploi_avi$id == input$Participant], 1), "<br/><br/>",
            "</B>The mean Exploitation for the whole sample is: <B>",
            round(mean(long_pan$exploi_av, na.rm =T),1),
            sep="")
    })
  }
  )
  
  output$exploi_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    exploi_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(exploi_av = mean(exploi_av)) %>% ungroup(.)
    exploi_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(exploi_per_indu = mean(exploi, na.rm = T)) %>% ungroup(.) 
    paste("The mean Exploitation for this industry is: <B>",
          round(mean(exploi_per$exploi_per_indu[which(exploi_per$cat_indu == input$industry_exploi)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$exploi_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    exploi_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(exploi_av = mean(exploi_av)) %>% ungroup(.)
    exploi_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(exploi_per_time = mean(exploi, na.rm = T)) %>% ungroup(.) 
    paste("The mean Exploitation for entrepreneurs at this stage is: \n<B>", 
          round(mean(exploi_per$exploi_per_time[which(exploi_per$cat_time == input$time_exploi)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  # Exploration
  observeEvent( input$do, {
    output$explor_plot <- renderPlot({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      long_pan %>% 
        mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
        ggplot(aes(x = wave, y = explor)) +
        geom_line(aes(linetype = highlight_flag, color = "red")) +
        scale_linetype_manual(values=c("solid", "blank")) +
        stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
        theme(legend.position = "bottom") +
        labs(title = "",
             x = "Time point in the survey",
             y = "Exploration") +
        guides(linetype = FALSE) + 
        scale_y_continuous(breaks = 1:7, labels = c("To an extremely \n small extent", "To a very \nsmall extent", "To a \nsmall extent", "To a \nmoderate extent", 
                                                    "To a \nlarge extent",
                                                    "To a very \nlarge extent", "To an extremely \nlarge extent")) +
        scale_colour_manual(name = '', 
                            values =c('red'='red','blue'='blue'), labels = c('Sample mean Exploration', 'Your Exploration'))
    })
  }
  )
  
  observeEvent( input$do, {
    output$explor_mean_person_sample_text <- renderText({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      explor_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(explor_av = mean(explor_av)) %>% ungroup(.)
      explor_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(explor_per_indu = mean(explor, na.rm = T)) %>% ungroup(.) 
      paste("Your mean Exploration is: \n<B>", round(explor_avi$explor_av[explor_avi$id == input$Participant], 1), "<br/><br/>",
            "</B>The mean Exploration for the whole sample is: <B>",
            round(mean(long_pan$explor_av, na.rm =T),1),
            sep="")
    })
  }
  )
  
  output$explor_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    explor_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(explor_av = mean(explor_av)) %>% ungroup(.)
    explor_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(explor_per_indu = mean(explor, na.rm = T)) %>% ungroup(.) 
    paste("The mean Exploration for this industry is: <B>",
          round(mean(explor_per$explor_per_indu[which(explor_per$cat_indu == input$industry_explor)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$explor_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    explor_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(explor_av = mean(explor_av)) %>% ungroup(.)
    explor_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(explor_per_time = mean(explor, na.rm = T)) %>% ungroup(.) 
    paste("The mean Exploration for entrepreneurs at this stage is: \n<B>", 
          round(mean(explor_per$explor_per_time[which(explor_per$cat_time == input$time_explor)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  # Number of errors
  observeEvent( input$do, {
    output$errorcount_plot <- renderPlot({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      long_pan %>% 
        mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
        ggplot(aes(x = wave, y = errorcount)) +
        geom_line(aes(linetype = highlight_flag, color = "red")) +
        scale_linetype_manual(values=c("solid", "blank")) +
        stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
        theme(legend.position = "bottom") +
        labs(title = "",
             x = "Time point in the survey",
             y = "Number of errors over the course of the past two weeks") +
        guides(linetype = FALSE) + 
        scale_colour_manual(name = '', 
                            values =c('red'='red','blue'='blue'), labels = c('Sample mean Number of errors', 'Your Number of errors'))
    })
  }
  )
  
  observeEvent( input$do, {
    output$errorcount_mean_person_sample_text <- renderText({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      errorcount_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errorcount_av = mean(errorcount_av)) %>% ungroup(.)
      errorcount_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(errorcount_per_indu = mean(errorcount, na.rm = T)) %>% ungroup(.) 
      paste("<br/><br/>Your mean Number of errors is: \n<B>", round(errorcount_avi$errorcount_av[errorcount_avi$id == input$Participant], 1), "<br/><br/>",
            "</B>The mean Number of errors of the whole sample is: <B>",
            round(mean(long_pan$errorcount_av, na.rm =T),1),"<br/><br/><br/><br/>",
            sep="")
    })
  }
  )
  output$errorcount_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    errorcount_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errorcount_av = mean(errorcount_av)) %>% ungroup(.)
    errorcount_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(errorcount_per_indu = mean(errorcount, na.rm = T)) %>% ungroup(.) 
    paste("The mean Number of errors of this industry is: <B>",
          round(mean(errorcount_per$errorcount_per_indu[which(errorcount_per$cat_indu == input$industry_errorcount)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$errorcount_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    errorcount_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errorcount_av = mean(errorcount_av)) %>% ungroup(.)
    errorcount_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(errorcount_per_time = mean(errorcount, na.rm = T)) %>% ungroup(.) 
    paste("The mean Number of errors of entrepreneurs at this stage is: \n<B>", 
          round(mean(errorcount_per$errorcount_per_time[which(errorcount_per$cat_time == input$time_errorcount)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  # Severity of errors
  names(long_pan)
  observeEvent( input$do, {
    output$errser_plot <- renderPlot({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      long_pan %>% 
        mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
        ggplot(aes(x = wave, y = errser_mean)) +
        geom_line(aes(linetype = highlight_flag, color = "red")) +
        scale_linetype_manual(values=c("solid", "blank")) +
        stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
        theme(legend.position = "bottom") +
        labs(title = "",
             x = "Time point in the survey",
             y = "Error severity") +
        guides(linetype = FALSE) + 
        scale_y_continuous(breaks = 1:5, labels = c("Not at all \nserious", "Somewhat \nserious", "Moderately \nserious", "Very \nserious", 
                                                    "Extremely \nserious")) +
        scale_colour_manual(name = '', 
                            values =c('red'='red','blue'='blue'), labels = c('Sample mean Error severity', 'Your Error severity'))
    })
  }
  )
  
  observeEvent( input$do, {
    output$errser_mean_person_sample_text <- renderText({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      errser_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errser_av = mean(errser_mean)) %>% ungroup(.)
      errser_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(errser_per_indu = mean(errser_mean, na.rm = T)) %>% ungroup(.) 
      paste("Your mean Error severity is: \n<B>", round(errser_avi$errser_av[errser_avi$id == input$Participant], 1), "<br/><br/>",
            "</B>The mean Error severity of the whole sample is: <B>",
            round(mean(long_pan$errser_av, na.rm =T),1),
            sep="")
    })
  }
  )
  output$errser_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    errser_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errser_av = mean(errser_mean)) %>% ungroup(.)
    errser_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(errser_per_indu = mean(errser_mean, na.rm = T)) %>% ungroup(.) 
    paste("The mean Error severity for this industry is: <B>",
          round(mean(errser_per$errser_per_indu[which(errser_per$cat_indu == input$industry_errser)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$errser_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    errser_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(errser_av = mean(errser_mean)) %>% ungroup(.)
    errser_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(errser_per_time = mean(errser_mean, na.rm = T)) %>% ungroup(.) 
    paste("The mean Error severity for entrepreneurs at this stage is: \n<B>", 
          round(mean(errser_per$errser_per_time[which(errser_per$cat_time == input$time_errser)], na.rm = T),1),
          "</B>",
          sep="")
  })
  names(long_pan)
  
  # Thriving
  observeEvent( input$do, {
    output$thriv_plot <- renderPlot({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      long_pan %>% 
        mutate(highlight_flag = ifelse(id == input$Participant, F, T)) %>%
        ggplot(aes(x = wave, y = thriv)) +
        geom_line(aes(linetype = highlight_flag, color = "red")) +
        scale_linetype_manual(values=c("solid", "blank")) +
        stat_summary(fun.y=mean, aes(color="blue"), geom="line",group=1) +
        theme(legend.position = "bottom") +
        labs(title = "",
             x = "Time point in the survey",
             y = "Thriving") +
        guides(linetype = FALSE) + 
        scale_y_continuous(breaks = 1:7, labels = c("Strongly \ndisagree", "Disagree", "Somewhat \ndisagree", "Neither agree \nnor disagree", "Somewhat \nagree",
                                                    "Agree", "Strongly agree")) +
        scale_colour_manual(name = '', 
                            values =c('red'='red','blue'='blue'), labels = c('Sample mean Thriving', 'Your Thriving'))
    })
  }
  )
  
  observeEvent( input$do, {
    output$thriv_mean_person_sample_text <- renderText({
      validate(
        need(input$Participant != "", "Please enter your ParticipantID")
      )
      thriv_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(thriv_av = mean(thriv_av)) %>% ungroup(.)
      thriv_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(thriv_per_indu = mean(thriv, na.rm = T)) %>% ungroup(.) 
      paste("Your mean Thriving is: \n<B>", round(thriv_avi$thriv_av[thriv_avi$id == input$Participant], 1), "<br/><br/>",
            "</B>The mean Thriving for the whole sample is: <B>",
            round(mean(long_pan$thriv_av, na.rm =T),1),
            sep="")
    })
  }
  )
  
  output$thriv_mean_indu_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    thriv_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(thriv_av = mean(thriv_av)) %>% ungroup(.)
    thriv_per <- long_pan %>% group_by(cat_indu) %>% dplyr::summarise(thriv_per_indu = mean(thriv, na.rm = T)) %>% ungroup(.) 
    paste("The mean Thriving for this industry is: <B>",
          round(mean(thriv_per$thriv_per_indu[which(thriv_per$cat_indu == input$industry_thriv)], na.rm = T),1),
          "</B>",
          sep="")
  })
  
  output$thriv_mean_time_text <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    thriv_avi <- long_pan %>% group_by(id) %>% dplyr::summarise(thriv_av = mean(thriv_av)) %>% ungroup(.)
    thriv_per <- long_pan %>% group_by(cat_time) %>% dplyr::summarise(thriv_per_time = mean(thriv, na.rm = T)) %>% ungroup(.) 
    paste("The mean Thriving for entrepreneurs at this stage is: \n<B>", 
          round(mean(thriv_per$thriv_per_time[which(thriv_per$cat_time == input$time_thriv)], na.rm = T),1),
          "</B>",
          sep="")
  })
  output$sample_N <- renderText({
    paste("Sample size: <em>N = ",
    length(unique(long_pan$id)), "</em>" ,"<br>",
    "Percentage missing data: ", 100*(round(mean(is.na(long_pan)),4)), " %",
    sep = "")

    
  })
  output$age_dist <- renderPlot({
    long_pan$age_4[long_pan$age_4 < 18] = NA
    hist(long_pan$age_4, main = "", xlab = "Age", ylab = "count", xlim=c(18,50), breaks = "Sturges")
  }, height = 400, width = 600)
  
  
  output$sex_dist <- renderPlotly({
    lbls_gender <- c("Male", "Female")
    count_sex <- long_pan %>% group_by(sex, id) %>% tally() %>% group_by(sex) %>% tally() %>%
      .[2] %>% .[1:2,] %>% unlist(.) 
    df_sex <- as_tibble(cbind(count_sex, lbls_gender))
    plot_ly(df_sex, labels = ~lbls_gender, values = ~count_sex, type = 'pie') %>% 
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$lang_dist <- renderPlotly({
  lbls_lang <- c("Hindi", "English", "Other", "Marathi", "Telugu", "Odia")
  count_lang <- long_pan %>% group_by(lang, id) %>% tally() %>% group_by(lang) %>% tally() %>%
    .[2] %>% .[1:6,] %>% unlist(.) 
  df_lang <- as_tibble(cbind(count_lang, lbls_lang))
  plot_ly(df_lang, labels = ~lbls_lang, values = ~count_lang, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$edu_dist <- renderPlotly({
  lbls_edu <- c("Secondary School", "Higher Secondary School", "University Degree", "Doctorate Degree", "Other")
  count_edu <- long_pan %>% group_by(edu, id) %>% tally() %>% group_by(edu) %>% tally() %>%
    .[2] %>% .[1:5,] %>% unlist(.) 
  df_edu <- as_tibble(cbind(count_edu, lbls_edu))
  plot_ly(df_edu, labels = ~lbls_edu, values = ~count_edu, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$octyp_dist <- renderPlotly({
  lbls_octyp <- c("Self-employed", "Self-employed (+ employed for < 35h)", "Self-employed (+ employed for > 35h)")
  count_octyp <- long_pan %>% group_by(octyp, id) %>% tally() %>% group_by(octyp) %>% tally() %>%
    .[2] %>% unlist(.) 
  df_octyp <- as_tibble(cbind(count_octyp, lbls_octyp))
  plot_ly(df_octyp, labels = ~lbls_octyp, values = ~count_octyp, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$octyp_past_dist <- renderPlotly({
  lbls_octyp_past <- c("Self-employed", "Self-employed & employed", "Employee", "Unemployed", "Other", "Student")
  count_octyp_past <- long_pan %>% group_by(octyp_past, id) %>% tally() %>% group_by(octyp_past) %>% tally() %>%
    .[2] %>% .[1:6,] %>% unlist(.) 
  df_octyp_past <- as_tibble(cbind(count_octyp_past, lbls_octyp_past))
  plot_ly(df_octyp_past, labels = ~lbls_octyp_past, values = ~count_octyp_past, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$dist_indu <- renderPlotly({
  lbls_indu <- c("Information, Communications, \n or Technology", "Finance, Real Estate,\n or Business Services", "Health, Education, Government, \n or Social and Consumer Services",
            "Wholesale, Retail", "Manufacturing, Logistics", "Agriculture, Extractive, \n or Construction", "Other")
  count_indu <- long_pan %>% group_by(indu, id) %>% tally() %>% group_by(indu) %>% tally() %>%
    .[2] %>% .[1:7,] %>% unlist(.) 
  
  df_indu <- as_tibble(cbind(count_indu, lbls_indu))
  
  plot_ly(df_indu, labels = ~lbls_indu, values = ~count_indu, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$dist_owner <- renderPlotly({
  lbls_owner <- c("Entrepreneur owns or \nholds shares in business", "Entrepreneur does not own or \nhold shares in business")
  count_owner <- long_pan %>% group_by(owner, id) %>% tally() %>% group_by(owner) %>% tally() %>%
    .[2] %>% .[1:2,] %>% unlist(.) 
  df_owner <- as_tibble(cbind(count_owner, lbls_owner))
  plot_ly(df_owner, labels = ~lbls_owner, values = ~count_owner, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$dist_shared_owner <- renderPlotly({
  lbls_shared_owner <- c("Entrepreneur is \nthe only owner", "Shared ownership")
  count_shared_owner <- long_pan %>% group_by(coown, id) %>% tally() %>% group_by(coown) %>% tally() %>%
    .[2] %>% .[1:2,] %>% unlist(.) 
  df_shared_owner <- as_tibble(cbind(count_shared_owner, lbls_shared_owner))
  plot_ly(df_shared_owner, labels = ~lbls_shared_owner, values = ~count_shared_owner, type = 'pie') %>% 
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
 
  
  output$timebuiss <- renderPlot({
  count_timebuiss <- long_pan %>% group_by(id, timebuiss) %>% tally() %>% as_tibble(.) %>% .$timebuiss/365
  hist(count_timebuiss, main = "", xlab = "Years since business foundation (for businesses under 7 years)", ylab = "count", breaks = 30, xlim=c(0,7))
  })
  
  output$accri_eval <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("Imagine you set a goal, namely, introducing a new product to the market. You were motivated and have worked hard for your goal. 
    However, suddenly your project stops running smoothly - financial difficulties occur, one of your main supporters jump off.  
    Now you have to decide for or against making further investments. At this point you may experience an Action Crisis.",
    "<br></br>Action Crises are associated with disengagement and a loss of motivation. They may cause stress reactions due to fear of failure and threats to self-identity.<br></br>", sep = "\n")
  })
  
  output$accri_eval2 <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("You can overcome an action crisis by re-evaluating your goals and focusing on your current state of resources. You may ask yourself: 
    What do you need to be able to decide whether to continue or not? What would be the next step? 
    <br></br>Something you should avoid in this situation are neverending cost-benefit considerations.
    Better: Focus on your resources and make an informed decision. Overcoming an action crisis does not necessarily mean to ignore obstacles and continue the path you chose initially. 
    Rather, re-consider your options and stay open for alternatives.
    <br></br> Example item:<em> I doubted whether I should continue striving to reach my business goals or withdraw from them.</em>")
  })
  
  output$exploi_eval <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("Your degree of exploitation is an indicator of how well you are at performing routine business activities; whether you are able to 
    maximize and optimize the use of already existing assets and human capital. 
    <br></br>The ability to exploit current opportunities is associated with short-term high financial performance. People low in exploitation tend to put a strong focus on exploring 
    novel options and often disregard the value they might create by exploiting current opportunities.
    and growth. 
    <br></br>")
  })
  
  output$exploi_eval2 <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("If you score low on exploitation, you may re-consider your current business strategies to achieve better outcomes: Which areas of your business already work well and offer you 
    the opportunity to increase short-term outcomes by exploiting what you already achieved? 
    <br></br> Being able to adapt to environmental affordances by switching between 
    exploitative and exploratory business strategies is key to high performance and entrepreneurial satisfaction. If you find out that you are committed to only one strategic focus  
    it might be valuable to consider how to balance your business strategy to maintain both personal well-being and performance.
    <br></br> Example item: <em>Activities of which a lot of experience has been accumulated by yourself.</em>")
  })
  
  output$explor_eval <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("Exploration means seeking non-routine activities, novel opportunities, strategies, deals and niches. Entrepreneurs who focus on exploration 
    are eager to create innovative products and services. Exploration results in high long-term business performance and growth.
    <br></br>
    On the other hand, exploration is associated with risk and uncertainty. That means, exclusively focusing on the creation of novel opportunities 
    might drain your personal and financial resources in the long run. 
    <br></br>")
  })
  
  output$explor_eval2 <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("
    Key to high performance is a clever and use of exploratory business strategies. Oftentimes, people have preferences for certain forms of behavior. Counting on one's intuition may help in some situations, but oftentimes the entrepreneur 
    is well-advised to recognize his/ her habitual patterns and adjust them if needed. 
    <br></br>
    You may compare your exploration and exploitation scores and see whether a pattern emerges (Are you more of an exploiter or an explorer?). Ask yourself what guides you in your decisions for certain business strategies; 
    is it purely strategic planning or personal preferences?
    Means to increase your exploration include seeking advice as well as proactively acquiring new skills in the areas you plan to explore more. 
    <br></br> Example item: <em>Searching for new possibilities with respect to products/ services, processes, or markets.</em>")
  })
  
  output$error_eval <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("As an entrepreneur, your well-being is likely 
           linked to the success of your business. Discontinuous events, such as setbacks, mistakes, or business failure 
           may have strong short- and long-term effects on your well-being and development. 
           Because starting a business is basically an experiment, the appearance of setbacks is practically inevitable. 
           <br></br>
           However, failure and setbacks may function as negative feedback that lead to a re-evaluation of previously held knowledge 
           and expectations, thus enhancing long-term resilience and performance. 
           <br></br>
           If you feel that you make many errors or that the errors you make have strong effects on your success as an entrepreneur, re-consider each of those errors 
           and find out whether you can use them as learning resources. What did those errors point out? How can you use that for your personal development and how might it 
           help your business flourish? Errors do not necessarily have to be avoided; on the contrary, they often bear potential for growth and development.")
  })
  
  output$thriv_eval <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("Thriving describes a state in which a person simultaneously experiences vitality and learning at work. 
    Thriving is heaviliy related to performance, goal progress, and well-being. 
    <br></br>
    Entrepreneurs who thrive at their job are equipped with the personal resources they
    need to develop their job-related skills and expertise and to take advantage of new opportunities.")
  })
  
  output$thriv_eval2 <- renderText({
    validate(
      need(input$Participant != "", "Please enter your ParticipantID")
    )
    paste0("
    One of the key drivers of thriving at work is perceived autonomy. For many people, one major reasons for becoming an entrepreneur is the level of autonomy that is 
    associated with this form of occupation. If you value atonomy, one of the ways to enhance your thriving at work might be to increase your level of autonomy. 
    Think about when your work-related autonomy is threatened and consider ways to enhance your feeling of autonomy in those situations. 
    <br></br>
    Another way to enhance thriving at work is social exchange. Consider ways to improve the social climate at work; exchange with your employees and do not forget to 
    regularly provide and seek feedback!
    <br></br> Example items: <br> <em> At work, I have energy and spirit. <br>  
           At work, I am developing a lot as a person.</em>")
  })
  
}

shinyApp(ui, server)

