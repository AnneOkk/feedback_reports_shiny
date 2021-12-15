library(shiny)
library(shinythemes)
library(shinydashboard)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(plotly)
library(shinydashboardPlus)
library(shinyWidgets)
library(haven)
library("sass")

# Preparation -------------------------------------------------------------

indu_list <- c(
  "Agriculture, Extractive, or Construction", "Manufacturing, Logistics",
  "Health, Education, Government, or Social and Consumer Services",
  "Wholesale, Retail", "Information, Communications, or Technology",
  "Finance, Real Estate, or Business Services"
)


# Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarmenu",
    menuItem("Study information", tabName = "home", icon = icon("info")),
    menuSubItem("Sample information", tabName = "info", icon = icon("users")),
    menuSubItem("Adverse events", tabName = "events", icon = icon("chevron-right")),
    
    menuItem(numericInput("Participant", label = "Please enter your Participant ID", value = "ID")),
    menuItem(actionButton("do", "See results")),
    menuItem("Contact Information", tabName = "admin", icon = icon("paper-plane", lib = "font-awesome"))
  ),
  tags$footer(tags$img(src = "output-onlinepngtools-1.png", width = "130px", height = "40px"), align = "center", class = "footer")
)
  

# Body --------------------------------------------------------------------

getwd()
body <- dashboardBody(
  setBackgroundImage(
    src = "background_stars.jpg",
    shinydashboard = TRUE),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
  tabItems(
    tabItem(
      tabName = "home",
      fluidRow(
        column(12,
          align = "center",
          h1("Feedback Reports"),
          h2("Dealing with entrepreneurial work events")
        )
      )
    ),
    tabItem(
      tabName = "info",
      fluidRow(
        column(
          offset = 0.2,
          width = 11, title = "Participants", status = "primary",
          solidHeader = TRUE,
          height = "70px",
          h4(htmlOutput("sample_N", style = "color: white"),
            background = NULL
          )
        )
      ),
      fluidRow(
        column(
          4,
          box(
            title = "Sample age", width = NULL, status = "primary",
            solidHeader = TRUE,
            plotOutput("age_dist", height = "300px"),
            icon("question-circle"),
            tags$div(
              "Research published in the",
              tags$a(
                href = "https://hbr.org/2018/07/research-the-average-age-of-a-successful-startup-founder-is-45#",
                "Harvard Business Review (2018)"
              ),
              "shows that the average age at which a successful founder started their company is 45. According to the report,
                         'that’s among the top 0.1% of startups based on growth in their first five years'."
            )
          )
        ),
        column(
          7,
          box(
            title = "Gender",
            width = NULL,
            status = "primary",
            solidHeader = TRUE,
            collapsible = F,
            column(
              5,
              title = "Gender distribition", status = "primary",
              p(htmlOutput("majority_gender")),
              br(),
              plotlyOutput("t1gender_dist", height = "310px")
            ),
            column(
              7,
              title = "Buisnesses owned by women worldwide", status = "primary",
              icon("question-circle"),
              tags$div(
                "According to data of the",
                tags$a(
                  href = "https://blogs.worldbank.org/opendata/women-entrepreneurs-needed-stat",
                  "Gender Data Portal of the World Bank Group,"
                ),
                "1 in 3 businesses are owned by a woman."
              ),
              br(),
              plotOutput("gender_worldwide", height = "300px")
            )
          )
        )
      ),
      fluidRow(
        column(
          4,
          box(title = "Location", width = NULL, status = "primary", solidHeader = TRUE,
              column(12,
                     p(htmlOutput("majority_loc")),
                     plotlyOutput("loc_dist", height = "280px")
                     )
              )
        ),
        column(
          7,
          box(title = "Industry", width = NULL, status = "primary", solidHeader = TRUE,
              column(8,
                     plotlyOutput("dist_indu", height = "315px")
              ),
              column(4,
                     br(),
                     p(htmlOutput("majority_indu")),
                     br(),
                     icon("question-circle"),
                     tags$div("According to",
                              tags$a(href = "https://www.freshbooks.com/wp-content/uploads/2018/04/2018selfemploymentreport.pdf", "Freshbooks,"),
                              "the highest number of self-employed professionals work in the construction/trades field, followed by retail,
                              real estate, and business services.")
              )
            )
          )
      ),
      fluidRow(
        column(7, 
          box(title = "Participants' current employment status", width = NULL, status = "primary", solidHeader = TRUE, 
              column(7,
                     p(htmlOutput("majority_occ")),
                     plotlyOutput("octyp_dist", height = "280px")
                     ),
              column(5,
                     br(),
                     icon("question-circle"),
                     tags$div("Being a part-time entrepreneur may position individuals to exhibit innovative behavior in their employee roles. Results of a study by",
                     tags$a(href = "https://journals.sagepub.com/doi/full/10.1177/0149206318779127?casa_token=icb6YKn31P8AAAAA%3AsrsJet9n1tUW4HuD7h9TT9ReCvjgfooD6dVVNHKuXcDrNNBm0fXuc8xCBJiAAJadPurRPCcHDkRT",
                            "Marshall et al. (2019)"),
                            "using a sample of 1,221 employee responses across 137 organizational units show that part-time entrepreneurship provides an opportunity for 
                            individuals to acquire knowledge and skills conducive to enacting innovative behaviors as employees. Individual differences in goal orientations and work-unit climates for innovation strengthen these relationships.")
                     )
              )
          ),
        column(4,
               box(
                 title = "Participants' business experience", status = "primary", solidHeader = TRUE, width = NULL,
                 plotOutput("time_dist")
               )
        )
      )
    ),
    tabItem(
      tabName = "events",
      fluidRow(
        column(
          7,
          box(title = "Negative events: Most common areas", status = "primary", solidHeader = TRUE, width = NULL,
              p(htmlOutput("majority_evdes")),
              br(),
              plotlyOutput("evdes_dist", height = "280px")
          )
        )
      ),
      fluidRow(
        column(
          7,
          box(title = "Most commonly used words", status = "primary", solidHeader = TRUE, width = NULL,
              column(6,
                     br(), 
                     tags$div("Because entrepreneurs provided written texts describing negative work events that happened over the past week, we applied 
                     natural language processing procedures to analyse and synthesize the text entries. Specifically, we cleaned the texts by 
                     normalizing the text entries, removing stop words, and performing tokenization and lemmatization. Next, we created a wordcloud displaying the most commonly used 
                     nouns and adjectives.", br(), br(), "For more information on the natural language processing procedure and the packages used, please visit the",
                     tags$a(href = "https://bnosac.github.io/udpipe/en/index.html", "UDPipe Documentation"))
                     ),
              column(6,
                     br(),
                     tags$img(src = "wordcloud.png", width = "380px", height = "280px"),
                     br(),
                     br())
              )
          )
        )
      )
    )
  )


# UI ----------------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Feedback \nReports"),
  sidebar,
  body
)


# Server ------------------------------------------------------------------



server <- function(input, output, session) {
  long_pan <- read.csv("Data/comp_df.csv")
  # "sample_N" --------------------------------------------------------------

  output$sample_N <- renderText({
    paste("Sample size: <em>N</em> = ",
      length(unique(long_pan$ID_code)),
      sep = ""
    )
  })

  # "age_dist" --------------------------------------------------------------
  
  output$age_dist <- renderPlot(
    {
      part_age <- long_pan[long_pan$ID_code == input$Participant, ] %>%
        dplyr::select(dplyr::matches("t1age_1")) %>% .[1, 1]
      younger_than <- round(nrow(long_pan[long_pan$t1age_1 > part_age, ]) / (nrow(long_pan[long_pan$t1age_1 > 18, ])) * 100, 0)
      text <- paste("You are younger than", younger_than, "percent of the entrepreneurs \nin the sample.\n\n\n")
      
      ggplot(long_pan, aes(x=t1age_1))+
        ggtitle(text) + 
        theme(text = element_text(size = 14)) + 
        theme(plot.title = element_text(size=14)) +
        theme(axis.text.x = element_text(color = "black", size = 14, face = "plain")) +
        theme(axis.text.y = element_text(color = "black", size = 14, face = "plain")) +
        theme(legend.text = element_text(size = 14)) +  
        geom_histogram(binwidth = 5, boundary = 0, closed = "right", fill="#443983", color="#e9ecef") + 
        scale_x_continuous(name = "Age", lim = c(15, 70)) + 
        scale_y_continuous(name = "Number of participants") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
    })
  
  
  # "t1gender_dist" -----------------------------------------------------------

  output$t1gender_dist <- renderPlotly({
    lbls_gender <- c("Male", "Female")
    count_gender <- long_pan %>%
      group_by(t1gender, ID_code) %>%
      tally() %>%
      group_by(t1gender) %>%
      tally() %>%
      .[2] %>%
      .[1:2, ] %>%
      unlist(.)
    df_t1gender <- as_tibble(cbind(count_gender, lbls_gender))
    plot_ly(df_t1gender, labels = ~lbls_gender, values = ~count_gender,            
            marker = list(colors = c('#440154FF', '#21908CFF'),
                          line = list(color = '#FFFFFF', width = 1)), 
            type = "pie", width = 280, height = 280) %>%
      layout(
        title = "",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        showlegend = FALSE, margin = list(l=0, r=0, b=10, t=0, pad=4)
      )
  })
  
  library(scales)
  show_col(viridis_pal()(7))
  
  

# gender_worldwide --------------------------------------------------------
  output$gender_worldwide <- renderPlot(
    {
  df_gender_worldwide <- data.frame(region=c("World", "East Asia & \n Pacific", "Europe & \n Central Asia", "Latin America & \n Carribean", "Middle East & \n North Africa",
                          "South Asia", "Sub-Saharan \n Africa"),
                   percentage=c(34, 47, 33, 50, 23, 18, 29))
  
  ggplot(data=df_gender_worldwide, aes(x=region, y=percentage)) +
    geom_bar(stat="identity", fill = "#21908CFF") +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(size=14)) +
    theme(axis.text.x = element_text(color = "black", size = 13, face = "plain")) +
    theme(axis.text.y = element_text(color = "black", size = 13, face = "plain")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),  axis.title.y =element_blank(), 
          axis.title.x = element_text(size = 10, hjust = -3.0)) +
    geom_text(aes(label = paste0(percentage, "%")), hjust=1.5, colour = "white") +
    labs(title = "Share of small, medium, and large firms with a woman \namong the principal owners", x = "Region") +
    coord_flip() + 
    ylab("Source = https://blogs.worldbank.org/opendata/women-entrepreneurs-needed-stat")
    })
  


  # "majority_gender" -------------------------------------------------------

  output$majority_gender <- renderText({
    maj_gender <- if (sum(long_pan$t1gender == 1, na.rm = T) > sum(long_pan$t1gender == 2, na.rm = T)) {
      print("male")
    }
    paste0("The majority of the participants in the sample defined themselves as ", maj_gender)
  })

  # "loc_dist" --------------------------------------------------------------
  
  output$majority_loc <- renderText({
    maj_loc <- if (names(tail(sort(table(long_pan$t1location)), 1)) == "1") {
    print("the Netherlands")
  } else if (names(tail(sort(table(long_pan$t1location)), 1)) == "2") {
    print("India")
  } else if (names(tail(sort(table(long_pan$t1location)), 1)) == "3") {
    print("Other")
  } else if (names(tail(sort(table(long_pan$t1location)), 1)) == "4") {
    print("Germany")
  } else if (names(tail(sort(table(long_pan$t1location)), 1)) == "5") {
    print("Canada")
  } else if (names(tail(sort(table(long_pan$t1location)), 1)) == "6") {
    print("USA")
  } else {
    print("UK")
  }
  
  paste0("Most entrepreneurs in the sample have their business in ", maj_loc, ".")
  
  })

  output$loc_dist <- renderPlotly({
    lbls_loc <- c("Netherlands", "India", "Other", "Germany", "Canada", "USA", "UK")
    count_loc <- long_pan %>%
      group_by(t1location, ID_code) %>%
      tally() %>%
      group_by(t1location) %>%
      tally() %>%
      .[2] %>%
      .[1:7, ] %>%
      unlist(.)
    df_loc <- as_tibble(cbind(count_loc, lbls_loc))

    plot_ly(df_loc, labels = ~lbls_loc, values = ~count_loc, 
            marker = list(colors = c('#440154FF','#443A83FF', '#21908CFF','#31688EFF', '#35B779FF', '#8FD744FF', '#FDE725FF'),
                          line = list(color = '#FFFFFF', width = 1)), 
            type = "pie", width = 480, height = 250) %>%
      layout(
        title = "",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(x = 0, y = 1.0), margin = list(l=0, r=0, b=5, t=10, pad=4)
      ) 
  })

  # "octyp_dist" ------------------------------------------------------------

  output$octyp_dist <- renderPlotly({
    lbls_octyp <- c("Entrepreneur, \nno other job", "Entrepreneur and \nworking for an \nemployer", "Entrepreneur and \nstudent", "Entrepreneur and \nvolunteer", "Other")
    count_octyp <- long_pan %>%
      group_by(t1occ, ID_code) %>%
      tally() %>%
      group_by(t1occ) %>%
      tally() %>%
      .[2] %>%
      .[1:5, ] %>%
      unlist(.)
    
    output$majority_occ <- renderText({maj_occ <- if (names(tail(sort(table(long_pan$t1occ)), 1)) == "1") {
        print("only as entrepreneurs")
      } else if (names(tail(sort(table(long_pan$t1occ)), 1)) == "2") {
        print("entrepreneurs and employees")
      } else if (names(tail(sort(table(long_pan$t1occ)), 1)) == "3") {
        print("entrepreneurs and students")
      } else if (names(tail(sort(table(long_pan$t1occ)), 1)) == "4") {
        print("entrepreneurs and volunteers")
      } else {
        print("Other")
      }
      
    paste0("Most participants in the sample work as ", maj_occ)})

    
    df_octyp <- as_tibble(cbind(count_octyp, lbls_octyp))
    plot_ly(df_octyp, labels = ~lbls_octyp, values = ~count_octyp, 
                    marker = list(colors = c('#440154FF','#443A83FF', '#21908CFF', '#31688EFF','#35B779FF', '#8FD744FF', '#FDE725FF'),
                                  line = list(color = '#FFFFFF', width = 1)), type = "pie", width = 500, height = 270) %>%
      layout(
        title = "",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(x = -0.4, y = 1.0), margin = list(l=0, r=0, b=15, t=5, pad=4)
      ) 
  })
  

  # "time_dist" -------------------------------------------------------------

  output$time_dist <- renderPlot(
    {
      outlier_cutoff <- quantile(long_pan$t1timebuiss, 0.75, na.rm = T) + 1.5 * IQR(long_pan$t1timebuiss, na.rm = T)
      index_outlier <- which(long_pan$t1timebuiss > outlier_cutoff)
      long_pan_noout <- long_pan[-index_outlier, ] %>% drop_na(t1timebuiss)

      long_pan_noout$t1timebuiss <- round((long_pan_noout$t1timebuiss / 30), 0) 
      total <- sum(long_pan$t1timebuiss < 0, na.rm = T) + sum(long_pan$t1timebuiss > 0, na.rm = T)
      not_founded <- round(100 * (sum(long_pan$t1timebuiss < 0, na.rm = T) / total), 0)

      text <- paste("About ", not_founded, 
      " percent of the participants in the sample had not \nestablished the business when the survey was taken but were still \nin the preparatory phase.\n\n",
        sep = "")

      long_pan_noout <- long_pan_noout %>%
        mutate(`Business status`=case_when(t1timebuiss<=0 ~ "pre-foundation \nphase",
                                    t1timebuiss >0 ~ "founded")) 
      
      ggplot(long_pan_noout, aes(x=t1timebuiss, fill=`Business status`, main=text ))+
        ggtitle(text) + 
        theme(text = element_text(size = 14)) + 
        theme(plot.title = element_text(size=14)) +
        theme(axis.text.x = element_text(color = "black", size = 14, face = "plain")) +
        theme(axis.text.y = element_text(color = "black", size = 14, face = "plain")) +
        theme(legend.text = element_text(size = 14)) +  
        geom_histogram(color="white", binwidth = 6, boundary = 0, closed = "right") + 
        scale_x_continuous(name = "Time since business foundation in months", breaks = seq(-12, 72, 6), lim = c(-12, 72), labels = seq(-12, 72, 6)) + 
        scale_y_continuous(name = "Number of participants") +
        scale_fill_manual(values = c("founded" = "#440154FF",
                                     "pre-foundation \nphase" = "#35B779FF")) + 
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
        theme(legend.position = c(0.8, 0.7)) +
        theme(plot.margin=unit(c(l=0.5,r=0.7,b=0.8,t=0.5), 'cm'))
    })

  # "dist_indu" -------------------------------------------------------------

  output$dist_indu <- renderPlotly({
    lbls_indu <- c(
      "Information, Communications, \nor Technology", "Finance, Real Estate,\nor Business Services", "Health, Education, Government, \nor Social and Consumer Services",
      "Wholesale, Retail", "Manufacturing, Logistics", "Agriculture, Extractive, \nor Construction", "Other"
    )
    count_indu <- long_pan %>%
      group_by(t1sector, ID_code) %>%
      tally() %>%
      group_by(t1sector) %>%
      tally() %>%
      .[2] %>%
      .[1:7, ] %>%
      unlist(.)

    df_indu <- as_tibble(cbind(count_indu, lbls_indu))

    plot_ly(df_indu, labels = ~lbls_indu, values = ~count_indu, 
            marker = list(colors = c('#440154FF','#443A83FF', '#21908CFF', '#35B779FF', '#31688EFF', '#8FD744FF', '#FDE725FF'),
                          line = list(color = '#FFFFFF', width = 1)), type = "pie", width = 580, height = 300) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(x = -0.4, y = 1.0), margin = list(l=0, r=0, b=30, t=20, pad=4)
      )
  })

  # "majority_indu" ---------------------------------------------------------

  output$majority_indu <- renderText({
    maj_indu <- if (names(tail(sort(table(long_pan$t1sector)), 1)) == "6") {
      print("Agriculture, Extractive, or Construction")
    } else if (names(tail(sort(table(long_pan$t1sector)), 1)) == "5") {
      print("Manufacturing, Logistics")
    } else if (names(tail(sort(table(long_pan$t1sector)), 1)) == "4") {
      print("Wholesale, Retail")
    } else if (names(tail(sort(table(long_pan$t1sector)), 1)) == "3") {
      print("Health, Education, Government, or Social and Consumer Services")
    } else if (names(tail(sort(table(long_pan$t1sector)), 1)) == "2") {
      print("Finance, Real Estate, or Business Services")
    } else {
      print("Information, Communications, or Technology")
    }

    paste0("Most businesses in the sample operate in the ", maj_indu, " industry.")
  })
  
  
  # "evdes_dist" ------------------------------------------------------------
  
  output$evdes_dist <- renderPlotly({
    lbls_evdes <- c("Financial difficulties", 
                    "Conflicts with clients, stakeholders or colleagues", 
                    "Conflicts between clients, stakeholders, or colleagues",
                    "Legal issues", 
                    "Absence or a lack of personnel or support", 
                    "Problems related to material, service supply or quality", 
                    "Mistakes or mishaps", 
                    "Another negative work event not associated with these categories")
    count_evdes <- long_pan %>%
      group_by(max_sev, ID_code) %>%
      tally() %>%
      group_by(max_sev) %>%
      tally() %>%
      .[2] %>%
      .[1:8, ] %>%
      unlist(.)
    df_evdes <- as_tibble(cbind(count_evdes, lbls_evdes))
    plot_ly(df_evdes, labels = ~lbls_evdes, values = ~count_evdes, 
                    marker = list(colors = c('#fde725','#277f8e', '#a0da39', '#46327e','#a0da39', '#440154', '#1fa187', '#365c8d'),
                                  line = list(color = '#FFFFFF', width = 1)), type = "pie", width = 850, height = 280) %>%
      layout(
        title = "",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        legend = list(x = 1.9, y = 0.9), margin = list(l=0, r=0, b=15, t=10, pad=4)
      ) 
  })
  
  # "majority_evdes" ---------------------------------------------------------
  
  output$majority_evdes <- renderText({
    maj_evdes <- if (names(tail(sort(table(long_pan$max_sev)), 1)) == "1") {
      print("'financial difficulties'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "2") {
      print("'conflicts with clients, stakeholders or colleagues'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "3") {
      print("'conflicts between clients, stakeholders, or colleagues'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "4") {
      print("'legal issues'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "5") {
      print("'absence or a lack of personnel or support'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "6") {
      print("'problems related to material, service supply or quality'")
    } else if (names(tail(sort(table(long_pan$max_sev)), 1)) == "7") {
      print("'mistakes or mishaps'")
    } else {
      print("'another negative work event not associated with these categories'")
    }
    
    paste0("In the study, we asked participants to think of a negative work event that happened to them as entrepreneurs over the past week. 
           Participants were then asked to to select the area in which the negative work event occurred and to describe the event briefly. 
           Most entrepreneurs experienced a negative event related to \n", maj_evdes, ".")
  })
  

}

shinyApp(ui, server)
