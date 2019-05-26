
PostsDF <- read.csv("Posts.csv")
library(shiny)
library(shinyjs)

ui <- fluidPage (
  useShinyjs(),
  titlePanel(title = h2("Stack Exchange Programming Language  Analysis Framework",align="center")),
  br(),
  br(),
  navbarPage("Select Type",
             tabPanel("Posts Analysis",
       sidebarLayout(
           sidebarPanel(
             selectInput("select", label = h3("Select Prefered Language"), 
                                      choices = list("R Language" = "r", "Python" = "python", "Java" = "java","C++" = "c++","Go" = "go","Matlab" = "matlab", "C#"= "c#","Octave" = "octave", "Ruby"= "ruby", "C"= "c" ), 
                                      selected = "r"),
                         
                         
                          hr(),
                          submitButton("Analyze", icon("refresh")),
                          br()
                          
                          
                          
                        ),
                        mainPanel(
                          
                          tabsetPanel(type = "tab",
                                      tabPanel("No. Of Posts vs Time",icon = icon("bar-chart-o"),plotOutput("postplot",height = 800, width = 800)),
                                      tabPanel("No. Of Posts for selected language vs Time",icon = icon("bar-chart-o"),plotOutput("langplot",height = 800, width = 800)),
                                      tabPanel("Language wise Post Trends over Time",icon = icon("bar-chart-o"),plotOutput("trendplot",height = 800, width = 800)),
                                      tabPanel("Posts by language",icon = icon("bar-chart-o"),plotOutput("postlanplot",height = 800, width = 800)),
                                      tabPanel("Language wise avg time to get answers",icon = icon("bar-chart-o"),plotOutput("timeplot",height = 800, width = 800)),
                                      tabPanel("Word Cloud",icon = icon("bar-chart-o"),plotOutput("wordcloud",height = 800, width = 800)),
                                      tabPanel("Summary",icon = icon("table"),tableOutput("summary"))
                          
                          
                          
                          
                        )
                    )
                )
             ),
       tabPanel("Posts Analysis According to Date",
                sidebarLayout(
                  sidebarPanel(
                    
                    br(),
                    br(),
                    selectInput("selectd", label = h3("Select Prefered Language"), 
                                choices = list("R Language" = "r", "Python" = "python", "Java" = "java","C++" = "c++","Go" = "go","Matlab" = "matlab", "C#"= "c#","Octave" = "octave", "Ruby"= "ruby", "C"= "c" ), 
                                selected = "r"),
                    
                    
                  
                    hr(),
                    
                    br(),
                    br(),
                      
                      dateInput('fromdate',
                                label = 'From',
                                value = as.Date(head(PostsDF$CreationDate, 1))
                                
                      ),
                    
                      dateInput('todate',
                                label = 'To',
                                value = as.Date(tail(PostsDF$CreationDate, 1))
                                
                      ),
                    hr(),
                    submitButton("Analyze", icon("refresh")),
                    br()
                    
                    
                    
                  ),
                  mainPanel(
                    tabsetPanel(type = "tab",
                                tabPanel("No. Of Posts vs Time",icon = icon("bar-chart-o"),plotOutput("postplotd",height = 800, width = 800)),
                                tabPanel("No. Of Posts for selected language vs Time",icon = icon("bar-chart-o"),plotOutput("langplotd",height = 800, width = 800)),
                                tabPanel("Language wise Post Trends over Time",icon = icon("bar-chart-o"),plotOutput("trendplotd",height = 800, width = 800)),
                                tabPanel("Posts by language",icon = icon("bar-chart-o"),plotOutput("postlanplotd",height = 800, width = 800)),
                                tabPanel("Language wise avg time to get answers",icon = icon("bar-chart-o"),plotOutput("timeplotd",height = 800, width = 800)),
                                tabPanel("Word Cloud",icon = icon("bar-chart-o"),plotOutput("wordcloudd",height = 800, width = 800)),
                                tabPanel("Summary",icon = icon("table"),tableOutput("summaryd"))
                                
                    )
                    
                  )
                )
       ),
       tabPanel("Posts Analysis For Countries",
          sidebarLayout(
            sidebarPanel(
              
              br(),
              br(),
              sliderInput("postsno", "No. of Posts:",
                          min = 50, max = 2000,
                          value = 200),
              
              
              
              hr(),
              br(),
            
              
              
              hr(),
              submitButton("Analyze", icon("refresh")),
              br()
              
              
              
            ),
            mainPanel(
              tabsetPanel(type = "tab",
                          tabPanel("Posts by Country",icon = icon("bar-chart-o"),plotOutput("postcountry",height = 800, width = 800)),
                          tabPanel("Average Views by Country",icon = icon("bar-chart-o"),plotOutput("viewscountry",height = 800, width = 800))
                          
                          
              )
              
            )
          )
       )
       
  )
)
                        
                        

