#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(DT)
library(data.table)
library(tidyverse)
IMDB <- data.table::fread("IMDb movies.csv", stringsAsFactors = FALSE, drop = c("production_company", "description", "writer", "actors",
                                                                                "imdb_title_id", "title","reviews_from_users", "reviews_from_critics",
                                                                                "language", "votes"),
                          data.table = FALSE)
IMDB[] = lapply(IMDB, gsub, pattern="\\$", replacement="")
IMDB <- IMDB[Reduce(`&`, lapply(IMDB, function(x) !(is.na(x)|x==""))),]
IMDB$date_published <- as.Date(IMDB$date_published)
IMDB <- IMDB %>% 
    mutate_at(vars(year, duration, avg_vote, budget, usa_gross_income, 
                   worlwide_gross_income, metascore), as.numeric)
IMDB <- IMDB %>% drop_na()
colnames(IMDB)[1] <- "title"
colnames(IMDB)[3] <- "date"
colnames(IMDB)[8] <- "average vote"
colnames(IMDB)[9] <- "budget ($)"
colnames(IMDB)[10] <- "u.s. revenue ($)"
colnames(IMDB)[11] <- "worldwide revenue ($)"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    navbarPage("IMDB Movie Data",
               tabPanel("Dataset",
                        sidebarLayout(
                            sidebarPanel(
                                checkboxGroupInput("show_vars", "Columns in IMDB to show:",
                                                   names(IMDB), selected = names(IMDB))),
                            mainPanel(
                                DT::dataTableOutput("mytable")))
               ),
               tabPanel("Movies by Year",
                        sliderInput("year", "Select year range",
                                    1931, 2020, c(1960, 2010), step = 1, sep = ""),
                        selectInput("genre", "Genre (a movie can have multiple genres)",
                                    c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
                                      "Crime", "Drama", "Family", "Fantasy", "History",
                                      "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                      "Sport", "Thriller", "War", "Western")
                        ),
                        plotlyOutput("histplot")),
               tabPanel("Exploring Relationship",
                        column(4,
                               wellPanel(h4("Filter"),
                                         uiOutput("dateUI"),
                                         sliderInput("runtime", "Movie runtime (minutes)",
                                                     63, 238, c(60, 120), step = 1),
                                         selectInput("genre2", "Genre",
                                                     c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
                                                       "Crime", "Drama", "Family", "Fantasy", "History",
                                                       "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                                                       "Sport", "Thriller", "War", "Western"))
                               ),
                               wellPanel(
                                   selectInput("xvar", "X-axis variable", choices = NULL),
                                   selectInput("yvar", "Y-axis variable", choices = NULL), 
                                   selectInput("coe", "Statistic for measuring correlation",
                                               c("Pearson correlation coefficient", "Spearman correlation coefficient", "Kendall's Tau")),
                                   span(h5("Options:"),
                                        checkboxInput("linear", "Linear trend")
                                   ))),
                        column(8, 
                               plotlyOutput("plot"),
                               wellPanel(
                                   span("Number of movies selected:",
                                        textOutput("n_movies")
                                   )),
                               wellPanel(
                                 tableOutput("table1")
                                   ), 
                               
                        )
                        
                        
               )
    )))

