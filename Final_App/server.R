#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(plotly)
library(base)
library(ggplot2)

IMDB <- data.table::fread("IMDb movies.csv", stringsAsFactors = FALSE, drop = c("production_company", "description", "writer", "actors",
                                                                                "imdb_title_id", "title", "reviews_from_users", "reviews_from_critics",
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



shinyServer(function(input, output, session) {
    
    ### first tab
    output$mytable = DT::renderDataTable({
        req(IMDB)[, input$show_vars, drop = FALSE]
    })
    ### second tab
    output$histplot <- renderPlotly({
        movie_year <- req(IMDB) %>%
            filter(year >= input$year[1],
                   year <= input$year[2])
        if (input$genre != "All") {
            movie_year <- movie_year %>% filter(grepl(input$genre, movie_year$genre, fixed = TRUE))
        }
        P <- ggplot(movie_year, aes(x=year)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) +
            coord_cartesian(expand=FALSE) + 
            labs(x='release year', y='number of movies')+ 
            theme_classic()
        ggplotly(P)
    })
    
    ### third tab
    output$dateUI <- renderUI({
        mydates <- req(IMDB) %>% 
            select(date) %>%
            summarise(range=range(date, na.rm = TRUE))
        begins <- mydates$range[1]
        ends <- mydates$range[2]
        dateRangeInput("daterange", "Release date range:",
                       start = "2000-01-01",
                       end   = "2010-01-01",
                       min = begins,
                       max = ends,
                       format = "mm/dd/yy",
                       separator = " - ")
        
    })
    
    observeEvent(input$xvar,{
        updateSelectInput(session, "xvar", choices=c("budget ($)", "u.s. revenue ($)", 
                                                     "worldwide revenue ($)", "metascore", "average vote"))
    },
    once=TRUE)
    
    observeEvent(input$yvar,{
        updateSelectInput(session, "yvar", choices=c("u.s. revenue ($)", "budget ($)", 
                                                     "worldwide revenue ($)", "metascore", "average vote"))
    },
    once=TRUE)
    
    filtered.movie <- reactive(
        if (input$genre2 != "All") {
            filtered.movie <- req(IMDB) %>% 
                filter(
                duration >= input$runtime[1],
                duration <= input$runtime[2],
                date >= req(input$daterange[1]),
                date <= req(input$daterange[2]),
                grepl(input$genre2, req(IMDB)$genre, fixed = TRUE))
            return(filtered.movie)}
        else{
            filtered.movie <- req(IMDB) %>% 
                filter(
                duration >= input$runtime[1],
                duration <= input$runtime[2],
                date >= req(input$daterange[1]),
                date <= req(input$daterange[2]))
            return(filtered.movie)
        }
    )
    
    
    
    output$plot <- renderPlotly({
        fit <- lm(get(input$yvar) ~ get(input$xvar), 
                  data=filtered.movie())
        filtered.movie2 <- filtered.movie() %>% mutate(linear=fit$fitted)
        p2 <- 
            plot_ly(
                data = filtered.movie2,
                type='scatter',
                mode='markers',
                x = ~get(input$xvar),
                y = ~get(input$yvar),
                marker = list(color = 'navyblue',
                              opacity = 0.5,
                              line = list(color = 'black')),
                hovertemplate = paste0('<b>title:</b>',filtered.movie()$title, '<br>',
                                       '<b>%{xaxis.title.text}:</b> %{x} <br>',
                                       '<b>%{yaxis.title.text}:</b> %{y}',
                                       '<extra></extra>'),
                hoverlabel = list(bgcolor="white"),
                hoverinfo = 'text') %>%
            layout(
                xaxis = list(title=input$xvar, zeroline=FALSE),
                yaxis = list(title=input$yvar, zeroline=FALSE),
                showlegend = F)
        
        if(input$linear){
            return(p2 %>% add_lines(x=~get(input$xvar),y=~linear, name ="linear trend",mode = 'lines', 
                                    inherit = FALSE, data = filtered.movie2))
        } else{
            return(p2)
        }
        
    })
    
    output$n_movies <- renderText({ 
        nrow(filtered.movie()) })
    
    output$table1 <- renderTable({
        fmovie <- filtered.movie()
        xv <- input$xvar
        yv <- input$yvar
        if(input$coe =="Pearson correlation coefficient"){
            r <- cor(filtered.movie()[,xv], filtered.movie()[,yv],
                     method = "pearson")
            df <- data.frame(r=r)
            colnames(df) <- "Pearson correlation coeffcient"
            name.width <- max(sapply(names(df), nchar))
            df <- format(df, width = name.width, justify = "centre")
            return(df)
        }
        if(input$coe =="Spearman correlation coefficient"){
            r <- cor(filtered.movie()[,xv], filtered.movie()[,yv],method = "spearman")
            df <- data.frame(r=r)
            colnames(df) <- "Spearman correlation coeffcient"
            name.width <- max(sapply(names(df), nchar))
            df <- format(df, width = name.width, justify = "centre")
            return(df)
            }
        if(input$coe =="Kendall's Tau"){
            r <- cor(filtered.movie()[,xv], filtered.movie()[,yv], method = "kendall")
            df <- data.frame(r=r)
            colnames(df) <- "Kendall's Tau"
            name.width <- max(sapply(names(df), nchar))
            df <- format(df, width = name.width, justify = "centre")
            return(df)
            }
    })
})



