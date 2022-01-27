# wrap your ui in a call to secure_app(), which can also include custom css etc
# for the login page.
# change login text etc. using set_labels()
# NOTE! to load keyring package on ubuntu need to install
# sudo apt-get install libsecret-1-dev
# sudo apt-get install libsodium-dev
# NOTE that there are permissions issues when udpating the sqlite file.
# you can chmod 777 the entire folder but this is probably not ideal......

library(readr)
#library(keyring)
library(shiny)
library(shinymanager)
library(pushshiftR)
library(wordcloud2)
#library(htmlwidgets)


# ##### SET UP INITIAL USERS DATABASE
# # Init DB using credentials data
# credentials <- data.frame(
#     user = c("cbelanger"),
#     password = c("a"),
#     # password will automatically be hashed
#     admin = c(TRUE),
#     stringsAsFactors = FALSE
# )
# # Init the database without keyring, with passphrase
# create_db(
#     credentials_data = credentials,
#     sqlite_path = "database.sqlite", # will be created
#     #passphrase = key_get("R-shinymanager-key", "belangeranalytics", keyring="shiny")
#     passphrase = "saf765asdf"
# )
####################################



shinymanager::set_labels(language = "en", "Please authenticate" = "Please enter your login credentials.")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Reddit Comment Analysis -- Draft Demo App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(width = 3,
                     shiny::textInput("search_q",
                                      "Search Text (double quotes for exact match)"),
                     shiny::selectInput("search_size",
                                        "Number of Comments:",
                                        choices = c(10, 100, 500, 1000, 5000, 10000)),
                     shiny::textInput("search_subreddit",
                                      "Subreddit(s) (comma-separated)"),
                     shiny::textInput("search_author",
                                      "User Search"),
                     shiny::actionButton("search_button",
                                         "Run Search",
                                         icon = shiny::icon("search", verify_fa = FALSE)),
                     shiny::hr(),
                     shiny::downloadButton("comments_download", label = "Download Comments"),
                     shiny::hr(),

                     # Input: Select a file ----
                     shiny::fileInput("file1", "Upload Comments",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv"))


        ),

        # Show a plot of the generated distribution
        mainPanel(
            shiny::tabsetPanel(
                shiny::tabPanel("Reddit Comments",
                                DT::dataTableOutput("comments_table")
                ),
                shiny::tabPanel("Subreddits",
                                column(width = 6, shiny::tableOutput("subreddit_table")),
                                column(width = 6, shiny::plotOutput("subreddit_plot"))
                ),

                shiny::tabPanel("Authors",
                                column(width = 6, shiny::tableOutput("author_table")),
                                column(width = 6, shiny::plotOutput("author_plot"))
                ),

                shiny::tabPanel("ngram Frequency",
                                column(width = 6, shiny::tableOutput("freq1gram_table")),
                                column(width = 6, shiny::tableOutput("freq2gram_table"))),

                shiny::tabPanel("Wordcloud",
                                shiny::h1("Word cloud 200 single words, excluding top result (search term)"),
                                wordcloud2::wordcloud2Output("wordcloud")
                ),

                shiny::tabPanel("Posts Over Time",
                                column(width = 6, shiny::plotOutput("time_plot")),
                                column(width = 6, shiny::plotOutput("time_heatmap"))),

                shiny::tabPanel("COWO Processing",
                                shiny::h1("Process and Download Data for COWO"),
                                shiny::p("This algorithm processes the loaded Reddit comments as follows:"),
                                tags$ul(
                                    tags$li("OPTIONALLY: Transforms a single n-gram into a 1-gram by replacing spaces with underscores."),
                                    tags$li("Removes stop words."),
                                    tags$li("Selects ONLY the comments (no author, subreddit, etc.)."),
                                    tags$li("Creates a text file with one comment per line.")
                                ),
                                shiny::textInput("cowo_2gram_input", "Custom n-gram to preserve (EXPERIMENTAL)"),
                                shiny::downloadButton("cowo_download", label = "Download Data Processed for COWO")
                                )

            )

        )
    )
)




# Wrap your UI with secure_app, enabled admin mode or not

ui <- shinymanager::secure_app(ui,
                               tags_top = tags$img(
                                   src = "https://www.belangeranalytics.com/img/logo/logo-for-svg.svg", width = 250
                               ),
                               enable_admin = TRUE)

# MUST INCLUDE SESSION TO USE SHINYMANAGER.
server <- function(input, output, session) {
    # set these up initially for download handler
    q <- size <- author <- subreddit <- NULL

    # check_credentials directly on sqlite db
    res_auth <- shinymanager::secure_server(
        check_credentials = shinymanager::check_credentials(
            "database.sqlite",
            #passphrase = key_get("R-shinymanager-key", "belangeranalytics", keyring="shiny")
            passphrase = "saf765asdf"
        )
    )

    # set up custom stop words
    custom_stop_words <- bind_rows(tidytext::stop_words,
                                   dplyr::tibble(word = c(1:10, "it's", "it’s", "http", "https", "i’m")))

    # set up notifications
    notify <- function(msg, id = NULL, duration = NULL, type = "message") {
        showNotification(msg, id = id, duration = duration, closeButton = FALSE, type = type)
    }

    # set up empty data
    reddit_comments <- reactiveValues(data = dplyr::tribble(~id, ~author, ~body, ~subreddit, ~score, ~created_utc, ~created_datetime))

    ## SEARCH BUTTON


    observeEvent(input$search_button,
                 {

                     # set up validation flag
                     valid <- TRUE
                     # GET INPUT
                     q <<- size <<- author <<- subreddit <<- NULL

                     if (!is.null(input$search_q)) q <<- input$search_q
                     if (!is.null(input$search_size)) size <<- as.numeric(input$search_size)
                     if (!is.null(input$search_author)) author <<- input$search_author
                     if (!is.null(input$search_subreddit)) subreddit <<- input$search_subreddit

                     # BASIC INPUT VALIDATION: must select school.
                     if (FALSE){
                         notify("Search parameters invalid.", type = "error")
                         valid <- FALSE
                     }

                     # IF VALIDATED, DO THE THING
                     if (valid){
                         message_id <- notify(sprintf("Loading %s comments. Expect this to take at least %s seconds.", size, round(size/100) + 5), type = "message")

                         # try the API
                         response <- try(pushshiftR::get_reddit_comments(q = q, size = size, fields = "id,author,body,subreddit,score", author = author, subreddit = subreddit))

                         # validate the response
                         if ("tbl_df" %in% class(response)) {
                             if (nrow(response) > 0){
                                 # we got at least 1 result
                                 reddit_comments$data <- response

                                 # UPDATE USER
                                 message("Successful API query")
                                 notify("Successful API query. Finished fetching data.", type = "message", duration = 5, id = message_id)
                                 message(reddit_comments$data)

                             } else { # 0 rows
                                 message("no results found")
                                 notify("API returned a response, but no results found.", type = "error", duration = 5, id = message_id)
                             }
                         }

                         # if we got an error, we remove the notification and show an error notification
                         if ("try-error" %in% class(response)){

                             message(response[1])
                             removeNotification(id)
                             showNotification("Error accessing Pushshift API.", type = "error")
                         }

                         # On successful update:
                         # submitted <<- TRUE
                         # shiny::updateActionButton(session = getDefaultReactiveDomain(),
                         #                           inputId = "test_submit",
                         #                           label = "Results Submitted!",
                         #                           icon = icon("check"))


                     }

                 })


    # Render the comments table
    output$comments_table <- DT::renderDataTable(reddit_comments$data)

    # Download button handler

    output$comments_download <- downloadHandler(
        filename = function() {
            # message(paste0("q = ",q))
            # if (!is.null(q)) paste("reddit-comments-",q,"-", Sys.Date(), ".csv", sep="")
            # if (is.null(q)) paste("reddit-comments-", Sys.Date(), ".csv", sep="")
            stringr::str_replace_all(paste("reddit-comments-",q,"-", Sys.Date(), ".csv", sep=""), '"', "'")
        },
        content = function(file) {write_csv(reddit_comments$data, file)}
    )

    # Upload handler
    observeEvent(input$file1,{
        message("File uploaded")

        tryCatch({
            new_data <- readr::read_csv(input$file1$datapath)
            message(head(new_data))
            # Input validation: make sure it has all the right columns
            if (!all(c("author", "body", "id", "subreddit", "created_utc", "created_datetime", "score") %in% names(new_data))) {
                notify("Error: Input not in correct format. Please use a .csv file downloaded from this web app.")
            } else {
                reddit_comments$data <- new_data %>%
                    dplyr::mutate(created_datetime = as.POSIXct(created_utc, origin = "1970-01-01", tz = "EST"))
                notify("Comments successfully loaded from file.", duration = 5)
            }

        })

    })


    ####### SUBREDDITS
    # Render the subreddit table
    output$subreddit_table <- renderTable(
        reddit_comments$data %>%
            group_by(subreddit) %>%
            count(sort = TRUE) %>%
            ungroup() %>%
            mutate(percent = sprintf("%.1f%%", (n / sum(n))*100)) %>%
            slice_head(n=20)
    )

    output$subreddit_plot <- renderPlot(
        reddit_comments$data %>%
            group_by(subreddit) %>%
            count(sort = TRUE) %>%
            ungroup() %>%
            mutate(pct = sprintf("%.1f%%", 100 * n/sum(n))) %>%
            #mutate(plot_adjust = if_else(n > .5 * max(n), n - .1 * (max(n) - min(n)), n + .1 * (max(n) - min(n)))) %>%
            mutate(plot_adjust =  n + .1 * (max(n) - min(n))) %>%
            # mutate(plot_colour = if_else(n > .5 * max(n), "white", "black")) %>%
            slice_head(n=10) %>%
            ggplot() +
            geom_col(aes(x=reorder(subreddit,n), y = n), fill = "lightblue") +
            geom_text(aes(x=reorder(subreddit,n), y = plot_adjust, label = pct, fontface = "bold", colour = plot_colour), colour = "black") +
            coord_flip() +
            theme_minimal() +
            labs(y="Count",
                 x = "Subreddit",
                 title = "Top 10 Subreddits by Search Term Prevalence",
                 subtitle = paste0("Search terms: q=",q,"; subreddit=",subreddit,"; author=",author)) +
            scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))

    )



    ####### AUTHORS
    # Render the author table
    output$author_table <- renderTable(
        reddit_comments$data %>%
            group_by(author) %>%
            count(sort = TRUE) %>%
            ungroup() %>%
            mutate(percent = sprintf("%.1f%%", (n / sum(n))*100)) %>%
            slice_head(n=20)
    )

    output$author_plot <- renderPlot(
        reddit_comments$data %>%
            group_by(author) %>%
            count(sort = TRUE) %>%
            ungroup() %>%
            mutate(pct = sprintf("%.1f%%", 100 * n/sum(n))) %>%
            mutate(plot_adjust = if_else(n > .5 * max(n), n - .1 * (max(n) - min(n)), n + .1 * (max(n) - min(n)))) %>%
            mutate(plot_colour = if_else(n > .5 * max(n), "white", "black")) %>%
            slice_head(n=10) %>%
            ggplot() +
            geom_col(aes(x=reorder(author,n), y = n), fill = "lightblue") +
            geom_text(aes(x=reorder(author,n), y = plot_adjust, label = pct, fontface = "bold", colour = plot_colour), colour = "black") +
            coord_flip() +
            theme_minimal() +
            labs(y="Count",
                 x = "Author",
                 title = "Top 10 Authors by Search Term Prevalence",
                 subtitle = paste0("Search terms: q=",q,"; subreddit=",subreddit,"; author=",author)) +
            scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))

    )


    ## 1-GRAM and 2-GRAM FREQUENCIES
    # get all 1-grams, remove stop words, count, get top 10
    output$freq1gram_table <- renderTable(
        reddit_comments$data %>%
            select(id, body) %>%
            tidytext::unnest_tokens(word, body) %>%
            dplyr::anti_join(custom_stop_words) %>%
            dplyr::group_by(word) %>%
            dplyr::count(sort = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::slice_head(n = 10)
    )

    # get all 2-grams, remove any with stop words, count, get top 10
    output$freq2gram_table <- renderTable(
        reddit_comments$data %>%
            select(id, body) %>%
            tidytext::unnest_tokens(ngram, body, token = "ngrams", n = 2) %>%
            separate(ngram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
            filter(!word1 %in% custom_stop_words$word,
                   !word2 %in% custom_stop_words$word) %>%
            select(-word1, -word2) %>%
            dplyr::group_by(ngram) %>%
            dplyr::count(sort = TRUE) %>%
            dplyr::ungroup() %>%
            dplyr::slice_head(n = 10)
    )

    ###################
    # word cloud
    output$wordcloud <- wordcloud2::renderWordcloud2({
        reddit_comments$data %>%
            select(id, body) %>%
            tidytext::unnest_tokens(word, body) %>%
            dplyr::anti_join(custom_stop_words) %>%
            dplyr::group_by(word) %>% count(sort = TRUE) %>%
            ungroup() %>%
            slice(-1) %>%
            slice_head(n=200) %>%
            wordcloud2::wordcloud2()
    })


    ##################
    # TIME PLOTS
    output$time_plot <- renderPlot({
        timespan <- difftime(max(reddit_comments$data$created_datetime),
                             min(reddit_comments$data$created_datetime),
                             units = "hours")
        plot_timespan_unit <- dplyr::case_when(
            timespan < 6 ~ "minutes",
            timespan < 24 * 3 ~ "hours",
            timespan < 24 * 14 ~ "days",
            timespan < 24 * 7 * 8 ~ "weeks",
            timespan < 24 * 7 * 4 * 24 ~ "months",
            TRUE ~ "years"
            #
            # == "years" ~ "months",
            # timespan_unit == "months" ~ "days",
            # timespan_unit == "days" ~ "hours",
            # # timespan_unit == "hours" ~ "minutes",
            # TRUE ~ "hours"
        )

        message(sprintf("timespan: %s", timespan))
        message(sprintf("plot_timespan_unit: %s", plot_timespan_unit))

        reddit_comments$data %>% mutate(plot_date = lubridate::floor_date(created_datetime, unit = plot_timespan_unit)) %>%
            group_by(plot_date) %>%
            count() %>%
            ggplot(aes(x=plot_date, y = n)) +
            geom_col(fill = "lightblue") +
           # geom_smooth(method = "loess", formula = "y ~ x" )+
            theme_minimal() +
            labs(title = "Search term results over time",
                 x = "Time")


    })

    output$time_heatmap <- renderPlot({
        NULL

        reddit_comments$data %>%
            select(created_datetime) %>%
            mutate(plot_day = lubridate::wday(created_datetime, label = TRUE)) %>%
            mutate(plot_hour = lubridate::hour(created_datetime)) %>%
            group_by(plot_day, plot_hour) %>%
            count() %>%
            ggplot(aes(x=plot_day, y=plot_hour, fill = n)) +
            geom_tile() +
            theme_minimal() +
            scale_fill_viridis_c(option = "C") +
            scale_x_discrete(drop = FALSE) + # keep unused levels
            scale_y_continuous(limits = c(0, 24))+
            labs(title = "Heat map of search term by weekday/time (EST)",
                 subtitle = "White zones indicate no data found.",
                 x = "Weekday",
                 y = "Hour")

    })


    ### PROCESS AND DOWNLOAD FOR COWO
    output$cowo_download <- downloadHandler(
        filename = function() {

            stringr::str_replace_all(paste("reddit-comments-cowo-",q,"-", Sys.Date(), ".txt", sep=""), '"', "'")
        },
        content = function(file) {

            message(sprintf("cowo_2gram_input = %s", input$cowo_2gram_input))

            cowo_text <- reddit_comments$data %>%
                dplyr::select(id, body)

            if (!is.null(input$cowo_2gram_input) & !(input$cowo_2gram_input == "")){

                #custom_2gram_input <- "coffee maker"
                cowo_2gram_replace <- stringr::str_replace(input$cowo_2gram_input, " ", "_")

                cowo_text <- cowo_text %>%
                    mutate(body = stringr::str_replace_all(body, input$cowo_2gram_input, cowo_2gram_replace))
            }

            cowo_text <- cowo_text %>%
                select(id, body) %>%
                tidytext::unnest_tokens(word, body) %>%
                dplyr::anti_join(custom_stop_words) %>%
                group_by(id) %>%
                nest(data = c(word)) %>%
                ungroup() %>%
                mutate(text = purrr::map_chr(data, function(x) stringr::str_flatten(unlist(x), collapse = " "))) %>%
                select(text)

            write_delim(cowo_text, file, col_names = FALSE)
            }
    )

}

# Run the application
shinyApp(ui = ui, server = server)
