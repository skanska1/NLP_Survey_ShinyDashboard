## Import libraries 
#########################################################################################################
# For data analysis
library(textreadr)  # For read_document
library(tidytext)   # For tokenization, stop_words, get_sentiments, cast_dtm
library(tidyverse)
library(dplyr)      # For data_frame (i.e. tibble), count
library(tidyr)      # For spread
library(reshape2)   # For acast (i.e. convert to matrix)
library(quanteda)
library(tm)
library(stringr)

# For graphic
library(shiny)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(leaflet)    # renderLeaflet function
library(leaflet.extras)  # renderLeaflet function
library(spData)
library(ggplot2)    # tidyverse data visualization package
library(plotly)
library(igraph)     # For graph_from_data_fram
library(ggraph)     # For geom_edge_link
library(wordcloud)  # For word clouds
library(RColorBrewer) # For brewer palette
library(scales)     # For percent_format
library(shinyjs)


## Prepare sentiment lexicons 
#########################################################################################################
# Get separate sentiment lexicons
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Combine all sentiment lexicons
sentiments <- bind_rows(
  mutate(afinn, lexicon="afinn"),
  mutate(nrc, lexicon= "nrc"),
  mutate(bing, lexicon="bing"))

# Get only positive and negative NRC lexicon
nrc_pos_neg <- nrc %>%
  filter(sentiment %in% c("positive", "negative"))


## Functions 
#########################################################################################################
#' Function render yes-no plot for selected and non-selected country
#' @param selected_country (string) Selected country name, Null if no country is selected
#' @return plot object to show in UI
render_yes_no_plot <- function(selected_country) {
  
  if (selected_country == "") {
    # Plot the rank
    q6_ratio <<- q6_token %>%
      count(word, sort = TRUE) %>%
      mutate(ratio = n/n_sample) 
    
    yes_no_plot <- q6_ratio %>%
      ggplot(aes(word, ratio*100, fill = bing_cloud_color)) + 
      geom_bar(stat="identity") +
      scale_x_discrete(limits=levels(q6_token$word),
                       label = toupper(levels(q6_token$word))) +
      geom_text(aes(label=percent(ratio)), hjust=-0.3, size=5) +
      theme_minimal() +
      labs(y=NULL, x=NULL) + 
      theme(legend.position = "none",
            text = element_text(size=20),
            axis.text.x = element_blank()) +
      ylim(0, 100) +
      coord_flip()
  } 
  else {
    # Find index of the selected country
    sample_index <- q1_country %>%
      filter(name == selected_country) %>%
      select(sample_list)
    sample_index <- str_split(sample_index$sample_list, ",")[[1]]
    sample_index <- as.integer(sample_index)
    
    q6_filtered <- q6_token %>%
      filter(sample %in% sample_index)
    
    if (nrow(q6_filtered) != 0) {
      
      # We have some data, go ahead and count!
      q6_ratio <<- q6_filtered %>%
        count(word, sort = TRUE) %>%
        mutate(ratio = n/length(sample_index)) 
      
      # Fill missing frequency for UI purpose
      if (nrow(q6_ratio) == 1 & q6_ratio$word[1] == "yes") {
        q6_ratio <<- bind_rows(
          q6_ratio,
          tibble(word = c("no"), n = c(0), ratio = c(0)))
        
      } else if (nrow(q6_ratio) == 1 & q6_ratio$word[1] == "no") {
        q6_ratio <<- bind_rows(
          q6_ratio,
          tibble(word = c("yes"), n = c(0), ratio = c(0)))
      }
    }
    else {
    
      # Add missing yes or no frequency
      q6_ratio <<- tibble(word = c("yes", "no"),
                          n = c(0, 0),
                          ratio = c(0, 0))
      q6_ratio$word <- factor(q6_ratio$word)
    }
    
    # Plot the ratio
    yes_no_plot <- q6_ratio %>%
      ggplot(aes(word, ratio*100, fill = bing_cloud_color)) + 
      geom_bar(stat="identity") +
      scale_x_discrete(limits=levels(q6_token$word),
                       label = toupper(levels(q6_token$word))) +
      geom_text(aes(label=percent(ratio)), hjust=-0.3, size=5) +
      theme_minimal() +
      labs(y=NULL, x=NULL) + 
      theme(legend.position = "none",
            text = element_text(size=20),
            axis.text.x = element_blank()) +
      ylim(0, 100) +
      coord_flip()
  }
  
  plot <- renderPlot({
    yes_no_plot
  })
  
  return(plot)
}

#' Function render missing value text 
#' #' @return missing value text object to show in UI
render_missing_value_text <- function() {
  return(renderText({
    # Compute missing ratio
    missing_ratio <- 1 - sum(q6_ratio$ratio)
    sprintf(missing_text, percent(missing_ratio))
  }))
}

#' Function render leaflet map for selected and non-selected country
#' @param selected_country (string) Selected country name, Null if no country is selected
#' @return leaflet object to show in UI
render_leaflet_map <- function(selected_country) {
  
  if (selected_country == "") {
    # No country selected, show all countries as red
    map_leaflet <- leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% #"Esri.WorldStreetMap" 
      addPolygons(data = st_as_sfc(q1_country$geom), 
                  color = "red", 
                  fillOpacity = 0.8, 
                  weight = 2, 
                  label = sprintf("%1$s: %2$d", q1_country$name_long, q1_country$count),
                  layerId = q1_country$name)
    
  }
  else {
    # Country selected, show selected one as red, the rest as grey
    selected_country_df <- q1_country %>%
      filter(name == selected_country)
    rest_country_df <- q1_country %>%
      filter(name != selected_country)
    
    # Render polygon
    
    map_leaflet <- leaflet() %>% 
      addProviderTiles("CartoDB.Positron") %>% #"Esri.WorldStreetMap" 
      addPolygons(data = st_as_sfc(rest_country_df$geom), 
                  color = "grey", 
                  fillOpacity = 0.5, 
                  weight = 1, 
                  label = sprintf("%1$s: %2$d", rest_country_df$name_long, rest_country_df$count),
                  layerId = rest_country_df$name,
                  group = "rest_country") %>%
      addPolygons(data = st_as_sfc(selected_country_df$geom), 
                  color = "red", 
                  fillOpacity = 0.8, 
                  weight = 2, 
                  label = sprintf("%1$s: %2$d", selected_country_df$name_long, selected_country_df$count),
                  layerId = selected_country_df$name,
                  group = "selected_country") 
  }
    
  map <- renderLeaflet({
    map_leaflet
  })
  
  return(map)
}


## Get file, formatting, setup variables
#########################################################################################################
# Set working directory to where the text file is
setwd("C:/Users/Skander Pc/Desktop/Text analytics")

# Read text file
nlp_ans <- read_document(file = "NLP answers.txt")

# Replace all answer that could not be translated to null
nlp_ans[nlp_ans == "Oops! Didn't catch that"] <- "NULL"

# Remove "You said" amd the last " from the answers
nlp_ans <- substr(nlp_ans, start=11 , stop = nchar(nlp_ans)-1)

# Create a dataframe with specified dimension
n_question <- 6
n_sample <- 30
nlp_matrix <- matrix(nlp_ans, nrow = n_sample, ncol = n_question, byrow = TRUE)
nlp_df <- as.data.frame(nlp_matrix)

# Change column names
names(nlp_df) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6")

# Set custom colors
nrc_cloud_color  <- brewer.pal(8, "Dark2") %>% append(c("black", "skyblue"))
bing_cloud_color <- rev(brewer.pal(3, "Dark2")[1:2])

# Remove unnecessary column from world data
country_df <- world %>%
  select(name_long, geom)

# Set missing value text format
missing_text <- "%1$s Missing Value"


## Data Preparation for Map
## From Question 1: Where are you from?
#########################################################################################################
# Create dataframe of first question
q1_df <- tibble(sample = 1:n_sample, 
                text = as.character(nlp_df$Q1))

# Don't know why "states" is in stop word, so we need to exclude it
stop_words_without_states <- stop_words %>%
  filter(word != "states")

# Tokenize the answers
q1_token <- q1_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_without_states, by = "word") 

# Collapse country name that might be separated by tokennization
q1_country_name <- q1_token %>% 
  group_by(sample) %>% 
  summarise(name = paste(word, collapse = " "))

q1_country <- q1_country_name %>%
  group_by(name) %>%
  summarise(sample_list = paste(sample, collapse = ","),
            count = n())

# Combine the answer country list with geographic data
for (i in 1:nrow(q1_country)) {
  
  # Get index of the matched country
  index <- grep(q1_country$name[i], tolower(country_df$name_long))
  
  if(!identical(index, integer(0))) {
    q1_country$name_long[i] <- country_df$name_long[index]
    q1_country$geom[i] <- country_df$geom[index]
  }
}


## Data Preparation For Yes-No Plot
#########################################################################################################
# Create dataframe of sixth question
q6_df <- tibble(sample = 1:n_sample, 
                text = as.character(nlp_df$Q6))

# Tokenize the answers
q6_token <- q6_df %>%
  unnest_tokens(word, text) %>%
  inner_join(tibble(word = c("yes", "no")), by = "word") 

q6_token$word <- as.factor(q6_token$word)


## Data Preparation For Benefits and Difficulties Plot
#########################################################################################################
q2_df <- tibble(sample = 1:n_sample, 
                text = as.character(nlp_df$Q2))

q2_token <- q2_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) 

q2_token$word[q2_token$word == "difficulties"] <- "difficult"
q2_token$word[q2_token$word == "languages"] <- "language"
q2_token$word[q2_token$word == "communicate"] <- "communication"
q2_token$word[q2_token$word == "culture"] <- "cultures"

#Tokenize q3
q3_df <- tibble(sample = 1:n_sample, 
                text = as.character(nlp_df$Q3))
#Creating customized stop words for Q3
cust_stop_q3 <-tibble(word = c("lot"))

#Combined same words together
q3_token_1 <- q3_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop_q3)

q3_token_1$word[q3_token_1$word == "understanding"] <- "understand"
q3_token_1$word[q3_token_1$word == "benefit"] <- "benefits"
q3_token_1$word[q3_token_1$word == "country"] <- "countries"
q3_token_1$word[q3_token_1$word == "culture"] <- "cultures"


## Data Preparation For Sentiments Wordclouds
#########################################################################################################
# Create dataframe of forth question
q4_df <- tibble(sample = 1:n_sample, 
                text = as.character(nlp_df$Q4))

# Tokenize the answers
q4_token <- q4_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") 


## Data Preparation For Culture Differences Bi-Gram
#########################################################################################################
q56_df <- tibble(sample = 1: n_sample, 
                 text5 = as.character(nlp_df$Q5),
                 text6 = as.character(nlp_df$Q6))

# Convert last question to factor and add into outcome column
for (i in 1:n_sample) {
  
  # Save as factor "YES
  index <- grep("yes", tolower(nlp_df$Q6[i]))
  if(!identical(index, integer(0))) {
    q56_df$text6[i] <- "YES"
    next
  }
  
  # Save as factor "NO"
  q56_df$text6[i] <- "NO"
}
q56_df$text6 <- as.factor(q56_df$text6)

# Tokenize into bi-grams
q_bigrams <- q56_df %>%
  unnest_tokens(bigram, text5, token = "ngrams", n=2)

# All
q_bigrams_count <- q_bigrams %>% 
  select(sample, bigram) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1) & !is.na(word2))%>%
  count(word1, word2, sort = T)

# Yes
q_bigrams_count_yes <- q_bigrams %>% 
  filter(text6 == "YES") %>%
  select(sample, bigram) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1) & !is.na(word2))%>%
  count(word1, word2, sort = T)

# No
q_bigrams_count_no <- q_bigrams %>% 
  filter(text6 == "NO") %>%
  select(sample, bigram) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1) & !is.na(word2))%>%
  count(word1, word2, sort = T)


## Data Preparation For Naive Bayes Model 
#########################################################################################################
# Create dataframe that combine all question anwers
combined_nlp_df <- tibble(id = 1:n_sample, 
                          text = paste(nlp_df$Q1, nlp_df$Q2, nlp_df$Q3, nlp_df$Q4, nlp_df$Q5))

# Convert last question to factor and add into outcome column
for (i in 1:n_sample) {
  
  # Save as factor "YES
  index <- grep("yes", tolower(nlp_df$Q6[i]))
  if(!identical(index, integer(0))) {
    combined_nlp_df$outcome[i] <- "YES"
    next
  }
  
  # Save as factor "NO"
  combined_nlp_df$outcome[i] <- "NO"
}
combined_nlp_df$outcome <- as.factor(combined_nlp_df$outcome)

# Convert to Corpus
nlp_corpus <- Corpus(VectorSource(combined_nlp_df$text)) 

# Convert to document-feature matrix
nlp_dfm <- dfm(corpus(nlp_corpus), tolower = TRUE)

# Weight the docuemtn
nlp_dfm <- dfm_trim(nlp_dfm, min_termfreq = 2, min_docfreq = 1)
nlp_dfm <- dfm_weight(nlp_dfm)

# Setup factor for prediction result
performance_factor <- factor(c("True Positive", "True Negative", "False Positive", "False Negative"),
                             levels = c("True Positive", "True Negative", "False Positive", "False Negative"))


## Server Output
#########################################################################################################
shinyServer(function(input, output) {
  
  # Initially disable selected all button
  disable("selected_all")

  ## Overview Tab
  ##################################################################################
  # Render Yes-No Plot
  output$yes_no_plot = render_yes_no_plot("")
  
  # Render Missing Value Ratio
  output$missing = render_missing_value_text()
  
  # Render country map
  output$map = render_leaflet_map("")
  
  # Handle map click event
  observe({
    
    event <- input$map_shape_click
    if(!is.null(event$id)) {
      # Render selected country
      output$map = render_leaflet_map(event$id)
      
      # Render yes-no plot
      output$yes_no_plot = render_yes_no_plot(event$id)
      
      # Render Missing Value Ratio
      output$missing = render_missing_value_text()
      
      # Enable selected all button
      enable("selected_all")
    }
    
  })
  
  # Handle selected_all button clicked
  observeEvent(input$selected_all, {
    # Render all countries
    output$map = render_leaflet_map("")
    
    # Render yes-no plot
    output$yes_no_plot = render_yes_no_plot("")
    
    # Render Missing Value Ratio
    output$missing = render_missing_value_text()
    
    # Disable selected all button
    disable("selected_all")
  })
  
  
  ## Benefits and Difficulties Tab
  ##################################################################################
  #Q2
  output$pros_cons <- renderPlot({
    if (input$selected_pros_cons == 'Difficulties'){
      q2_token %>%
        count(word, sort = TRUE)%>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n), n, fill=word)) +
        theme_minimal() +
        scale_fill_brewer(palette="Paired")+
        ggtitle('Difficulties')+
        theme(plot.title = element_text(size = 30, face = "bold", vjust = 1, lineheight = 0.6),
              axis.line = element_line("darkgrey", 1)) +
        theme(axis.text.x=element_text(size=15, vjust=0.5),
              axis.text.y=element_text(size=15, vjust=0.5))+
        theme(axis.title.x=element_text(size=20, vjust=0.5,face = "bold"),
              axis.title.y=element_text(size=20, vjust=0.5,face = "bold"))+
        geom_col(show.legend = FALSE) + 
        ylab('Frequency') + 
        xlab(NULL)+
        coord_flip()
    }
    else {
      q3_token_1 %>%
        count(word, sort = TRUE) %>%
        top_n(10) %>%
        ggplot(aes(reorder(word, n),n, fill=word)) +
        theme_minimal() +
        scale_fill_brewer(palette="Paired")+
        ggtitle('Benefits')+
        theme(plot.title = element_text(size = 30, face = "bold", vjust = 1, lineheight = 0.6),
              axis.line = element_line("darkgrey", 1)) +
        theme(axis.text.x=element_text(size=15, vjust=0.5),
              axis.text.y=element_text(size=15, vjust=0.5))+
        theme(axis.title.x=element_text(size=20, vjust=0.5,face = "bold"),
              axis.title.y=element_text(size=20, vjust=0.5,face = "bold"))+
        geom_col(show.legend = FALSE) +
        ylab('Frequency') +
        xlab(NULL)+
        coord_flip()
    }
  })
  
  
  ## Sentiment Tab
  ##################################################################################
  #Render an image of wordcloud with sentiments about Q4
  output$sentiment <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 3000, height = 4000)
    q4_token %>%
      inner_join(nrc) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors = nrc_cloud_color,
                       max.words=100,
                       scale = c(10, 10),
                       title.size = 11,
                       match.colors = TRUE,
                       title.bg.colors = "white")
    
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 600,
         height = 600,
         alt = "This is alternate text")
  }, 
  deleteFile = TRUE)
  
  
  ## Culture Differences Tab
  ##################################################################################
  output$ngram <- renderPlot({
    
    if(input$yesno == "Yes") {
      q_bigrams_graph_yes <- q_bigrams_count_yes %>%
        graph_from_data_frame()
      yes_bigram <- ggraph(q_bigrams_graph_yes, layout = "fr") +
        geom_edge_link()+
        geom_node_point()+
        geom_node_text(aes(label=name), vjust =1, hjust=1, size=6) + 
        theme(panel.background = element_rect(fill = "#F4F4F4"))
      yes_bigram
    }
    else if(input$yesno == "No") {
      q_bigrams_graph_no <- q_bigrams_count_no %>%
        graph_from_data_frame()
      no_bigram <- ggraph(q_bigrams_graph_no, layout = "fr") +
        geom_edge_link()+
        geom_node_point()+
        geom_node_text(aes(label=name), vjust =1, hjust=1, size=6) + 
        theme(panel.background = element_rect(fill = "#F4F4F4"))
      no_bigram
    }
    else{
      # Plot bigrams diagram
      q_bigrams_graph <- q_bigrams_count %>%
        graph_from_data_frame()
      all_bigram <- ggraph(q_bigrams_graph, layout = "fr") +
        geom_edge_link()+
        geom_node_point()+
        geom_node_text(aes(label=name), vjust =1, hjust=1, size=6) + 
        theme(panel.background = element_rect(fill = "#F4F4F4"))
      all_bigram
    }
  })#closing the n-gram renderPlot
  
  
  ## Naive Bayes Classification Tab
  ##################################################################################
  # Render Naive Bayes Classification
  output$nbm = renderPrint({
    
    # Random index for training
    train_index <<- sample(1:n_sample, size = input$training_number)
    nlp_dfm_train <- nlp_dfm[train_index]
    nlp_dfm_test  <<- nlp_dfm[-train_index]
    
    # Train the model
    nb_model <<- textmodel_nb(nlp_dfm_train, combined_nlp_df$outcome[train_index])
    model_summary <- summary(nb_model)
    model_summary
  })
  
  # Render Naive Bayes Classification Test Result
  output$predict = renderPrint({
    # Make it re-render
    input$training_number
    
    # Predict test data
    test_predict <<- predict(nb_model, nlp_dfm_test)
    test_predict
  })
  
  # Render Naive Bayes Classification Test Result
  output$predict_plot = renderPlot({
    # Make it re-render
    input$training_number
    
    # Compare predict result with real output
    tp <- 0
    tn <- 0
    fp <- 0
    fn <- 0
    test_index <- setdiff(1:n_sample, train_index) 
    for (i in 1:length(test_index)) {
      if (test_predict[i] == combined_nlp_df$outcome[test_index[i]]
          & test_predict[i] == "YES") {
        tp <- tp + 1
      } 
      else if (test_predict[i] == combined_nlp_df$outcome[test_index[i]]
               & test_predict[i] == "NO") {
        tn <- tn + 1
      }
      else if (test_predict[i] == "YES" 
               & combined_nlp_df$outcome[test_index[i]] == "NO") {
        fp <- fp + 1
      }
      else {
        fn <- fn + 1
      }
    }
    
    # Plotting performance
    tibble(group = c("Correct", "Correct", "Incorrect", "Incorrect"),
           result = performance_factor,
           n = c(tp, tn, fp, fn)) %>%
      ggplot(aes(result, n, fill = group)) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      geom_text(aes(label=n), vjust=-0.7, size=7) +
      theme(text = element_text(size=20),
            axis.line = element_line("darkgrey", 1)) +
      ylim(0, length(test_index) + 0.25) +
      labs(y=NULL, x=NULL)
  })
  
})
