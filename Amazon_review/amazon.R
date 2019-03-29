library(shinydashboard)
library(shiny)
library(rio)
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(SnowballC)
library(syuzhet)
library(wordcloud2)
library(RColorBrewer)
library(stringr)
library(plotly)
library(googleVis)
library(tidytext)
library(tidyr)
data("stop_words")
library(radarchart)
library(webshot)
library(htmlwidgets)
library(fmsb)
library(wordcloud2)
webshot::install_phantomjs()

df<- read.csv("~/Desktop/all/winter_break/amazon/amazon_alexa.tsv")
#converting to csv format
convert("amazon_alexa.tsv","amazon_alexa.csv")
df<- read.csv("~/Desktop/all/winter_break/amazon/amazon_alexa.csv", stringsAsFactors = FALSE)
View(df)

#2:Frequency
tidy_reviews<- df %>%
  select(date,verified_reviews) %>%
  unnest_tokens("word", verified_reviews)

top_words_of_reviews<-tidy_reviews %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

top_20<-top_words_of_reviews[1:20,]


#emotions:
sample<-df%>%
  mutate(score=get_sentiment(df$verified_reviews))

sample1<-get_nrc_sentiment(df$verified_reviews)%>%
  select(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)

sample$senti<-ifelse(sample$score>0,"positive",ifelse(sample$score<0,"negative","netural"))

new_df<-merge(sample,sample1,by.x=0,by.y = 0)

positive_verified_reviews<-filter(new_df,senti=="positive")%>%
  select(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
negative_verified_reviews<-filter(new_df,senti=="negative")%>%
  select(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)
netural_verified_reviews<-filter(new_df,senti=="netural")%>%
  select(anger,anticipation,disgust,fear,joy,sadness,surprise,trust)

labs <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
scores <- list(
  "positive" = c(colSums(positive_verified_reviews)),
  "negative" = c(colSums(negative_verified_reviews)),
  "netural" = c(colSums(netural_verified_reviews))
)

data <- data.frame("Label"=c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust"),
                     "positive" = c(colSums(positive_verified_reviews)),
                     "negative" = c(colSums(negative_verified_reviews)),
                     "netural" = c(colSums(netural_verified_reviews)))


#3: Time Series
review_sentiment <- tidy_reviews %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment)

tidy_reviews$Date<-as.Date(paste(tidy_reviews$date),format="%d-%b-%y ")

tidy_reviews$New_Date<-as.Date(tidy_reviews$Date,format="%Y-%m-%d") 

#4: cloud
Word.df <- as.vector(top_words_of_reviews$word)
top_words_of_reviews$senti_value<-as.data.frame(get_sentiment(Word.df))
top_words_of_reviews$Sentiment[top_words_of_reviews$senti_value>0]<-"positive"
top_words_of_reviews$Sentiment[top_words_of_reviews$senti_value<0]<-"negative"
top_words_of_reviews$Sentiment[top_words_of_reviews$senti_value==0]<-"netural"

# top_words_of_reviews$Sentiment<-ifelse(top_words_of_reviews$senti_value>0,"positive",ifelse(top_words_of_reviews$senti_value,"negative","netural"))


positive_cloud<-top_words_of_reviews%>%
  filter(Sentiment=="positive")%>%
  select(word,n)
negative_cloud<-top_words_of_reviews%>%
  filter(Sentiment=="negative")%>%
  select(word,n)
netural_cloud<-top_words_of_reviews%>%
  filter(Sentiment=="netural")%>%
  select(word,n)



ui<- dashboardPage(title = 'This is my Page title',
  dashboardHeader(
    title = "Amazon Alexia Reviews"
   ),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Top reviews", tabName = "reviews", icon = icon("align-left")),
    menuItem("Frequency",tabName = "frequency",icon = icon("calculator")),
    menuItem("Time Analysis",tabName = "time",icon = icon("chart-line")),
    menuItem("Emotions",tabName = "emotions",icon = icon("smile-beam")),
    menuItem("Wordcloud",tabName = "cloud",icon=icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "reviews",
          fluidRow(
              column(width=9,
                box(width = 18,
                dataTableOutput("reviewstable"))
                    ),#end of the column
                box(
                  width = 3,
                  title = "Data Filter",
                  selectInput("reviewtype",em("Select Type of review "),choices = c("Positive","Negative","Netural")),
                  # sliderInput("slider", label = em("Sentiment Range"), min = -5, 
                  #             max = 5, value = c(-1, 1)),
                  uiOutput("sliderrange")
                    )
                )
        
      ),#end of first tab
      
      tabItem(tabName = "frequency",
              box(width=18,
                  dataTableOutput("table")),
              box(width=18,
                  h2(strong('Distribution of Top 10 words Frequency'), 
                              align ="center"),
                  plotlyOutput("freq",
                               height=500))
              ),#endofsecondtab
      
      
      tabItem(tabName = "time",
              fluidRow(
                column(width=9,
                       box(width = 12,
                           h2(strong('Distribution of Word Frequency of Over the Months'), 
                              align ="center"),
                           plotlyOutput("Time",
                                      height=500))
                ),#endofcolumn,
                box(
                  width = 3,
                  title = "Data Filter",
                  selectInput("time_type",em("Select Type of words "),choices = c("Positive","Negative"))
                )
              )#endoffluidrow
              ),#endofthirdtab,
      tabItem(tabName = "emotions",
              fluidRow(
                
                # column(
                #   width=9,
                  
                  # fluidRow(
                  #   valueBoxOutput("Positive",width=3),
                  #   valueBoxOutput("Negative",width=3),
                  #   valueBoxOutput("Netural",width=3)
                  # ),
                  
                  # fluidRow(
                    box(
                      width = 12,
                      h2(strong('Distribution of All other Emotions'), 
                         align ="center"),
                      h6(strong("Hover for the distribution of particular emotion "),align="center"),
                      # plotlyOutput("allemotion",
                      #              height=500)
                      chartJSRadarOutput("radar", width = "450", height = "450")
                    # )
                  # )
                  
                )#end of column
              )#end of fluidrow
      ),#endoffourth tab
      tabItem(tabName = "cloud",
              fluidRow(
                column(width=9,
                       box(width = 15,
                           # plotOutput("Cloud",
                           #            height=500),
                           wordcloud2Output("word_cloud"),
                           # radioButtons("format","format:",c("png","bmp","pdf","jpeg")),
                           # checkboxInput('savePlot', "Check to save"),
                           downloadButton("download",strong("Download Image"))
                       )
                ),#endofcolumn,
                box(
                  width = 3,
                  title = "Data Filter",
                  selectInput("type",em("Select Type of words "),choices = c("Positive","Negative","Netural"))
                )
              )#endoffluidrow
      )#endotab
    )#endoftabitems
  ),#end of dashboard
  skin="yellow"
)#end of dashboardpage




server <- function(input, output,session) {
  
  output$sliderrange<-renderUI({
    
    if(input$reviewtype=="Negative")
      {
      sliderInput("slider", label = em("Sentiment Range"), min = -5, 
                  max = 0, value = c(-3, -1))
      }
      else{
          if(input$reviewtype=="Positive")
            {
            sliderInput("slider", label = em("Sentiment Range"), min = 0, 
                      max = 5, value = c(1, 3))
          }
        else{
          sliderInput("slider", label = em("Sentiment Range"), min = 0, 
                      max = 0, value = c(0, 0))
        }
      }
    
  })
    
  res <- eventReactive(input$slider, {
    df%>%
    mutate(score=get_sentiment(verified_reviews))%>%
    filter(score >= input$slider[1] & score <= input$slider[2] )%>%
    select(verified_reviews, score)
  })
  
  output$reviewstable <- renderDataTable({
    res()
  })
#########################################################  
  
  
  output$table<-renderDataTable({
      top_words_of_reviews%>%
      select(word,n,Sentiment)
  })
  
  
  output$freq<-renderPlotly(
    {ggplot(top_20, aes(x=reorder(word,-n), y=n, fill=word))+
      geom_bar(stat="identity", colour="black")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Number of Times Word Appears in Reviews")+
      xlab("Top 10 Words")+
      theme(legend.position="none")+
      guides(fill=FALSE)
    }
  )
  
##################################################################### 
  
  
  review_sentiment_plot <-reactive(
    if(input$time_type=="Negative"){
        tidy_reviews %>%
        inner_join(get_sentiments("bing")) %>% 
        filter(sentiment=="negative") %>%
        count(New_Date, sentiment)
    }
    else{
        tidy_reviews %>%
        inner_join(get_sentiments("bing")) %>% 
        filter(sentiment=="positive") %>%
        count(New_Date, sentiment)
    }
  )
  
  output$Time<-renderPlotly({
      ggplot(review_sentiment_plot(), aes(x=New_Date, y=n))+
      geom_line(color="red")+
      theme_minimal()+
      ylab("Frequency of Words in reviews")+
      xlab("Date")
  })
###############################################  
  output$radar <- renderChartJSRadar({
  chartJSRadar(data, maxScale = 3000,showToolTipLabel=TRUE)
    # chartJSRadar(scores = scores[[1]], labs = labs, maxScale = 100,showToolTipLabel=TRUE)
    })
  
  # saveWidget(output$radar, "plt.html")
  # 
  # webshot("plt.html")
  # 
  # magick::image_read("webshot.png")
###############################################  
  
  
  output$word_cloud = renderWordcloud2({
    if(input$type=="Positive"){
      mygraph=wordcloud2(positive_cloud, shape="star",size = 1.5, color = "green")#figPath = "peace.png"
      # wordcloud2(positive_cloud,minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
      # rotateRatio = 1)
    }
    else{
      if(input$type=="Negative"){
        mygraph=wordcloud2(negative_cloud, shape='triangle',color="red")
      }
      else{
        mygraph=wordcloud2(netural_cloud, size=1.6, color='random-light', backgroundColor="black")
      }
    }
    
  })
  
  output$download <- downloadHandler(
    filename = function() { paste("WordCloud",'.png') },
    content = function(file) {
      saveWidget(my_graph,"tmp.html",selfcontained = F)
      webshot("tmp.html","fig_1.png", delay =5, vwidth = 480, vheight=480) 
    })
  
  
}

shinyApp( ui,  server)
