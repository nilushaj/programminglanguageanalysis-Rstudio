library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(XML)
library(lubridate)
library(reshape2)
library(corrplot)
library(plotrix)
library(shinyjs)
library(ggmap)
library(rworldmap)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)


server <- function(input, output, session) {
  

  
  
  # mapping function to extract programming langauge from tag list
  ds_language <- function(x) {
    if(!is.na(match("r",tolower(x)))){
      "r"
    }else if(!is.na(match("python",tolower(x)))){
      "python"
    }else if(!is.na(match("java",tolower(x)))){
      "java"
    }else if(!is.na(match("c++",tolower(x)))){
      "c++"
    }else if(!is.na(match("go",tolower(x)))){
      "go"
    }else if(!is.na(match("matlab",tolower(x)))){
      "matlab"
    }
    else if(!is.na(match("c#",tolower(x)))){
      "c#"}
    else if(!is.na(match("octave",tolower(x)))){
      "octave"
    }else if(!is.na(match("ruby",tolower(x)))){
      "ruby"
    }
    else if(!is.na(match("c",tolower(x)))){
      "c"
    }else {
      "rest_of_the_world"
    }
  }
  
  # function to perform reverse geocoding
  coords2country = function(points)
  {  
    countriesSP <- getMap(resolution='low')
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    as.character(indices$ADMIN)  #returns country name
  }
 
  # mapping function to set country for given location
  postLocation <- function(locationName){
    if(!is.na(locationName)){
      tryCatch(coords2country(geocode(locationName, output = "latlon", source = "dsk")),
               warning = function(w) {
                 
                 print("warning"); 
                 # handle warning here
               },
               error = function(e) {
                 print("error");
                 # handle error here
               })
    }
  }
  
  # utility function to sort data
  reorder_size <- function(x) {
    factor(x, levels = names(sort(table(x))))
  }

  # load Posts.csv
  PostsDF <- read.csv("Posts.csv")
  # load Users.csv
  UsersDF <- read.csv("Users.csv")
  
  
  PostsDF$AnswerCount[is.na(PostsDF$AnswerCount)] <- 0
  PostsDF$ViewCount[is.na(PostsDF$ViewCount)] <- 0
  PostsDF$FavoriteCount[is.na(PostsDF$FavoriteCount)] <- 0
  
  
  
  
  
  
  # change data type
  PostsDF$CreationDate <- strptime(PostsDF$CreationDate, 
                                   "%Y-%m-%d")
  PostsDF$CreationDate <-as.POSIXct(PostsDF$CreationDate )
  PostsDF$CreationDate <- as.Date(PostsDF$CreationDate)
  
 
    
 
  
 
 
  
  # cleanup the tag column
  PostsDF$tag_list <- lapply(str_split(PostsDF$Tags,"<|>"),
                             function(x){x%>%unlist()}) %>% 
    lapply(.,function(x){x[x!=""]})
  
  # extract language tags from tag list
  PostsDF$prog_lang <- sapply(PostsDF$tag_list,ds_language)
  
  
  # Posts by date
  
  output$postplot <- renderPlot({
  
    
    ggplot(data = PostsDF, aes(x = CreationDate)) +
      geom_histogram(aes(fill = ..count..), bins = 50) +
      theme(legend.position = "none") +
      xlab("Time") + ylab("Number of Posts") + 
      scale_fill_gradient(low = "midnightblue", high = "aquamarine4")+
      ggtitle("Posts By Time") +
      theme_bw()
    
  })
  output$postplotd <- renderPlot({
    PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
    
    ggplot(data = PostsDF, aes(x = CreationDate)) +
      geom_histogram(aes(fill = ..count..), bins = 50) +
      theme(legend.position = "none") +
      xlab("Time") + ylab("Number of Posts") + 
      scale_fill_gradient(low = "midnightblue", high = "aquamarine4")+
      ggtitle(paste("Posts By Time - From", input$fromdate, "To", input$todate)) +
      theme_bw()
    
  })
  
  # Posts by date for language
  output$langplot <- renderPlot({
    check <- PostsDF[(PostsDF$prog_lang ==input$select) & (PostsDF$prog_lang !="rest_of_the_world"),]
    validate(
      need(nrow(check)!=0, "No Records Found!!")
    )
    ggplot(data = PostsDF[(PostsDF$prog_lang== input$select ) & (PostsDF$prog_lang !="rest_of_the_world"),], aes(x = CreationDate)) +
      geom_histogram(aes(fill = ..count..), bins = 50) +
      theme(legend.position = "none") +
      xlab("Time") + ylab("Number of Posts") +
      ggtitle(paste("Posts by date for", toupper(input$select), sep=" "))+
      scale_fill_gradient(low = "midnightblue", high = "aquamarine4") + 
      theme_bw()
  })
  output$langplotd <- renderPlot({
    
    PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
    check <- PostsDF[(PostsDF$prog_lang ==input$select) & (PostsDF$prog_lang !="rest_of_the_world"),]
    validate(
      need(nrow(check)!=0, "No Records Found!!")
    )
    ggplot(data = PostsDF[(PostsDF$prog_lang== input$selectd ) & (PostsDF$prog_lang !="rest_of_the_world"),], aes(x = CreationDate)) +
      geom_histogram(aes(fill = ..count..), bins = 50) +
      theme(legend.position = "none") +
      xlab("Time") + ylab("Number of Posts") +
      ggtitle(paste("Posts by date for", toupper(input$selectd)," From ",input$fromdate,"To",input$todate))+
      scale_fill_gradient(low = "midnightblue", high = "aquamarine4") + 
      theme_bw()
  })
  
  
  # posts by language over time
  
  output$trendplot <- renderPlot({
    
    langDF <- PostsDF[,c('CreationDate', 'prog_lang')]
    langDF$date <- format(langDF$CreationDate, '%b-%Y')
    langDF <- langDF[langDF$prog_lang != 'rest_of_the_world',]
    aggLangDF <- aggregate(langDF$prog_lang, by=list(langDF$date, langDF$prog_lang), length)
    colnames(aggLangDF) <- c('date', 'tag', 'count')
    aggLangDF$date <- as.Date(paste("01",aggLangDF$date, sep = "-"),"%d-%b-%Y")
    
    ggplot(aggLangDF, aes(x=date, y=count, group=tag)) +
      geom_point(aes(shape=tag)) +
      geom_line(aes(color=tag)) + 
      ggtitle("Language wise Post Trends over Time") +
      theme_bw()
    
  })
  output$trendplotd <- renderPlot({
    PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
    
    langDF <- PostsDF[,c('CreationDate', 'prog_lang')]
    langDF$date <- format(langDF$CreationDate, '%b-%Y')
    langDF <- langDF[langDF$prog_lang != 'rest_of_the_world',]
    aggLangDF <- aggregate(langDF$prog_lang, by=list(langDF$date, langDF$prog_lang), length)
    colnames(aggLangDF) <- c('date', 'tag', 'count')
    aggLangDF$date <- as.Date(paste("01",aggLangDF$date, sep = "-"),"%d-%b-%Y")
    
    ggplot(aggLangDF, aes(x=date, y=count, group=tag)) +
      geom_point(aes(shape=tag)) +
      geom_line(aes(color=tag)) + 
      ggtitle(paste("Language wise Post Trends over Time -"," From ",input$fromdate,"To",input$todate))+
      theme_bw()
    
  })
  # Posts by language
  output$postlanplot <- renderPlot({
    
    ggplot(PostsDF[PostsDF$prog_lang !="rest_of_the_world",], aes(reorder_size(prog_lang))) +
      geom_bar(aes(fill = prog_lang)) +
      stat_count(aes(label = ..count..), geom = "text")+
      theme(legend.position="none", axis.title.x = element_blank()) +
      ylab("Number of Posts") +
      xlab("Programming Languages")+
      ggtitle("Posts By Language") +
      theme_bw()
  })
  output$postlanplotd <- renderPlot({
    PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
    ggplot(PostsDF[PostsDF$prog_lang !="rest_of_the_world",], aes(reorder_size(prog_lang))) +
      geom_bar(aes(fill = prog_lang)) +
      stat_count(aes(label = ..count..), geom = "text")+
      theme(legend.position="none", axis.title.x = element_blank()) +
      ylab("Number of Posts") +
      xlab("Programming Languages")+
      ggtitle(paste("Posts By Language -"," From ",input$fromdate,"To",input$todate))+
      theme_bw()
  })
  
  
  # Language wise avg time to get answers
  
  # average time to get answers by language per year
 
   output$timeplot <- renderPlot({
     mergeddf <-merge(PostsDF[,c('Id',
                                 'CreationDate',
                                 'PostTypeId',
                                 'Score',
                                 'ViewCount',
                                 'OwnerUserId',
                                 'ParentId',
                                 'AcceptedAnswerId',
                                 'prog_lang')],
                      PostsDF[,c('Id',
                                 'CreationDate',
                                 'PostTypeId',
                                 'Score',
                                 'ViewCount',
                                 'OwnerUserId',
                                 'ParentId',
                                 'AcceptedAnswerId',
                                 'prog_lang')],by.x='AcceptedAnswerId',by.y='Id')
     
     mergeddf$time_to_answer <- difftime(mergeddf$CreationDate.y,
                                         mergeddf$CreationDate.x,
                                         units = "mins")
     
     outlierdf <- mergeddf[mergeddf$time_to_answer>129600,]
     
     mergeddf <- mergeddf[mergeddf$time_to_answer<=129600,]
     
     agg_time <- aggregate(mergeddf$time_to_answer, 
                           by=list(year(mergeddf$CreationDate.x),
                                   mergeddf$prog_lang.x), 
                           mean)
     colnames(agg_time) <- c('year','language', 'avg_time_to_answer')
     ggplot(data=agg_time[agg_time$language!='rest_of_the_world',], 
            aes(x=language, y=as.numeric(avg_time_to_answer)/60,
                fill=as.factor(year))) +
       geom_bar(stat="identity", 
                position=position_dodge())+
              theme(legend.position="right",
             axis.title.x = element_blank()) +
       ylab("Avg Hours to Accepted Answer") +
       labs('custom text')+
       ggtitle("Avg Time to get answers by Language")+ 
       theme_bw()
  })
   output$timeplotd <- renderPlot({
     PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
     mergeddf <-merge(PostsDF[,c('Id',
                                 'CreationDate',
                                 'PostTypeId',
                                 'Score',
                                 'ViewCount',
                                 'OwnerUserId',
                                 'ParentId',
                                 'AcceptedAnswerId',
                                 'prog_lang')],
                      PostsDF[,c('Id',
                                 'CreationDate',
                                 'PostTypeId',
                                 'Score',
                                 'ViewCount',
                                 'OwnerUserId',
                                 'ParentId',
                                 'AcceptedAnswerId',
                                 'prog_lang')],by.x='AcceptedAnswerId',by.y='Id')
     
     mergeddf$time_to_answer <- difftime(mergeddf$CreationDate.y,
                                         mergeddf$CreationDate.x,
                                         units = "mins")
     
     outlierdf <- mergeddf[mergeddf$time_to_answer>129600,]
     
     mergeddf <- mergeddf[mergeddf$time_to_answer<=129600,]
     
     agg_time <- aggregate(mergeddf$time_to_answer, 
                           by=list(year(mergeddf$CreationDate.x),
                                   mergeddf$prog_lang.x), 
                           mean)
     colnames(agg_time) <- c('year','language', 'avg_time_to_answer')
     ggplot(data=agg_time[agg_time$language!='rest_of_the_world',], 
            aes(x=language, y=as.numeric(avg_time_to_answer)/60,
                fill=as.factor(year))) +
       geom_bar(stat="identity", 
                position=position_dodge())+
       theme(legend.position="right",
             axis.title.x = element_blank()) +
       ylab("Avg Hours to Accepted Answer") +
       labs('custom text')+
       ggtitle(paste("Avg Time to get answers by Language -","From",input$fromdate,"To",input$todate))+
      
       theme_bw()
   })
   #plot wordcloud
   output$wordcloud<-renderPlot({
     cloud <- PostsDF[(PostsDF$prog_lang ==input$select) & (PostsDF$prog_lang !="rest_of_the_world"),]
    
     if(nrow(cloud)==0){
       validate(
         need(nrow(cloud)!=0, "No Records Found!!")
       )
     }
     else{
     myCorpus <- Corpus(VectorSource(cloud$Body))
     
    
     
     myCorpus <- tm_map(myCorpus, tolower)
   
     
     myCorpus<- tm_map(myCorpus,removePunctuation)
     
     
     myCorpus <- tm_map(myCorpus, removeNumbers)
     
     myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
     myCorpus <- tm_map(myCorpus, stripWhitespace)
   
     
     
     myCorpus = tm_map(myCorpus, removeWords,c("/","\\","//","/","@","<P>","</P>"))
   
     
     
     #creating document term matrix
     myDTM = DocumentTermMatrix(myCorpus,control = list(minWordLength = 1))
     m = as.matrix(myDTM)
     
     #finding the most frequent word
     v<-sort(colSums(m), decreasing = TRUE)
     d<-data.frame(word=names(v),freq=v)
     
     #generatethe wordcloud
     set.seed(1056)
     wordcloud(words=d$word,freq = d$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors =brewer.pal(8,"Dark2"))
     
     
     }
     })
   
   
   
   output$wordcloudd<-renderPlot({
     PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
     
     cloud <- PostsDF[(PostsDF$prog_lang ==input$selectd) & (PostsDF$prog_lang !="rest_of_the_world"),]
     if(nrow(cloud)==0){
       validate(
         need(nrow(cloud)!=0, "No Records Found!!")
       )
     }
     else{
     myCorpus <- Corpus(VectorSource(cloud$Body))
     myCorpus <- tm_map(myCorpus, tolower)
     myCorpus<- tm_map(myCorpus,removePunctuation)
     myCorpus <- tm_map(myCorpus, removeNumbers)
     myCorpus <- tm_map(myCorpus, removeWords,stopwords("english"))
     myCorpus <- tm_map(myCorpus, stripWhitespace)
     
     myCorpus = tm_map(myCorpus, removeWords,c("/","\\","//","/","@","<P>","</P>"))
     
     #creating document term matrix
     myDTM = DocumentTermMatrix(myCorpus,control = list(minWordLength = 1))
     m = as.matrix(myDTM)
     
     #finding the most frequent word
     v<-sort(colSums(m), decreasing = TRUE)
     d<-data.frame(word=names(v),freq=v)
     
     #generatethe wordcloud
     set.seed(1056)
     wordcloud(words=d$word,freq = d$freq,min.freq = 1,max.words = 200,random.order = FALSE,rot.per = 0.35,colors =brewer.pal(8,"Dark2"))
     
     } 
     
   })
   #summary
   output$summary <- renderTable({
     
     
     x <- data.frame("Result"=c("No. of Posts","No of Questions","No. of Answers per Question" ),"Values"=c(nrow(PostsDF),sum(na.omit(PostsDF$PostTypeId) == 1),dim(PostsDF[(PostsDF$PostTypeId==2),])[1]/
                                                                                                              dim(PostsDF[(PostsDF$PostTypeId==1),])[1]))
     return(x)
   })
   
   output$summaryd <- renderTable({
     PostsDF <- filter(PostsDF, PostsDF$CreationDate >= as.Date(input$fromdate), PostsDF$CreationDate <= as.Date(input$todate))
    
     
     x <- data.frame("Result"=c("No. of Posts","No of Questions","No. of Answers per Question" ),"Values"=c(nrow(PostsDF),sum(na.omit(PostsDF$PostTypeId) == 1),dim(PostsDF[(PostsDF$PostTypeId==2),])[1]/
                                  dim(PostsDF[(PostsDF$PostTypeId==1),])[1]))
     return(x)
    
   })
   
   PostUserDF <-merge(PostsDF[,c('Id',
                                 'CreationDate',
                                 'PostTypeId',
                                 'Score',
                                 'ViewCount',
                                 'OwnerUserId',
                                 'ParentId',
                                 'AcceptedAnswerId',
                                 'prog_lang')],
                      UsersDF[,c('Id',
                                 'CreationDate',
                                 'Reputation',
                                 'DisplayName',
                                 'Location',
                                 'Views',
                                 'UpVotes',
                                 'DownVotes')],by.x='OwnerUserId',by.y='Id')
   PostUserDF<-PostUserDF[!with(PostUserDF,is.na(PostUserDF$Location)),]
   
   filteredPostUserDf <-PostUserDF[(PostUserDF$OwnerUserId!=-1)&(PostUserDF$prog_lang!='rest_of_the_world'),]
   filteredPostUserDf<- head( filteredPostUserDf, 2500 )
   # get country
   filteredPostUserDf$Country <- sapply(as.character(filteredPostUserDf$Location),postLocation)
   filteredPostUserDf$Country <- as.character(filteredPostUserDf$Country)
   filteredPostUserDf$Counter <- 1
   
  
   #posts from different countries
   
   output$postcountry <- renderPlot({
     filteredPostUserDf<- head( filteredPostUserDf, input$postsno )
     
     CountryLangDF <- aggregate(filteredPostUserDf$Counter,
                                by=list(filteredPostUserDf$Country,
                                        filteredPostUserDf$prog_lang),
                                sum)
     
     colnames(CountryLangDF) <- c('country','language', 'num_posts')
     ggplot(data=CountryLangDF[(CountryLangDF$country!='NULL') & 
                                 (CountryLangDF$country!='warning') & 
                                 (CountryLangDF$num_posts>1),],
            aes(x=reorder(country,num_posts), 
                y=as.numeric(num_posts),
                fill=as.factor(language))) +
       geom_bar(stat="identity", 
                position=position_dodge()) + 
       coord_flip()+
       theme(legend.position="right",
             axis.title.x = element_blank()) +
       ylab("Post Count") +
       xlab("Country")+
       ggtitle("Posts by Country and Language")+theme_bw()
   })
   #avg views for countries
   output$viewscountry <- renderPlot({
     filteredPostUserDf<- head( filteredPostUserDf, input$postsno )
     
     CountryLangAgeDF <- aggregate(filteredPostUserDf[!is.na(as.numeric(filteredPostUserDf$Views)),'Views'],
                                   by=list(filteredPostUserDf[!is.na(filteredPostUserDf$Views),'Country'],
                                           filteredPostUserDf[!is.na(filteredPostUserDf$Views),'prog_lang']),
                                   mean)
     
     colnames(CountryLangAgeDF) <- c('country','language', 'avg_views')
     
     ggplot(data=CountryLangAgeDF[(CountryLangAgeDF$country!='NULL') & 
                                    (CountryLangAgeDF$country!='warning') ,],
            aes(x=reorder(country,-avg_views), 
                y=as.numeric(avg_views),
                fill=as.factor(language))) +
       geom_bar(stat="identity", 
                position=position_dodge()) + 
       coord_flip()+
       theme(legend.position="right",
             axis.title.x = element_blank()) +
       ylab("Avg Views") +
       xlab("Country")+
       ggtitle("Avg Views by Country and Language")+theme_bw()
   })
   
 
  
}
