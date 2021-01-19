library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(psych)
library(readxl)
library(Amelia)
library(corrplot)
library(eRm)
library(ltm)
library(Hmisc)
library(Rmisc)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(ggdendro)
library(tidyverse)
library(WrightMap)
library(rstatix)
library(gt)
library(randomForest)

# load and tidy data
df_question <- read_xlsx("ScaleQuestions.xlsx")
df_question$Item <- as.character(df_question$Item)


#Name of datafile download from Verint
file <- "CyberCompetencies.xlsx"

#Scoring functions
likertNum <- function(x){
    case_when(
        x == "Very Accurate" ~ 5, #Likert Scale 1 begin
        x == "Moderately Accurate" ~ 4,
        x == "Neither Accurate Nor Inaccurate" ~ 3,
        x == "Moderately Inaccurate" ~ 2,
        x == "Very Inaccurate" ~ 1,  #Likert Scale 1 end
        x == "Strongly Agree" ~ 5, #Likert Scale 2 begin
        x == "Moderately Agree" ~ 4,
        x == "Neither Agree nor Disagree" ~ 3,
        x == "Moderately Disagree" ~ 2,
        x == "Strongly Disagree" ~ 1 #Likert Scale 2 end
    )
}

#count of missing items and standard deviation of responses: Personality
df_mis_P <-  read_xlsx(file) %>%
    gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key=Question, value=Answer) %>% 
    group_by(`Record ID`) %>% 
    summarise(Miss_P = sum(is.na(Answer))/174)    #174 total Personality items

#count of missing items: Cognitive
df_mis_C <-  read_xlsx(file) %>% 
    gather(`Pattern_Q1 (Q23)`: `3D_Q16 (Q47)`, key=Question, value=Answer) %>%
    group_by(`Record ID`) %>%
    summarise(Miss_C = sum(is.na(Answer))/25, 
              sincere=sum(Answer=="I don't know", na.rm = TRUE)/25) #25 total Cognitive items

#load and pre-process raw data
df_preprocess <- read_xlsx(file) %>% 
    dplyr::rename(
        `Analogies_Q25 (Q34)` = `Analogies_Q16 (Q34)`, #fix mistake on label of question from ICAR; Analogies_Q25 used in test instead of Analogies_Q16
        `OSCP`        =`FormalCerts (Q58_1)`, #label certifications
        `OSCE`        = `FormalCerts (Q58_2)`,
        `GPEN`      = `FormalCerts (Q58_3)`,
        `GXPN`      = `FormalCerts (Q58_4)`,
        `GCIH`      = `FormalCerts (Q58_5)`,
        `CEH`       = `FormalCerts (Q58_6)`,
        `CISSP`     = `FormalCerts (Q58_7)`,
        `CISM`        = `FormalCerts (Q58_8)`,
        `Security` = `FormalCerts (Q58_9)`,
        `OtherCert` = `FormalCerts (Q58_10)`) %>% 
    mutate(   `OSCP`      = if_else(OSCP =="OSCP", "Yes", "No"),
              `OSCE`      = if_else(OSCE =="OSCE", "Yes", "No"),
              `GPEN`      = if_else(GPEN =="GPEN", "Yes", "No"),
              `GXPN`      = if_else(GXPN =="GXPN", "Yes", "No"),
              `GCIH`      = if_else(GCIH =="GCIH", "Yes", "No"),
              `CEH`       = if_else(CEH =="CEH", "Yes", "No"),
              `CISSP`     = if_else(CISSP == "CISSP", "Yes", "No"),
              `CISM`      = if_else(CISM == "CISM", "Yes", "No"), 
              `Security` = if_else(`Security`=="Security", "Yes", "No")) %>% 
    gather(`Rank (Q70)`:`Hexidecimal (Q69)`, key= "Question", value = "Response" ) %>% #cleaning demographic feature labels
    separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
    select (-Question2) %>% 
    pivot_wider(names_from = Question, values_from = Response) %>% 
    gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key="Question", value = "Response") %>% #cleaning personality feature labels
    separate(col = `Question`, into=c("Question2", "Question"),  sep = "[(]", remove="TRUE") %>% 
    select(-Question2) %>% 
    separate(col = `Question`, into=c("Question", "Question2"),  sep = "[)]", remove="TRUE") %>% 
    select(-Question2)  %>% 
    pivot_wider(names_from = Question, values_from = Response) %>% 
    gather(`Pattern_Q1 (Q23)`:`3D_Q16 (Q47)`, key= "Question", value = "Response" ) %>%  #cleaning cognitive feature labels
    separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
    select(-Question2) %>% pivot_wider(names_from = Question, values_from = Response) %>%
    mutate(Duration_min = round((Completed - Started),2)) %>% #calculate questionnaire completion time
    mutate(Duration_min = replace_na(Duration_min, 0)) %>%  
    left_join(df_mis_P, by = "Record ID") %>% #join in missingness stat for Personality features
    left_join(df_mis_C, by = "Record ID") %>% #join in missingness stat and sincere stat for Cognitive features
    drop_na(Work_Role) %>% #drop rows with no data based on entry to work role
    arrange(Work_Role) %>% #arrange data by work role
    rownames_to_column("ID") %>% #add a new ID label beginning with 1
    select(-`Record ID`) %>% #remove Verint ID label
    select(ID,Miss_P, Miss_C, sincere, Duration_min, Rank:`3D_Q16`) #select features to carry forward in analysis

#Replace Work roles with simple categories
df_preprocess$Work_Role <-  gsub("Tier-1 - Remote Operator|Tier-1 - Capability Developer|Tier-1 - Exploitation Analyst", "Tier_1", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_Other", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_Other", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 4 -  Other authorized cyber work role|I am currently not assigned a cyber work role or I am assigned as a student", "Tier_Other", df_preprocess$Work_Role)
df_preprocess$Bachelors_CS <- replace_na(df_preprocess$Bachelors_CS, "No")
df_preprocess$Masters_CS <- replace_na(df_preprocess$Masters_CS, "No")
df_preprocess$Degree <- gsub("Bachelor's Degree [(]4 year[)]", "Bachelors", df_preprocess$Degree)
df_preprocess$Degree <- gsub("Master's Degree or higher", "Masters & Up", df_preprocess$Degree)
df_preprocess$Degree <- gsub("Some college or university but did not graduate", "Some College", df_preprocess$Degree)
df_preprocess$Degree <- gsub("High School Graduate", "High School", df_preprocess$Degree)
df_preprocess$Degree <- gsub("Associate Degree [(]2 year[)]", "Associates", df_preprocess$Degree)
df_preprocess$Experience <- gsub("6 years or more", ">5Years", df_preprocess$Experience)
df_preprocess$Experience <- gsub("5 years or less", "<=5Years", df_preprocess$Experience)
df_preprocess$Duration_min <- as.numeric(df_preprocess$Duration_min)
df_preprocess$Degree <- factor(levels =c("High School", "Associates", "Some College", "Bachelors", "Masters & Up") , df_preprocess$Degree)
df_preprocess$GTScore <- gsub("121 or higher", ">120", df_preprocess$GTScore)
df_preprocess$GTScore <- gsub("120 or lower", "<=120", df_preprocess$GTScore)
df_preprocess$GTScore <- gsub("N/A or prefer not to report", "not reported", df_preprocess$GTScore)

df_rawscore <- df_preprocess %>%   
    mutate_at(vars(`Q1_A_1` : `Q21_A_16`), likertNum) %>% #score likert items
    mutate(
        `3D_Q16` = if_else(`3D_Q16` =="E", 1, 0), #score cognitive questions according to scoring key from ICAR
        `3D_Q24` = if_else(`3D_Q24` == "C", 1, 0),
        `3D_Q29` = if_else(`3D_Q29` == "F", 1, 0),
        `3D_Q42` = if_else(`3D_Q42` == "F", 1, 0),
        `3D_Q58` = if_else(`3D_Q58` == "C", 1, 0),
        Analogies_Q14 = if_else(Analogies_Q14 == "C", 1, 0),
        Analogies_Q25 = if_else(Analogies_Q25 == "A", 1, 0),
        Analogies_Q4 = if_else(Analogies_Q4 == "E", 1, 0),
        Analogies_Q5 = if_else(Analogies_Q5 == "D", 1, 0),
        Analogies_Q8 = if_else(Analogies_Q8 == "H", 1, 0),
        Matrix_Q43 = if_else(Matrix_Q43=="D", 1, 0),
        Matrix_Q48 = if_else(Matrix_Q48 == "D", 1, 0),
        Matrix_Q50 = if_else(Matrix_Q50 == "E", 1, 0),
        Matrix_Q53 = if_else(Matrix_Q53 == "C", 1, 0),
        Matrix_Q55 = if_else(Matrix_Q55 == "D", 1, 0),
        Pattern_Q1 = if_else(Pattern_Q1=="169", 1, 0),
        Pattern_Q3 = if_else(Pattern_Q3 == "47", 1, 0),
        Pattern_Q35 = if_else(Pattern_Q35 == "S", 1, 0),
        Pattern_Q58 = if_else(Pattern_Q58 == "N", 1, 0),
        Pattern_Q6 = if_else(Pattern_Q6 == "F", 1, 0),
        Verbal_Q14 = if_else(Verbal_Q14 == "8", 1, 0),
        Verbal_Q16 = if_else(Verbal_Q16 == "It's impossible to tell", 1, 0),
        Verbal_Q17 = if_else(Verbal_Q17 == "47", 1, 0),
        Verbal_Q32 = if_else(Verbal_Q32 == "Thursday", 1, 0),
        Verbal_Q4 = if_else(Verbal_Q4 == "5", 1, 0))


#Reverse Scores for select column 
df_rawscore2 <- df_rawscore
columnsToReverse <-  c('Q10_A_16','Q10_A_17','Q10_A_18','Q10_A_19','Q10_A_20',
                       'Q18_A_2', 'Q18_A_4', 'Q18_A_6',
                       'Q22_A_7', 'Q22_A_8', 'Q22_A_9', 'Q22_A_10',
                       'Q21_A_1', 'Q21_A_3','Q21_A_5','Q21_A_7','Q21_A_9', 'Q21_A_11', 'Q21_A_13', 'Q21_A_15') 

df_rawscore2[,columnsToReverse] <- 6-df_rawscore2[, columnsToReverse]  #reverse scoring requires subtracting from number 6 (one more than max score of 5)

#Item statistics of difficulty (mean) and standard deviation
item_stats <- df_rawscore2 %>%
    select(ID, Q1_A_1:Q21_A_16) %>%
    gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
    summarySE(groupvars = "Question", measurevar = "Score", na.rm = TRUE) %>% arrange(Score) %>%
    select(Question, Score, sd) %>% mutate(Score = round(Score, 1), sd=round(sd, 1)) %>% 
    mutate(Rank = rank (-Score, ties.method = "random" ) ) #Rank order questions from easiest to hardest

item_stats <- rename("Mean" = "Score", item_stats)

#Infrequency of response calculation
easy_mean <- item_stats %>%   #Average score of 20 easiest questions
  filter(Rank<=10) %>% select(Mean)
easy_mean <- mean(easy_mean$Mean, na.rm = TRUE)

#Imputation of mean values for personality items by work role   
df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
    mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) 
df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_Other"))  %>% 
    mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))        
df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b) #bind together the Tier 1 and the Tier 2/3/4 dataframes

df_infreq <-  
  df_rawscore3 %>% 
    select(ID, Q1_A_1:Q21_A_16) %>% 
    gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
    left_join(item_stats) %>% 
    filter(Rank<=10) %>% ungroup() %>% 
    group_by(ID)  %>% 
    summarise(infreq= abs(easy_mean-mean(Score, na.rm = TRUE))) #lower infreq is better  

df_rawscore4 <- df_rawscore3 %>% left_join(df_infreq, by="ID")


# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("united"),
    title("Factor Analysis Tool"),
    header=tagList(useShinydashboard()),
    position = "fixed-top",

    tabPanel("Factor Exploration", icon = icon ("flag-usa"), br(), br(), br(),

           box(title="Model variables and Group Differences", status="primary", solidHeader = TRUE, width = 4,
               
              sliderInput("factors",
                      "Number of factors to explore:",
                      min = 1,
                      max = 27,
                      value = 12,
                      step=1),
  
              sliderInput("items",
                      "Maximum number of items per factor:",
                      min = 1,
                      max = 12,
                      value = 8,
                      step=1),
        
              sliderInput("pvalue",
                       "Confidence level for difference in means:",
                       min = .01,
                       max = .25,
                       value = .10,
                       step=.01),
              
              valueBoxOutput("metric", width= 4),
              
              gt_output("table")
           
           ),
    
          
         box(title="Missingness of Data", status="primary", solidHeader = TRUE, width = 4,
            
             sliderInput("miss_limit",
                         "Respondent missingness threshold percentage:",
                         min = 0,
                         max = 1,
                         value = .25,
                         step=.01),
             
            plotOutput("plot1")
           ),
        
          box(title="Infrequency of Responses", status="primary", solidHeader = TRUE, width = 4,
              
              
              sliderInput("infreq_limit",
                          "Infrequency threshold value (based on high response ave):",
                          min = 0,
                          max = 2,
                          value = .75,
                          step= .05),
             
             plotOutput("plot2")
         ),
        
           plotOutput("plot3", height="600px")
      ),
           
       tabPanel("Group Comparison", icon = icon ("chart-bar"), br(), br(), br(),
            
            box(title="Group Comparison - Mean Scores", status="primary", solidHeader = TRUE, width = 4,
                
                plotOutput("plot4", height="600px")),
            
            box(title="Scale Correlations and Clustering", status="primary", solidHeader = TRUE, width = 8,
                
                plotOutput("plot5", height="600px"))
      ),
    
        tabPanel("Questionnaire", icon = icon ("table"), br(), br(), br(),
             
                 downloadButton("downloadData", "Download Data"),
             
                 gt_output("table2"))
    )
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  efa_data <- reactive ({
        df_rawscore4 %>% 
        filter(Miss_P <= input$miss_limit) %>% 
        filter(infreq <= input$infreq_limit) %>% 
        select(Q1_A_1:Q21_A_15)
    })
    
    efa_cor <- reactive ({
        cor(efa_data(), use = "pairwise.complete")
    })
    
    factors_data <- reactive({
        fa(r = efa_cor(), nfactors = input$factors)
    })
    
    total_scales <- reactive ({
        paste("V",input$factors, sep="")
    })
    
    df_efa <- reactive({
        factors_data()[["loadings"]] %>% 
        as.matrix.data.frame() %>% 
        as.data.frame() %>% 
        rownames_to_column("Item") %>% 
        gather(V1:total_scales(), key="Scale", value="Loading") %>% 
        mutate(Scale = gsub("V", "Scale_", Scale)) %>%
        mutate(Scale = as.factor(Scale) ) %>% 
        right_join(df_question) %>% 
            dplyr::group_by(Question) %>% 
            mutate(Rank = rank(-1*abs(Loading))) %>% 
            filter(Rank==1) %>%  
            dplyr::group_by(Scale) %>% 
            mutate(LoadRank = rank(-1*abs(Loading))) %>% 
            filter(LoadRank<=input$items) %>% 
            select(-Item, -Rank, -LoadRank )  
    })

    df_revised_scored <- reactive ({ 
         df_rawscore3 %>% select(ID, Work_Role, Rank, Q1_A_1:Q21_A_16) %>% 
         gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
         left_join(df_efa(), by="Question") %>% 
         select(Scale, ID:Question, Score) %>% 
         drop_na(Scale) %>% 
         group_by(ID, Work_Role, Scale) %>% 
         summarise(Score=mean(Score))})
 
    df_revised_scored_order <- reactive ({
        df_revised_scored()  %>% #compute summary statistics for Tier1 and Tier_Other
        summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>% 
        select(Work_Role,Scale,Score) %>% 
        pivot_wider(names_from = Work_Role, values_from = Score) %>% 
        mutate(Difference = abs(Tier_1 - Tier_Other)) %>% 
        summarySE(groupvars= "Scale", measurevar = "Difference") %>% 
        mutate(Rank = rank(-1*Difference)) %>% select(Scale, Rank)  })
    
    stat_test <- reactive ({
        df_revised_scored() %>%
            group_by(Scale) %>%
            t_test(Score ~ Work_Role) 
    })
    
    sig_scales <- reactive ({
        stat_test() %>% mutate(Significant = if_else(p<input$pvalue, "Yes", "No")) %>% arrange(p)
        })

    #Plot of factor analysis results
    
    output$plot3 <- renderPlot({
        df_efa () %>% 
            left_join(sig_scales()) %>% 
            summarySE(measurevar = "Loading", groupvars = c("Scale", "Trait", "Significant", "p" )) %>%
            ungroup() %>% group_by(Scale) %>% drop_na(Scale) %>% 
            mutate(Scale = factor(Scale, levels=sig_scales()$Scale)) %>% 
            ggplot() + 
            geom_col (aes(x=Trait, y=N, fill=Significant)) + 
            scale_fill_manual(values=c("darkgray", "skyblue")) +
            xlab("") +
            ylab("count of questions") +
            coord_flip() + 
            theme(legend.position = "blank") +
            theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
            theme(strip.text = element_text(size=12, color="blue"))+
            facet_grid(.~Scale)
    })
    
    #Plot of group comparison 
    output$plot4 <- renderPlot ({
        df_revised_scored()  %>% #compare summary statistics for Tier1 and Tier_Other
            summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>%
            left_join(df_revised_scored_order())  %>% 
            ggplot(aes(x=reorder(Scale, Rank, fun=max), y=Score, color=Work_Role, group=Work_Role)) +
            geom_point(aes(size=Work_Role)) +
            scale_size_manual(values=c(3,2,2)) +
            geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
            coord_flip() + xlab(" ") + ylab("mean score") +
            scale_color_manual(values=c("red", "darkgray")) +
         theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
            theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
            ylim(0,5) 
         })
 
     #Histogram of Missingness
     output$plot1 <- renderPlot ({
         df_rawscore4 %>% 
             mutate(FilterIn = if_else(Miss_P<=input$miss_limit, "Y", "N")) %>%
             ggplot() +
         geom_histogram(aes(x=Miss_P, fill=FilterIn )) +
                scale_fill_manual(values=c("tomato", "lightblue")) +
                geom_vline(xintercept = input$miss_limit, linetype="dashed", color="red", size=1) +
                geom_text(aes(x=input$miss_limit, y=0, angle=90), nudge_y=5, nudge_x=-.01,label= "threshold", size=4) +
                xlab("missingness percentage") + 
         theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
                facet_grid(Work_Role~.)+
                ggtitle("missingness levels of respondents") +
                theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
         theme(strip.text = element_text(size=12, color="blue"))
      })
     
     #Histogram of infrequency
     output$plot2 <- renderPlot ({
       df_rawscore4 %>% 
         mutate(FilterIn = if_else(infreq<=input$infreq_limit, "Y", "N")) %>%
         ggplot() +
         geom_histogram(aes(x=infreq, fill=FilterIn )) +
         scale_fill_manual(values=c("tomato", "lightblue")) +
         geom_vline(xintercept = input$infreq_limit, linetype="dashed", color="red", size=1) +
         geom_text(aes(x=input$infreq_limit, y=0, angle=90), nudge_y=5, nudge_x=-.01,label= "threshold", size=4) +
         xlab("infrequency score (lower is better)") + 
         xlim(0,2) +
         theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
         facet_grid(Work_Role~.)+
         ggtitle("infrequency scores of respondents") +
         theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
         theme(strip.text = element_text(size=12, color="blue"))
     })
     
     #Correlation Plot
     output$plot5 <- renderPlot ({
                corrplot(cor(df_revised_scored() %>%
                             select(ID, Scale, Score) %>%
                             pivot_wider(names_from=Scale, values_from = Score) %>%
                             ungroup() %>% 
                             dplyr::select(-ID, -Work_Role)),
                      method="color", order="hclust", type="full", addrect=6, cl.lim=c(-1,1), 
                      addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=.7 , number.cex=.7, tl.cex=.9)
     })

     #Scales with significant differences between groups
     output$metric <- renderValueBox ({
       valueBox(stat_test() %>% filter(p<input$pvalue) %>% count(), 
                subtitle = "#Scales with Group Difference",
                color=if_else(stat_test() %>% filter(p<input$pvalue) %>% count() >= 3, "lime", if_else(stat_test() %>% filter(p<input$pvalue) %>% count()>= 1, "green", "red")) )
        })
     

     #Table of count of sample size
     output$table <- render_gt ({ 
       df_rawscore4 %>% 
         filter(Miss_P <= input$miss_limit) %>% 
         filter(infreq <= input$infreq_limit) %>% 
             group_by(Work_Role) %>% 
         summarise(count=n()) %>%  
         select(Work_Role, count) %>% pivot_wider(names_from = "Work_Role", values_from = "count" ) %>%  
         gt() %>% 
         tab_header(title=md("adjusted sample size for factor analysis"))
     })
     
   
     
     #Random Forest Modeling
     df_model <- reactive({
       df_rawscore4 %>% 
       filter(Miss_P <= input$miss_limit) %>% 
       filter(infreq <= input$infreq_limit) %>% 
       select(Work_Role, Q1_A_1:Q21_A_16) %>% 
       mutate_at(vars(Q1_A_1:Q21_A_16), scale) %>% 
       mutate(Work_Role = as.factor(Work_Role))
     })
     
     set.seed(103)
     #Random Forest Model
     rfmodel <- reactive({
       randomForest(Work_Role ~ ., data = df_model(), ntree = 500, mtry = 12, importance = TRUE, proximity=TRUE)
     })
   
     importance <- reactive({
       rfmodel()$importance %>% as.data.frame() %>% 
       rownames_to_column("Question") %>% select(Question, MeanDecreaseAccuracy) 
     })
     
    minImportance <- reactive ({
       min(importance()$MeanDecreaseAccuracy)
     })
     
     rangeImportance <- reactive ({
       diff(range(importance()$MeanDecreaseAccuracy))
     })
     
     importance2 <- reactive ({
       importance() %>% group_by(Question) %>% 
       mutate(PredValue = (MeanDecreaseAccuracy+abs(minImportance()))/rangeImportance()) %>% 
       mutate(PredValue = if_else(PredValue>.8, "A", if_else(PredValue>.6, "B", if_else(PredValue>.4, "C", if_else(PredValue>.2, "D", "F" )))))
     })
     
     #Question Table
     output$table2 <- render_gt({
       df_efa() %>% 
         left_join(sig_scales()) %>%
         mutate(Loading = round(Loading, 2)) %>%
         left_join(item_stats) %>% 
         left_join(importance2(), by="Question") %>% 
         arrange(abs(p)) %>% 
         select (Scale, Trait, Question, Loading, Mean, sd, PredValue, Content)  %>% 
         gt(groupname_col = "Scale", rowname_col="Question") %>% 
         tab_spanner(label="item metrics", columns=matches("Loading|Mean|sd|PredValue")) %>% 
         tab_header(
           title=md("Table of Scales and Items for Revised Questionnaire") )
     })
     
     #Download Button    
     output$downloadData <- downloadHandler(
       filename = function () {
         paste("Questionnaire_Refined", "csv", sep=".") 
       },
       content=function(file) {
         write.csv(df_efa() %>% mutate(Loading = round(Loading, 2)) %>% 
                     select (Scale, Trait, Question, Loading, Content) %>%
                     arrange(Scale), file )
       })

 }

# Run the application 
shinyApp(ui = ui, server = server)
