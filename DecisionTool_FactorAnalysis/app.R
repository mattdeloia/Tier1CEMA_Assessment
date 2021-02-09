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
library(WrightMap)

library(gt)
library(randomForest)
#library(bslib)
library(thematic)
library(tidyverse)
library(moderndive)
library(skimr)
library(infer)

library(DT)
library(rstatix)

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
    dplyr::select (-Question2) %>% 
    pivot_wider(names_from = Question, values_from = Response) %>% 
    gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key="Question", value = "Response") %>% #cleaning personality feature labels
    separate(col = `Question`, into=c("Question2", "Question"),  sep = "[(]", remove="TRUE") %>% 
    dplyr::select(-Question2) %>% 
    separate(col = `Question`, into=c("Question", "Question2"),  sep = "[)]", remove="TRUE") %>% 
    dplyr::select(-Question2)  %>% 
    pivot_wider(names_from = Question, values_from = Response) %>% 
    gather(`Pattern_Q1 (Q23)`:`3D_Q16 (Q47)`, key= "Question", value = "Response" ) %>%  #cleaning cognitive feature labels
    separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
    dplyr::select(-Question2) %>% pivot_wider(names_from = Question, values_from = Response) %>%
    mutate(Duration_min = round((Completed - Started),2)) %>% #calculate questionnaire completion time
    mutate(Duration_min = replace_na(Duration_min, 0)) %>%  
    left_join(df_mis_P, by = "Record ID") %>% #join in missingness stat for Personality features
    left_join(df_mis_C, by = "Record ID") %>% #join in missingness stat and sincere stat for Cognitive features
    drop_na(Work_Role) %>% #drop rows with no data based on entry to work role
    arrange(Work_Role) %>% #arrange data by work role
    rownames_to_column("ID") %>% #add a new ID label beginning with 1
    dplyr::select(-`Record ID`) %>% #remove Verint ID label
    dplyr::select(ID,Miss_P, Miss_C, sincere, Duration_min, Rank:`3D_Q16`) #select features to carry forward in analysis

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

df_demographic <- df_preprocess %>% dplyr::select (ID,Rank:Hexidecimal)
n_Tier <- df_demographic %>% group_by(Work_Role) %>% summarise(n_tot=n())
df_hobbies <- df_demographic %>% 
  dplyr::select (ID, Work_Role, BuiltCPU, Gaming, Edit_game, Scripts, SOHO, Hexidecimal) %>%
  gather(BuiltCPU:Hexidecimal, key="Hobby", value="Response") 
df_hobbies$Response <- replace_na(df_hobbies$Response, "No")

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
    dplyr::select(ID, Q1_A_1:Q21_A_16) %>%
    gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
    summarySE(groupvars = "Question", measurevar = "Score", na.rm = TRUE) %>% arrange(Score) %>%
    dplyr::select(Question, Score, sd) %>% mutate(Score = round(Score, 1), sd=round(sd, 1)) %>% 
    mutate(Rank = rank (-Score, ties.method = "random" ) ) #Rank order questions from easiest to hardest

item_stats <- rename("Mean" = "Score", item_stats)

#Infrequency of response calculation
easy_mean <- item_stats %>%   #Average score of 20 easiest questions
  filter(Rank<=10) %>% dplyr::select(Mean)
easy_mean <- mean(easy_mean$Mean, na.rm = TRUE)

#Imputation of mean values for personality items by work role   
df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
    mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) 
df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_Other"))  %>% 
    mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))        
df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b) #bind together the Tier 1 and the Tier 2/3/4 dataframes

df_infreq <-  
  df_rawscore3 %>% 
    dplyr::select(ID, Q1_A_1:Q21_A_16) %>% 
    gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
    left_join(item_stats) %>% 
    filter(Rank<=10) %>% ungroup() %>% 
    group_by(ID)  %>% 
    summarise(infreq= abs(easy_mean-mean(Score, na.rm = TRUE))) #lower infreq is better  

df_rawscore4 <- df_rawscore3 %>% left_join(df_infreq, by="ID")

#Scoring Cognitive Test: percent correct by test type
df_scored <- df_rawscore4 %>% 
  group_by (ID) %>% 
  mutate(
    Problem_Solving = mean(c(Q1_A_1, Q1_A_2, Q1_A_3, Q1_A_4, Q1_A_5), na.rm = TRUE),
    Depth_Thought = mean (c(Q3_A_1, Q3_A_2, Q3_A_3, Q3_A_4, Q3_A_5, Q3_A_6, Q3_A_7), na.rm = TRUE),
    Mental_Quickness = mean (c(Q4_A_1, Q4_A_2, Q4_A_3, Q4_A_4, Q4_A_5, Q4_A_6, Q4_A_7), na.rm = TRUE),
    Need_Cognition = mean(c(Q5_A_1, Q5_A_2, Q5_A_3, Q5_A_4, Q5_A_5, Q5_A_6), na.rm = TRUE), 
    Love_Learning = mean(c(Q6_A_1, Q6_A_2, Q6_A_3, Q6_A_4, Q6_A_5, Q6_A_6), na.rm = TRUE),
    Creativity = mean(c(Q7_A_1, Q7_A_2, Q7_A_3, Q7_A_4, Q7_A_5, Q7_A_6, Q7_A_7, Q7_A_8, Q7_A_9, Q7_A_10, Q7_A_11, Q7_A_12, Q7_A_13), na.rm = TRUE),
    Concientiousness = mean(c(Q8_A_1, Q8_A_2, Q8_A_3, Q8_A_4, Q8_A_5, Q8_A_6, Q8_A_7, Q8_A_8, Q8_A_9, Q8_A_10), na.rm = TRUE),
    Orderliness = mean(c(Q9_A_1, Q9_A_2, Q9_A_3, Q9_A_4, Q9_A_5), na.rm = TRUE),
    Deprivation_Sensitivity = mean(c(Q10_A_1, Q10_A_2, Q10_A_3, Q10_A_4, Q10_A_5), na.rm = TRUE),
    Joyous_Exploration = mean(c(Q10_A_6, Q10_A_7, Q10_A_8, Q10_A_9, Q10_A_10), na.rm = TRUE),
    Social_Curiosity = mean(c(Q10_A_11, Q10_A_12, Q10_A_13, Q10_A_14, Q10_A_15), na.rm = TRUE),
    Stress_Tolerance = mean(c(Q10_A_16, Q10_A_17, Q10_A_18, Q10_A_19, Q10_A_20), na.rm = TRUE),
    Thrill_Seeking = mean(c(Q10_A_21, Q10_A_22, Q10_A_23, Q10_A_24, Q10_A_25), na.rm = TRUE),
    Interpersonal_Understanding = mean(c(Q11_A_1, Q11_A_2, Q11_A_3), na.rm = TRUE),
    Self_Confidence = mean(c(Q11_A_4, Q11_A_5, Q11_A_6, Q11_A_7), na.rm = TRUE),
    Suspension_Judgement = mean(c(Q11_A_8, Q11_A_9, Q11_A_10, Q11_A_11, Q11_A_12), na.rm = TRUE),
    Self_Discipline = mean(c(Q12_A_1, Q12_A_2, Q12_A_3, Q12_A_4), na.rm = TRUE), 
    Industriness = mean(c(Q13_A_1, Q13_A_2, Q13_A_3, Q13_A_4, Q13_A_5), na.rm = TRUE), 
    Enthusiasm = mean(c(Q14_A_1, Q14_A_2, Q14_A_3, Q14_A_4, Q14_A_5), na.rm = TRUE),
    Flow_Proneness = mean(c(Q15_A_1, Q15_A_2, Q15_A_3, Q15_A_4, Q15_A_5, Q15_A_6), na.rm = TRUE),
    Cyberwork_Confidence = mean(c(Q16_A_1, Q16_A_2, Q16_A_3, Q16_A_4, Q16_A_5, Q16_A_6), na.rm = TRUE),
    General_SelfEfficacy = mean(c(Q17_A_1, Q17_A_2, Q17_A_3, Q17_A_4, Q17_A_5, Q17_A_6, Q17_A_7, Q17_A_8), na.rm = TRUE),
    Resilience = mean(c(Q18_A_1, Q18_A_2, Q18_A_3, Q18_A_4, Q18_A_5, Q18_A_6), na.rm = TRUE),
    Teamwork = mean(c(Q19_A_1, Q19_A_2, Q19_A_3, Q19_A_4, Q19_A_5), na.rm = TRUE),
    Leadership = mean(c(Q20_A_1, Q20_A_2, Q20_A_3, Q20_A_4, Q20_A_5, Q20_A_6, Q20_A_7), na.rm = TRUE),
    Intellectual_Openness = mean(c(Q22_A_1, Q22_A_2, Q22_A_3, Q22_A_4, Q22_A_5, Q22_A_6, Q22_A_7, Q22_A_8, Q22_A_9, Q22_A_10), na.rm = TRUE),
    Tolerance = mean(c(Q21_A_1, Q21_A_2, Q21_A_3, Q21_A_4, Q21_A_5, Q21_A_6, Q21_A_7, Q21_A_8, 
                       Q21_A_9, Q21_A_10, Q21_A_11, Q21_A_12, Q21_A_13, Q21_A_14, Q21_A_15, Q21_A_16), na.rm = TRUE),
    Analogies = mean(c(Analogies_Q14, Analogies_Q25,Analogies_Q4,Analogies_Q5,Analogies_Q8), na.rm = TRUE),
    Matrix = mean(c(Matrix_Q43, Matrix_Q48, Matrix_Q50, Matrix_Q53, Matrix_Q55), na.rm = TRUE),
    Pattern = mean(c(Pattern_Q1, Pattern_Q3, Pattern_Q35, Pattern_Q58, Pattern_Q6), na.rm = TRUE),
    ThreeD = mean(c(`3D_Q16`, `3D_Q24`, `3D_Q29`, `3D_Q42`, `3D_Q58`), na.rm = TRUE),
    Verbal = mean(c(Verbal_Q14, Verbal_Q16, Verbal_Q17, Verbal_Q32, Verbal_Q4), na.rm = TRUE) ) %>% 
  mutate(Cog_tot = mean(c(Analogies_Q14, Analogies_Q25,Analogies_Q4,Analogies_Q5,Analogies_Q8,
                          Matrix_Q43, Matrix_Q48, Matrix_Q50, Matrix_Q53, Matrix_Q55,
                          Pattern_Q1, Pattern_Q3, Pattern_Q35, Pattern_Q58, Pattern_Q6,
                          `3D_Q16`, `3D_Q24`, `3D_Q29`, `3D_Q42`, `3D_Q58`,
                          Verbal_Q14, Verbal_Q16, Verbal_Q17, Verbal_Q32, Verbal_Q4), na.rm = TRUE ))

personalityscales <- c("Problem_Solving", "Depth_Thought", "Mental_Quickness", "Need_Cognition", "Love_Learning", "Creativity","Concientiousness", "Orderliness", "Deprivation_Sensitivity", "Joyous_Exploration", "Social_Curiosity", "Stress_Tolerance", "Thrill_Seeking", "Interpersonal_Understanding", "Self_Confidence", "Suspension_Judgement", "Self_Discipline", "Industriness", "Enthusiasm", "Flow_Proneness", "Cyberwork_Confidence", "General_SelfEfficacy", "Resilience", "Teamwork",  "Leadership", "Intellectual_Openness", "Tolerance"    )

hypothesisplot <- function (personalitytrait, missingness, infrequency, confidence) {
  visualise(df_scored %>%
              filter(Miss_P < missingness, infreq < infrequency) %>%
              gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
              filter(Trait==personalitytrait) %>% 
              specify(formula = Score ~ Work_Role) %>%
              generate(reps = 1000, type = "bootstrap") %>% 
              calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) ) +
    shade_confidence_interval(endpoints = df_scored %>%
                                filter(Miss_P < missingness, infreq < infrequency) %>%
                                gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
                                filter(Trait==personalitytrait) %>% 
                                specify(formula = Score ~ Work_Role) %>%
                                generate(reps = 1000, type = "bootstrap") %>% 
                                calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) %>% 
                                get_confidence_interval(level = confidence, type = "percentile")) +
    geom_vline(xintercept = 0, linetype="dashed", color= "red", size = 1.5)  +
    labs(subtitle = personalitytrait)
}

p_value <- function(personalitytrait, missingness, infrequency) {
  df_scored %>%  
    filter(Miss_P < missingness, infreq < infrequency) %>%
    gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
    filter(Trait==personalitytrait) %>% 
    specify(formula = Score ~ Work_Role) %>% 
    hypothesize(null = "independence") %>% 
    generate(reps = 1000, type = "permute") %>% 
    calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")) %>% 
    get_p_value(obs_stat = df_scored %>%  
                  filter(Miss_P < missingness, infreq < infrequency) %>%
                  gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
                  filter(Trait==personalitytrait) %>% 
                  specify(formula = Score ~ Work_Role) %>%  
                  calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")), 
                direction = if_else((df_scored %>%  
                                      filter(Miss_P < missingness, infreq < infrequency) %>%
                                      gather(Problem_Solving:Tolerance, key=Trait, value=Score) %>% 
                                      filter(Trait==personalitytrait) %>% 
                                      specify(formula = Score ~ Work_Role) %>% 
                                      calculate (stat = "diff in means", order = c("Tier_1", "Tier_Other")))>0, "right", "left"))
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("united"),
    title="Tier1 Data Analysis",
    header=tagList(useShinydashboard()),
    position = "fixed-top",
    
    tabPanel("Demographic Data", icon = icon ("flag-usa"), br(), br(), br(),
             
             tabsetPanel(
               
               tabPanel("Experience",
                        box(title="Rank", status="primary", solidHeader = TRUE, dataTableOutput("ranktable") ),
                        box(title="Experience", status="primary", solidHeader = TRUE, plotOutput("experience") )
                        ),
               
               tabPanel ("Education",
                         box(title="Education High Degree", status="primary", solidHeader = TRUE, plotOutput("education") ),
                         box(title="CS Degrees", status="primary", solidHeader = TRUE, plotOutput("education2") )
                         ) ,
    
               tabPanel ("GT Scores and Certifications",
                        box(title="Gt Scores", status="primary", solidHeader = TRUE, plotOutput("gtscore") ),
                        box(title="Certifications", status="primary", solidHeader = TRUE, plotOutput("certifications") ) ),
               
               tabPanel ("Hobbies",
                         box(title="Hobbies", status="primary", solidHeader = TRUE, plotOutput("hobbies") ),
                         box(title="Preferences", status="primary", solidHeader = TRUE, plotOutput("hobbies2") )
   ) )) ,
    
    tabPanel("Personality: Group Comparison", icon = icon ("flag-usa"), br(), br(), br(),
             
             sidebarLayout( 
               
            sidebarPanel(
           
                 gt_output("table"),
           
                 sliderInput("miss_limit",
                               "Respondent missingness threshold percentage:",
                               min = 0,
                               max = 1,
                               value = .25,
                               step = .01),
                   
                   plotOutput("plot1", height="250px") ,
           
                   sliderInput("infreq_limit",
                               "Infrequency threshold value (based on high response ave):",
                               min = 0,
                               max = 2,
                               value = .75,
                               step = .05),
                   
                   plotOutput("plot2", height="250px")
             
             ), 
            
            mainPanel (
               
              plotOutput("plotsummary", height = "800px") )
             
               )
            ),
    
    tabPanel("Personality: Resampling", icon = icon ("flag-usa"), br(), br(), br(),
               
             sidebarLayout (
               
               sidebarPanel (
               
                          sliderInput("confidenceinterval", "CI for difference in mu (population):",
                                  min = .90,
                                  max = 1.0,
                                  value = .95,
                                  step = .01),
                      
                      radioButtons("personalitytrait", label = "Personality Scale", choices = personalityscales, selected = "Problem_Solving"),
                      
                      verbatimTextOutput("trait") ),
               
               mainPanel (
                      
                     
                      valueBoxOutput("p_value", width=12),
                      
                      plotOutput("resampleplot")) 
              
             ) ),
             
          tabPanel("Exploratory Factor Analysis", icon = icon ("flag-usa"), br(), br(), br(),

              box(title="Factor Analsis variables and alpha value", status="primary", solidHeader = TRUE, width = 4,

              sliderInput("factors",
                      "Number of factors to explore:",
                      min = 1,
                      max = 27,
                      value = 12,
                      step = 1),

              sliderInput("items",
                      "Maximum number of items per factor:",
                      min = 1,
                      max = 12,
                      value = 8,
                      step = 1),

              sliderInput("alpha",
                       "alpha for difference in means:",
                       min = .01,
                       max = .25,
                       value = .10,
                       step = .01),
             
              valueBoxOutput("metric", width = 12)
              ),
             
              box(title="Group Comparison - Mean Scores", status="primary", solidHeader = TRUE, width = 8,
                  
                  plotOutput("plot4", height = "600px") )
              ,
           

           box(title="New Factor Construct", status="primary", solidHeader = TRUE, width = 12,
               
               plotOutput("plot3", height = "600px") ) 
           ),
           
       
      tabPanel("New Factor Correlations", icon = icon ("chart-bar"), br(), br(), br(),
           

            box(title="Scale Correlations and Clustering", status="primary", solidHeader = TRUE, width = 12,

                plotOutput("plot5", height = "600px"))
      ),

        tabPanel("New Construct Questionnaire", icon = icon ("table"), br(), br(), br(),

                 downloadButton("downloadData", "Download Data"),

                 gt_output("table2"))
    )
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    
  #Demographic 
  #Rank
  
  output$ranktable <- renderDataTable ({ 
    datatable(df_demographic %>% 
              dplyr::select(Work_Role, Rank) %>% 
              group_by(Work_Role, Rank) %>%
              summarise(n=n()) %>% 
              spread(key=Rank, value=n) )
  })
  
  
  #Experience
  output$experience <- renderPlot ({
    df_demographic %>% 
    dplyr::select(Work_Role, Experience) %>% 
    group_by(Work_Role, Experience) %>%
    summarise(n=n()) %>% 
    left_join(n_Tier) %>% 
    mutate(n=n/n_tot) %>% 
    ggplot(aes(x=Experience, y=n, fill=Work_Role)) + 
    geom_col(position = "dodge2") +
    scale_fill_manual(values=c("lightgreen", "darkgray", "lightgreen", "gray")) +
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("") + xlab("") + ylim(0,1) 
    # facet_grid(Work_Role~.) 
  })
  
  #Education
  output$education <- renderPlot ({df_demographic %>% 
    dplyr::select(Work_Role, Degree) %>% 
    group_by(Work_Role, Degree) %>%
    summarise(n=n()) %>% 
    left_join(n_Tier) %>% 
    mutate(n=n/n_tot) %>% 
    na.omit(Degree) %>% 
    ggplot(aes(x=reorder(Degree, -n, fun=mean), y=n, fill=Work_Role, group=Work_Role)) + 
    geom_col(position="dodge2") +
    geom_line(aes(linetype=Work_Role)) +
    scale_fill_manual(values=c("red", "lightblue", "lightgreen", "gray")) +
    scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("") + xlab("") + ylim(0,1) 
    })
  
  #Education CD Degrees
  output$education2 <- renderPlot ({df_demographic %>% 
    dplyr::select(ID, Work_Role, Bachelors_CS, Masters_CS) %>% 
    mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
           Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
    gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>% 
    group_by(Work_Role, Degree) %>%
    summarise(n=sum(n)) %>% 
    left_join(n_Tier) %>% 
    mutate(n=n/n_tot) %>% 
    ggplot(aes(x=Degree, y=n, fill=Work_Role)) + 
    geom_col(position = "dodge2") +
    scale_fill_manual(values=c("red", "lightblue", "lightgreen", "gray")) +
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("% of Tier group") + xlab("") + ylim(0,1)
    })
  
  #Certifications
  output$certifications <- renderPlot ({
    df_demographic %>% 
    dplyr::select(ID, Work_Role, OSCP:`Security`) %>%
    gather(OSCP:`Security`, key=Cert, value=n) %>% 
    mutate(n=if_else(n=="Yes", 1, 0)) %>% 
    group_by(Work_Role, Cert) %>%
    summarise(n=sum(n)) %>% 
    left_join(n_Tier) %>%
    mutate(n=round(n/n_tot,2)) %>% 
    dplyr::select(-n_tot) %>%
    ggplot(aes(x=reorder(Cert, n, FUN = mean), y=n, color=Work_Role, group=Work_Role)) + 
    geom_point(size=2) +
    geom_line(aes(linetype=Work_Role)) +
    scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
    scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("percentage of respondents") + xlab("") +
    coord_flip() 
  })
  
  
  #GT Score
  output$gtscore <- renderPlot ({
    df_demographic %>% 
    dplyr::select (ID, Work_Role, GTScore) %>% 
    group_by(Work_Role, GTScore) %>% summarise(n=n()) %>% 
    left_join(n_Tier) %>% 
    mutate(n= n/n_tot) %>% 
    ggplot(aes(x=GTScore, y=n, fill=Work_Role)) + 
    geom_col (position = "dodge2") +
    scale_fill_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
    #facet_grid(Work_Role~.) +   
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("percentage of respondents") + xlab("") 
  })
  
  #Hobbies
  output$hobbies <- renderPlot ({
  df_hobbies %>%  
    mutate(Response = if_else(Response =="Yes", 1, 0)) %>% 
    group_by(Work_Role, Hobby) %>% 
    summarise(Response = sum(Response)) %>% 
    left_join(n_Tier) %>% 
    mutate(Response = Response/n_tot) %>% 
    ggplot(aes(x=reorder(Hobby, Response, FUN=mean), y=Response, color=Work_Role, group=Work_Role)) + 
    geom_point(size=2) +
    geom_line(aes(linetype=Work_Role)) +
    scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
    scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
    theme(legend.title= element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=12)) +
    ylab("percent of respondents") +
    xlab("") +
    ylim(0,1) +
    coord_flip() 
  })
  
  #Hobbies2
  output$hobbies2 <- renderPlot ({
    df_demographic %>% 
    dplyr::select(ID, Work_Role, CPU_OS, Game_Platform, Game_type) %>% 
    gather(CPU_OS:Game_type, key=Category, value=Response) %>%
    group_by(Work_Role, Category, Response) %>% 
    summarise(n=n()) %>% 
    left_join(n_Tier) %>% 
    mutate(n= n/n_tot) %>%
    na.omit(Response) %>% 
    ggplot(aes(x=reorder(Response, n, FUN=mean), y=n, fill=Work_Role)) + 
    geom_col(position="dodge2") + 
    facet_grid(Category~., scales = "free_y") + 
    theme(legend.position = "top", axis.text = element_text(size=12)) +
    xlab("") + ylim(0,1) +
    ylab("percent of respondents") +
    coord_flip() 
  })
  
  #Exploratory Factor Analysis
    
  efa_data <- reactive ({
        df_rawscore4 %>% 
        filter(Miss_P <= input$miss_limit) %>% 
        filter(infreq <= input$infreq_limit) %>% 
        dplyr::select(Q1_A_1:Q21_A_15)
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
            dplyr::select(-Item, -Rank, -LoadRank )  
    })

    df_revised_scored <- reactive ({ 
         df_rawscore3 %>% dplyr::select(ID, Work_Role, Rank, Q1_A_1:Q21_A_16) %>% 
         gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
         left_join(df_efa(), by="Question") %>% 
        mutate(Score = if_else(Loading>0, Score, 6-Score) ) %>% #reverse scoring based on item loading
         dplyr::select(Scale, ID:Question, Score) %>% 
         drop_na(Scale) %>% 
         group_by(ID, Work_Role, Scale) %>% 
         summarise(Score=mean(Score))})
 
    df_revised_scored_order <- reactive ({
        df_revised_scored()  %>% #compute summary statistics for Tier1 and Tier_Other
        summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>% 
        dplyr::select(Work_Role,Scale,Score) %>% 
        pivot_wider(names_from = Work_Role, values_from = Score) %>% 
        mutate(Difference = abs(Tier_1 - Tier_Other)) %>% 
        summarySE(groupvars= "Scale", measurevar = "Difference") %>% 
        mutate(Rank = rank(-1*Difference)) %>% dplyr::select(Scale, Rank)  })
    
    stat_test <- reactive ({
        df_revised_scored() %>%
            group_by(Scale) %>%
            t_test(Score ~ Work_Role) 
    })
    
    sig_scales <- reactive ({
        stat_test() %>% mutate(Significant = if_else(p<input$alpha, "Yes", "No")) %>% arrange(p)
        })

    #Plot of factor analysis results
    
    output$plot3 <- renderPlot({
        df_efa () %>% 
            left_join(sig_scales()) %>% 
        mutate(Loading2 = if_else(Loading > 0, "positive", "negative")) %>% 
            summarySE(measurevar = "Loading", groupvars = c("Scale", "Trait", "Significant", "p", "Loading2" )) %>%
            ungroup() %>% group_by(Scale) %>% drop_na(Scale) %>% 
            mutate(Scale = factor(Scale, levels=sig_scales()$Scale)) %>% 
            mutate(Trait = as_factor(Trait)) %>% 
            ggplot() + 
            geom_col (aes(x=Trait, y=N, color=Significant, fill=Loading2),alpha=.5) + 
            scale_color_manual(values=c("gray", "green")) +
          scale_fill_manual(values=c("red", "darkgray"))+
            xlab("") +
            ylab("count of questions") +
            coord_flip() + 
            theme(legend.position = "blank") +
            theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
            theme(strip.text = element_text(size=12, color="blue"))+
            facet_grid(.~Scale)
    })
    
    #Plot of group comparison using new factors
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
               # facet_grid(Work_Role~.)+
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
         #facet_grid(Work_Role~.)+
         ggtitle("infrequency scores of respondents") +
         theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
         theme(strip.text = element_text(size=12, color="blue"))
     })
     
     #Correlation Plot
     output$plot5 <- renderPlot ({
                corrplot(cor(df_revised_scored() %>%
                             dplyr::select(ID, Scale, Score) %>%
                             pivot_wider(names_from=Scale, values_from = Score) %>%
                             ungroup() %>% 
                             dplyr::select(-ID, -Work_Role)),
                      method="color", order="hclust", type="full", addrect=6, cl.lim=c(-1,1), 
                      addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=.7 , number.cex=.7, tl.cex=.9)
     })

     #Scales with significant differences between groups
     output$metric <- renderValueBox ({
       valueBox(stat_test() %>% filter(p<input$alpha) %>% count(), 
                subtitle = "#Scales with Group Difference",
                color=if_else(stat_test() %>% filter(p<input$alpha) %>% count() >= 3, "lime", if_else(stat_test() %>% filter(p<input$alpha) %>% count()>= 1, "green", "red")) )
        })
     

     #Table of count of sample size
     output$table <- render_gt ({ 
       df_rawscore4 %>% 
         filter(Miss_P <= input$miss_limit) %>% 
         filter(infreq <= input$infreq_limit) %>% 
             group_by(Work_Role) %>% 
         summarise(count=n()) %>%  
         dplyr::select(Work_Role, count) %>% pivot_wider(names_from = "Work_Role", values_from = "count" ) %>%  
         gt() %>% 
         tab_header(title=md("sample size for analysis"))
     })
     
   
     
     #Random Forest Modeling
     df_model <- reactive({
       df_rawscore4 %>% 
       filter(Miss_P <= input$miss_limit) %>% 
       filter(infreq <= input$infreq_limit) %>% 
       dplyr::select(Work_Role, Q1_A_1:Q21_A_16) %>% 
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
       rownames_to_column("Question") %>% dplyr::select(Question, MeanDecreaseAccuracy) 
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
         dplyr::select (Scale, Trait, Question, Loading, Mean, sd, PredValue, Content)  %>% 
         gt(groupname_col = "Scale", rowname_col="Question") %>% 
         tab_style(style = list (
           cell_text(color = "red")),
           locations = cells_body(columns= vars(PredValue), rows = if_else(PredValue %in% c("A", "B", "C"), TRUE, FALSE))) %>% 
         tab_style(style = list (
           cell_text(color = "red")),
           locations = cells_body(columns= vars(Trait), rows = if_else(Loading < 0, TRUE, FALSE))) %>% 
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
                     dplyr::select (Scale, Trait, Question, Loading, Content) %>%
                     arrange(Scale), file )
       })


     #Scale Personality scored data with mean of zero and std deviation of 1
     df_scored_summary <- reactive ({
       df_scored %>% ungroup() %>% 
       filter(Miss_P <input$miss_limit, infreq<input$infreq_limit) %>% 
       dplyr::select(ID, Work_Role, Problem_Solving:Tolerance) %>% 
       mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
       gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>% 
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE)
     })

     
     df_scored_order <- reactive({
       df_scored_summary() %>% 
       filter(Work_Role=="Tier_1") %>% 
       mutate(order=rank(Score)) %>% 
       dplyr::select(Dimension, order) #compute summary statistics for Tier 1 
     })
     
     #Visualization of summary statistics for Personality results by work role      
     output$plotsummary <- renderPlot({
       df_scored_summary () %>% left_join(df_scored_order())  %>% 
       ggplot(aes(x=reorder(Dimension, order, fun=max), y=Score, color=Work_Role, group=Work_Role)) +
       geom_point(aes(size=Work_Role)) +
       scale_size_manual(values=c(3,2,2)) +
       geom_line(aes(linetype=Work_Role)) +
       scale_linetype_manual(values=c("dashed", "blank", "blank", "blank"))+
       geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
       coord_flip() + xlab(" ") + ylab("mean scaled score") +
       scale_color_manual(values=c("red", "darkgray", "darkgray", "darkgray")) +
       theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
       ylim(-2,2) 
     })

     
     #Bootstrap Resample Plot for Comparison of Groups
    output$resampleplot <- renderPlot( {
      hypothesisplot(input$personalitytrait, input$miss_limit, input$infreq_limit, input$confidenceinterval)
    })
     
    output$p_value <- renderValueBox({ 
      valueBox(round(p_value(input$personalitytrait, input$miss_limit, input$infreq_limit)$p_value,2) ,
               paste("p value:",input$personalitytrait), 
               color = if_else(p_value(input$personalitytrait, input$miss_limit, input$infreq_limit)$p_value<=(1-input$confidenceinterval),"green", "red"),
               width= NULL) 
      })
                                      
     output$trait <- renderPrint({input$personalitytrait})
     
 }

# Run the application 
shinyApp(ui = ui, server = server)
