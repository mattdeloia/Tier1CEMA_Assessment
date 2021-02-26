#Load libraries
library(readxl)
library(Rmisc)
library(Amelia)
library(Hmisc)
library(corrplot)
library(eRm)
library(ltm)
library(psych)
library(tidyverse)
library(ggdendro)
library(infer)
library(DT)
library(randomForest)
library(gt)
library(gmodels)
library(caret)
library(mlbench)

#Name of datafile download from Verint
file <- "CyberCompetencies.xlsx"

# load and tidy data
df_question <- read_xlsx("ScaleQuestions.xlsx")
df_question$Item <- as.character(df_question$Item)


#Name of datafile download from Verint
file <- "CyberCompetencies.xlsx"
miss_limit = .20

#Scoring functions
likertNum <- function(x){
  case_when(
    x == "Very Accurate" ~ 5, #Likert Scale 1 begin
    x == "Moderately Accurate" ~ 4,
    x == "Neither Accurate nor Inaccurate" ~ 3,
    x == "Moderately Inaccurate" ~ 2,
    x == "Very Inaccurate" ~ 1,  #Likert Scale 1 end
    x == "Strongly Agree" ~ 5, #Likert Scale 2 begin
    x == "Moderately Agree" ~ 4,
    x == "Neither Agree nor Disagree" ~ 3,
    x == "Moderately Disagree" ~ 2,
    x == "Strongly Disagree" ~ 1 #Likert Scale 2 end
  )
}

#count of missing item: Personality
df_mis_P <-  read_xlsx(file) %>%
  gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key=Question, value=Answer) %>% 
  group_by(`Record ID`) %>% 
  summarise(Miss_P = sum(is.na(Answer))/174)    #174 total Personality items

#count of missing items and sincere metric: Cognitive
df_mis_C <-  read_xlsx(file) %>% 
  gather(`Pattern_Q1 (Q23)`: `3D_Q16 (Q47)`, key=Question, value=Answer) %>%
  group_by(`Record ID`) %>%
  summarise(Miss_C = sum(is.na(Answer))/25, 
            sincere=sum(Answer=="I don't know", na.rm = TRUE)) #25 total Cognitive items

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
  mutate(Rank = gsub("E1 - E9", "Enlisted", Rank)) %>% 
  mutate(Rank = gsub("O1 or higher", "Officer", Rank)) %>% 
  mutate(Rank = gsub("WO1 - CW5", "Warrant", Rank)) %>% 
  mutate(Work_Role = gsub("Tier-1 - Remote Operator", "Tier1_RO", Work_Role)) %>%
  mutate(Work_Role = gsub("Tier-1 - Capability Developer", "Tier1_CD", Work_Role)) %>% 
  mutate(Work_Role = gsub("Tier-1 - Exploitation Analyst", "Tier1_EA", Work_Role)) %>% 
  mutate(Work_Role = gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_2", Work_Role)) %>% 
  mutate(Work_Role = gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_3", Work_Role)) %>% 
  mutate(Work_Role =  gsub("Tier 4 -  Other authorized cyber work role", "Tier_4", Work_Role)) %>% 
  mutate(Work_Role =  gsub("I am currently not assigned a cyber work role or I am assigned as a student", "Tier_Other", Work_Role)) %>% 
  mutate(Bachelors_CS = replace_na(Bachelors_CS, "No")) %>% 
  mutate(Masters_CS = replace_na(Masters_CS, "No")) %>% 
  mutate(Degree = gsub("Bachelor's Degree [(]4 year[)]", "Bachelors", Degree)) %>% 
  mutate(Degree = gsub("Master's Degree or higher", "Masters", Degree)) %>% 
  mutate(Degree = gsub("Some college or university but did not graduate", "Some College", Degree)) %>% 
  mutate(Degree = gsub("High School Graduate", "High School", Degree)) %>% 
  mutate(Degree = gsub("Associate Degree [(]2 year[)]", "Associates", Degree)) %>% 
  mutate(Degree = factor(Degree, levels =c("High School", "Associates", "Some College", "Bachelors", "Masters"))) %>% 
  mutate(Experience = gsub("6 years or more", ">5Years", Experience)) %>% 
  mutate(Experience = gsub("5 years or less", "<=5Years", Experience)) %>% 
  mutate(Duration_min = as.numeric(Duration_min)) %>%
  mutate(GTScore = gsub("121 or higher", ">120", GTScore)) %>% 
  mutate(GTScore = gsub("120 or lower", "<=120", GTScore)) %>%
  mutate(GTScore = gsub("N/A or prefer not to report", "not reported", GTScore)) %>% 
  mutate(ID = as.character(`Record ID`)) %>% 
  dplyr::select(ID,Miss_P, Miss_C, sincere, Duration_min, Rank:`3D_Q16`) %>% 
  arrange(Work_Role) #arrange data by work role#select features to carry forward in analysis

df_rawscore <- df_preprocess %>% 
  mutate_at(vars(`Q1_A_1` : `Q21_A_16`), likertNum) %>% #score likert items
  mutate(
    `3D_Q16` = if_else(`3D_Q16` =="E", 1, 0), #score cognitive questions according to scoring key from ICAR
    `3D_Q24` = if_else(`3D_Q24` == "C", 1, 0),
    `3D_Q29` = if_else(`3D_Q29` == "F", 1, 0),
    `3D_Q42` = if_else(`3D_Q42` == "E", 1, 0),
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

item_stats <- df_rawscore2 %>%
  dplyr::select(ID, Q1_A_1:Q21_A_16) %>%
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
  summarySE(groupvars = "Question", measurevar = "Score", na.rm = TRUE) %>% arrange(Score) %>%
  dplyr::select(Question, Score, sd) %>% mutate(Score = round(Score, 1), sd=round(sd, 1)) %>% 
  mutate(Rank = rank(-Score, ties.method = "random")) %>% arrange(Rank)

#Infrequency of response calculation
easy_mean <- item_stats %>%   #Average score of 20 easiest questions
  filter(Rank<=10) %>% dplyr::select(Score)
easy_mean <- mean(easy_mean$Score, na.rm = TRUE)

df_infreq <-  
  df_rawscore2 %>% 
  dplyr::select(ID, Q1_A_1:Q21_A_16) %>% 
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
  left_join(item_stats %>% select(-Score), by="Question") %>% 
  filter(Rank<=10) %>% ungroup() %>% 
  group_by(ID)  %>% 
  summarise(infreq= abs(easy_mean-mean(Score, na.rm = TRUE))) #lower infreq is better  

df_rawscore3 <- df_rawscore2 %>% 
  left_join(df_infreq, by="ID") %>% 
  mutate(Survey_Completion = if_else(Miss_C<.5 & Miss_P <.5 & sincere<5 & infreq<1, "Yes", "No"))
df_rawscore3 %>% group_by(Work_Role, Survey_Completion) %>% summarise(n=n())

#Rasch scoring and analysis of 25 cognitive questions
df_Rasch <- df_rawscore3 %>% 
        filter(Miss_C<.5, sincere<=5) %>% #filter out based on missingness and sincerity of effort
        dplyr::select(ID, Pattern_Q1:`3D_Q16`) %>% 
        column_to_rownames("ID") 

df_Rasch %>% gather(Pattern_Q1: `3D_Q16`, key = item, value=score) %>% 
  summarySE(groupvars = "item", measurevar = "score", na.rm = TRUE)

rm <- RM(df_Rasch)
pp <- person.parameter(rm)
df_Rasch2 <-  pp$theta.table %>% 
        rownames_to_column("ID") %>% 
        mutate(Proficiency=round(`Person Parameter`, 2)) %>%  dplyr::select(ID, Proficiency) 

summary(rm)
etapar <- -coef(rm) 
round(sort(etapar), 2)
plot(pp)
plotjointICC(rm)
plotPImap(rm, sorted = TRUE)
#Reliability of Person Separation
summary(SepRel(pp))
pmfit  <- PersonMisfit(pp)
summary(pmfit)
#Item fit
Di_itemfit <- itemfit(pp)
Di_itemfit

#Scoring Personality: average response on likert scale
#Scoring Cognitive Test: percent correct by test type
df_scored <- df_rawscore3 %>% 
#  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
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
                Verbal_Q14, Verbal_Q16, Verbal_Q17, Verbal_Q32, Verbal_Q4), na.rm = TRUE )) %>%
        left_join(df_Rasch2, by="ID") #join in Rasch scores for proficiency based on 25 cognitive items

