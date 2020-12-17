#Load libraries
library(readxl)
library(Rmisc)
library(Amelia)
library(Hmisc)
library(corrplot)
library(eRm)
library(ltm)
library(tidyverse)
library(psych)
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

#establish a threshold to discard respondent data based on missingness
miss_limit = .20 #percentage of missing data allowed

#count of missing items and standard deviation of responses: Personality
df_mis_P <-  read_xlsx(file) %>%
  gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key=Question, value=Answer)  %>%
  group_by(`Record ID`) %>% 
  mutate_at(vars(Answer), likertNum) %>%  
  summarise(Miss_P = sum(is.na(Answer))/174, sd_P = sd(Answer, na.rm = TRUE))    #174 total Personality items
 
#count of missing items: Cognitive
df_mis_C <-  read_xlsx(file) %>% 
  gather(`Pattern_Q1 (Q23)`: `3D_Q16 (Q47)`, key=Question, value=Answer) %>%
  group_by(`Record ID`) %>% 
  summarise(Miss_C = sum(is.na(Answer))/25) #25 total Cognitive items

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
        `Security+` = `FormalCerts (Q58_9)`,
        `OtherCert` = `FormalCerts (Q58_10)`) %>% 
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
        left_join(df_mis_C, by = "Record ID") %>% #join in missingness stat for Cognitive features
        drop_na(Work_Role) %>% #drop rows with no data based on entry to work role
        arrange(Work_Role) %>% #arrange data by work role
        rownames_to_column("ID") %>% #add a new ID label beginning with 1
        select(-`Record ID`) %>% #remove Verint ID label
        select(ID,Miss_P, Miss_C, sd_P, Duration_min, Rank:`3D_Q16`) #select features to carry forward in analysis

#Replace Work roles with simple categories
df_preprocess$Work_Role <-  gsub("Tier-1 - Remote Operator|Tier-1 - Capability Developer|Tier-1 - Exploitation Analyst", "Tier_1", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_2", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_3", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 4 -  Other authorized cyber work role|I am currently not assigned a cyber work role or I am assigned as a student", "Tier_4", df_preprocess$Work_Role)
df_preprocess$Duration_min <- as.numeric(df_preprocess$Duration_min)
#plot a missmap of raw data
missmap(df_preprocess %>% select(ID: `3D_Q16`, Duration_min) %>% arrange(Duration_min), rank.order=FALSE, main = "Missing values vs observed")

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

#Exploration of Non-purposeful responses and determination of infrequency of responses based on False Keyed (High Endorsement) and True Keyed (Low Endorsement) items
item_stats <- df_rawscore2 %>%
        filter(Miss_P< miss_limit) %>%
        select(ID, Q1_A_1:Q21_A_16) %>%
        gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
        summarySE(groupvars = "Question", measurevar = "Score", na.rm = TRUE) %>% arrange(Score)

df_infreq <-  df_rawscore2 %>% 
        group_by(ID) %>% 
        mutate(infreq= .5*(6-mean(c(Q22_A_1, Q1_A_5, Q22_A_4, Q3_A_6, Q1_A_1, Q17_A_4),na.rm = TRUE )) +  #item with highest response averages
        .5*(mean(c(Q21_A_13, Q15_A_5, Q8_A_3, Q10_A_23, Q12_A_3, Q14_A_4), na.rm=TRUE)))  %>%  #items with lowest response averages
        select(ID,infreq, sd_P ) %>% ungroup() %>% 
        mutate_at (vars(infreq, sd_P), scale) %>% 
        group_by(ID) %>% 
        mutate(infreq2=mean(c(infreq, -(sd_P)), na.rm = TRUE) ) %>% select (ID, infreq, sd_P, infreq2) #lower infreq is better; higher sd is better

df_infreq %>% ggplot() + geom_boxplot(aes(y=infreq)) #boxplot of infreq scale

# #Imputation of mean values for personality items by work role   
# df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_1" ) ) %>% #Tier 1 group
#         mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) #personality items
# df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_2", "Tier_3", "Tier_4"))  %>% #Tier 2, 3, & 4 group
#         mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) #personality items        
# df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b) #bind together the Tier 1 and the Tier 2/3/4 dataframes

#Rasch scoring and analysis of 25 cognitive questions
df_Rasch <- df_rawscore2 %>% 
        filter(Miss_C<miss_limit) %>% 
        dplyr::select(ID, Pattern_Q1:`3D_Q16`) %>% 
        column_to_rownames("ID") 
rm <- RM(df_Rasch)
pp <- person.parameter(rm)
df_Rasch2 <-  pp$theta.table %>% 
        rownames_to_column("ID") %>% 
        mutate(Proficiency=round(`Person Parameter`, 2)) %>%  dplyr::select(ID, Proficiency) 

summary(rm)
etapar <- -coef(rm) 
round(sort(etapar), 2)
plot(pp)
plotjointICC(rm.res)
plotPImap(rm.res, sorted = TRUE)
#Reliability of Person Separation
summary(SepRel(pp))
pmfit  <- PersonMisfit(pp)
summary(pmfit)
#Item fit
Di_itemfit <- itemfit(pp)
Di_itemfit

#Item Analysis using psych package
keys.list <- list(
        Problem_Solving = c('Q1_A_1', 'Q1_A_2', 'Q1_A_3', 'Q1_A_4', 'Q1_A_5'),
        Depth_Thought = c('Q3_A_1', 'Q3_A_2', 'Q3_A_3', 'Q3_A_4', 'Q3_A_5', 'Q3_A_6', 'Q3_A_7'),
        Mental_Quickness = c('Q4_A_1', 'Q4_A_2', 'Q4_A_3', 'Q4_A_4', 'Q4_A_5', 'Q4_A_6', 'Q4_A_7'),
        Need_Cognition = c('Q5_A_1', 'Q5_A_2', 'Q5_A_3', 'Q5_A_4', 'Q5_A_5', 'Q5_A_6'), 
        Love_Learning = c('Q6_A_1', 'Q6_A_2', 'Q6_A_3', 'Q6_A_4', 'Q6_A_5', 'Q6_A_6'),
        Creativity = c('Q7_A_1', 'Q7_A_2', 'Q7_A_3', 'Q7_A_4', 'Q7_A_5', 'Q7_A_6', 'Q7_A_7', 'Q7_A_8', 'Q7_A_9', 'Q7_A_10', 'Q7_A_11', 'Q7_A_12', 'Q7_A_13'),
        Concientiousness = c('Q8_A_1', 'Q8_A_2', 'Q8_A_3', 'Q8_A_4', 'Q8_A_5', 'Q8_A_6', 'Q8_A_7', 'Q8_A_8', 'Q8_A_9', 'Q8_A_10'),
        Orderliness = c('Q9_A_1', 'Q9_A_2', 'Q9_A_3', 'Q9_A_4', 'Q9_A_5'),
        Deprivation_Sensitivity = c('Q10_A_1', 'Q10_A_2', 'Q10_A_3', 'Q10_A_4', 'Q10_A_5'),
        Joyous_Exploration = c('Q10_A_6', 'Q10_A_7', 'Q10_A_8', 'Q10_A_9', 'Q10_A_10'),
        Social_Curiosity = c('Q10_A_11', 'Q10_A_12', 'Q10_A_13', 'Q10_A_14', 'Q10_A_15'),
        Stress_Tolerance = c('Q10_A_16', 'Q10_A_17', 'Q10_A_18', 'Q10_A_19', 'Q10_A_20'),
        Thrill_Seeking = c('Q10_A_21', 'Q10_A_22', 'Q10_A_23','Q10_A_24', 'Q10_A_25'),
        Interpersonal_Understanding = c('Q11_A_1', 'Q11_A_2', 'Q11_A_3'),
        Self_Confidence = c('Q11_A_4', 'Q11_A_5', 'Q11_A_6', 'Q11_A_7'),
        Suspension_Judgement = c('Q11_A_8', 'Q11_A_9', 'Q11_A_10', 'Q11_A_11', 'Q11_A_12'),
        Self_Discipline = c('Q12_A_1', 'Q12_A_2', 'Q12_A_3', 'Q12_A_4'), 
        Industriness = c('Q13_A_1', 'Q13_A_2', 'Q13_A_3', 'Q13_A_4','Q13_A_5'), 
        Enthusiasm = c('Q14_A_1', 'Q14_A_2', 'Q14_A_3', 'Q14_A_4', 'Q14_A_5'),
        Flow_Proneness = c('Q15_A_1', 'Q15_A_2', 'Q15_A_3', 'Q15_A_4', 'Q15_A_5', 'Q15_A_6'),
        Cyberwork_Confidence = c('Q16_A_1', 'Q16_A_2', 'Q16_A_3', 'Q16_A_4', 'Q16_A_5', 'Q16_A_6'),
        General_SelfEfficacy = c('Q17_A_1', 'Q17_A_2', 'Q17_A_3', 'Q17_A_4', 'Q17_A_5', 'Q17_A_6', 'Q17_A_7', 'Q17_A_8'),
        Resilience = c('Q18_A_1', 'Q18_A_2', 'Q18_A_3', 'Q18_A_4', 'Q18_A_5', 'Q18_A_6'),
        Teamwork = c('Q19_A_1', 'Q19_A_2', 'Q19_A_3', 'Q19_A_4', 'Q19_A_5'),
        Leadership = c('Q20_A_1', 'Q20_A_2', 'Q20_A_3', 'Q20_A_4', 'Q20_A_5', 'Q20_A_6', 'Q20_A_7'),
        Intellectual_Openness = c('Q22_A_1', 'Q22_A_2', 'Q22_A_3', 'Q22_A_4', 'Q22_A_5', 'Q22_A_6', 'Q22_A_7', 'Q22_A_8', 'Q22_A_9', 'Q22_A_10'),
        Tolerance = c('Q21_A_1', 'Q21_A_2', 'Q21_A_3', 'Q21_A_4', 'Q21_A_5', 'Q21_A_6', 'Q21_A_7',
                 'Q21_A_8', 'Q21_A_9', 'Q21_A_10', 'Q21_A_11', 'Q21_A_12', 'Q21_A_13', 'Q21_A_14', 'Q21_A_15', 'Q21_A_16') )

item_analysis_data <- df_rawscore2 %>% select (Q1_A_1:Q21_A_16)
scores <- scoreItems(keys.list,item_analysis_data)
scores


#Scoring Personality: average response on likert scale
#Scoring Cognitive Test: percent correct by test type
df_scored <- df_rawscore2 %>% 
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

#Visualizations #####################################################################################
#Comparison of Raw Score% and Theta
df_scored %>% filter(Miss_C<miss_limit) %>% 
        drop_na(Cog_tot, Proficiency, Duration_min) %>% 
        ungroup() %>% 
        mutate_at(vars(Cog_tot), scale) %>% 
        ggplot(aes(x=Cog_tot, y=Proficiency)) + 
        geom_point(color="blue") + 
        ylim(-4,4) + ylab("Proficiency (theta)") +
        geom_abline(slope = 1, linetype="dashed", color="red", size=1) + 
        xlim(-4,4) +  
        xlab("25-item raw score (z-score)") + 
        ggsave("Proficiency_RawScore_Comparison.jpg", width = 5, height = 4, units = "in" )

#Personality raw scores
df_scored %>% filter(Miss_P<miss_limit) %>% 
        gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>% 
        ggplot(aes(x=reorder(Dimension, Score, FUN=mean, na.rm=TRUE), y=Score)) +
        geom_boxplot() +
        coord_flip() + xlab("") + 
        ggtitle("Boxplot: Average Scores by Personality Dimension") +
        labs(caption = "Note: 1=Low Self-report; 5=High Self-report") +
        ggsave("Personality_RawScores.jpg", width = 10, height = 6, units = "in" ) #save to folder

#Cognitive raw scores
df_scored %>% filter(Miss_C<miss_limit) %>% select(ID, Analogies:Cog_tot)  %>% 
        gather(Analogies:Cog_tot, key=Dimension, value=Score) %>% 
        ggplot(aes(x=reorder(Dimension, Score, FUN=mean, na.rm=TRUE), y=Score)) +
        geom_boxplot() +
        coord_flip() + xlab("") +
        ylab("Score (% correct)") +
        ggtitle("Boxplot: Scores by Cognitive Test Type") +
        ggsave("Cognitive_RawScores.jpg", width = 10, height = 6, units = "in" ) #save to folder

#Correlation Plot of Personality Dimensions
dfcorrplot <-  df_scored %>%  #dataframe of select features in a matrix
        filter(Miss_P<miss_limit) %>% 
        select(ID, Problem_Solving:Tolerance) %>%
        column_to_rownames("ID") %>% as.data.frame()

corrplot(cor(dfcorrplot), method="color", order="hclust", type="full", addrect=10, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=2, number.font=.5 , number.cex=.5, tl.cex=.5) #corrplot personality items

#Correlation Plot Cognitive
dfcorrplot2 <-  df_scored %>% 
        filter(Miss_C<miss_limit) %>% 
        select(ID, Analogies:Verbal) %>%
        column_to_rownames("ID") %>% as.data.frame()

corrplot(cor(dfcorrplot2), method="color", order="hclust", type="full", addrect=2, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=2, number.font=.5 , number.cex=.5) #corrplot cognitive items

#Visualization of Personality Test Results
#Scale Personality scored data with mean of zero and std deviation of 1
df_scored2 <- df_scored %>% filter(Miss_P <miss_limit) %>% ungroup() %>% 
        select(ID, Work_Role, Problem_Solving:Tolerance) %>% 
        mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
        gather(Problem_Solving:Tolerance, key=Dimension, value=Score) 

df_scored2_summary <-  df_scored2  %>% #compute summary statistics for all work roles
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score")

df_scored2_order <- df_scored2 %>% #compute summary statistics for Tier 1 work role
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score") %>% 
        filter(Work_Role=="Tier_1") %>% 
        mutate(order=rank(Score)) %>% 
        select(Dimension, order)

#Visualization of summary statistics for Personality results by work role      
df_scored2_summary %>% left_join(df_scored2_order) %>%  filter(N>5) %>% #apply a filter to ensure n is greater than 5
        ggplot(aes(x=reorder(Dimension, order), y=Score, color=Work_Role, group=Work_Role)) +
        geom_point(aes(size=Work_Role)) +
        scale_size_manual(values=c(3,2,2)) +
        geom_line(aes(linetype=Work_Role)) +
        scale_linetype_manual(values=c("dashed", "blank", "blank", "blank"))+
       #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
        coord_flip() + xlab(" ") + ylab("mean scaled score") +
        scale_color_manual(values=c("red", "darkgray", "darkgray", "darkgray")) +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
        ylim(-1,1) +
        ggtitle("Scaled Score Mean by Work Role") +
        ggsave("PersonalityScores_WorkRole.jpg", width = 10, height = 6, units = "in" ) #save to folder

#Visualization of Cognitive Test Results
#Scale Cognitive scored data
df_scored3 <- df_scored %>% filter(Miss_C < miss_limit) %>% ungroup() %>% 
        select(ID, Work_Role, Analogies:Verbal, Cog_tot, Proficiency) %>% 
        mutate_at(vars(Analogies:Verbal), scale) %>% #scale function
        gather(Analogies:Proficiency, key=Test, value=Score) 

df_scored3_summary <-  df_scored3  %>%  #compute summary statistics for all work roles
        summarySE(groupvars = c("Work_Role", "Test"), measurevar = "Score", na.rm = TRUE)

df_scored3_order <- df_scored3 %>% #compute summary statistics for Tier 1 work role
        summarySE(groupvars = c("Work_Role", "Test"), measurevar = "Score") %>% 
        filter(Work_Role=="Tier_1") %>% 
        mutate(order=rank(Score)) %>% 
        select(Test, order)

#Visualization of summary statistics for Cognitive results by work role     
df_scored3_summary %>% left_join(df_scored3_order)  %>%
        filter(N>5) %>% 
        ggplot(aes(x=reorder(Test, Score, FUN=max), y=Score, color=Work_Role, group=Work_Role)) +
        geom_point(aes(size=Work_Role)) +
        scale_size_manual(values=c(3,2,2)) +
        geom_line(aes(linetype=Work_Role)) +
        scale_linetype_manual(values=c("dashed", "blank", "blank", "blank"))+
        #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
        coord_flip() + xlab(" ") + ylab("overall proficiency & test mean scaled score ") +
        scale_color_manual(values=c("red", "darkgray", "darkgray", "darkgray")) +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
        ylim(-3,3) +
        ggtitle("Cognitive Scores by Work Role") +
        ggsave("CognitveScores_WorkRole.jpg", width = 10, height = 6, units = "in" ) #save to folder

########################################### Working Line - Under Construction #################
