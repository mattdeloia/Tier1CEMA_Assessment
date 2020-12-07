#load libraries
library(readxl)
library(Rmisc)
library(Amelia)
library(Hmisc)
library(tidyverse)

#functions
likertNum <- function(x){
        case_when(
                x == "Very Accurate" ~ 5,
                x == "Moderately Accurate" ~ 4,
                x == "Neither Accurate Nor Inaccurate" ~ 3,
                x == "Moderately Inaccurate" ~ 2,
                x == "Very Inaccurate" ~ 1,
                x == "Strongly Agree" ~ 5,
                x == "Moderately Agree" ~ 4,
                x == "Neither Agree nor Disagree" ~ 3,
                x == "Moderately Disagree" ~ 2,
                x == "Strongly Disagree" ~ 1
        )
}

#load raw data and pre-process
df <- read_xlsx("CyberCompetencies.xlsx") %>% 
        drop_na(`Work_Role (Q50)`) %>% 
        rownames_to_column("ID") %>% 
        select(-`Record ID`) %>% 
        dplyr::rename(
                `OSCP`        =`FormalCerts (Q58_1)`,
                `OSCE`        = `FormalCerts (Q58_2)`,
                `GPEN`      = `FormalCerts (Q58_3)`,
                `GXPN`      = `FormalCerts (Q58_4)`,
                `GCIH`      = `FormalCerts (Q58_5)`,
                `CEH`       = `FormalCerts (Q58_6)`,
                `CISSP`     = `FormalCerts (Q58_7)`,
                `CISM`        = `FormalCerts (Q58_8)`,
                `Security+` = `FormalCerts (Q58_9)`,
                `OtherCert` = `FormalCerts (Q58_10)`) %>% 
        mutate(Duration_min = round((Completed - Started),2)) %>%
        mutate(Duration_min=replace_na(Duration_min, 0)) %>% 
        mutate(Complete = if_else(Duration_min >0, "Yes", "No"))

missmap(df %>% select(ID: Completed), main = "Missing values vs observed")

df_preprocess <- df %>% 
        gather(`Rank (Q70)`:`Hexidecimal (Q69)`, key= "Question", value = "Response" ) %>%
        separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
        select (-Question2) %>% 
        pivot_wider(names_from = Question, values_from = Response) %>% 
        gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key="Question", value = "Response") %>% 
        separate(col = `Question`, into=c("Question2", "Question"),  sep = "[(]", remove="TRUE") %>% 
        select(-Question2) 

df_preprocess$Question <- gsub("[)]", "", df_preprocess$Question)


df_preprocess2 <- df_preprocess %>% pivot_wider(names_from = Question, values_from = Response) %>% 
        gather(`Pattern_Q1 (Q23)`:`3D_Q16 (Q47)`, key= "Question", value = "Response" ) %>% 
        separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
        select(-Question2) %>% pivot_wider(names_from = Question, values_from = Response) %>%
        select(ID, Rank:`3D_Q16`, Complete, Duration_min)

missmap(df_preprocess2 %>% select(ID: `3D_Q16`, Complete) %>% arrange(Complete), rank.order=FALSE, main = "Missing values vs observed")

########################################### Working Line - Under Construction #################3


df_rawscore <- df_preprocess2 %>%   
        mutate_at(vars(`Q1_A_1` : `Q21_A_16`), likertNum) %>% 
        mutate(
        `3D_Q16` = if_else(`3D_Q16` =="E", 1, 0),
        `3D_Q24` = if_else(`3D_Q24` == "C", 1, 0),
        `3D_Q29` = if_else(`3D_Q29` == "F", 1, 0),
        `3D_Q42` = if_else(`3D_Q29` == "F", 1, 0),
        `3D_Q58` = if_else(`3D_Q58` == "C", 1, 0),
        Analogies_Q14 = if_else(Analogies_Q14 == "C", 1, 0),
        Analogies_Q16 = if_else(Analogies_Q16 == "H", 1, 0),
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
columnsToReverse <-  c('Q10_A_16','Q10_A_17','Q10_A_18','Q10_A_19','Q10_A_20', 'Q18_A_2', 'Q18_A_4', 'Q18_A_6', 'Q22_A_7', 'Q22_A_8', 'Q22_A_9', 'Q22_A_10', 'Q21_A_1', 'Q21_A_3','Q21_A_5','Q21_A_7','Q21_A_9', 'Q21_A_11', 'Q21_A_13', 'Q21_A_15') 
 
df_rawscore2[,columnsToReverse] <- 6-df_rawscore2[, columnsToReverse] 

#Imputation Code     
df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier-1 - Remote Operator", 
                                                            "Tier-1 - Exploitation Analyst", 
                                                            "Tier-1 - Capability Developer" ) )
df_rawscore2_a[31:204] <- impute( df_rawscore2_a[31:204], object = NULL, method = "median/mode", flag = FALSE)

df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier 2- Data Architect, Network Analyst, System Analyst (All Master Proficiency level only)",
                                                            "Tier 3- Other work role directly assigned to a Cyber Mission Force Team",
                                                            "Tier 4 -  Other authorized cyber work role",
                                                            "I am currently not assigned a cyber work role or I am assigned as a student")) 
df_rawscore2_b[31:204] <- impute( df_rawscore2_b[31:204], object = NULL, method = "median/mode", flag = FALSE)

df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b)

#Scoring Personality Dimensions and Cognitive Test
df_scored <- df_rawscore3 %>% 
        group_by (ID) %>% 
        mutate(
        Problem_Solving = sum(Q1_A_1, Q1_A_2, Q1_A_3, Q1_A_4, Q1_A_5),
        Depth_Thought = sum (Q3_A_1, Q3_A_2, Q3_A_3, Q3_A_4, Q3_A_5, Q3_A_6, Q3_A_7),
        Mental_Quickness = sum (Q4_A_1, Q4_A_2, Q4_A_3, Q4_A_4, Q4_A_5, Q4_A_6, Q4_A_7),
        Need_Cognition = sum (Q5_A_1, Q5_A_2, Q5_A_3, Q5_A_4, Q5_A_5, Q5_A_6), 
        Love_Learning = sum (Q6_A_1, Q6_A_2, Q6_A_3, Q6_A_4, Q6_A_5, Q6_A_6),
        Creativity = sum (Q7_A_1, Q7_A_2, Q7_A_3, Q7_A_4, Q7_A_5, Q7_A_6, Q7_A_7, Q7_A_8, Q7_A_9, Q7_A_10, Q7_A_11, Q7_A_12, Q7_A_13),
        Concientiousness = sum (Q8_A_1, Q8_A_2, Q8_A_3, Q8_A_4, Q8_A_5, Q8_A_6, Q8_A_7, Q8_A_8, Q8_A_9, Q8_A_10),
        Orderliness = sum (Q9_A_1, Q9_A_2, Q9_A_3, Q9_A_4, Q9_A_5),
        Deprivation_Sensitivity = sum (Q10_A_1, Q10_A_2, Q10_A_3, Q10_A_4, Q10_A_5),
        Joyous_Exploration = sum (Q10_A_6, Q10_A_7, Q10_A_8, Q10_A_9, Q10_A_10),
        Social_Curiosity = sum (Q10_A_11, Q10_A_12, Q10_A_13, Q10_A_14, Q10_A_15),
        Stress_Tolerance = sum (Q10_A_16, Q10_A_17, Q10_A_18, Q10_A_19, Q10_A_20),
        Thrill_Seeking = sum (Q10_A_21, Q10_A_22, Q10_A_23, Q10_A_24, Q10_A_25),
        Interpersonal_Understanding = sum (Q11_A_1, Q11_A_2, Q11_A_3),
        Self_Confidence = sum (Q11_A_4, Q11_A_5, Q11_A_6, Q11_A_7),
        Suspension_Judgement = sum (Q11_A_8, Q11_A_9, Q11_A_10, Q11_A_11, Q11_A_12),
        Self_Discipline = sum (Q12_A_1, Q12_A_2, Q12_A_3, Q12_A_4), 
        Industriness = sum (Q13_A_1, Q13_A_2, Q13_A_3, Q13_A_4, Q13_A_5), 
        Enthusiasm = sum (Q14_A_1, Q14_A_2, Q14_A_3, Q14_A_4, Q14_A_5),
        Flow_Proneness = sum (Q15_A_1, Q15_A_2, Q15_A_3, Q15_A_4, Q15_A_5, Q15_A_6),
        Cyberwork_Confidence = sum (Q16_A_1, Q16_A_2, Q16_A_3, Q16_A_4, Q16_A_5, Q16_A_6),
        General_SelfEfficacy = sum (Q17_A_1, Q17_A_2, Q17_A_3, Q17_A_4, Q17_A_5, Q17_A_6, Q17_A_7, Q17_A_8),
        Resilience = sum (Q18_A_1, Q18_A_2, Q18_A_3, Q18_A_4, Q18_A_5, Q18_A_6),
        Teamwork = sum (Q19_A_1, Q19_A_2, Q19_A_3, Q19_A_4, Q19_A_5),
        Leadership = sum (Q20_A_1, Q20_A_2, Q20_A_3, Q20_A_4, Q20_A_5, Q20_A_6, Q20_A_7),
        Intellectual_Openness = sum (Q22_A_1, Q22_A_2, Q22_A_3, Q22_A_4, Q22_A_5, Q22_A_6, Q22_A_7, Q22_A_8, Q22_A_9, Q22_A_10),
        Tolerance = sum (Q21_A_1, Q21_A_2, Q21_A_3, Q21_A_4, Q21_A_5, Q21_A_6, Q21_A_7, Q21_A_8, Q21_A_9, Q21_A_10, Q21_A_11, Q21_A_12, Q21_A_13, Q21_A_14, Q21_A_15, Q21_A_16) ) 

#scale data
df_scored2 <- df_scored %>% ungroup() %>% 
        select(ID, Work_Role, Problem_Solving:Tolerance) %>% 
        mutate_at(vars(Problem_Solving:Tolerance), scale)

#gather Data

df_scored3 <- df_scored2 %>% gather(Problem_Solving:Tolerance, key=Dimension, value=Score) 

#Replace Work roles with simple categories
df_scored3$Work_Role <-  gsub("Tier-1 - Remote Operator|Tier-1 - Capability Developer|Tier-1 - Exploitation Analyst", "Tier_1", df_scored3$Work_Role)
df_scored3$Work_Role <-  gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_2", df_scored3$Work_Role)
df_scored3$Work_Role <-  gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_3", df_scored3$Work_Role)
df_scored3$Work_Role <-  gsub("Tier 4 -  Other authorized cyber work role|I am currently not assigned a cyber work role or I am assigned as a student", "Tier_4", df_scored3$Work_Role)

df_scored3_summary <-  df_scored3 %>%  
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score")
#Visualization        
df_scored3_summary %>% filter(Work_Role !="Tier_2") %>% ggplot(aes(x=reorder(Dimension, Score, FUN=min), y=Score, color=Work_Role)) +
        geom_point() + coord_flip() + xlab(" ") + 
        scale_color_manual(values=c("red", "blue", "gray", "gray")) +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") + ylim(-1,1) 

# #Rasch scoring cognitive questions
# df_cognitive <- df_scores %>% select(ID, Pattern_Q1:3D_Q16) %>% column_to_rownames("ID") 
# rm.res <- RM(df_cognitive)
# pp <- person.parameter(rm.res)
# df_cognitive2 <-  pp$theta.table %>% 
#         rownames_to_column("ID") %>% 
#         mutate(Proficiency=round(`Person Parameter`, 2)) %>%  select(ID, Proficiency) 