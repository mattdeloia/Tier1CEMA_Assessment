#load libraries
library(readxl)
library(Rmisc)
library(Amelia)
library(Hmisc)
library(tidyverse)
library(corrplot)
library(eRm)

#name of datafile
file <- "CyberCompetencies.xlsx"

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

miss_limit = .10
#count number of missing items (Personality)
df_mis_P <-  read_xlsx(file) %>% 
        gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key=Question, value=Answer) %>% 
        group_by(`Record ID`) %>% summarise(Pers_Miss = sum(is.na(Answer))/174)


#count number of missing items (Cognitive)
df_mis_C <-  read_xlsx(file) %>% gather(`Pattern_Q1 (Q23)`: `3D_Q16 (Q47)`, key=Question, value=Answer) %>% 
        group_by(`Record ID`) %>% summarise(Cog_Miss = sum(is.na(Answer))/25)

#load raw data and pre-process
df_preprocess <- read_xlsx(file) %>% 
        dplyr::rename(
                `Analogies_Q25 (Q34)` = `Analogies_Q16 (Q34)`,
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
        gather(`Rank (Q70)`:`Hexidecimal (Q69)`, key= "Question", value = "Response" ) %>%
        separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
        select (-Question2) %>% 
        pivot_wider(names_from = Question, values_from = Response) %>% 
        gather(`1_ProblemSolver (Q1_A_1)`: `21_ToleranceOfAmbiguity (Q21_A_16)`, key="Question", value = "Response") %>% 
        separate(col = `Question`, into=c("Question2", "Question"),  sep = "[(]", remove="TRUE") %>% 
        select(-Question2) %>% 
        separate(col = `Question`, into=c("Question", "Question2"),  sep = "[)]", remove="TRUE") %>% 
        select(-Question2)  %>% 
        pivot_wider(names_from = Question, values_from = Response) %>% 
        gather(`Pattern_Q1 (Q23)`:`3D_Q16 (Q47)`, key= "Question", value = "Response" ) %>% 
        separate(col = `Question`, into=c("Question", "Question2"),  sep = " ", remove="TRUE") %>% 
        select(-Question2) %>% pivot_wider(names_from = Question, values_from = Response) %>%
        mutate(Duration_min = round((Completed - Started),2)) %>%
        mutate(Duration_min = replace_na(Duration_min, 0)) %>%  
        left_join(df_mis_P) %>% 
        left_join(df_mis_C) %>% 
        drop_na(Work_Role) %>% 
        arrange(Work_Role) %>% 
        rownames_to_column("ID") %>%
        select(-`Record ID`) %>% 
        select(ID, Rank:`3D_Q16`, Pers_Miss, Cog_Miss, Duration_min)

#Replace Work roles with simple categories
df_preprocess$Work_Role <-  gsub("Tier-1 - Remote Operator|Tier-1 - Capability Developer|Tier-1 - Exploitation Analyst", "Tier_1", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_2", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_3", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 4 -  Other authorized cyber work role|I am currently not assigned a cyber work role or I am assigned as a student", "Tier_4", df_preprocess$Work_Role)

missmap(df_preprocess %>% select(ID: `3D_Q16`, Duration_min) %>% arrange(Duration_min), rank.order=FALSE, main = "Missing values vs observed")

df_rawscore <- df_preprocess %>%   
        mutate_at(vars(`Q1_A_1` : `Q21_A_16`), likertNum) %>% 
        mutate(
        `3D_Q16` = if_else(`3D_Q16` =="E", 1, 0),
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
columnsToReverse <-  c('Q10_A_16','Q10_A_17','Q10_A_18','Q10_A_19','Q10_A_20', 'Q18_A_2', 'Q18_A_4', 'Q18_A_6', 'Q22_A_7', 'Q22_A_8', 'Q22_A_9', 'Q22_A_10', 'Q21_A_1', 'Q21_A_3','Q21_A_5','Q21_A_7','Q21_A_9', 'Q21_A_11', 'Q21_A_13', 'Q21_A_15') 
 
df_rawscore2[,columnsToReverse] <- 6-df_rawscore2[, columnsToReverse] 

#Imputation Code     
df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
        mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% 
        mutate_at(vars(Pattern_Q1:`3D_Q16`),~ifelse(is.na(.x), 0, .x))
    
df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_2", "Tier_3", "Tier_4"))  %>% 
        mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>% 
        mutate_at(vars(Pattern_Q1:`3D_Q16`),~ifelse(is.na(.x), 0, .x)) 


df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b)

#Rasch scoring cognitive questions
df_Rasch <- df_rawscore3 %>% filter(Cog_Miss<miss_limit) %>% select(ID, Pattern_Q1:`3D_Q16`) %>% 
        column_to_rownames("ID") 

rm.res <- RM(df_Rasch)
pp <- person.parameter(rm.res)
df_Rasch2 <-  pp$theta.table %>% 
        rownames_to_column("ID") %>% 
        mutate(Proficiency=round(`Person Parameter`, 2)) %>%  select(ID, Proficiency) 

#Scoring Personality Dimensions and Cognitive Test and normalize to 1
df_scored <- df_rawscore3 %>% 
        group_by (ID) %>% 
        mutate(
        Problem_Solving = sum(Q1_A_1, Q1_A_2, Q1_A_3, Q1_A_4, Q1_A_5)/5,
        Depth_Thought = sum (Q3_A_1, Q3_A_2, Q3_A_3, Q3_A_4, Q3_A_5, Q3_A_6, Q3_A_7)/7,
        Mental_Quickness = sum (Q4_A_1, Q4_A_2, Q4_A_3, Q4_A_4, Q4_A_5, Q4_A_6, Q4_A_7)/7,
        Need_Cognition = sum (Q5_A_1, Q5_A_2, Q5_A_3, Q5_A_4, Q5_A_5, Q5_A_6)/6, 
        Love_Learning = sum (Q6_A_1, Q6_A_2, Q6_A_3, Q6_A_4, Q6_A_5, Q6_A_6)/6,
        Creativity = sum (Q7_A_1, Q7_A_2, Q7_A_3, Q7_A_4, Q7_A_5, Q7_A_6, Q7_A_7, Q7_A_8, Q7_A_9, Q7_A_10, Q7_A_11, Q7_A_12, Q7_A_13)/13,
        Concientiousness = sum (Q8_A_1, Q8_A_2, Q8_A_3, Q8_A_4, Q8_A_5, Q8_A_6, Q8_A_7, Q8_A_8, Q8_A_9, Q8_A_10)/10,
        Orderliness = sum (Q9_A_1, Q9_A_2, Q9_A_3, Q9_A_4, Q9_A_5)/5,
        Deprivation_Sensitivity = sum (Q10_A_1, Q10_A_2, Q10_A_3, Q10_A_4, Q10_A_5)/5,
        Joyous_Exploration = sum (Q10_A_6, Q10_A_7, Q10_A_8, Q10_A_9, Q10_A_10)/5,
        Social_Curiosity = sum (Q10_A_11, Q10_A_12, Q10_A_13, Q10_A_14, Q10_A_15)/5,
        Stress_Tolerance = sum (Q10_A_16, Q10_A_17, Q10_A_18, Q10_A_19, Q10_A_20)/5,
        Thrill_Seeking = sum (Q10_A_21, Q10_A_22, Q10_A_23, Q10_A_24, Q10_A_25)/5,
        Interpersonal_Understanding = sum (Q11_A_1, Q11_A_2, Q11_A_3)/3,
        Self_Confidence = sum (Q11_A_4, Q11_A_5, Q11_A_6, Q11_A_7)/4,
        Suspension_Judgement = sum (Q11_A_8, Q11_A_9, Q11_A_10, Q11_A_11, Q11_A_12)/5,
        Self_Discipline = sum (Q12_A_1, Q12_A_2, Q12_A_3, Q12_A_4)/4, 
        Industriness = sum (Q13_A_1, Q13_A_2, Q13_A_3, Q13_A_4, Q13_A_5)/5, 
        Enthusiasm = sum (Q14_A_1, Q14_A_2, Q14_A_3, Q14_A_4, Q14_A_5)/5,
        Flow_Proneness = sum (Q15_A_1, Q15_A_2, Q15_A_3, Q15_A_4, Q15_A_5, Q15_A_6)/6,
        Cyberwork_Confidence = sum (Q16_A_1, Q16_A_2, Q16_A_3, Q16_A_4, Q16_A_5, Q16_A_6)/6,
        General_SelfEfficacy = sum (Q17_A_1, Q17_A_2, Q17_A_3, Q17_A_4, Q17_A_5, Q17_A_6, Q17_A_7, Q17_A_8)/8,
        Resilience = sum (Q18_A_1, Q18_A_2, Q18_A_3, Q18_A_4, Q18_A_5, Q18_A_6)/6,
        Teamwork = sum (Q19_A_1, Q19_A_2, Q19_A_3, Q19_A_4, Q19_A_5)/5,
        Leadership = sum (Q20_A_1, Q20_A_2, Q20_A_3, Q20_A_4, Q20_A_5, Q20_A_6, Q20_A_7)/7,
        Intellectual_Openness = sum (Q22_A_1, Q22_A_2, Q22_A_3, Q22_A_4, Q22_A_5, Q22_A_6, Q22_A_7, Q22_A_8, Q22_A_9, Q22_A_10)/10,
        Tolerance = sum (Q21_A_1, Q21_A_2, Q21_A_3, Q21_A_4, Q21_A_5, Q21_A_6, Q21_A_7, Q21_A_8, Q21_A_9, Q21_A_10, Q21_A_11, Q21_A_12, Q21_A_13, Q21_A_14, Q21_A_15, Q21_A_16)/16,
        Analogies = sum(Analogies_Q14, Analogies_Q25,Analogies_Q4,Analogies_Q5,Analogies_Q8)/5,
        Matrix = sum(Matrix_Q43, Matrix_Q48, Matrix_Q50, Matrix_Q53,Matrix_Q55)/5,
        Pattern = sum(Pattern_Q1, Pattern_Q3, Pattern_Q35, Pattern_Q58, Pattern_Q6)/5,
        ThreeD = sum(`3D_Q16`, `3D_Q24`, `3D_Q29`, `3D_Q42`, `3D_Q58`)/5,
        Verbal = sum(Verbal_Q14, Verbal_Q16, Verbal_Q17, Verbal_Q32, Verbal_Q4)/5 ) %>% 
        mutate(Cog_Total = sum(Analogies, Matrix, Pattern, ThreeD, Verbal)/5) %>% 
        left_join(df_Rasch2)


df_scored %>% filter(Pers_Miss<miss_limit) %>% 
        gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>% 
        ggplot(aes(x=reorder(Dimension, Score, FUN=mean), y=Score)) +
        geom_boxplot() +
        coord_flip() + xlab("") + 
        ggtitle("Boxplot: Average Scores by Personality Dimension") +
        labs(caption = "Note: 1=Low Self-report; 5=High Self-report") +
        ggsave("Personality_RawScores.jpg", width = 10, height = 6, units = "in" )

df_scored %>% filter(Cog_Miss<miss_limit) %>% 
        gather(Analogies:Verbal, key=Dimension, value=Score) %>% 
        ggplot(aes(x=reorder(Dimension, Score, FUN=mean), y=Score)) +
        geom_boxplot() +
        coord_flip() + xlab("") +
        ylab("Score (% correct)") +
        ggtitle("Boxplot: Scores by Cognitive Test Type") +
        ggsave("Cognitive_RawScores.jpg", width = 10, height = 6, units = "in" )

#Correlation Plot Personality
dfcorrplot <-  df_scored %>% 
        filter(Pers_Miss<miss_limit) %>% 
        select(ID, Problem_Solving:Tolerance) %>%
        column_to_rownames("ID") %>% as.data.frame()

corrplot(cor(dfcorrplot), method="color", order="hclust", type="full", addrect=10, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag=FALSE, number.digits=2, number.font=.5 , number.cex=.5, tl.cex=.5)

#Correlation Plot Cognitive
dfcorrplot2 <-  df_scored %>% 
        filter(Cog_Miss<miss_limit) %>% 
        select(ID, Analogies:Verbal) %>%
        column_to_rownames("ID") %>% as.data.frame()

corrplot(cor(dfcorrplot2), method="color", order="hclust", type="full", addrect=1, cl.lim=c(-1,1), addCoef.col="black", rect.col="green", diag=FALSE, number.digits=2, number.font=.5 , number.cex=.5)


#scale Personality scored data
df_scored2 <- df_scored %>% filter(Pers_Miss < .1) %>% ungroup() %>% 
        select(ID, Work_Role, Problem_Solving:Tolerance) %>% 
        mutate_at(vars(Problem_Solving:Tolerance), scale) %>%
        gather(Problem_Solving:Tolerance, key=Dimension, value=Score) 

df_scored2_summary <-  df_scored2  %>% 
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score")
df_scored2_order <- df_scored2 %>% 
        summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score") %>% 
        filter(Work_Role=="Tier_1") %>% mutate(order=rank(Score)) %>% select(Dimension, order)
#Visualization        
df_scored2_summary %>% left_join(df_scored2_order) %>%  filter(N>5) %>% 
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
        ggsave("PersonalityScores_WorkRole.jpg", width = 10, height = 6, units = "in" )

#scale Cognitive scored data
df_scored3 <- df_scored %>% filter(Cog_Miss < .1) %>% ungroup() %>% 
        select(ID, Work_Role, Analogies:Verbal, Proficiency) %>% 
        mutate_at(vars(Analogies:Verbal), scale) %>%
        gather(Analogies:Proficiency, key=Test, value=Score) 

df_scored3_summary <-  df_scored3  %>% 
        summarySE(groupvars = c("Work_Role", "Test"), measurevar = "Score")
df_scored3_order <- df_scored3 %>% 
        summarySE(groupvars = c("Work_Role", "Test"), measurevar = "Score") %>% 
        filter(Work_Role=="Tier_1") %>% mutate(order=rank(Score)) %>% select(Test, order)

#Visualization        
df_scored3_summary %>% left_join(df_scored3_order) %>%  filter(N>6) %>% 
        ggplot(aes(x=reorder(Test, order), y=Score, color=Work_Role, group=Work_Role)) +
        geom_point(aes(size=Work_Role)) +
        scale_size_manual(values=c(3,2,2)) +
        geom_line(aes(linetype=Work_Role)) +
        scale_linetype_manual(values=c("dashed", "blank", "blank", "blank"))+
        #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
        coord_flip() + xlab(" ") + ylab("mean scaled score & overall proficiency(theta)") +
        scale_color_manual(values=c("red", "darkgray", "darkgray", "darkgray")) +
        theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
        ylim(-1.5,1.5) +
        ggtitle("Cognitive Scores by Work Role") +
        ggsave("CognitveScores_WorkRole.jpg", width = 10, height = 6, units = "in" )

########################################### Working Line - Under Construction #################
