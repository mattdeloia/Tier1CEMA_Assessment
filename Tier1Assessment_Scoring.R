#load libraries
library(tidyverse)
library(readxl)
library(Rmisc)

#functions
likertNum <- function(x){
        case_when(
                x == "Very Accurate" ~ 5,
                x == "Moderately Accurate" ~ 4,
                x == "Neither Accurate Nor Inaccurate" ~ 3,
                x == "Moderately Inaccurate" ~ 2,
                x == "Very Inaccurate" ~ 1
        )
}

#load raw data and pre-process
df_raw <- read_xlsx("CyberCompetencies.xlsx") %>% 
        drop_na(`Work_Role (Q50)`) %>% 
        rownames_to_column("ID") %>% 
        select(-`Record ID`) %>% 
        dplyr::rename(OSCP=`FormalCerts (Q58_1)`,
                `xx`        = `FormalCerts (Q58_2)`,
                `GPEN`      = `FormalCerts (Q58_3)`,
                `GXPN`      = `FormalCerts (Q58_4)`,
                `GCIH`      = `FormalCerts (Q58_5)`,
                `CEH`       = `FormalCerts (Q58_6)`,
                `CISSP`     = `FormalCerts (Q58_7)`,
                `yy`        = `FormalCerts (Q58_8)`,
                `Security+` = `FormalCerts (Q58_9)`,
                `OtherCert` = `FormalCerts (Q58_10)`) %>% 
        mutate(Duration_min = round((Completed - Started),2)) %>%
        mutate(Duration_min=replace_na(Duration_min, 0)) %>% 
        mutate(Complete = if_else(Duration_min >0, "Yes", "No"))


df_preprocess <- df_raw %>% 
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
        

########################################### Working Line - Under Construction #################3
#Scoring of Raw Data         


cogntive <- 88:94
df[cogntive] <- lapply(df[cogntive], as.factor) 
#Impute the mode
df[4:10] <- impute( df[4:10], object = NULL, method = "median/mode", flag = FALSE)
df[88:94] <- impute( df[88:94], object = NULL, method = "median/mode", flag = FALSE)

df_scored <- df %>%   
        mutate_at(vars(Q1_A_1:Q21_A10), likertNum) %>% 
        mutate(Pattern_Q58 = if_else(Pattern_Q58==4, 1, 0),
        Pattern_Q1 = if_else(Pattern_Q1==4, 1, 0),
        `3D_Q24` = if_else(`3D_Q24`==3, 1, 0),
        Verbal_Q4 = if_else(Verbal_Q4==4, 1, 0),
        Verbal_Q16 = if_else(Verbal_Q16==4, 1, 0),
        Matrix_Q55 = if_else(Matrix_Q55==4, 1, 0) )
        

#Rasch scoring cognitive questions
df_cognitive <- df_scores %>% select(ID, Pattern_Q1:3D_Q16) %>% column_to_rownames("ID") 
rm.res <- RM(df_cognitive)
pp <- person.parameter(rm.res)
df_cognitive2 <-  pp$theta.table %>% 
        rownames_to_column("ID") %>% 
        mutate(Proficiency=round(`Person Parameter`, 2)) %>%  select(ID, Proficiency)        
