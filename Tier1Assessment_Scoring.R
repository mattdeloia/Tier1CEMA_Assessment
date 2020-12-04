#load libraries
library(tidyverse)
library(readxl)

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

#load data
df <- read_excel("xx.xls")
df <- df %>% rownames_to_column("ID")  
df <- df %>% gather(`Rank (Q70):`Hexidecimal (Q69)`, key= "Question", value = "Response" ) %>%
separate(col = `Question`, into=c("Question", "Question2"), sep = " ", remove="TRUE")%>%separate(col= `Q#`, into=c("Question#", "Short Title"), sep = "\\.", remove="TRUE")

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
