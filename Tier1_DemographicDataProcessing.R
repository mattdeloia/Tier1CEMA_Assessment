library(DT)
library(randomForest)
library(gt)

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
  arrange(Work_Role) %>% #arrange data by work role
  rownames_to_column("ID") %>% #add a new ID label beginning with 1
  dplyr::select(-`Record ID`) %>% #remove Verint ID label
  dplyr::select(ID,Miss_P, Miss_C, sincere, Duration_min, Rank:`3D_Q16`) #select features to carry forward in analysis

#Replace Work roles with simple categories
df_preprocess$Work_Role <-  gsub("Tier-1 - Remote Operator|Tier-1 - Capability Developer|Tier-1 - Exploitation Analyst", "Tier_1", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 2- Data Architect, Network Analyst, System Analyst [(]All Master Proficiency level only[)]", "Tier_2", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 3- Other work role directly assigned to a Cyber Mission Force Team", "Tier_3", df_preprocess$Work_Role)
df_preprocess$Work_Role <-  gsub("Tier 4 -  Other authorized cyber work role|I am currently not assigned a cyber work role or I am assigned as a student", "Tier_4", df_preprocess$Work_Role)
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
#df_preprocess$Degree <- factor(levels =c("High School", "Associates", "Some College", "Bachelors", "Masters & Up") , df_preprocess$Degree)
df_preprocess$GTScore <- gsub("121 or higher", ">120", df_preprocess$GTScore)
df_preprocess$GTScore <- gsub("120 or lower", "<=120", df_preprocess$GTScore)
df_preprocess$GTScore <- gsub("N/A or prefer not to report", "not reported", df_preprocess$GTScore)

# df_demographic <- df_preprocess %>% 
#   dplyr::select (ID,Rank:Hexidecimal)



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

df_question <- read_xlsx("ScaleQuestions.xlsx")
df_question$Item <- as.character(df_question$Item)

item_statsb <- item_stats %>% left_join(df_question)

#Infrequency of response calculation
easy_mean <- item_stats %>%   #Average score of 20 easiest questions
  filter(Rank<=10) %>% dplyr::select(Mean)
easy_mean <- mean(easy_mean$Mean, na.rm = TRUE)

#Imputation of mean values for personality items by work role   
# df_rawscore2_a <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
#     mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) 
# df_rawscore2_b <- df_rawscore2 %>% filter (Work_Role %in% c("Tier_Other"))  %>% 
#     mutate_at(vars(Q1_A_1:Q21_A_16),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))        
# df_rawscore3 <- rbind(df_rawscore2_a, df_rawscore2_b) #bind together the Tier 1 and the Tier 2/3/4 dataframes

df_infreq <-  
  df_rawscore2 %>% 
  dplyr::select(ID, Q1_A_1:Q21_A_16) %>% 
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
  left_join(item_stats) %>% 
  filter(Rank<=10) %>% ungroup() %>% 
  group_by(ID)  %>% 
  summarise(infreq= abs(easy_mean-mean(Score, na.rm = TRUE))) #lower infreq is better  

df_rawscore3 <- df_rawscore2 %>% 
  left_join(df_infreq, by="ID") %>% 
  mutate(Survey_Completion = if_else(Miss_C<.25 & Miss_P <.25 & sincere<5 & infreq<1, "Yes", "No"))

########################33


#Rank
#Experience
Completion <- df_rawscore3 %>% select(Work_Role, Survey_Completion) %>% 
  group_by(Work_Role, Survey_Completion) %>% summarise(n = n()) %>% 
  pivot_wider(names_from = "Survey_Completion", values_from = "n") %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(Completed = Yes) %>% select(Work_Role, Completed)

df_rawscore3 %>%   mutate(Prior_Tier1 = replace_na(Prior_Tier1, "No")) %>%  
  select(Work_Role, Rank, Prior_Tier1) %>% 
  group_by(Work_Role, Rank) %>%
  summarise(n=n()) %>% 
  spread(key=Rank, value=n) %>% left_join(Completion) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  gt(groupname_col = FALSE) %>% 
  tab_header(title=md("sample size for analysis")) %>% 
  grand_summary_rows(
    columns = vars(`E1 - E9`, `O1 or higher`, `WO1 - CW5`, Completed),
    fns = list(Total = "sum"))
  
#Experience
df_experience <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
  select(Work_Role, Rank, Experience) %>% 
  drop_na(Experience) %>% 
  group_by(Work_Role, Rank, Experience) %>%
  summarise(n=n()) %>% 
  left_join(df_rawscore3 %>% 
              mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
              select(Work_Role, Rank, Experience) %>% 
              drop_na(Experience) %>% 
              group_by(Work_Role, Rank) %>%
              summarise(n_tot=n())) %>% 
  mutate(n=n/n_tot) %>%
  mutate(n = if_else(Experience=="<=5Years", round(-1*n,1), round(n,1))) %>% 
  mutate(Rank = factor(Rank, levels = c("E1 - E9","O1 or higher","WO1 - CW5"))) %>% 
  mutate(Work_Role = factor(Work_Role, levels = c("Tier_Other", "Tier_1"))) 

df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
  select(Work_Role, Rank, Degree) %>% 
  drop_na(Degree) %>% 
  group_by(Work_Role, Rank, Degree) %>%
  filter(Work_Role=="Tier_1") %>% 
  summarise(n=n()) 

df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  select(ID, Work_Role, Rank, Bachelors_CS, Masters_CS) %>% 
  mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
         Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
  gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>% 
t_test(n~Work_Role)

 
 df_experience %>%  
   ggplot(aes(x=Work_Role, y=n, fill=Experience)) +
   geom_col() +
   geom_text(aes(label=abs(n)), vjust = if_else(df_experience$n>=0, 0, 1), hjust = if_else(df_experience$n>=0, 1, -1)) +
   scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
   coord_flip() +
   scale_y_continuous(labels=abs) +
   ylab("% of Tier group") + 
   xlab("") + 
   ylim(-1,1) +
   geom_hline(yintercept = 0, lwd = 2, color="White") +
   facet_grid(Rank~.) +
   theme_light() +
   theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12), axis.text.x = element_blank(),
         axis.title = element_text(size=12),
         legend.text = element_text(size=12),
         strip.text = element_text(size=12, color="blue"))+
   ggsave("Experience.png", width = 8, height = 4.5)
  


#Education
 df_education <- df_rawscore3 %>% 
   mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
   mutate(Degree = if_else(Degree %in% c("Associates", "Some College"), "Some College", Degree)) %>% 
   select(Work_Role, Rank, Degree) %>% 
   drop_na(Degree) %>% 
   group_by(Work_Role,  Degree) %>%
   summarise(n=n()) %>% 
   left_join(df_rawscore3 %>% 
               mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
               select(Work_Role, Rank, Degree) %>% 
               drop_na(Degree) %>% 
               group_by(Work_Role, Rank) %>%
               summarise(n_tot=n())) %>% 
   mutate(n=n/n_tot) %>%
   mutate(n = if_else(Degree %in% c("High School", "Associates", "Some College"), round(-1*n,1), round(n,1))) %>% 
   mutate(Rank = factor(Rank, levels = c("E1 - E9","O1 or higher","WO1 - CW5"))) %>% 
   mutate(Work_Role = factor(Work_Role, levels = c("Tier_Other", "Tier_1"))) %>% 
   mutate(Degree=factor(Degree, levels=c("High School", "Some College", "Masters & Up", "Bachelors")))
 
 df_education %>%  
   ggplot(aes(x=Work_Role, y=n, fill=Degree)) +
   geom_col() +
   #geom_text(aes(label=if_else(n>0, n, " ")), position = "stack") +
   scale_fill_manual(values=c("lightgray", "darkgray",  "green", "lightgreen")) +
   coord_flip() +
   scale_y_continuous(labels=abs) +
   ylab("% of Tier group") + 
   xlab("") + 
   ylim(-1,1) +
   scale_y_continuous(labels=abs) +
   geom_hline(yintercept = 0, lwd = 2, color="White") +
   facet_grid(Rank~.) +
   theme_light() +
   theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
         axis.title = element_text(size=12),
         legend.text = element_text(size=12),
         strip.text = element_text(size=12, color="blue")) +
   ggsave("Education.png", width = 8, height = 4.5)
 
# df_demographic %>% 
#   mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
#   select(Work_Role, Rank, Degree) %>% 
#   group_by(Work_Role, Rank, Degree) %>%
#   summarise(n=n()) %>% 
#   left_join(n_Tier) %>% 
#   mutate(n=n/n_tot) %>% 
#   na.omit(Degree) %>%
#   mutate(Rank = factor(Rank, levels = c("WO1 - CW5", "O1 or higher", "E1 - E9"))) %>% 
#   ggplot(aes(x=Work_Role, y=n, fill=Rank)) + 
#   geom_col() +
#  # geom_text(aes(label=round(n, 2)), position = "stack", vjust=1, size=3 ) +
#   scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
#   theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
#   ylab("% of Tier group") + xlab("") +
#   facet_grid(.~Degree) + 
#   ggtitle("Highest Education Level by Work Role")

#Education CD Degrees
df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  select(ID, Work_Role, Rank, Bachelors_CS, Masters_CS) %>% 
  mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
         Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
  gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>% 
  group_by(Work_Role, Degree) %>%
  summarise(n=sum(n)) %>% 
  left_join( select(ID, Work_Role, Rank, Bachelors_CS, Masters_CS) %>% 
               mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
                      Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
               gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>%
              group_by(Work_Role) %>%
              summarise(n_tot=n())) %>% 
  mutate(n=n/n_tot) %>% 
  mutate(Rank = factor(Rank, levels = c("WO1 - CW5", "O1 or higher", "E1 - E9"))) %>% 
  ggplot(aes(x=Work_Role, y=n, fill=Rank)) + 
  geom_col() +
  scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("% of Tier group") + xlab("") +
  facet_grid(.~Degree) +
  ggtitle("CS Degrees by Work Role")

#Certifications
df_demographic %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
            select(ID, Work_Role, OSCP:`Security`) %>%
            gather(OSCP:`Security`, key=Cert, value=n) %>% 
            mutate(n=if_else(n=="Yes", 1, 0)) %>% 
            group_by(Work_Role, Cert) %>%
            summarise(n=sum(n)) %>% 
            left_join(n_Tier) %>%
            mutate(n=round(n/n_tot,2)) %>% 
            select(-n_tot) %>%
  ggplot(aes(x=reorder(Cert, n, FUN = mean), y=n, color=Work_Role, group=Work_Role)) + 
  geom_point(size=3) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  scale_linetype_manual(values=c("dashed", "dashed", "blank", "blank")) +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylab("% of Tier group") + xlab("") +
  ylim(0,.4) +
  coord_flip() +
  ggsave("Certifications.png", width = 8, height = 4.5, units = "in" )
            
#GT Score
df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
            select (ID, GTScore) %>% 
  group_by( GTScore) %>% summarise(n=n()) %>% 
  left_join(n_Tier) %>% 
  mutate(n= n/n_tot) %>% 
  mutate(Rank = factor(Rank, levels = c("WO1 - CW5", "O1 or higher", "E1 - E9"))) %>% 
  ggplot(aes(x=Work_Role, y=n, fill=Rank)) + 
  geom_col() +
  scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
  #facet_grid(Work_Role~.) +   
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("% of Tier group") + xlab("") +
  facet_grid(.~GTScore) +
  ggtitle("GT Score Categories by Work Role")

#Hobbies
df_hobbies <- df_demographic %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  select (ID, Work_Role, BuiltCPU, Gaming, Edit_game, Scripts, SOHO, Hexidecimal) %>%
  gather(BuiltCPU:Hexidecimal, key="Hobby", value="Response") 
df_hobbies$Response <- replace_na(df_hobbies$Response, "No")
df_hobbies %>%  
  mutate(Response = if_else(Response =="Yes", 1, 0)) %>% 
  group_by(Work_Role, Hobby) %>% 
  summarise(Response = sum(Response)) %>% 
  left_join(n_Tier) %>% 
  mutate(Response = Response/n_tot) %>% 
  ggplot(aes(x=reorder(Hobby, Response, FUN=mean), y=Response, color=Work_Role, group=Work_Role)) + 
  geom_point(size=3) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  scale_linetype_manual(values=c("dashed", "dashed", "blank", "blank")) +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylab("% of Tier group") +
  xlab("") +
  ylim(0,1) +
  coord_flip() + ggsave("Hobbies.png", width = 4.5, height = 5.5, units = "in" )

#Hobbies2
df_demographic %>% 
  select(ID, Work_Role, CPU_OS, Game_Platform, Game_type) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  gather(CPU_OS:Game_type, key=Category, value=Response) %>%
  group_by(Work_Role, Category, Response) %>% 
  summarise(n=n()) %>% 
  left_join(n_Tier) %>% 
  mutate(n= n/n_tot) %>%
  na.omit(Response) %>% 
  ggplot(aes(x=reorder(Response, n, FUN=mean), y=n, fill=Work_Role)) + 
  geom_col(position="dodge2") + 
  scale_fill_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  facet_grid(Category~Work_Role, scales = "free_y") + 
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  xlab("") +
  ylab("% of Tier group") +
  coord_flip() +
  ggsave("Hobbies2.png", width = 4.5, height = 5.5, units = "in" )

t_test_calc <- df_demographic %>% select(Work_Role, Edit_game) %>% 
  na.omit() %>% 
    mutate(Edit_game = if_else(Edit_game=="Yes", 1, 0))

t.test(t_test_calc$Edit_game ~ t_test$Work_Role)
  


#Random Forest Modeling################################
set.seed(102)
df_randomforest <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  mutate(Degree = if_else(Degree=="High School", 0, if_else(Degree %in% c("Associates", "Some College"), 1, if_else (Degree=="Bachelors", 3, if_else(Degree=="Masters & Up", 4, 0))))) %>% 
  gather(Bachelors_CS:Masters_CS, OSCP:Security, BuiltCPU, Gaming, Edit_game:Hexidecimal, key=Measure, value=Score) %>% 
  mutate(Score = if_else(Score=="Yes", 1, 0)) %>% 
  pivot_wider(names_from = "Measure", values_from= "Score") %>% 
  select(Work_Role, Degree, Bachelors_CS:Hexidecimal)

df_randomforest_a <- df_randomforest %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
     mutate_at(vars(Degree:Hexidecimal),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) 
df_randomforest_b  <- df_randomforest %>% filter (Work_Role %in% c("Tier_Other"))  %>% 
     mutate_at(vars(Degree:Hexidecimal),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))        
df_randomforest2 <- rbind(df_randomforest_a, df_randomforest_b) #bind together the Tier 1 and the Tier 2/3/4 dataframes

  

df_randomforest2[1:19] <- lapply(df_randomforest2[1:19], factor)
# Split into Train and Testation sets
# Training Set : Testation Set = 75 : 25 (random)
train <- sample(nrow(df_randomforest2), .75*nrow(df_randomforest2), replace = FALSE)
TrainSet <- df_randomforest2[train,]
TestSet <- df_randomforest2[-train,]

summary(TrainSet)
summary(TestSet)
# Create a Random Forest model with default parameters
model1 <- randomForest(Work_Role ~ ., data = TrainSet, importance = TRUE)
model1

model2 <- randomForest(Work_Role ~ ., data = TrainSet, ntree = 400, mtry = 3, importance = TRUE, proximity=TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Work_Role)

# Predicting on Test set
predTest <- predict(model2, TestSet, type = "class")
# Checking classification accuracy
table(predTest,TestSet$Work_Role)
mean(predTest == TestSet$Work_Role)

# To check important variables
importance(model2)        
varImpPlot(model2) 

randforest_report <- importance(model2) %>% as.data.frame() 
max_importance <- max(randforest_report$MeanDecreaseAccuracy)
randforest_report2 <-   rownames_to_column(randforest_report, var="Feature") %>% 
  mutate(Importance=round(MeanDecreaseAccuracy/max_importance,1), Model="Random Forests") %>% 
  select (Model, Feature, Importance) %>% 
  arrange(-Importance)

# model_crossvalid <- train(Work_Role ~ ., data = df_randomforest, method="rf", trControl = trainControl(method ="cv", number = 5, verboseIter=TRUE))
#model_crossvalid



# Variable Selection using k-NN model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Heart_Attack_Diagnosis~., data=training, method="knn", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# k-nn implementation with all the numeric predictor features initially selected without variable selection
# The tuneLength parameter tells the algorithm to try different default values for the main parameter



set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train( Heart_Attack_Diagnosis~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Heart_Attack_Diagnosis )

mean(knnPredict == testing$Heart_Attack_Diagnosis)
