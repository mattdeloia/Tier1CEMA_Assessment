library(psych)
library(Hmisc)
library(Rmisc)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(ggdendro)
library(tidyverse)
library(WrightMap)
library(eRM)
library(readxl)
library(gt)
library(mlbench)
library(clustertend)
library(cluster)
library(magrittr)
library(fpc)
library(caret)
library(RANN)
library(gmodels)
library(corrplot)


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

item_analysis_data <- df_rawscore3 %>% select (Q1_A_1:Q21_A_16)
scores <- scoreItems(keys.list,item_analysis_data)
scores




#%>% 
#  mutate(Rank = rank (-Score, ties.method = "random" ) )


df_pca <- df_rawscore3 %>%
    filter(infreq<1, Miss_P<.75) %>% select(Q1_A_1:Q22_A_10) %>% as.matrix()

imputation <-  preProcess(df_pca, method="knnImpute")
df_pca2 <- predict(imputation, df_pca)
  
PCA <- prcomp(df_pca2)
summary(PCA)
print(PCA)
PCA$center
PCA$rotation
eigen_exp <- fviz_eig(PCA, ncp=20)
eigen_exp
var <- get_pca_var(PCA)
head(var$contrib)

#Remove rows with missing values and keep only complete cases
efa_data <- df_rawscore3 %>% 
  filter(infreq<1) %>% select(Q1_A_1:Q22_A_10) 

efa_data %>% rownames_to_column ("ID") %>% count ()

#Create the correlation matrix
efa_cor <- cor(efa_data, use = "pairwise.complete")
lowerCor(efa_data)

#principle components analysis
options(max.print = 1e7)
pc3 <- principal(efa_cor, nfactors=10, rotate="varimax")
print.psych(pc3, digits=2,  cut = 0.3, sort = TRUE)

#Very Simple Structure Fit
vss(efa_data, n =27, rotate = "varimax")
VSS.scree(efa_data)

#Factor analysis of the data
fa.parallel(efa_data)
factors_data <- fa(r = efa_cor, nfactors = 12)

#Analysis of the items

#Getting the item loadings and arrange by factors
print.psych(factors_data, digits=2, cut= 0.3, sort = TRUE)

#Construction of Refined Questionnaire
df_question <- read_xlsx("ScaleQuestions.xlsx")
df_question$Item <- as.character(df_question$Item)


df_efa <- factors_data[["loadings"]] %>% 
  as.matrix.data.frame() %>% 
  as.data.frame() %>% 
  rownames_to_column("Item") %>%
  gather(V1:V12, key="Scale", value="Loading") %>% 
  right_join(df_question) %>%
  dplyr::group_by(Question) %>% 
  mutate(Rank = rank(-1*abs(Loading))) %>% 
  filter(Rank<=2) %>%  
  dplyr::group_by(Scale) %>% 
  mutate(LoadRank = rank(-1*abs(Loading))) %>% 
  filter(LoadRank<=10) %>% 
  select(-Item, -Rank, -LoadRank )

#Scoring by new scale construct
df_revised_scored <- df_rawscore3 %>% 
  filter(infreq < 1) %>% 
  select(ID, Work_Role, Rank, Q1_A_1:Q22_A_10) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  gather(Q1_A_1:Q22_A_10, key=Question, value=Score) %>% 
  left_join(df_efa, by="Question") %>%
  mutate(Score = if_else(Loading>0, Score, 6-Score) ) %>% #reverse scoring based on item loading
  select(Scale, ID:Question, Score) %>% 
  drop_na(Scale)%>% 
  summarySE(groupvars = c('ID', 'Work_Role', 'Scale'), measurevar = "Score", na.rm = TRUE) %>% 
  drop_na()
  

corrplot(cor(df_revised_scored %>%
               select(ID, Scale, Score) %>%
               pivot_wider(names_from= 'Scale', values_from = 'Score') %>% ungroup() %>% 
               drop_na() %>% 
               dplyr::select(-ID)),
         method="color", order="hclust", type="full", addrect=8, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=.5 , number.cex=.5, tl.cex=.5)

#Visualization of components to revised questionnaire

#t-test for difference in Group means

library(rstatix)
stat_test <- df_revised_scored %>%
  group_by(Scale) %>%
  t_test(Score ~ Work_Role)
stat_test %>% filter(p<.20) %>% count()
sig_scales <- stat_test %>% mutate(Significant = if_else(p<.10, "Yes", "No")) %>% arrange(p)

df_efa %>% left_join(sig_scales) %>%
  mutate(Loading2 = if_else(Loading > 0, "positive", "negative")) %>%
  summarySE(measurevar = "Loading", groupvars = c("Scale", "Trait", "Significant", "p", "Loading2" )) %>%
  ungroup() %>% group_by(Scale) %>% 
  mutate(Scale = factor(Scale, levels=sig_scales$Scale)) %>% 
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


df_revised_scored %>% 
  ggplot(aes(x=reorder(Scale, Score, FUN=mean, na.rm=TRUE), y=Score)) +
  geom_boxplot(aes(fill=Work_Role)) +
  coord_flip() + xlab("") + 
  ggtitle("Boxplot: Average Scores by Personality Dimension") +
  labs(caption = "Note: 1=Low Self-report; 5=High Self-report") +
  ggsave("Personality_RevisedScalesScores.jpg", width = 10, height = 6, units = "in" )

stat_groupdifferences <-  df_revised_scored  %>% #compute summary statistics for Tier1 and Tier_Other
  summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>% 
  select(Work_Role,Scale,Score) %>% 
  pivot_wider(names_from = Work_Role, values_from = Score) %>% 
  mutate(Difference = abs(Tier_1 - Tier_Other)) %>% 
  summarySE(measurevar = "Difference") %>% 
  select(Difference)
 
stat_groupdifferences$Difference 
 
df_revised_scored_order <- df_revised_scored  %>% #compute summary statistics for Tier1 and Tier_Other
  summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>% 
  select(Work_Role,Scale,Score) %>% 
  pivot_wider(names_from = Work_Role, values_from = Score) %>% 
  mutate(Difference = abs(Tier_1 - Tier_Other)) %>% 
  summarySE(groupvars= "Scale", measurevar = "Difference") %>% 
  mutate(Rank = rank(-1*Difference)) %>% select(Scale, Rank)

#Visualization of summary statistics for Personality results by work role      
df_revised_scored  %>% #compute summary statistics for Tier1 and Tier_Other
  summarySE(groupvars = c("Work_Role", "Scale"), measurevar = "Score", na.rm = TRUE) %>%
  left_join(df_revised_scored_order)  %>% 
  ggplot(aes(x=reorder(Scale, Rank, fun=max), y=Score, color=Work_Role, group=Work_Role)) +
  geom_point(aes(size=Work_Role)) +
  scale_size_manual(values=c(3,2,2)) +
  #geom_line(aes(linetype=Work_Role)) +
  #scale_linetype_manual(values=c("dashed", "blank"))+
  geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+ 
  coord_flip() + xlab(" ") + ylab("mean score") +
  scale_color_manual(values=c("red", "darkgray")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylim(0,5) +
  ggtitle("Mean Score by Work Role") +
  ggsave("PersonalityScores_WorkRole_RevisedScale.jpg", width = 10, height = 6, units = "in" ) #save to folder


question_table2 <- df_efa %>% 
  left_join(sig_scales) %>% 
  mutate(Loading = round(Loading, 2)) %>%
  left_join(item_stats) %>% 
  arrange(p) %>% 
  select (Scale, Trait, Question, Loading, Score, sd, Content)
write_excel_csv(question_table2, "question_table2.csv")

#data table of revised scales
df_efa %>% 
  left_join(sig_scales) %>% 
  mutate(Loading = round(Loading, 2)) %>%
  left_join(item_stats) %>% 
  arrange(p) %>% 
  select (Scale, Trait, Question, Loading, Score, sd, Content)  %>% 
  gt(groupname_col = "Scale", rowname_col="Question") %>% 
  tab_spanner(label="item metrics", columns=matches("Loading|Mean|sd")) %>% 
  tab_header(
    title=md("Table of Scales and Items for Revised Questionnaire") )
                      
#missingness plot
df_rawscore3 %>% 
  mutate(FilterIn = if_else(Miss_P<=miss_limit, "Y", "N")) %>%
  ggplot() +
  geom_histogram(aes(x=Miss_P, fill=FilterIn )) +
  scale_fill_manual(values=c("tomato", "lightblue")) +
  geom_vline(xintercept = miss_limit, linetype="dashed", color="red", size=1) +
  geom_text(aes(x=miss_limit, y=0, angle=90), nudge_y=5, nudge_x=-.01,label= "threshold", size=4) +
  xlab("missingness percentage") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  facet_grid(Work_Role~.)+
  ggtitle("missingness levels of respondents") +
  theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
  theme(strip.text = element_text(size=12, color="blue"))

#infrequency plot
infreq_limit = .75
df_rawscore3 %>% 
  mutate(FilterIn = if_else(infreq<=infreq_limit, "Y", "N")) %>%
  ggplot() +
  geom_histogram(aes(x=infreq, fill=FilterIn )) +
  scale_fill_manual(values=c("tomato", "lightblue")) +
  geom_vline(xintercept = infreq_limit, linetype="dashed", color="red", size=1) +
  geom_text(aes(x=infreq_limit, y=0, angle=90), nudge_y=5, nudge_x=-.01,label= "threshold", size=4) +
  xlab("infrequency score (lower is better)") + 
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12)) +
  facet_grid(Work_Role~.)+
  ggtitle("infrequency scores of respondents") +
  theme(legend.title= element_text(color="black", size=10), legend.position = "blank") +
  theme(strip.text = element_text(size=12, color="blue"))

######################## New construct

#Item Analysis using psych package
keys.list <- list(
  Discipline =	c('Q8_A_3',	'Q8_A_4',	'Q8_A_5',	'Q8_A_7',	'Q8_A_9',	'Q8_A_10',	'Q12_A_2',	'Q13_A_1'),
  Self_Consciousness =	c( "Q1_A_1" ,	"Q5_A_2"	,"Q6_A_5",	"Q11_A_4",	"Q11_A_5",	"Q11_A_6",	"Q12_A_3",	"Q19_A_3" ),
  Self_Efficacy	= c("Q17_A_3",	"Q17_A_4",	"Q17_A_5",	"Q17_A_6",	"Q17_A_7",	"Q17_A_8",	"Q20_A_2",	"Q20_A_6" ),
  Skill_Confidence =	c("Q9_A_1",	"Q10_A_3",	"Q15_A_2",	"Q16_A_1",	"Q16_A_2",	"Q16_A_3",	"Q16_A_4",	"Q16_A_6"),
  Thrill_Seeking = c(	"Q10_A_16",	"Q10_A_17",	"Q10_A_19",	"Q10_A_21",	"Q10_A_22",	"Q10_A_23",	"Q10_A_24",	"Q10_A_25"),
  Enthusiasm =	c("Q11_A_7",	"Q14_A_1",	"Q14_A_2",	"Q14_A_3",	"Q14_A_4",	"Q18_A_1",	"Q18_A_4",	"Q18_A_6"),
  Cognitive_Proficiency =	c("Q1_A_3",	"Q1_A_5",	"Q4_A_2",	"Q4_A_3",	"Q4_A_4",	"Q4_A_6",	"Q4_A_7",	"Q7_A_12"),
  Social_Curiosity =	c("Q10_A_11",	"Q10_A_12",	"Q10_A_13",	"Q10_A_14",	"Q10_A_15",	"Q11_A_1",	"Q11_A_2",	"Q11_A_3"),
  Intellectual_Curiosity =	c('Q4_A_5',	'Q5_A_1',	'Q5_A_4',	'Q6_A_1',	'Q10_A_4',	'Q10_A_8',	'Q10_A_9',	'Q22_A_6'),
  Analytical =	c('Q9_A_4',	'Q9_A_5',	'Q10_A_1',	'Q11_A_9',	'Q11_A_10',	'Q11_A_11',	'Q11_A_12',	'Q13_A_5'),
  Creativity =	c('Q7_A_1',	'Q7_A_2',	'Q7_A_5',	'Q7_A_7',	'Q7_A_9',	'Q7_A_10',	'Q7_A_11',	'Q7_A_13'),
  Openness =	c('Q6_A_2',	'Q6_A_3',	'Q6_A_6',	'Q19_A_2',	'Q20_A_5',	'Q20_A_7',	'Q22_A_2',	'Q22_A_4'))
  
  
  

item_analysis_data <- df_rawscore3 %>% select (Q1_A_1:Q21_A_16)
scores <- scoreItems(keys.list,item_analysis_data)
scores


#Item statistics of difficulty (mean) and standard deviation
item_stats <- df_rawscore3 %>%
  filter(infreq<1) %>%
  dplyr::select(ID, Q1_A_1:Q21_A_16) %>%
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
  summarySE(groupvars = "Question", measurevar = "Score", na.rm = TRUE) %>% arrange(Score) %>%
  dplyr::select(Question, Score, sd) %>% mutate(Score = round(Score, 1), sd=round(sd, 1)) %>% 
  mutate(Rank = rank (-Score, ties.method = "random" ) ) #Rank order questions from easiest to hardest

item_stats %>% ggplot() + 
  geom_jitter(aes(x=Score, y=sd)) + 
  geom_smooth(aes(x=Score, y=sd))

item_stats %>% gather(Score:sd, key=measure, value=score) %>% 
  ggplot() + geom_boxplot(aes(x=measure, y = score)) +
  facet_grid(measure~., scales = "free_y")

df_question <- read_xlsx("ScaleQuestions.xlsx")
df_question$Item <- as.character(df_question$Item)

item_stats_2 <- df_rawscore3 %>%
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  dplyr::select(ID,Work_Role, Q1_A_1:Q21_A_16) %>%
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>%
  summarySE(groupvars = c("Question", "Work_Role"), measurevar = "Score", na.rm = TRUE) %>% 
  select(Question, Work_Role, Score) %>% 
  pivot_wider(names_from = "Work_Role", values_from = "Score") %>% 
  mutate(Difference = Tier_1-Tier_Other) %>%
  mutate(Difference_abs = abs(Tier_1-Tier_Other)) %>% 
  arrange(Difference_abs) %>% 
  mutate(rank = rank(-Difference_abs, ties.method = "random")) %>% left_join(df_question) 

library(lavaan)
#factor analysis (Tolerance)
m3a <- "f =~ Q21_A_1 + Q21_A_2 + Q21_A_3 + Q21_A_4 + Q21_A_5 + Q21_A_6 + Q21_A_7 + Q21_A_8 + Q21_A_9 + Q21_A_10 + Q21_A_11 + Q21_A_12 + Q21_A_13 + Q21_A_14 + Q21_A_15 + Q21_A_16 "
alpha <- df_rawscore3 %>% filter(infreq < 1) %>%  select(Q21_A_1 , Q21_A_2 , Q21_A_3 , Q21_A_4 , Q21_A_5 , Q21_A_6 , Q21_A_7 , Q21_A_8 , Q21_A_9 , Q21_A_10 , Q21_A_11 , Q21_A_12 , Q21_A_13 , Q21_A_14 , Q21_A_15 , Q21_A_16)
onefactoranalysis <- cfa(m3a, data=df_rawscore3 %>% filter(infreq < 1), std.lv=TRUE)
summary(onefactoranalysis, fit.measures=TRUE, standardized=TRUE)
alpha(alpha)

#factor analysis (Teamwork)
m3a <- "f =~ Q19_A_1 +  Q19_A_2 + Q19_A_3 + Q19_A_4 + Q19_A_5"
alpha <- df_rawscore3 %>% filter(infreq < 1) %>%  select(Q19_A_1,  Q19_A_2, Q19_A_3, Q19_A_4, Q19_A_5)
onefactoranalysis <- cfa(m3a, data=df_rawscore3 %>% filter(infreq < 1), std.lv=TRUE)
summary(onefactoranalysis, fit.measures=TRUE, standardized=TRUE)
alpha(alpha)




