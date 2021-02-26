
#kmeans cluster analysis

#load libraries
library(clValid)
library(clustertend)
library(factoextra)
df_cluster <- df_rawscore3 %>%
  filter(infreq<1, Miss_P<.75) %>% select(Q1_A_1:Q22_A_10) %>% as.matrix()
imputation <-  preProcess(df_cluster, method="knnImpute")
df_cluster2 <- predict(imputation, df_cluster)


clmethods <- c("hierarchical", "kmeans", "pam")
evaluation <- clValid (df_cluster2, nClust = 2:6,
                       clMethods = clmethods, validation = c("internal", "stability"))
#Summary
summary(evaluation)

kmeans_clust <- kmeans(df_cluster2, 2)
fviz_cluster(list(data=df_cluster2, cluster=kmeans_clust$cluster), ellipse.type - "norm", geom = "point", stand=FALSE, palette = "jco", ggthem = theme_classic()) + ggsave("cluster_viz.png", width = 6.2, height = 5.4/2, units = "in")


df_clust_scored <- df_rawscore3 %>% 
  filter(infreq<1, Miss_P<.75) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
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
    Intellectual_Openness = mean(c(Q22_A_1, Q22_A_2, Q22_A_3, Q22_A_4, Q22_A_5, Q22_A_6, Q22_A_7, Q22_A_8, Q22_A_9, Q22_A_10), na.rm = TRUE)) %>% 
  cbind(cluster=kmeans_clust$cluster) %>% 
  select(ID, Work_Role, cluster, Problem_Solving:Intellectual_Openness)

df_impute <- df_clust_scored %>%
  ungroup() %>% 
  select(Problem_Solving:Intellectual_Openness) %>%
  as.matrix()
imputation <-  preProcess(df_impute, method="knnImpute")
df_impute2 <- predict(imputation, df_impute) %>% as.data.frame()

df_clust_scored2 <-  df_clust_scored %>% 
  select(ID, Work_Role, cluster) %>% 
  cbind(df_impute2)

df_rawscore3 %>% 
  filter(infreq<1, Miss_P<.75) %>%
  cbind(cluster=kmeans_clust$cluster) %>% 
  group_by(cluster, Work_Role) %>% summarise(n=n())
#Visualization of Personality Test Results

df_clust_summary <-  df_clust_scored2  %>% gather(Problem_Solving:Intellectual_Openness, key=Dimension, value=Score) %>% 
  summarySE(groupvars = c("cluster", "Dimension"), measurevar = "Score", na.rm = TRUE) %>% 
  mutate(cluster = as.factor(cluster))#compute summary statistics for Tier1 and Tier_Other

df_scored2_order <- df_scored2 %>% 
  summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE) %>% 
  filter(Work_Role=="Tier_1") %>% 
  mutate(order=rank(Score)) %>% 
  select(Dimension, order) #compute summary statistics for Tier 1 

df_clust_summary %>% 
  select(Dimension, cluster, Score) %>% 
  pivot_wider(names_from = "cluster", values_from = "Score") %>% 
  mutate(delta = abs( `1`-`2` ) ) %>% 
  gather(`1`:`2`, key=Cluster, value=Score) %>% 
  mutate(ci = cbind(df_clust_summary$ci)) %>% 
  ggplot(aes(x=reorder(Dimension, delta, fun=max), y=Score, color=Cluster)) +
  geom_point(aes(shape=Cluster)) +
  scale_color_manual(values=c("blue", "orange")) +
  geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+
  theme(legend.title = element_text(color="black", size=12), legend.position = "blank", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  geom_hline(yintercept = 0, linetype="dashed", color="darkgray") +
  coord_flip() + xlab("") +
  ggsave("Cluster_Personality_Results.png", width = 6.2, height = 5.4, units = "in")
  
#Rasch scoring and analysis of 25 cognitive questions
df_Rasch <- df_rawscore3 %>%
  filter(infreq<1, Miss_P<.75) %>% 
  cbind(cluster=kmeans_clust$cluster) %>% 
  filter(Miss_C<.5, sincere<=5) %>% #filter out based on missingness and sincerity of effort
  dplyr::select(ID, Pattern_Q1:`3D_Q16`) %>% 
  column_to_rownames("ID") 

df_Rasch %>% gather(Pattern_Q1: `3D_Q16`, key = item, value=score) %>% 
  summarySE(groupvars = "item", measurevar = "score", na.rm = TRUE)

rm <- RM(df_Rasch)
pp <- person.parameter(rm)

df_Rasch2 <-  pp$theta.table %>% 
  rownames_to_column("ID") %>% 
  mutate(Proficiency=round(`Person Parameter`, 2)) %>%  dplyr::select(ID, Proficiency) %>% 
  left_join(df_clust_scored, by="ID") %>% 
  mutate(cluster = as.factor(cluster))

df_Rasch2 %>% ggplot() + geom_boxplot(aes(x=cluster, y=Proficiency)) 

df_Rasch2 %>% summarySE(groupvars = "cluster", measurevar = "Proficiency")

t.test(df_Rasch2$Proficiency~df_Rasch2$cluster)

#Analysis of new construct

df_revised_scored2 <- df_rawscore3 %>% 
  filter(infreq<1, Miss_P<.75) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>%
  dplyr::group_by (ID) %>% 
  mutate(
    Discipline =	mean(c(Q8_A_3,	Q8_A_4,	Q8_A_5,	Q8_A_7,	Q8_A_9,	Q8_A_10,	Q12_A_2,	Q13_A_1),na.rm = TRUE),
    Self_Consciousness =	mean(c( Q1_A_1 ,	Q5_A_2	,Q6_A_5,	6-Q11_A_4,	6-Q11_A_5,	6-Q11_A_6,	Q12_A_3,	Q19_A_3 ), na.rm = TRUE),
    Self_Efficacy	= mean(c(Q17_A_3,	Q17_A_4,	Q17_A_5,	Q17_A_6,	Q17_A_7,	Q17_A_8,	Q20_A_2,	Q20_A_6 ),na.rm = TRUE),
    Skill_Confidence = mean(c(6-Q9_A_1,	Q10_A_3,	Q15_A_2,	Q16_A_1,	Q16_A_2,	Q16_A_3,	Q16_A_4,	Q16_A_6), na.rm = TRUE),
    Thrill_Seeking = mean(c(	Q10_A_16,	Q10_A_17,	Q10_A_19,	Q10_A_21,	Q10_A_22,	Q10_A_23,	Q10_A_24,	Q10_A_25), na.rm = TRUE),
    Enthusiasm =	mean(c(Q11_A_7,	Q14_A_1,	Q14_A_2,	Q14_A_3,	Q14_A_4,	Q18_A_1,	Q18_A_4,	Q18_A_6), na.rm = TRUE),
    Cognitive_Proficiency =	mean(c(Q1_A_3,	Q1_A_5,	Q4_A_2,	Q4_A_3,	Q4_A_4,	Q4_A_6,	Q4_A_7,	Q7_A_12), na.rm = TRUE),
    Social_Curiosity =	mean(c(Q10_A_11,	Q10_A_12,	Q10_A_13,	Q10_A_14,	Q10_A_15,	Q11_A_1,	Q11_A_2,	Q11_A_3), na.rm = TRUE),
    Intellectual_Curiosity =	mean(c(Q4_A_5,	Q5_A_1,	Q5_A_4,	Q6_A_1,	Q10_A_4,	Q10_A_8,	Q10_A_9,	Q22_A_6), na.rm = TRUE),
    Analytical =	mean(c(Q9_A_4,	Q9_A_5,	Q10_A_1,	Q11_A_9,	Q11_A_10,	Q11_A_11,	Q11_A_12,	Q13_A_5), na.rm = TRUE), 
    Creativity = mean(c(Q7_A_1,	Q7_A_2,	Q7_A_5,	Q7_A_7,	Q7_A_9,	Q7_A_10,	Q7_A_11,	Q7_A_13), na.rm = TRUE),
    Openness =	mean(c(Q6_A_2,	Q6_A_3,	Q6_A_6,	Q19_A_2,	Q20_A_5,	Q20_A_7,	Q22_A_2,	Q22_A_4), na.rm = TRUE)) %>% 
  cbind(cluster=kmeans_clust$cluster) %>% 
  select(ID, Rank, JQR_Proficiency, Experience, Degree, Bachelors_CS, Masters_CS, SOHO, GTScore, Edit_game, BuiltCPU, Work_Role, cluster, Discipline:Openness)

df_impute2b <- df_revised_scored2 %>%
  ungroup() %>% 
  select(Discipline:Openness) %>%
  as.matrix()
imputation2 <-  preProcess(df_impute2b, method="knnImpute")
df_impute2b <- predict(imputation2, df_impute2b) %>% as.data.frame()

df_revised_scored2b <-  df_revised_scored2 %>% 
  select(ID, Work_Role, cluster) %>% 
  cbind(df_impute2b) %>% as.data.frame()



df_revised_scored %>% left_join(df_clust_scored %>% select(ID, cluster), by="ID") %>% 
  summarySE(groupvars = c("Scale","cluster"), measurevar = "Score" ) %>% 
  select(Scale, cluster, Score) %>% 
  pivot_wider(names_from = "cluster", values_from = "Score") %>% 
  mutate(delta = abs( `1`-`2` ) ) %>% 
  gather(`1`:`2`, key=Cluster, value=Score) %>% 
  #mutate(ci = cbind(df_clust_summary$ci)) %>% 
  ggplot(aes(x=reorder(Scale, delta, fun=max), y=Score, color=Cluster)) +
  geom_point() +
  #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+
  coord_flip()

df_revised_scored2b %>% gather(Discipline:Openness, key=Scale, value=Score) %>% 
  summarySE(groupvars = c("Scale","cluster"), measurevar = "Score" ) %>% 
  select(Scale, cluster, Score) %>% 
  pivot_wider(names_from = "cluster", values_from = "Score") %>% 
  mutate(delta = abs( `1`-`2` ) ) %>% 
  gather(`1`:`2`, key=Cluster, value=Score) %>% 
  #mutate(ci = cbind(df_clust_summary$ci)) %>% 
  ggplot(aes(x=reorder(Scale, delta, fun=max), y=Score, color=Cluster)) +
  geom_point() +
  #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+
  coord_flip()
  

df_revised_scored2 %>% 
  mutate(cluster=as.factor(cluster)) %>% 
  gather(Degree:BuiltCPU, key=Category, value=Response) %>% 
   ggplot(aes(x=Response, group = cluster, fill=cluster)) + geom_bar(aes(y=..prop..), stat="count",  position = "dodge") + facet_wrap(Category~., scales="free")

clust_tot <- df_revised_scored2 %>%  mutate(cluster = as.factor(cluster))%>% 
  group_by(cluster) %>% summarise(total=n())

df_revised_scored2 %>% 
  mutate(cluster = as.factor(cluster)) %>%
    group_by(cluster, Rank) %>% 
  summarise(n = n()) %>% left_join(clust_tot) %>% 
  mutate(n=n/total)

