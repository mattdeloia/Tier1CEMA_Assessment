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

#Remove rows with missing values and keep only complete cases
efa_data <- df_rawscore4 %>% select(Q1_A_1:Q21_A_15)

efa_data %>% rownames_to_column ("ID") %>% count ()

#Create the correlation matrix
efa_cor <- cor(efa_data, use = "pairwise.complete")
lowerCor(efa_data)

#principle components analysis
options(max.print = 1e7)
pc3 <- principal(efa_cor, nfactors=14, rotate="varimax")
print.psych(pc3, digits=2,  cut = 0.3, sort = TRUE)

#Very Simple Structure Fit
vss(efa_data, n =27, rotate = "varimax")
VSS.scree(efa_data)

#Factor analysis of the data
fa.parallel(efa_data)
factors_data <- fa(r = efa_cor, nfactors = 14)

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
  gather(V1:V14, key="Scale", value="Loading") %>% 
  mutate(Scale = gsub("V", "Scale_", Scale)) %>% 
  mutate(Scale = as.factor(Scale)) %>% 
  right_join(df_question) %>% 
  dplyr::group_by(Question) %>% 
  mutate(Rank = rank(-1*abs(Loading))) %>% 
  filter(Rank==1) %>%  
  dplyr::group_by(Scale) %>% 
  mutate(LoadRank = rank(-1*abs(Loading))) %>% 
  filter(LoadRank<=8) %>% 
  select(-Item, -Rank, -LoadRank )

#Scoring by new scale construct
df_revised_scored <- df_rawscore4 %>% select(ID, Work_Role, Rank, Q1_A_1:Q21_A_16) %>% 
  gather(Q1_A_1:Q21_A_16, key=Question, value=Score) %>% 
  left_join(df_efa, by="Question") %>%
  mutate(Score = if_else(Loading>0, Score, 6-Score) ) %>% #reverse scoring based on item loading
  select(Scale, ID:Question, Score) %>% 
  drop_na(Scale)%>% 
  group_by(ID, Work_Role, Scale) %>% 
  summarise(Score=mean(Score))

corrplot(cor(df_revised_scored %>%
               select(ID, Scale, Score) %>%
               pivot_wider(names_from=Scale, values_from = Score) %>% ungroup() %>% 
               dplyr::select(-ID, -Work_Role)),
         method="color", order="hclust", type="full", addrect=10, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=.5 , number.cex=.5, tl.cex=.5)

#Visualization of components to revised questionnaire

#t-test for difference in Group means
t.test(Score~Work_Role, df_revised_scored)
library(rstatix)
stat_test <- df_revised_scored %>%
  group_by(Scale) %>%
  t_test(Score ~ Work_Role)
stat_test %>% filter(p<.20) %>% count()
sig_scales <- stat_test %>% mutate(Significant = if_else(p<.10, "Yes", "No")) %>% arrange(p)

df_efa %>% left_join(sig_scales) %>% 
  summarySE(measurevar = "Loading", groupvars = c("Scale", "Trait", "Significant", "p" )) %>%
  ungroup() %>% group_by(Scale) %>% 
  mutate(Scale = factor(Scale, levels=sig_scales$Scale)) %>% 
  ggplot() + 
  geom_col (aes(x=Trait, y=N, fill=Significant)) + 
  scale_fill_manual(values=c("gray", "green")) +
  xlab("") +
  ylab("count of questions") +
  coord_flip() + 
  theme(legend.position = "blank") +
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

#data table of revised scales
df_efa %>% 
  left_join(sig_scales) %>% 
  mutate(Loading = round(Loading, 2)) %>%
  left_join(item_stats) %>% 
  arrange(p) %>% 
  select (Scale, Trait, Question, Loading, Mean, sd, Content)  %>% 
  gt(groupname_col = "Scale", rowname_col="Question") %>% 
  tab_spanner(label="item metrics", columns=matches("Loading|Mean|sd")) %>% 
  tab_header(
    title=md("Table of Scales and Items for Revised Questionnaire") )
                      
#missingness plot
df_rawscore4 %>% 
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
df_rawscore4 %>% 
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
