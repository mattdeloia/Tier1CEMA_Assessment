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
df_scored %>% filter(Miss_P<.33, infreq <1) %>% 
  gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>% 
  ggplot(aes(x=reorder(Dimension, Score, FUN=mean, na.rm=TRUE), y=Score)) +
  geom_boxplot(fill="skyblue", alpha =.5, outlier.colour = "gray") +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  coord_flip() + xlab("") + 
  # ggtitle("Boxplot: Average Scores by Personality Dimension") +
  labs(caption = "Note: 1=Low Self-report; 5=High Self-report") +
  ggsave("Personality_RawScores.png", width = 6.2, height = 5.5, units = "in" ) #save to folder

#Cognitive raw scores
Dimension <- c("ThreeD", "Pattern", "Matrix", "Verbal", "Analogies")
Score <- c(.23, .46, .44, .57, .37)
ICAR_metrics <- data.frame(Dimension, Score)

ggplot() +
  geom_boxplot(data = 
                 df_scored %>% filter(Miss_C<=.25, sincere<=3) %>% select(ID, Work_Role, Analogies:Cog_tot)  %>% 
                 gather(Analogies:Verbal, key=Dimension, value=Score), 
               aes(x=reorder(Dimension, Score, FUN=mean, na.rm=TRUE), y=Score),
               fill="skyblue") +
  geom_point(data=ICAR_metrics, aes(x=Dimension, y=Score), size=3, color="green") +
  coord_flip() + xlab("") +
  ylab("score (% correct)") +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue"))+
  labs(caption = "Note: green points reference average test performance") +
  #ggtitle("Boxplot: Scores by Cognitive Test Type") +
  ggsave("Cognitive_RawScores.png", width = 6.2, height = 5.4/2, units = "in" ) #save to folder


#Correlation Plot of Personality Dimensions
dfcorrplot <-  df_scored %>%  #dataframe of select features in a matrix
  filter(Miss_P<miss_limit) %>% 
  select(ID, Problem_Solving:Tolerance) %>%
  column_to_rownames("ID") %>% as.data.frame() %>% na.omit()

corrplot(cor(dfcorrplot), method="color", order="hclust", type="full", addrect=10, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=1, number.font=.5 , number.cex=.5, tl.cex=.5) #corrplot personality items

hclust(dist(data.frame(t(dfcorrplot)))) %>% ggdendrogram(rotate=TRUE) +
  ggsave("Dimension_Dendrogram.pdf", width=8.5, height=11, units = "in")

#Correlation Plot Cognitive
dfcorrplot2 <-  df_scored %>% 
  filter(Miss_C<miss_limit) %>% 
  select(ID, Analogies:Verbal) %>%
  column_to_rownames("ID") %>% as.data.frame()

corrplot(cor(dfcorrplot2), method="color", order="hclust", type="full", addrect=2, cl.lim=c(-1,1), 
         addCoef.col="black", rect.col="green", diag=FALSE, number.digits=2, number.font=.5 , number.cex=.5) #corrplot cognitive items

#Visualization of Personality Test Results
#Scale Personality scored data with mean of zero and std deviation of 1
df_scored2 <- df_scored %>% ungroup() %>% 
  filter(Miss_P <.33, infreq<1) %>% 
  select(ID, Work_Role, Problem_Solving:Tolerance) %>% 
  mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
  gather(Problem_Solving:Tolerance, key=Dimension, value=Score) 

df_scored2_summary <-  df_scored2  %>% 
  summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE) #compute summary statistics for Tier1 and Tier_Other

df_scored2_order <- df_scored2 %>% 
  summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE) %>% 
  select(Dimension, Work_Role, Score) %>% 
  pivot_wider(names_from = "Work_Role", values_from = "Score" ) %>% 
  mutate(sd = abs(Tier1_CD-Tier1_EA)) %>% 
  mutate(order=rank(sd)) %>% 
  select(Dimension, order) #compute summary statistics for Tier 1 

#Visualization of summary statistics for Personality results by work role      
library(rstatix)

df_scored2_summary %>% ungroup() %>% 
  filter(Work_Role %in% c("Tier1_CD", "Tier1_EA")) %>% 
  left_join(df_scored2_order)  %>% 
  left_join(
    df_scored2 %>%
      filter(Work_Role %in% c("Tier1_CD", "Tier1_EA")) %>% 
      dplyr::group_by(Dimension) %>%
      t_test(Score ~ Work_Role) %>%
      select(Dimension, p)
  ) %>%
  mutate(Significant = if_else(p<.05, "p<0.05", if_else(p<0.1, "p<0.1", "no_sig"))) %>%
  mutate(Significant = factor(Significant, levels=c("p<0.05", "p<0.1","no_sig"))) %>% 
  ggplot(aes(x=reorder(Dimension, order, fun=max), y=Score)) +
  geom_point(aes(color=Work_Role),  size=3) +
  geom_line(aes(color=Significant), size=1) +
  geom_hline(yintercept = 0, linetype="dashed", color="black") +
  geom_hline(yintercept = -.5, linetype="dashed", color="darkgray") +
  geom_hline(yintercept = .5, linetype="dashed", color="darkgray") +
  scale_color_manual(values=c("lightgray", "green", "skyblue", "red", "blue")) +
  #scale_linetype_manual(values=c("dotted", "blank", "blank", "blank"))+
  #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+
  coord_flip() + 
  xlab(" ") + 
  ylab("group scaled score (mean)") +
  theme(legend.title = element_text(color="black", size=12), legend.position = "blank", axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylim(-1.5,1.5) +
  #ggtitle("Scaled Score Mean by Work Role") +
  labs(caption = "Hypothesis Test: p < 0.05 (green bars); p < 0.1 (blue bars); Points: red - CD, blue - EA") +
  ggsave("PersonalityScores_WorkRole.png",  width = 6.2, height = 5.5, units = "in") #save to folder

df_scored %>% ungroup() %>% 
  filter(Miss_P <.33, infreq<1) %>% 
  select(ID, Work_Role, Problem_Solving:Tolerance) %>%
  mutate(Work_Role = if_else(Work_Role %in% c("Tier1_CD", "Tier1_EA", "Tier1_RO"), "Tier1", "Tier_Other")) %>%  
  mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
  gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>%  
  summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE) %>% 
  left_join(df_scored %>% ungroup() %>% 
              filter(Miss_P <.33, infreq<1) %>% 
              select(ID, Work_Role, Problem_Solving:Tolerance) %>%
              mutate(Work_Role = if_else(Work_Role %in% c("Tier1_CD", "Tier1_EA", "Tier1_RO"), "Tier1", "Tier_Other")) %>%  
              mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
              gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>% 
              summarySE(groupvars = c("Work_Role", "Dimension"), measurevar = "Score", na.rm = TRUE) %>% 
              select(Dimension, Work_Role, Score) %>% 
              filter(Work_Role == "Tier1") %>% 
              mutate(order=rank(Score)) %>% 
              select(Dimension, order)
  )  %>% 
  left_join(
    df_scored %>% ungroup() %>% 
              filter(Miss_P <.33, infreq<1) %>% 
              select(ID, Work_Role, Problem_Solving:Tolerance) %>%
              mutate(Work_Role = if_else(Work_Role %in% c("Tier1_CD", "Tier1_EA", "Tier1_RO"), "Tier1", "Tier_Other")) %>%  
              mutate_at(vars(Problem_Solving:Tolerance), scale) %>% #scale function
              gather(Problem_Solving:Tolerance, key=Dimension, value=Score) %>%  
      dplyr::group_by(Dimension) %>%
      t_test(Score ~ Work_Role) %>%
      select(Dimension, p)
  ) %>%
  mutate(Significant = if_else(p<.05, "p<0.05", if_else(p<0.1, "p<0.1", "no_sig"))) %>%
  mutate(Significant = factor(Significant, levels=c("p<0.05", "p<0.1","no_sig"))) %>% 
  ggplot(aes(x=reorder(Dimension, order, fun=max), y=Score)) +
  geom_point(aes(shape=Work_Role), color="gray",  size=3) +
  geom_line(aes(color=Significant), size=1) +
  geom_hline(yintercept = 0, linetype="dashed", color="black") +
  geom_hline(yintercept = -.5, linetype="dashed", color="darkgray") +
  geom_hline(yintercept = .5, linetype="dashed", color="darkgray") +
  scale_color_manual(values=c("green", "blue", "gray")) +
  #scale_linetype_manual(values=c("dotted", "blank", "blank", "blank"))+
  #geom_errorbar(aes(ymin=Score-ci, ymax=Score+ci), width=.1 )+
  coord_flip() + 
  xlab(" ") + 
  ylab("group scaled score (mean)") +
  theme(legend.title = element_text(color="black", size=12), legend.position = "blank", axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylim(-1.5,1.5) +
  #ggtitle("Scaled Score Mean by Work Role") +
  labs(caption = "Hypothesis Test: p < 0.05 (green bars); p < 0.1 (blue bars); Shapes: triangle - Tier1; circle - Other") +
  ggsave("PersonalityScores_Tier.png",  width = 6.2, height = 5.5, units = "in") #save to folder

library(ggridges)
#Visualization of summary statistics for Cognitive results by work role     
df_scored %>% filter(Miss_C < .33, sincere<5) %>% ungroup() %>% 
  select(Work_Role, Analogies:Verbal, Cog_tot, Proficiency) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>% 
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
  mutate(Work_Role = factor(Work_Role, levels=c("Tier1_CD", "Tier1_EA", "Tier_2-4"))) %>% 
  ggplot() + geom_density_ridges(aes(y=Work_Role,  x=Proficiency, fill=Work_Role), alpha=.5,
                                 quantile_lines=TRUE, quantile_fun=function(x,...)mean(x))+
  scale_fill_manual(values=c("red", "blue", "darkgray", "darkgray", "blue")) +
  theme(legend.title = element_text(color="black", size=12), legend.position = "blank", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylab("") +
  ggsave("CognitveScores_WorkRole.png", width = 6.2, height = 5.4/2, units = "in" ) #save to folder


df_scored %>% filter(Miss_C < .33, sincere<5) %>% ungroup() %>% 
  select(Work_Role, Analogies:Verbal, Cog_tot, Proficiency) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>% 
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
  mutate(Work_Role = factor(Work_Role, levels=c("Tier1_CD", "Tier1_EA", "Tier_2-4"))) %>% 
  group_by(Work_Role) %>% summarise(mean=mean(Proficiency))
########################################### Working Line - Under Construction #################
df_kNN <- df_scored %>% select(Work_Role,Problem_Solving:Tolerance ) %>%
  column_to_rownames("ID") %>% mutate(Work_Role = as.factor(Work_Role)) %>% 
  drop_na(Problem_Solving:Tolerance)

index <- createDataPartition(df_kNN$Work_Role, p = .6, list = FALSE)
training <- df_kNN[index,]
testing <- df_kNN[-index,]

training2 <- SMOTE(Work_Role ~., training, perc.over = 100, perc.under = 200)
summary(training2)
# Variable Selection using k-NN model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Work_Role~., data=training2, method="knn",
               preProcess="scale", 
               trControl=control)
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
knnFit <- train( Work_Role~ ., data = training2, method = "knn", trControl = ctrl,
                 preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, testing$Work_Role )

mean(knnPredict == testing$Work_Role)
