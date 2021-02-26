library(DT)
library(randomForest)
library(gt)
library(webshot)
webshot::install_phantomjs()

Survey_Completion <- df_rawscore3 %>% select(Work_Role, Survey_Completion) %>% 
  mutate(Work_Role = if_else(Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_3-4", Work_Role)) %>%
  mutate(Work_Role = factor(Work_Role, levels = c("Tier_Other", "Tier_3-4", "Tier_2", "Tier1_RO", "Tier1_EA" , "Tier1_CD"))) %>%
  group_by(Work_Role, Survey_Completion) %>% summarise(n = n()) %>% 
  pivot_wider(names_from = "Survey_Completion", values_from = "n") %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>% 
  mutate(Completed = Yes) %>% select(Work_Role, Completed)

#Rank
#Experience

df_rawscore3 %>%   mutate(Prior_Tier1 = replace_na(Prior_Tier1, "No")) %>%  
  select(Work_Role, Rank, Prior_Tier1) %>% 
  mutate(Work_Role = if_else(Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_3-4", Work_Role)) %>%
  mutate(Work_Role = factor(Work_Role, levels = c("Tier_3-4", "Tier_Other", "Tier1_CD", "Tier1_EA", "Tier1_RO", "Tier_2" ))) %>%   group_by(Work_Role, Rank) %>%
  summarise(n=n()) %>% 
  spread(key=Rank, value=n) %>% left_join(Survey_Completion) %>% 
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  gt(groupname_col = FALSE) %>% 
  grand_summary_rows(
    columns = vars(Enlisted, Officer, Warrant, Completed),
    fns = list(Total = "sum")) %>% 
  gtsave("sample_table.png")
  
#Experience
df_experience <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>%
  select(Work_Role, Rank, Experience) %>% 
  drop_na(Experience) %>% 
  group_by(Work_Role, Rank, Experience) %>%
  summarise(n=n()) %>% 
  left_join(df_rawscore3 %>% 
              mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>%
              select(Work_Role, Rank, Experience) %>% 
              drop_na(Experience) %>% 
              group_by(Work_Role, Rank) %>%
              summarise(n_tot=n())) %>% 
  mutate(n=n/n_tot) %>%
  mutate(n = if_else(Experience=="<=5Years", round(-1*n,1), round(n,1))) %>% 
  mutate(Rank = factor(Rank, levels = c("Enlisted","Officer","Warrant")))

test <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>% select(Work_Role, Experience) %>% drop_na() %>% 
  mutate(Experience=if_else(Experience=="<=5Years", 0, 1))

t.test(test$Experience~test$Work_Role)
       

 df_experience %>%
   mutate(Work_Role= factor(Work_Role, levels = c("Tier_Other", "Tier_1"))) %>% 
   ggplot(aes(x=Work_Role, y=n, fill=Experience)) +
   geom_col() +
   geom_text(aes(label=abs(n)), vjust = if_else(df_experience$n>=0, 0, 1), hjust = if_else(df_experience$n>=0, 1, -1)) +
   scale_fill_manual(values=c("skyblue", "lightgreen")) +
   coord_flip() +
   scale_y_continuous(labels=abs) +
   ylab("% of group") + 
   xlab("") + 
   geom_hline(yintercept = 0, lwd = 2, color="White") +
   facet_grid(Rank~., scales = "free") +
   theme_light() +
   theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12), axis.text.x = element_blank(),
         axis.title = element_text(size=12),
         legend.text = element_text(size=12),
         strip.text = element_text(size=12, color="blue"))+
   ggsave("Experience.png", width = 6.2, height = 5.5)
  
#Education
 df_education <- df_rawscore3 %>% 
   mutate(Degree = as.character(Degree)) %>% 
   mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>% 
   mutate(Degree = if_else(Degree %in% c("Associates", "Some College"), "Some College", Degree)) %>% 
   select(Work_Role, Rank, Degree) %>% 
   drop_na(Degree) %>% 
   group_by(Work_Role, Rank, Degree) %>%
   summarise(n=n()) %>% 
   left_join(df_rawscore3 %>% 
               mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>% 
               mutate(Degree = as.character(Degree)) %>% 
               select(Work_Role, Rank,  Degree) %>% 
               drop_na(Degree) %>% 
               group_by(Work_Role, Rank) %>%
               summarise(n_tot=n())) %>% 
   mutate(n=n/n_tot) %>%
   mutate(n = if_else(Degree %in% c("High School", "Some College"), round(-1*n,1), round(n,1))) %>% 
   mutate(Rank = factor(Rank, levels = c("Enlisted","Officer","Warrant"))) %>% 
   mutate(Degree = factor(Degree, levels= c("High School", "Some College", "Bachelors", "Masters")))
 
 df_education  %>% 
   mutate(Work_Role= factor(Work_Role, levels = c("Tier_Other", "Tier_1"))) %>% 
   ggplot(aes(x=Work_Role, y=n, fill=Degree)) +
   geom_col() +
   #geom_text(aes(label=if_else(n>0, n, " ")), position = "stack") +
   scale_fill_manual(values=c("gray", "lightblue",  "lightgreen", "darkgreen")) +
   coord_flip() +
   scale_y_continuous(labels=abs) +
   ylab("% of group") + 
   xlab("") + 
   ylim(-1,1) +
   scale_y_continuous(labels=abs) +
   geom_hline(yintercept = 0, lwd = 2, color="White") +
   facet_grid(Rank~., scales="free") +
   theme_light() +
   theme(legend.title = element_text(color="black", size=10), legend.position = "top", axis.text = element_text(size=10),
         axis.title = element_text(size=10),
         legend.text = element_text(size=10),
         strip.text = element_text(size=12, color="blue")) +
   ggsave("Education.png",  width = 6.2, height = 5.5)
 

#Education CD Degrees
df_rawscore3 %>% ungroup() %>% 
  mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>% 
  select(ID, Work_Role, Bachelors_CS, Masters_CS) %>%
   mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
         Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
  gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>% 
  group_by(Work_Role,  Degree) %>%
  summarise(n=sum(n)) %>% 
  left_join( df_rawscore3 %>% 
               mutate(Work_Role = if_else(Work_Role=="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4"|Work_Role=="Tier_Other", "Tier_Other", "Tier_1")) %>% 
               select(ID, Work_Role, Bachelors_CS, Masters_CS) %>% 
               mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
                      Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
               gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>%
              group_by(Work_Role) %>%
              summarise(n_tot=n())) %>% 
  mutate(n=n/n_tot) %>% 
  ggplot(aes(x=Work_Role, y=n, fill=Degree)) +
  geom_col(position="dodge") +
  scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("% of Tier group") + xlab("") +
  #facet_grid(.~Degree, scales = "free_x") +
  ggtitle("CS Degrees by Work Role")

#Certifications
df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>% 
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
            select(ID, Work_Role, OSCP:`Security`) %>%
            gather(OSCP:`Security`, key=Cert, value=n) %>% 
            mutate(n=if_else(n=="Yes", 1, 0)) %>% 
            group_by(Work_Role, Cert) %>%
            summarise(n=sum(n)) %>% 
  left_join(df_rawscore3 %>% 
              mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
              filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
              select(Work_Role, Rank) %>% 
              group_by(Work_Role) %>%
              summarise(n_tot=n())) %>%
            mutate(n=round(n/n_tot,2)) %>% 
            select(-n_tot) %>%
  mutate(Work_Role = factor(Work_Role, levels = c("Tier1_CD", "Tier1_EA", "Tier_2-4"))) %>% 
  ggplot(aes(x=reorder(Cert, n, FUN = mean), y=n, color=Work_Role, group=Work_Role)) + 
  geom_point(size=3) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "blue", "darkgray", "gray", "black")) +
  scale_linetype_manual(values=c("dashed", "dashed", "dashed", "blank", "blank")) +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylab("% of Tier group") + xlab("") +
  ylim(0,.50) +
  coord_flip() +
  ggsave("Certifications.png", width = 6.2, height = 5.5, units = "in" )
            
#GT Score
df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>%
            select (ID, Work_Role, GTScore) %>% 
  filter(GTScore !="not reported") %>% 
  group_by(Work_Role, GTScore) %>% summarise(n=n()) %>% 
  left_join(df_rawscore3 %>% 
              mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
              filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
              filter(GTScore !="not reported") %>% 
              select(Work_Role, Rank) %>% 
              group_by(Work_Role) %>%
              summarise(n_tot=n())) %>%
  mutate(n= n) %>% 
  ggplot(aes(x=Work_Role, y=n)) + 
  geom_col() +
  scale_fill_manual(values=c("darkgray", "skyblue", "lightgreen")) +
  #facet_grid(Work_Role~.) +   
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("% of Tier group") + xlab("") +
  facet_grid(.~GTScore) +
  ggtitle("GT Score Categories by Work Role")

#Hobbies
df_hobbies <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>%
  select (ID, Work_Role, BuiltCPU, Gaming, Edit_game, Scripts, SOHO, Hexidecimal) %>%
  gather(BuiltCPU:Hexidecimal, key="Hobby", value="Response") 
df_hobbies$Response <- replace_na(df_hobbies$Response, "No")
df_hobbies %>%  
  mutate(Response = if_else(Response =="Yes", 1, 0)) %>% 
  group_by(Work_Role, Hobby) %>% 
  summarise(Response = sum(Response)) %>% 
  left_join(
    df_rawscore3 %>% 
      mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
      filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>%  
      group_by(Work_Role) %>% 
      summarise(n_tot = n())
  ) %>% 
  mutate(Response = Response/n_tot) %>% 
  mutate(Work_Role = factor(Work_Role, levels = c("Tier1_CD", "Tier1_EA", "Tier_2-4"))) %>% 
  ggplot(aes(x=reorder(Hobby, Response, FUN=mean), y=Response, color=Work_Role, group=Work_Role)) + 
  geom_point(size=3) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "blue", "darkgray", "gray")) +
  scale_linetype_manual(values=c("dashed", "dashed", "dashed", "blank")) +
  theme(legend.title = element_text(color="black", size=12), legend.position = "top", axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, color="blue")) +
  ylab("% of Tier group") +
  xlab("") +
  ylim(0,1) +
  coord_flip() + ggsave("Hobbies.png", width = 6.2, height = 5.5, units = "in" )

#Hobbies2
df_rawscore3 %>% 
  select(ID, Work_Role, CPU_OS, Game_Platform, Game_type) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
  filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
  gather(CPU_OS:Game_type, key=Category, value=Response) %>%
  group_by(Work_Role, Category, Response) %>% 
  summarise(n=n()) %>% 
  left_join(
    df_rawscore3 %>% 
      mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_2-4", Work_Role)) %>%
      filter(!(Work_Role %in% c("Tier_Other", "Tier1_RO"))) %>% 
      group_by(Work_Role) %>% 
      summarise(n_tot = n())
  ) %>%  
  mutate(n= n/n_tot) %>%
  na.omit(Response) %>% 
  mutate(Work_Role = factor(Work_Role, levels = c("Tier1_CD", "Tier1_EA", "Tier_2-4"))) %>% 
  ggplot(aes(x=reorder(Response, n, FUN=mean), y=n, fill=Work_Role)) + 
  geom_col(position="dodge2") + 
  scale_fill_manual(values=c("red", "blue", "darkgray", "gray")) +
  facet_grid(Category~Work_Role, scales = "free_y") + 
  theme(legend.title = element_text(color="black", size=12), legend.position = "blank", axis.text = element_text(size=12),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text = element_text(size=10, color="blue")) +
  xlab("") +
  ylab("% of Tier group") +
  coord_flip() + ylim(0,.75) +
  ggsave("Hobbies2.png", width = 6.2, height = 5.5, units = "in" )

t_test_calc <- df_rawscore3 %>% 
  select(Work_Role, Edit_game) %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  na.omit() %>% 
    mutate(Edit_game = if_else(Edit_game=="Yes", 1, 0))

t.test(t_test_calc$Edit_game ~ t_test_calc$Work_Role)
  
#Random Forest Modeling################################
set.seed(102)
df_randomforest <- df_rawscore3 %>% 
  mutate(Work_Role = if_else(Work_Role =="Tier_2"|Work_Role=="Tier_3"|Work_Role=="Tier_4", "Tier_Other", Work_Role)) %>% 
  mutate(Degree = if_else(Degree=="High School", 0, if_else(Degree %in% c("Associates", "Some College"), 1, if_else (Degree=="Bachelors", 2, if_else(Degree=="Masters & Up", 3, 0))))) %>% 
  gather(Bachelors_CS:Masters_CS, OSCP:Security, BuiltCPU, Gaming, Edit_game:Hexidecimal, key=Measure, value=Score) %>% 
  mutate(Score = if_else(Score=="Yes", 1, 0)) %>% 
  pivot_wider(names_from = "Measure", values_from= "Score") %>% 
  select(Work_Role, Degree, Bachelors_CS:Hexidecimal) %>% 
  rownames_to_column("ID") %>% 
  group_by(ID) %>% 
  mutate(Certifications = sum(OSCP, OSCE, GPEN, GXPN, GCIH, CEH, CISSP, CISM, Security)) %>% 
  column_to_rownames("ID")

df_randomforest_a <- df_randomforest %>% filter (Work_Role %in% c("Tier_1" ) ) %>% 
     mutate_at(vars(Degree:Hexidecimal),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)) 
df_randomforest_b  <- df_randomforest %>% filter (Work_Role %in% c("Tier_Other"))  %>% 
     mutate_at(vars(Degree:Hexidecimal),~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))        
df_randomforest2 <- rbind(df_randomforest_a, df_randomforest_b) %>% 
  select(Work_Role, Degree, Bachelors_CS, Masters_CS, BuiltCPU:Certifications) #bind together the Tier 1 and the Tier 2/3/4 dataframes

  

df_randomforest2[1] <- lapply(df_randomforest2[1], factor)
df_randomforest2[3:10] <- lapply(df_randomforest2[3:10], factor)
# Split into Train and Testation sets
# Training Set : Testation Set = 75 : 25 (random)
train <- sample(nrow(df_randomforest2), .70*nrow(df_randomforest2), replace = FALSE)
TrainSet <- df_randomforest2[train,] %>% as.data.frame()
TestSet <- df_randomforest2[-train,] %>% as.data.frame()

library(DMwR)
TrainSet3 <- SMOTE(Work_Role ~., TrainSet, perc.over = 100, perc.under = 200)
summary(TrainSet3)

library(ROSE)
TrainSet2 <- ROSE(Work_Role~., data=TrainSet, seed=123)$data %>% as.data.frame()
summary(TrainSet2)
summary(TestSet)
# Create a Random Forest model with default parameters
model1 <- randomForest(Work_Role ~ ., data = TrainSet3, importance = TRUE)
model1

model2 <- randomForest(Work_Role ~ ., data = TrainSet3, ntree = 400, mtry = 3, importance = TRUE, proximity=TRUE)
model2

# Predicting on train set
predTrain <- predict(model1, TrainSet3, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet3$Work_Role)

# Predicting on Test set
predTest <- predict(model1, TestSet, type = "class")
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
model <- train(Work_Role~., data=TrainSet3, method="knn",
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
knnFit <- train( Work_Role~ ., data = TrainSet3, method = "knn", trControl = ctrl,
                 preProcess = c("center","scale"), tuneLength = 20)

#Output of kNN fit
knnFit

plot(knnFit)

knnPredict <- predict(knnFit,newdata = TestSet )
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, TestSet$Work_Role )

mean(knnPredict == TestSet$Work_Role)
