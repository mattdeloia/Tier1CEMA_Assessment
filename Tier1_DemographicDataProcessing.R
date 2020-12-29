library(DT)
library(randomForest)

df_demographic <- df_preprocess %>% select (ID,Rank:Hexidecimal)

n_Tier <- df_demographic %>% group_by(Work_Role) %>% summarise(n_tot=n())

#Rank
#Experience
datatable(df_demographic %>% 
  select(Work_Role, Rank) %>% 
  group_by(Work_Role, Rank) %>%
  summarise(n=n()) %>% 
  spread(key=Rank, value=n) )


#Experience
df_demographic %>% 
  select(Work_Role, Experience) %>% 
  group_by(Work_Role, Experience) %>%
  summarise(n=n()) %>% 
  left_join(n_Tier) %>% 
  mutate(n=n/n_tot) %>% 
  ggplot(aes(x=Experience, y=n, fill=Work_Role)) + 
  geom_col() +
  scale_fill_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("") + xlab("") +
  facet_grid(Work_Role~.) +
  ggtitle("Years Experience Level by Work Role")


#Education
df_demographic %>% 
  select(Work_Role, Degree) %>% 
  group_by(Work_Role, Degree) %>%
  summarise(n=n()) %>% 
  left_join(n_Tier) %>% 
  mutate(n=n/n_tot) %>% 
  ggplot(aes(x=Degree, y=n, color=Work_Role, group=Work_Role)) + 
  geom_point(size=2) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("") + xlab("") +
  ggtitle("Highest Education Level by Work Role")

#Education CD Degrees
df_demographic %>% 
  select(ID, Work_Role, Bachelors_CS, Masters_CS) %>% 
  mutate(Bachelors_CS = if_else(Bachelors_CS=="Yes", 1, 0),
         Masters_CS = if_else(Masters_CS=="Yes", 1, 0)) %>%
  gather(Bachelors_CS:Masters_CS, key=Degree, value=n) %>% 
  group_by(Work_Role, Degree) %>%
  summarise(n=sum(n)) %>% 
  left_join(n_Tier) %>% 
  mutate(n=n/n_tot) %>% 
  ggplot(aes(x=Degree, y=n, fill=Work_Role)) + 
  geom_col() +
  scale_fill_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("% of Tier group") + xlab("") +
  facet_grid(.~Work_Role) +
  ggtitle("CS Degrees by Work Role")

#Certifications
df_demographic %>% 
            select(ID, Work_Role, OSCP:`Security+`) %>%
            gather(OSCP:`Security+`, key=Cert, value=n) %>% 
            mutate(n=if_else(n=="Yes", 1, 0)) %>% 
            group_by(Work_Role, Cert) %>%
            summarise(n=sum(n)) %>% 
            left_join(n_Tier) %>%
            mutate(n=round(n/n_tot,2)) %>% 
            select(-n_tot) %>%
  ggplot(aes(x=reorder(Cert, n, FUN = mean), y=n, color=Work_Role, group=Work_Role)) + 
  geom_point(size=2) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ylab("percentage of Tier") + xlab("") +
  coord_flip() +
  ggtitle("Certifications by Work Role")
            
#GT Score
df_demographic %>% 
            select (ID, Work_Role, GTScore) %>% 
  group_by(Work_Role, GTScore) %>% summarise(n=n()) %>% 
  left_join(n_Tier) %>% 
  mutate(n= n/n_tot) %>% 
  ggplot(aes(x=GTScore, y=n, fill=Work_Role)) + 
  geom_col () +
  scale_fill_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  facet_grid(Work_Role~.) +   
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  ggtitle("GT Categories by Work Role")

#Hobbies
df_hobbies <- df_demographic %>% 
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
  geom_point(size=2) +
  geom_line(aes(linetype=Work_Role)) +
  scale_color_manual(values=c("red", "skyblue", "lightgreen", "gray")) +
  scale_linetype_manual(values=c("dashed", "blank", "blank", "blank")) +
  theme(legend.title= element_text(color="black", size=10), legend.position = "top") +
  xlab("") +
  coord_flip()

#Hobbies2
df_hobbies2 <- df_demographic %>% 
  select(ID, CPU_OS, Game_Platform, Game_type) %>% 
  gather(CPU_OS:Game_type, key=Category, value=Response) %>%
  group_by(Category, Response) %>% 
  summarise(n=n()) %>% 
  na.omit(Category)
  
df_hobbies2 %>% ggplot(aes(x=reorder(Response, n, FUN=mean), y=n)) + 
  geom_col() + 
  facet_grid(Category~., scales = "free_y") + 
  xlab("") +
  coord_flip() 

#Random Forest Modeling################################
set.seed(102)
df_randomforest <- df_demographic %>% mutate(Work_Role=if_else(Work_Role=="Tier_1", "Tier_1", "Other")) %>% 
  select(-ID, -CPU_type,-CPU_OS, -Game_Platform, -Game_type ) %>% na.omit()
df_randomforest[1:25] <- lapply(df_randomforest[1:25], factor)
# Split into Train and Testation sets
# Training Set : Testation Set = 75 : 25 (random)
train <- sample(nrow(df_randomforest), .75*nrow(df_randomforest), replace = FALSE)
TrainSet <- df_randomforest[train,]
TestSet <- df_randomforest[-train,]
# Create a Random Forest model with default parameters
model1 <- randomForest(Work_Role ~ ., data = TrainSet, importance = TRUE)
model1

model2 <- randomForest(Work_Role ~ ., data = TrainSet, ntree = 400, mtry = 5, importance = TRUE, proximity=TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Category)

# Predicting on Test set
predTest <- predict(model2, TestSet, type = "class")
# Checking classification accuracy
table(predTest,TestSet$Category)
mean(predTest == TestSet$Category)

# To check important variables
importance(model2)        
varImpPlot(model2) 

randforest_report <- importance(model2) %>% as.data.frame() 
max_importance <- max(randforest_report$MeanDecreaseAccuracy)
randforest_report2 <-   rownames_to_column(randforest_report, var="Feature") %>% 
  mutate(Importance=round(MeanDecreaseAccuracy/max_importance,1), Model="Random Forests") %>% 
  select (Model, Feature, Importance) %>% 
  arrange(-Importance)

model_crossvalid <- train(Category ~ ., data = model_data, method="rf", trControl = trainControl(method ="cv", number = 5, verboseIter=TRUE))
model_crossvalid
