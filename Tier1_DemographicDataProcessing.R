library(DT)

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
