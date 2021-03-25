
df<-read_csv("inputs/VENA HH data cleaned_without ID.v9_withoutPSN.csv")
df

#lots of columns- lets check colnames
colnames(df)
dim(df)
df$id
df %>% select(start,end,Settlement)
df

select(df$"settlement")
df %>% select(assistance_inkind_, Settlement)
class(df$lobule)

df %>% 
  filter(assistance_cash_==lobule)

# checking start_time classification
class(df$start)
class(df$end)
class()

#filtering the data set based on start date of 4th-sep
df %>%
  filter(start=="4-Sep-19")

# filtering data where the start time not equal to end time
#extracting uuid, start, and end from the filtered data set
#calculating only the number of rows from the filtered data st
#Asign this number to an object e.g. num_start_end_problems

num_start_end_problems<-df %>% 
  select(uuid,start,end,) %>% 
  filter(start!=end) %>% 
  select(uuid,start,end) %>% nrow()

# calculating % of data set with problem vs overall

percent_start_end_problems<- (num_start_end_problems/nrow(df))*100

percent_start_end_problems
round(percent_start_end_problems,2)

#Summarizing the data set by settlement and caculating the mean_hh_size
settlement_mean_hh_size<-df %>%
  group_by(Settlement) %>%
  summarise( 
  mean_hh_size= mean(HH_size_VENA,na.rm=T) %>% round(2)
  ) 
settlement_mean_hh_size

  # write summarize to CSV file
write_csv(x =settlement_mean_hh_size,file = "outputs/Mean_HH_size_by_settlement.csv")

Mean_hh_size<- mean(df$HH_size_VENA,na.rm = T)
#ploting historygramm
df %>% 
  ggplot(aes(x=HH_size_VENA))+
  geom_histogram()+ 
  geom_vline(xintercept = Mean_hh_size,colour="purple")+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30))+
  labs(x="Household size",y="frequency",
       title=paste("Mean household size=",round(Mean_hh_size,2)))+
  theme_bw()
  
?labs
df$HoH_age
# Adding colunm to the data set
#selecting new columns from the df
df %>% 
  mutate(
    i.respondent_sex_2= ifelse(respondent_sex_==1, "male", "female"),
    i.hoh_sex_2=ifelse(HoH_sex==1,"male", "female"),
    i.young_female_hoh=ifelse(i.hoh_sex_2=="female"&HoH_age<21,T,F)
  ) %>% 
  select(starts_with("i."))

df %>% 
  mutate(
    i.respondent_sex_2= ifelse(respondent_sex_==1, "male", "female"),
    i.hoh_sex_2=ifelse(HoH_sex==1,"male", "female"),
    i.young_female_hoh=ifelse(i.hoh_sex_2=="female"&HoH_age<21,T,F)
  ) %>% 
  select(starts_with("i.")) %>% 
  group_by(i.young_female_hoh) %>% 
  summarise(
    n=n(),
    prop_n=n/nrow(.)*100
  )

df %>% 
  mutate(
    i.respondent_sex_2= ifelse(respondent_sex_==1, "male", "female"),
    i.hoh_sex_2=ifelse(HoH_sex==1,"male", "female"),
    i.young_female_hoh=ifelse(i.hoh_sex_2=="female"&HoH_age<21,T,F)
  ) %>% 
  select(starts_with("i.")) %>% 
  filter(i.young_female_hoh==T) %>% 
  nrow()

df %>% 
  mutate(
    i.respondent_sex_2= ifelse(respondent_sex_==1, "male", "female"),
    i.hoh_sex_2=ifelse(HoH_sex==1,"male", "female"),
    i.young_female_hoh=ifelse(i.hoh_sex_2=="female"&HoH_age<21,T,F)
  ) %>% 
  select(starts_with("i.")) %>% 
  summarise(
    perc_young_female_hoh=mean(i.young_female_hoh, na.rm = TRUE)*100
  )
  


df %>% 
  mutate(
    data_cleaner="buyuki",
    hh_size_class=ifelse(HH_size_VENA<4,"small","large"),
    registered_size_vs_hh_size=ifelse(unregistered_hh_members_number<=HH_size_VENA,"good","bad")
    ) %>%
    select(uuid,
         Settlement,
         data_cleaner,
         HH_size_VENA,
         hh_size_class,registered_size_vs_hh_size)

  #summarise data by hh_size classification
summarise(
    n=n(),
prop_n=n/nrow(.)
    )

 df$marital_status_
df$head_of_household_
df$unregistered_hh_members_number

# new
df %>% 
filter("refugee_settlement_==rhino")

rh<-df %>% 
select(assistance_inkind_,refugee_settlement_,HoH_sex) %>%  
filter(refugee_settlement_=="rhino!=lobule")  
write.csv(x = rh,file = "outputs/rhino.csv")

# summarising by hoh_size by zone
Zone_mean_hoh_size<-df %>%
group_by(Zone) %>% 
  summarise(
    mean_hoh_size=mean(HH_size_VENA,na.rm = T)
  )  
# write summary
write.csv2(x =Zone_mean_hoh_size ,file = "outputs/Zone_mean_hh_size.csv") 
)

#new work
df %>% 
ggplot(aes(x = HH_size_VENA))+
  geom_histogram()+geom_vline(xintercept = Mean_hh_size,colour="pink")+scale_x_continuous(breaks =c(6,12,18,24,30,36))+
  scale_y_continuous(breaks = c(150,300,450,600,750,900,1050,1200))+labs(x="household size",y="frequency",tittle=paste("mean household size=",round(Mean_hh_size,2)))+
  theme_bw()
