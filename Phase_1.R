library(tidyverse)
install.packages("lubridate")

library(lubridate)

df<-read_csv(file = "inputs/vena_raw_simplified.csv")  

data_collection_start<- as_date("2019-09-01")
six_month_threshold<- data_collection_start - months(6)

df2<- df %>%
  mutate(
    i.arrived_last_6_months= ifelse(date_arrived>=six_month_threshold,TRUE,FALSE),
    i.hh_less_blankets_than_occupants= ifelse(cri_blanket<hh_size, TRUE, FALSE)
  ) %>% 
  
  select(
    starts_with("i.")
  )
df2


write_csv(x=df2, file = "outputs/hh_arrived_past_6_months.csv")





percent_hhs_arrived_past_6_months<-(hh_arrived_past_6_months/nrow(df))*100






write_csv(x= percent_hhs_arrived_past_6_months, file = "outputs/percent_hh_arrived_past_6_months.csv")  

# Food consumption score 
df2<- df %>%
  mutate(
    fcs_cereals_tuber=food_consumed_cereals_roots_tubers,
    fcs_pulses = food_consumed_pulses,
    fcs_oil_fats= food_consumed_fat,
    fcs_sugar= food_consumed_sugar,
    fcs_condiments= food_consumed_condiments,
    
  ) %>%
  rowwise() %>%
  mutate(
    fcs_veg = sum(food_consumed_veg_portal,food_consumed_green_veg,food_consumed_orange_veg, na.rm = TRUE),
    fcs_fruits= sum(food_consumed_fruits_portal, food_consumed_orange_fruits, na.rm = TRUE),
    fcs_meat_fish= sum(food_consumed_meat_portal, food_consumed_flesh_meat, food_consumed_organ_meat, food_consumed_fish, na.rm = TRUE),
    fcs_dairy= sum(food_consumed_dairy, food_consumed_eggs, na.rm = TRUE)
    
    
  ) %>%
  ungroup()

df2 %>% 
  select(
    starts_with("fcs")
  )



colnames(df)

df3<-df2 %>%
  mutate(
    i.fcs_score= (fcs_cereals_tuber*2)+ (fcs_pulses*3)+(fcs_veg*1)+(fcs_dairy*4)+(fcs_meat_fish*4)+(fcs_fruits*1)+(fcs_oil_fats*.5)+(fcs_sugar*.5)+(fcs_condiments*0)
  )# %>% 
#  
# select(
#   starts_with("i.")
# )


fcs_threshold<-df3 %>% 
  mutate(
    i.fcs_score_acceptable= ifelse(i.fcs_score>35, T, F),
    i.fcs_score_borderline= ifelse(i.fcs_score<=35& i.fcs_score>=21.5, T, F),
    i.fcs_score_poor= ifelse(i.fcs_score<21.5, T, F),
    i.fcs_category= ifelse(i.fcs_score>35, "acceptable", ifelse(i.fcs_score>=21.5, "borderline", "poor"))
  ) #%>% 

# select(
#   starts_with("i.")
# )

fcs_threshold

FCS_score = (cereals_tubers*2 + pulses_nuts_seeds*3 +
               vegetables*1 + fruits*1 +dairy*4 +
               meat_fish*4 + oil_fats*.5 +sweets*.5 +
               spices_condiments*0)

read_csv(file = "inputs/vena_raw_simplified.csv")

fcos_components<-df %>% 
  mutate(
    fcos_components<-c("food_consumed_cereals_roots_tubers, food_consumed_pulses, food_consumed_dairy, food_consumed_meat_portal, food_consumed_flesh_meat, food_consumed_organ_meat, food_consumed_fish, food_consumed_eggs, food_consumed_veg_portal, food_consumed_orange_veg, food_consumed_green_veg, food_consumed_fruits_portal, food_consumed_orange_fruits, food_consumed_fat, food_consumed_sugar, food_consumed_condiments, food_consumed_beverages, food_consumed_cereals_roots_tubers, food_consumed_pulses, food_consumed_dairy, food_consumed_meat_portal, food_consumed_flesh_meat, food_consumed_organ_meat, food_consumed_fish, food_consumed_eggs, food_consumed_veg_portal, food_consumed_orange_veg, food_consumed_green_veg, food_consumed_fruits_portal, food_consumed_orange_fruits, food_consumed_fat, food_consumed_sugar, food_consumed_condiments, food_consumed_beverages")
  )

write_csv(x=fcos_components, file = "outputs/food_consumption_score.csv", na ="")

df %>%
  group_by(uuid)
summarise(
  mean_food_consumption_score= mean(fcos_components, na.rm = TRUE) %>% round(2)
)


# hhs with less blankets 
i.hh_less_blankets_than_occupants<-df %>%
  mutate(
    i.hh_less_blankets_than_occupants= ifelse(cri_blanket<hh_size, TRUE, FALSE)
  ) %>% 
  select(
    i.hh_less_blankets_than_occupants
  )
?print  

write_csv(x= i.hh_less_blankets_than_occupants, file = "outputs/hh_less_blankets.csv") 




