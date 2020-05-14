library(tidyverse)
library(rpart)

summary(df_DS_comb_full$BIO_SEX)
summary(df_DS_comb_full$H1GI8_RACE)

view(select(df_DS_combined_full, contains('TO')))

df_DS_combined_full %>% 
    group_by(H1GI8_RACE, BIO_SEX) %>% 
    filter(!is.na(H1TO1_TRYcig)) %>% 
    count(H1TO1_TRYcig)

df_DS_combined_full %>% 
    group_by(., BIO_SEX, H1GI8_RACE) %>% 
    count(H1TO1_TRYcig)


summary(df_DS_combined_full$H1TO1_TRYcig)
summary(df_DS_combined_full$H1PR1)
summary(df_DS_combined_full$H1RE1)
summary(df_DS_combined_full$H1EE1)
summary(df_DS_combined_full$H1TO3_REGcig)
summary(df_DS_combined_full$H1TO9_FRNDS)


df_DS_combined_full %>% 
    group_by(H1TO1_TRYcig) %>% 
    count(H1TO3_REGcig)

df_DS_combined_full %>% 
    group_by(H1TO12_TRYalc) %>% 
    count(H1TO13_alc)

df_DS_combined_full %>% 
    group_by(H1TO3_REGcig) %>% 
    count(H1PR1)

df_DS_pract %>% 
    group_by(H1TO1_TRYcig) %>% 
    count(H1TO3_REGcig)

