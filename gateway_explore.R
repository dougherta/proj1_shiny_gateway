library(tidyverse)

summary(df_DS_comb_full$BIO_SEX)
summary(df_DS_comb_full$H1GI8_RACE)

view(select(df_DS_comb_full, contains('TO')))

df_DS_comb_full %>% 
    group_by(., BIO_SEX, H1GI8_RACE) %>% 
    count(., H1TO1_TRYcig, H1TO2_AGEcig)

summary(df_DS_comb_full$H1TO1_TRYcig)
summary(df_DS_comb_full$H1PR1)
summary(df_DS_comb_full$H1RE1)
summary(df_DS_comb_full$H1EE1)
class(df_DS_comb_full$H1EE1)
df_DS_comb_full$H1EE1
summary(df_DS_comb_full$H1)
