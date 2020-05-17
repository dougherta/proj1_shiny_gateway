library(tidyverse)
library(rpart)


view(select(df_DS_final, contains('TO')))

df_DS_final %>% 
    group_by(H1GI8_RACE, BIO_SEX) %>% 
    filter(!is.na(H1TO1_TRYcig)) %>% 
    count(H1TO1_TRYcig)






df_DS_final %>% 
    group_by(H1TO1_TRYcig) %>% 
    count(H1TO3_REGcig)

df_DS_final %>% 
    group_by(H1TO12_TRYalc) %>% 
    count(H1TO15)

df_DS_final %>% 
    group_by(H1TO3_REGcig) %>% 
    count(H1PR1)

df_DS_final %>% 
    group_by(H1TO1_TRYcig) %>% 
    count(H1TO3_REGcig)

summary(df_DS_final)
summary(df_DS_final$H1GI8_RACE)


df_DS_final %>%
    group_by(BIO_SEX, H1GI8_RACE ) %>%
    # group_by( H1TO30_TRYmj ) %>% 
    
    ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
    ggtitle("Self-report: Age for trying marijuana") +
    xlab("Age") +
    ylab("Count") +
    
    geom_bar() + coord_flip() + 
    scale_fill_brewer(palette= "Blues") +
        theme_bw() 


