# Preprocessing and data cleaning file


library(tidyverse)

# data file directory
directory = "C:/Users/Pixy/NYCDSA/Project1_Shiny/ICPSR_21600-V21/ICPSR_21600/"
i = c(1:31)

# load original source .rda data files 
for (x in i) {
    if (x < 10){
        folder = paste0("DS000",x,"/")
        fname = paste0("21600-000",x,"-Data.rda")
        fpath = paste0(directory, folder, fname)
        load(fpath)
    } else if (x >= 10){
        folder = paste0("DS00",x,"/")
        fname = paste0("21600-00",x,"-Data.rda")
        fpath = paste0(directory, folder, fname)
        if (file.exists(fpath) == TRUE) {
            load(fpath)
        } else x = x + 1
    }
}

## Read rds files into workspace

llist <- dir(path = './Add-Health_Data/')
j = length(llist)

for(i in 1:j){
    name = llist[i]
    dfname = substr(name, 1, 9)
    filen = paste0('./Add-Health_Data/', name)
    df <- readRDS(filen, refhook = NULL)
    assign(dfname, df)
}


# View and select relevant columns from data frames. 
# Save to new file for retrieval.

View(da21600.0001)
df_DS0001 <- da21600.0001 %>% 
        select(AID, IYEAR, BIO_SEX, SMP01, SMP03, H1GI1M, H1GI1Y, H1GI4, H1GI8, 
               matches("^(H1GI6|H1WP|H1TO|H1DS|H1PR|H1RE|H1EE)"))
saveRDS(df_DS0001, file = './Add-Health_Data/df_DS0001.rds', compress = FALSE)


# View and select relevant columns from data frames. 
# Save to new file for analysis.

View(da21600.0005)
df_DS0005 <- da21600.0005 %>% 
    select(AID, IYEAR2, H2GI1M, H2GI1Y, matches("^(H2WP|H2TO|H2DS|H2PR|H2RE|H2EE)"))
saveRDS(df_DS0005, file = './Add-Health_Data/df_DS0005.rds', compress = FALSE)


# View and select relevant columns from data frames. 
# Save to new file for analysis.

View(da21600.0008)
df_DS0008 <- da21600.0008 %>% 
    select(AID, IYEAR3, matches("^(H3WP|H3ED|H3LM|H3MR|H3DS|H3TO|H3RE)"))
saveRDS(df_DS0008, file = './Add-Health_Data/df_DS0008.rds', compress = FALSE)


# View and select relevant columns from data frames. 
# Save to new file for analysis.

View(da21600.0022)
df_DS0022 <- da21600.0022 %>% 
    select(AID, IYEAR4, matches("^(H4WP|H4ED|H4LM|H4RE|H4TO)"))
saveRDS(df_DS0022, file = './Add-Health_Data/df_DS0022.rds', compress = FALSE)


# View and select relevant columns from data frames. 
# Save to new file for analysis.

View(da21600.0017)
df_DS0017 <- da21600.0017 %>% 
    select(AID, EREXITST, ERGRADY)
saveRDS(df_DS0017, file = './Add-Health_Data/df_DS0017.rds', compress = FALSE)


df_DS_test <- inner_join(x = df_DS0001, y = df_DS0005)
df_DS_test <- inner_join(df_DS_test, df_DS0008)
df_DS_test <- inner_join(df_DS_test, df_DS0022)
saveRDS(df_DS_test, file = './Add-Health_Data/df_DS_combined.rds', compress = FALSE)
view(df_DS_test)
df_DS_test2 <- df_DS_test %>% inner_join(., df_DS0002) %>% 
    inner_join(., df_DS0004) %>% inner_join(., df_DS0006) %>% inner_join(., df_DS0007) %>% 
    inner_join(., df_DS0017) %>% inner_join(., df_DS0021) %>% inner_join(., df_DS0031)
saveRDS(df_DS_comb_full, file = './Add-Health_Data/df_DS_combined_full.rds', compress = FALSE)
view(df_DS_test2)


df_DS_comb_full <- readRDS('./Add-Health_Data/df_DS_combined_full.rds')
view(df_DS_comb_full)

df_DS_comb_full <- df_DS_comb_full %>% 
        rename(H1GI4_H = H1GI4, H1GI6A_W = H1GI6A, H1GI6B_BL = H1GI6B, H1GI6C_NAM = H1GI6C, 
               H1GI6D_AS = H1GI6D, H1GI6E_OTH = H1GI6E, H1GI8_RACE = H1GI8)    

str(df_DS_comb_full$H1GI8_RACE)


df_DS_comb_full <- df_DS_comb_full %>% 
    mutate(H1GI8_RACE = )

race <- as.character(df_DS_comb_full$H1GI8_RACE)

obs = length(race)
summary(race)
levels(race)



is.na(race[1])
print(df_DS_comb_full$H1GI6A_W[1] == '(1) (1) Marked')
race[1] = '(2) Black/African American'
df_DS_comb_full$H1GI6A_W[i] == '(1) (1) Marked'
head(race)

race
race[i] = 'Manual Check'


for (i in 1:obs){
    if (!is.na(race[i])) { i <- i + 1
            } else if (df_DS_comb_full$H1GI6A_W[i] == '(1) (1) Marked'){
                race[i] <- '(1) White'
            } else if (df_DS_comb_full$H1GI6B_BL[i] == '(1) (1) Marked'){
                race[i] <- '(2) Black/African American'
            } else if (df_DS_comb_full$H1GI6C_NAM[i] == '(1) (1) Marked'){
                race[i] <- '(3) American Indian/Native American'
            } else if (df_DS_comb_full$H1GI6D_AS[i] == '(1) (1) Marked'){
                race[i] <- '(4) Asian/Pacific Islander'
            } else if (df_DS_comb_full$H1GI6E_OTH[i] == '(1) (1) Marked') {
                race[i] <- '(5) Other'
            } else {
                race[i] <- 'Manual Check'
    }
}

race <- gsub("(1) (1) White", "(1) White", race, fixed = TRUE) 
race <- gsub("(2) (2) Black/African American", "(2) Black/African American", race, fixed = TRUE)
race <- gsub("(3) (3) American Indian/Native American", "(3) American Indian/Native American", race, fixed = TRUE) 
race <- gsub("(4) (4) Asian/Pacific Islander", "(4) Asian/Pacific Islander", race, fixed = TRUE)
race <- gsub("(5) (5) Other", "(5) Other", race, fixed = TRUE)

summary(df_DS_comb_full$H1GI8_RACE)
class(df_DS_comb_full$H1GI8_RACE)
levels(df_DS_comb_full$H1GI8_RACE)

summary(race)

race_fact <- factor(race)
summary(race_fact)


df_DS_comb_full$H1GI8_RACE <- factor(race_fact)
saveRDS(df_DS_comb_full, file = './Add-Health_Data/df_DS_combined_full.rds', compress = FALSE)

df_DS_comb_full <- arrange(df_DS_comb_full, desc(df_DS_comb_full$H1GI8_RACE))
view(df_DS_comb_full)


for (i in 1:obs){
    if (df_DS_comb_full$H1GI8_RACE[i] != "Manual Check") { i <- i + 1
    } else if (df_DS_comb_full$H1GI6D_AS[i] == "(1) (1) Marked (If Asian/Pacific Islander among R's answer ask Q"){
        df_DS_comb_full$H1GI8_RACE[i] <- '(4) Asian/Pacific Islander'
    } else if (df_DS_comb_full$H1GI4_H[i] == "(1) (1) Yes") {
        df_DS_comb_full$H1GI8_RACE[i] <- '(5) Other'
    } else {
        df_DS_comb_full$H1GI8_RACE[i] <- 'Manual Check'
    }
}

df_DS_comb_full$H1GI8_RACE[i] <- "(5) Other"
df_DS_comb_full <- arrange(df_DS_comb_full, desc(df_DS_comb_full$AID))

# cut IDs 94577924, 98546321
class(df_DS_comb_full$AID)
df_DS_comb_full <- filter(df_DS_comb_full, df_DS_comb_full$AID != '94577924', .preserve = T)
df_DS_comb_full <- filter(df_DS_comb_full, df_DS_comb_full$AID != '98546321', .preserve = T)

summary(df_DS_comb_full$H1GI8_RACE)
df_DS_comb_full$H1GI8_RACE <- fct_drop(df_DS_comb_full$H1GI8_RACE)

saveRDS(df_DS_comb_full, file = './Add-Health_Data/df_DS_combined_full.rds', compress = FALSE)


df_DS_comb_full <- df_DS_comb_full %>% 
    rename('H1TO1_TRYcig' = 'H1TO1',
           'H1TO2_AGEcig' = 'H1TO2',
           'H1TO3_REGcig' = 'H1TO3')

df_DS_comb_full <- df_DS_comb_full %>% 
    rename('H1TO6M_QUITM' = 'H1TO6M',
           'H1TO6Y_QUITY' = 'H1TO6Y',
           'H1TO8_TRYquit6M' = 'H1TO8',
           'H1TO9_FRNDS' = 'H1TO9',
           'DROP1' = 'H1TO10',
           'DROP2' = 'H1TO11',
           'H1TO12_TRYalc' = 'H1TO12',
           'H1TO13_alc' = 'H1TO13',
           'H1TO20_PARtrbl' = 'H1TO20',
           'H1TO21_SCHtrbl' = 'H1TO21',
           'H1TO22_FRDtrbl' = 'H1TO22',
           'H1TO23_SOtrbl' = 'H1TO23',
           'H1TO24_REGRET' = 'H1TO24',
           'H1TO30_TRYmj' = 'H1TO30',
           'H1TO34_TRYcoc' = 'H1TO34',
           'H1TO37_TRYglue' = 'H1TO37',
           'H1TO40_TRYilleg' = 'H1TO40',
           'H1TO40_TRYinj' = 'H1TO43',
           'H1TO40_TRYinj' = 'H1TO43',
           'H1TO50_AVAILcig' = 'H1TO50',
           'H1TO51_AVAILalc' = 'H1TO51',
           'H1TO52_AVAILill' = 'H1TO52',
           'DROP3' = 'H1TO53',
           'DROP4' = 'H1TO54A',
           'DROP5' = 'H1TO54B',
           'DROP6' = 'H1TO54C',
           'DROP7' = 'H1TO54D',
    )

df_DS_comb_full <- df_DS_comb_full %>% 
    select(!matches("^[DROP\\d]"))

saveRDS(df_DS_combined_full, file = './Add-Health_Data/df_DS_combined_full.rds', compress = FALSE)

levels(df_DS_combined_full$H1GI8_RACE)

df_DS_combined_full <- df_DS_combined_full %>% 
    mutate(H1GI8_RACE = fct_recode(H1GI8_RACE,
           "White" = "(1) White",
           "Black/African American" = "(2) Black/African American",
           "American Indian/Native American" = "(3) American Indian/Native American",
           "Asian/Pacific Islander" = "(4) Asian/Pacific Islander",
           "Other" = "(5) Other"
           ))


class(df_DS_combined_full$IYEAR)
levels(df_DS_combined_full$IYEAR)


df_DS_combined_full <- df_DS_combined_full %>% 
    mutate(H1GI8_RACE = fct_recode(H1GI8_RACE,
                                   "White" = "(1) White",
                                   "Black/African American" = "(2) Black/African American",
                                   "American Indian/Native American" = "(3) American Indian/Native American",
                                   "Asian/Pacific Islander" = "(4) Asian/Pacific Islander",
                                   "Other" = "(5) Other"
    ))


mod_fct_levels = function(add_var) {
    
    lev <- c(levels(add_var))
    
    qf <- as.character(str_locate_all(lev, "\\) ")) 
    qf <- tail(qf, 1) 
    qf <- unlist(strsplit(qf, "\\s|\\)"))
    qf <- as.integer(tail(qf, 1))
    
    new_lev <- substr(lev, start = qf, stop = 1000)
    new_lev <- str_trim(new_lev, side = 'both')
    
    return(new_lev)
}




mod_factors = function(add_var){
    new_lev <- mod_fct_levels(add_var) 
    old_lev <- levels(add_var)
    names(old_lev) = new_lev
    
    add_var <- fct_recode(add_var, !!!old_lev)
    
}



df_DS_pract$H1GI1M <- mod_factors(df_DS_pract$H1GI1M)


levels(df_DS_pract$H1GI1M)
str(df_DS_pract$IYEAR)


df_DS_pract$H1GI1Y <- df_DS_pract$H1GI1Y %>% 
    as.character(.) %>% substr(., 1, 4) %>% 
    as.numeric(.)


# library(prettyR)
# lbls <- sort(levels(df_DS_pract$H1GI4_H))
# lbls <- (sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
# df_DS_pract$H1GI4_H <- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", df_DS_pract$H1GI4_H))
# df_DS_pract$H1GI4_H <- add.value.labels(df_DS_pract$H1GI4_H, lbls)


# View and select relevant columns from data frames. 
# Save to new file for analysis.

df_DS_Wo_WP <- select(df_DS_combined_full, -matches("^(H\\dWP)")) # select everything but "relations w/ parents"
df_DS_Wo_h1gi6 <- select(df_DS_combined_full, -matches("^(H1GI6)")) # select everything but "full race data" (already aggregated)




df_DS_Wo_h1gi6 -> df_DS_combined_full
saveRDS(df_DS_combined_full, file = './Add-Health_Data/df_DS_combined_full.rds', compress = FALSE)
df_DS_combined_full <- readRDS(file = './Add-Health_Data/df_DS_combined_full.rds')
