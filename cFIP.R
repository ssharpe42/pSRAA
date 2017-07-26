#Baseball Data
setwd('/Users/Sam/Desktop/Baseball Data/')
library(dplyr)
library(lme4)
library(dbConnect)
library(Hmisc)

dbRetro=dbConnect(MySQL(),user="root",
                  host="localhost",
                  dbname="retrosheet",
                  password="root",
                  unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

#Retrosheet event files
Season = 2014
pbp = fetch(dbSendQuery(dbRetro, paste0("select * from events_bck where YEAR_ID=",Season)), n = -1)
pbp[, grepl('_FL$', names(pbp))] = lapply(pbp[, grepl('_FL$', names(pbp))], as.logical)
IP = group_by(pbp, PIT_ID) %>% summarise(IP = sum(EVENT_OUTS_CT)/3) 

#Game Logs
game_logs = fetch(dbSendQuery(dbRetro, paste0("select * from games_bck where YEAR_ID=",Season)),n = -1) 

#Join Home Plate Ump & Park Info
pbp = left_join(pbp, 
                    game_logs %>% select(GAME_ID, BASE4_UMP_ID, PARK_ID)) %>%
    #filter to bat events
    filter(BAT_EVENT_FL) %>%
    #Create event indicators
    mutate(HR = as.integer(EVENT_CD==23),
           BB = as.integer(EVENT_CD==14),
           HBP = as.integer(EVENT_CD==16),
           K = as.integer(EVENT_CD==3))
    
BB.glmer <- glmer(BB ~ BAT_HOME_ID + BAT_HAND_CD*PIT_HAND_CD + PARK_ID + 
                           (1|BAT_ID) + (1|PIT_ID) + (1|POS2_FLD_ID) + (1|BASE4_UMP_ID), 
                       data=pbp, family=binomial(link='probit'), nAGQ=0)

HR.glmer <- glmer(HR ~ BAT_HAND_CD*PIT_HAND_CD + PARK_ID + 
                           (1|BAT_ID) + (1|PIT_ID) , 
                       data=pbp, family=binomial(link='probit'), nAGQ=0)

HBP.glmer <- glmer(HBP ~ BAT_HOME_ID + BAT_HAND_CD*PIT_HAND_CD + PARK_ID + 
                           (1|BAT_ID) + (1|PIT_ID) + (1|POS2_FLD_ID) , 
                       data=pbp, family=binomial(link='probit'), nAGQ=0)

K.glmer <- glmer(K ~ BAT_HOME_ID + BAT_HAND_CD*PIT_HAND_CD + PARK_ID + 
                            (1|BAT_ID) + (1|PIT_ID) + (1|POS2_FLD_ID) , 
                        data=pbp, family=binomial(link='probit'), nAGQ=0)

saveRDS(BB.glmer,paste0('/Users/Sam/Desktop/Projects/cFIP/BB.glmer.',Season,'.RDS'))
saveRDS(HR.glmer,paste0('/Users/Sam/Desktop/Projects/cFIP/HR.glmer.',Season,'.RDS'))
saveRDS(HBP.glmer,paste0('/Users/Sam/Desktop/Projects/cFIP/HBP.glmer.',Season,'.RDS'))
saveRDS(K.glmer,paste0('/Users/Sam/Desktop/Projects/cFIP/K.glmer.',Season,'.RDS'))

BB.glmer=readRDS(paste0('/Users/Sam/Desktop/Projects/cFIP/BB.glmer.',Season,'.RDS'))
HR.glmer=readRDS(paste0('/Users/Sam/Desktop/Projects/cFIP/HR.glmer.',Season,'.RDS'))
HBP.glmer=readRDS(paste0('/Users/Sam/Desktop/Projects/cFIP/HBP.glmer.',Season,'.RDS'))
K.glmer=readRDS(paste0('/Users/Sam/Desktop/Projects/cFIP/K.glmer.',Season,'.RDS'))

# pbp = mutate(pbp,
#     BB.int = qnorm(BB.glmer@resp$mu),
#     HBP.int = qnorm(HBP.glmer@resp$mu),
#     HR.int = qnorm(HR.glmer@resp$mu),
#     K.int = qnorm(K.glmer@resp$mu)
# )
# group_by(pbp, PIT_ID) %>% 
#     summarise_each(funs(mean), BB.int, HBP.int, HR.int, K.int)%>%
#     left_join(., data.frame(PIT_ID = rownames(ranef(BB.glmer)$PIT_ID), 
#                             BB.r = ranef(BB.glmer)$PIT_ID[,1],
#                             HBP.r = ranef(HBP.glmer)$PIT_ID[,1], 
#                             HR.r = ranef(HR.glmer)$PIT_ID[,1], 
#                             K.r = ranef(K.glmer)$PIT_ID[,1])) %>%
#     ungroup %>%
#     mutate(BB = pnorm(BB.int + BB.r)- pnorm(BB.int),
#            HBP = pnorm(HBP.int + HBP.r)- pnorm(HBP.int),
#            HR = pnorm(HR.int + HR.r)- pnorm(HR.int),
#            K = pnorm(K.int + K.r)- pnorm(K.int),
#            cFIP = 13*HR + 3*(HBP+BB) - 2*K,
#            cFIPm = ((cFIP - mean(cFIP))/sd(cFIP))*15 + 100)
# 
# BB.int = mean(qnorm(BB.glmer@resp$mu))
# HBP.int = mean(qnorm(HBP.glmer@resp$mu))
# HR.int = mean(qnorm(HR.glmer@resp$mu))
# K.int = mean(qnorm(K.glmer@resp$mu))

BB.int = summary(BB.glmer)$coefficients['(Intercept)','Estimate'] 
HBP.int = summary(HBP.glmer)$coefficients['(Intercept)','Estimate']
HR.int = summary(HR.glmer)$coefficients['(Intercept)','Estimate']
K.int = summary(K.glmer)$coefficients['(Intercept)','Estimate']

PitcherEffects = data.frame(BB = pnorm(ranef(BB.glmer)$PIT_ID[,1] + BB.int) - pnorm(BB.int),
                        HBP = pnorm(ranef(HBP.glmer)$PIT_ID[,1]+ HBP.int) - pnorm(HBP.int),
                        HR = pnorm(ranef(HR.glmer)$PIT_ID[,1]+ HR.int) - pnorm(HR.int),
                        K = pnorm(ranef(K.glmer)$PIT_ID[,1]+ K.int) - pnorm(K.int),
                        PIT_ID = rownames(ranef(BB.glmer)$PIT_ID)) %>%
    left_join(., IP) %>%
    mutate(
           cFIP = 13*HR + 3*(HBP+BB) - 2*K,
           cFIPm = ((cFIP - wtd.mean(cFIP, IP))/sqrt(wtd.var(cFIP, IP)))*15 + 100)

saveRDS(PitcherEffects %>% 
            select(cFIP= cFIPm, PIT_ID) %>%
            mutate(YEAR_ID= Season)
        , paste0('/Users/Sam/Desktop/Projects/cFIP/cFIP',Season,'.RDS'))
