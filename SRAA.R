library(DBI)
library(dplyr)
library(baseballr)
library(pitchRx)
library(lme4)
library(mgcv)
library(dbConnect)
#scrape(start = "2015-01-01", end = "2015-12-01", connect = db$con)

db <- src_sqlite("/Users/Sam/Desktop/Baseball Data/pitchRx.sqlite3")
# run = tbl(db, 'runner')
# run_date <- mutate(run, date = substr(gameday_link, 15L, -10L))
# compute(run_date, name = 'run_date', temporary = FALSE)
# pitch = tbl(db, 'pitch')
# pitch_date <- mutate(pitch, date = substr(gameday_link, 15L, -10L))
# compute(pitch_date, name = 'pitch_date', temporary = FALSE)
# action = tbl(db, 'action')
# action_date <- mutate(action, date = substr(gameday_link, 15L, -10L))
# compute(action_date, name = 'action_date', temporary = FALSE)
# atbat = tbl(db, 'atbat')
# atbat_date <- mutate(atbat, date = substr(gameday_link, 15L, -10L))
# compute(atbat_date, name = 'atbat_date', temporary = FALSE)

# dbSendQuery(db$con, 'CREATE INDEX date_idx ON run_date(date)')
# dbSendQuery(db$con, 'CREATE INDEX date_idx2 ON pitch_date(date)')
# dbSendQuery(db$con, 'CREATE INDEX date_idx3 ON action_date(date)')
# dbSendQuery(db$con, 'CREATE INDEX date_idx4 ON atbat_date(date)')
# dbSendQuery(db$con, 'CREATE INDEX event_run_idx ON run_date(event)')
# dbSendQuery(db$con, 'CREATE INDEX pitch_idx ON pitch(gameday_link, num)')
# dbSendQuery(db$con, 'CREATE INDEX run_idx ON run_date(gameday_link, num)')
# dbSendQuery(db$con, 'CREATE INDEX action_idx ON action_date(gameday_link, num)')
# dbSendQuery(db$con, 'CREATE INDEX atbat_idx ON atbat_date(gameday_link, num)')

setwd("~/Desktop/Projects/pSRAA")
#Retrosheet event files
dbRetro=dbConnect(MySQL(),user="root",
                  host="localhost",
                  dbname="retrosheet",
                  password="root",
                  unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")

#Retrosheet event files
Season = 2016
retro = fetch(dbSendQuery(dbRetro, paste0("select * from events_bck where YEAR_ID=",Season)), n = -1)
retro[, grepl('_FL$', names(retro))] = lapply(retro[, grepl('_FL$', names(retro))], as.logical)

#Game Logs
game_logs = fetch(dbSendQuery(dbRetro, paste0("select * from games_bck where YEAR_ID=",Season)),n = -1) 

#Id Crosswalk 
ids =select(read.csv('https://raw.githubusercontent.com/chadwickbureau/register/master/data/people.csv'),
       key_mlbam, key_retro, name_last ,name_first ) %>%
       mutate(Name = paste(name_first, name_last)) %>%
       select(-name_last, -name_first)%>%
       filter(!(is.na(key_mlbam)|key_mlbam==''|is.na(key_retro)|key_retro==''))
id_cross = select(ids,-Name )

#Retrosheed with extra info and MLB ids
retro_sb = mutate(retro, 
               SB = RUN1_SB_FL+RUN2_SB_FL+RUN3_SB_FL, 
               CS = RUN1_CS_FL + RUN2_CS_FL + RUN3_CS_FL) %>%
    filter(SB>0|CS>0) %>%
    mutate(LeadRunner = ifelse(BASE3_RUN_ID!='', BASE3_RUN_ID,
                               ifelse(BASE2_RUN_ID!='',BASE2_RUN_ID,BASE1_RUN_ID)),
           LeadSB = ifelse(BASE3_RUN_ID!='', RUN3_SB_FL,
                           ifelse(BASE2_RUN_ID!='',RUN2_SB_FL,RUN1_SB_FL)),
           LeadCS = ifelse(BASE3_RUN_ID!='', RUN3_CS_FL,
                           ifelse(BASE2_RUN_ID!='',RUN2_CS_FL,RUN1_CS_FL)),
           LeadSB = as.integer(LeadSB),
           EventPitch = substr(PITCH_SEQ_TX, nchar(PITCH_SEQ_TX), nchar(PITCH_SEQ_TX)),
           Pitches = gsub('[.>N123+*]','',PITCH_SEQ_TX),
           pitch_no = nchar(Pitches)-1,
           Count = paste0(BALLS_CT,'-',STRIKES_CT),
           Bases = paste0(as.integer(BASE1_RUN_ID!=''),as.integer(BASE2_RUN_ID!=''),as.integer(BASE3_RUN_ID!='')),
           LeadBase = ifelse(BASE3_RUN_ID!='','Third',
                             ifelse(BASE2_RUN_ID!='','Second','First')),
           Season = substr(GAME_ID, 4, 7),
           gameday_link = paste0('gid_',substr(GAME_ID, 4,7),'_',substr(GAME_ID, 8,9),'_',substr(GAME_ID, 10,11),'_',
                                 tolower(AWAY_TEAM_ID),'mlb', '_',tolower(substr(GAME_ID, 1,3)),'mlb','_', pmax(as.integer(substr(GAME_ID,12,12)),1))) %>%
    left_join(., rename(id_cross, PIT_ID = key_retro, pitcher = key_mlbam)) %>%
    left_join(., rename(id_cross, BAT_ID = key_retro, batter = key_mlbam))%>%
    left_join(., rename(id_cross, BASE1_RUN_ID = key_retro, on_1b = key_mlbam))%>%
    left_join(., rename(id_cross, BASE2_RUN_ID = key_retro, on_2b = key_mlbam))%>%
    left_join(., rename(id_cross, BASE3_RUN_ID = key_retro, on_3b = key_mlbam)) %>%
    left_join(., game_logs %>% select(GAME_ID, PARK_ID)) %>%
    mutate(ID = paste(gameday_link, INN_CT, batter, pitcher,  on_1b ,on_2b , on_3b, Count, pitch_no))


#### Load in Jonathan Judge's cFIP- statistic  ####
cfip = readRDS('data/cFIP2016.RDS')

#### Jonathan Judge, Harry Pavlidis and Dan Turkenkopf's Swipe Rate Above Average (SRAA) ####

#SB/CS where lead runner was involved
SB = retro_sb %>%
    filter(Season == 2016, LeadSB==1|LeadCS==1) %>%
    #Jonathan Judge's cFIP- statistic
    left_join(., select(cfip,PIT_ID, cFIP))  %>%
    mutate(INN_CT = ifelse(INN_CT>9,'Extra',INN_CT))

#SRAA Random Effects model 
SRAA.glmer <- glmer(LeadSB ~ factor(INN_CT) + PARK_ID + cFIP+  
                        (1|PIT_ID) + (1 |POS2_FLD_ID) + (1|LeadRunner), 
                    data=SB, family=binomial(link='probit'), nAGQ=0)


SRAA.int = summary(SRAA.glmer)$coefficients['(Intercept)','Estimate']
cFIP.coef = summary(SRAA.glmer)$coefficients['cFIP','Estimate']
null = SRAA.int + median(SB$cFIP)*cFIP.coef

#Calculate Pitcher SRAA
SRAA.PIT = data.frame(SRAA = pnorm(ranef(SRAA.glmer)$PIT_ID[,1] + null) - pnorm(null) )%>%
    mutate(PIT_ID = rownames(ranef(SRAA.glmer)$PIT_ID))

#Calculate Catcher SRAA
SRAA.CAT= data.frame(SRAA = pnorm(ranef(SRAA.glmer)$POS2_FLD_ID[,1]+ SRAA.int) - pnorm(SRAA.int)) %>%
    mutate(POS2_FLD_ID = rownames(ranef(SRAA.glmer)$POS2_FLD_ID))

#Calculate Runner SRAA
SRAA.RUN= data.frame(SRAA = pnorm(ranef(SRAA.glmer)$LeadRunner[,1]+ SRAA.int) - pnorm(SRAA.int)) %>%
    mutate(LeadRunner = rownames(ranef(SRAA.glmer)$LeadRunner))



#Stolen Base / Caught Stealing Events with a Pitch
#Types of SB/CS to filter out:
#    N - No Pitch
#    1,2,3 - Pickoff Throw
#    '' - Empty
SB.pitch = retro_sb %>%
    filter(Season == 2016, LeadSB==1|LeadCS==1, !EventPitch %in% c('N','1','2','3','')) %>%
    #Jonathan Judge's cFIP- statistic
    left_join(., select(cfip,PIT_ID, cFIP))    %>%
    mutate(INN_CT = ifelse(INN_CT>9,'Extra',INN_CT))

# Gather More Pitch Specific Data from Pitchf/x
PITCH <- collect(filter(tbl(db, 'pitch'),gameday_link %in% SB.pitch$gameday_link),n=Inf) %>%
    mutate(event_num = as.integer(event_num))
ATBAT <- collect(filter(tbl(db, 'atbat'),gameday_link %in% SB.pitch$gameday_link),n=Inf)

# ACTION <-collect(filter(tbl(db, 'action'),gameday_link %in% SBretro$gameday_link),n=Inf) %>%
#     mutate(num = as.integer(num)) %>%
#     filter(event=='Pitching Substitution')


#Full joined Pitchf/x data
PITCHFX = PITCH%>%
    left_join(., ATBAT %>% select(pitcher, batter, o, gameday_link, num))  %>%
    group_by(gameday_link, inning, num) %>%
    arrange(event_num)%>%
    mutate(pitch_no = row_number()) %>% ungroup %>%
    mutate(balls = as.integer(substr(count,1,1)),
           strikes = as.integer(substr(count, 3,3)),
           pitch_no = ifelse(strikes<2, balls+strikes, pitch_no-1),
        ID = paste(gameday_link, inning, batter, pitcher, on_1b ,on_2b , on_3b, count, pitch_no))  


#Non matching events where Pitchf/x has the game
#Usually a pitcher change
#Sometimes SB without a pitch
NoMatch = filter(SB.pitch, !ID %in% PITCHFX$ID)%>% 
    select(ID, Bases,SB, CS) 

#Missing Plays in PITCHFX

#gid_2014_09_27_colmlb_lanmlb_1 12 500743 462985 543148 475100 NA 0-0 0    Steals Third and Scores on Wild Pitch
#gid_2014_09_26_houmlb_nynmlb_1 6 543257 477003 463610 NA NA 0-0 0         Pitcher change from 477003 - Jon Niese

#gid_2015_08_04_bosmlb_nyamlb_1 7 598265 458677 628329 NA NA 0-2 2         Pitcher change from 458677 Justin Wilson
#gid_2015_06_16_minmlb_slnmlb_1 7 435559 608379 456488 NA NA 0-0 0         Pitcher change from 608379 - Michael Wacha

#gid_2016_07_17_milmlb_cinmlb_1 9 457803 519293 458015 571740 NA 0-0 0     Steals Third and Scores on Passed Ball
#gid_2016_07_24_chnmlb_milmlb_1 1 541650 452657 NA 542340 NA 1-1 2         No pitch - Jon Lester held ball
#gid_2016_08_01_milmlb_sdnmlb_1 8 500208 592341 NA 571976 608671 0-0 0     Stole Third then pickle at second and stole home


SB.pitchfx = inner_join(SB.pitch,
                PITCHFX %>% select(des:spin_rate,num, event_num,count, ID), by = 'ID' ) %>%
    mutate(zone = as.factor(zone),
           zone_z = ifelse(pz<=sz_bot + (sz_top - sz_bot)/3,'Bottom Third',
                               ifelse(pz>=sz_bot + (sz_top - sz_bot)/3 &
                                          pz<=sz_bot + 2*(sz_top - sz_bot)/3 , 'Middle Third', 'Top Third')),
           zone_x = ifelse(px <=0, 'RHH Side','LHH Side' ),
           des_short = ifelse(grepl('Blocked|Dirt',des), 'Dirt', 
                              ifelse(grepl('Pitchout',des), 'Pitchout',
                                     ifelse(grepl('Swing|Bunt|Foul', des),'Swinging Strike','Strike/Ball')))) %>%
    filter(!is.na(start_speed))


#Smoothed estimated SB probability based on pitch location
loc_mod = gam(LeadSB ~ s(px, pz,by = as.factor(BAT_HAND_CD)) , family=binomial, data = SB.pitchfx)
#Assign probabilities from locations to all SB attempts
SB.pitchfx = mutate(SB.pitchfx, loc_prob = predict(loc_mod, SB.pitchfx, type = 'response'))


#Current SRAA model
SRAA.pitch.glmer <- glmer(LeadSB ~ factor(INN_CT) + PARK_ID+cFIP+
                              (1|PIT_ID) + (1 |POS2_FLD_ID) + (1|LeadRunner), 
                          data=SB.pitchfx, family=binomial(link='probit'), nAGQ=0)

#New SRAA with pitch context
PSRAA.glmer<- glmer(LeadSB ~ factor(INN_CT)  +start_speed+loc_prob+des_short+LeadBase+
                        (1|PIT_ID) + (1 |POS2_FLD_ID) + (1|LeadRunner), 
                    data=SB.pitchfx, family=binomial(link='probit'), nAGQ=0)

#Calculate Pitcher SRAA (with pitch)
SRAA.int = summary(SRAA.pitch.glmer)$coefficients['(Intercept)','Estimate']
cFIP.coef = summary(SRAA.pitch.glmer)$coefficients['cFIP','Estimate']
null = SRAA.int + median(SB.pitch$cFIP)*cFIP.coef

SRAA.PIT = data.frame(SRAA = pnorm(ranef(SRAA.pitch.glmer)$PIT_ID[,1] + null) - pnorm(null) )%>%
    mutate(PIT_ID = rownames(ranef(SRAA.pitch.glmer)$PIT_ID))

#Calculate Catcher SRAA (with pitch)
SRAA.CAT= data.frame(SRAA = pnorm(ranef(SRAA.pitch.glmer)$POS2_FLD_ID[,1]+ null) - pnorm(null)) %>%
    mutate(POS2_FLD_ID = rownames(ranef(SRAA.pitch.glmer)$POS2_FLD_ID))

#Calculate Runner SRAA (with pitch)
SRAA.RUN= data.frame(SRAA = pnorm(ranef(SRAA.pitch.glmer)$LeadRunner[,1]+ null) - pnorm(null)) %>%
    mutate(LeadRunner = rownames(ranef(SRAA.pitch.glmer)$LeadRunner))

#Calculate Pitcher PSRAA (with pitch)
PSRAA.int = summary(PSRAA.glmer)$coefficients['(Intercept)','Estimate']
speed.coef = summary(PSRAA.glmer)$coefficients['start_speed','Estimate']
loc.coef = summary(PSRAA.glmer)$coefficients['loc_prob','Estimate']

#Calculate null probability
null <- PSRAA.int +  median(SB.pitchfx$start_speed)*speed.coef + median(SB.pitchfx$loc_prob)*loc.coef

#Calculate Pitcher pSRAA
PSRAA.PIT = data.frame(PSRAA = pnorm(ranef(PSRAA.glmer)$PIT_ID[,1] + null) - pnorm(null) )%>%
    mutate(PIT_ID = rownames(ranef(PSRAA.glmer)$PIT_ID))

#Calculate Catcher pSRAA (with pitch)
PSRAA.CAT= data.frame(PSRAA = pnorm(ranef(PSRAA.glmer)$POS2_FLD_ID[,1]+ null) - pnorm(null)) %>%
    mutate(POS2_FLD_ID = rownames(ranef(PSRAA.glmer)$POS2_FLD_ID))

#Calculate Runner PSRAA (with pitch)
PSRAA.RUN= data.frame(PSRAA = pnorm(ranef(PSRAA.glmer)$LeadRunner[,1]+ null) - pnorm(null)) %>%
    mutate(LeadRunner = rownames(ranef(PSRAA.glmer)$LeadRunner))





