source('SRAA.R')
######## Summary Statistics & Graphs ###########
library(ggplot2)
library(ggthemes)
library(scales)
library(Hmisc)
library(tidyr)

#Count Attempts per Pitcher, Catcher, Batter
Att.PIT = count(SB.pitchfx, PIT_ID)
Att.CAT= count(SB.pitchfx, POS2_FLD_ID)
Att.RUN = count(SB.pitchfx, LeadRunner)

# Compare Old and New Methods as well as their Z-scores
CatcherMetrics = inner_join(PSRAA.CAT, SRAA.CAT) %>%
    left_join(., ids, by = c('POS2_FLD_ID'='key_retro')) %>%
    left_join(., Att.CAT) %>%
    mutate(SRAAm = (SRAA - wtd.mean(SRAA,n))/sqrt(wtd.var(SRAA,n)),
           PSRAAm = (PSRAA - wtd.mean(PSRAA,n))/sqrt(wtd.var(PSRAA,n))) %>%
    mutate(Changem = PSRAAm-SRAAm, Change = PSRAA-SRAA)

RunnerMetrics = inner_join(PSRAA.RUN, SRAA.RUN) %>%
    left_join(., ids, by = c('LeadRunner'='key_retro')) %>%
    left_join(., Att.RUN) %>%
    mutate(SRAAm = (SRAA - wtd.mean(SRAA,n))/sqrt(wtd.var(SRAA,n)),
           PSRAAm = (PSRAA - wtd.mean(PSRAA,n))/sqrt(wtd.var(PSRAA,n))) %>%
    mutate(Changem = PSRAAm-SRAAm, Change = PSRAA-SRAA)

PitcherMetrics = inner_join(PSRAA.PIT, SRAA.PIT) %>%
    left_join(., ids, by = c('PIT_ID'='key_retro')) %>%
    left_join(., Att.PIT) %>%
    mutate(SRAAm = (SRAA - wtd.mean(SRAA,n))/sqrt(wtd.var(SRAA,n)),
           PSRAAm = (PSRAA - wtd.mean(PSRAA,n))/sqrt(wtd.var(PSRAA,n))) %>%
    mutate(Changem = PSRAAm-SRAAm,Change = PSRAA-SRAA)

#Add fitted probabilities for use later
SB.pitchfx = mutate(SB.pitchfx, pred_wrand = predict(PSRAA.glmer, SB.pitchfx, type='response'),
                    pred = predict(PSRAA.glmer, SB.pitchfx, type='response', re.form = ~0))

#SB vs Pitch Speed
group_by(SB.pitchfx, start_speed = floor(start_speed/10)*10) %>%
    summarise(SB = mean(LeadSB),n())

ggplot(SB.pitchfx) + 
    stat_smooth(aes(x = start_speed, y =LeadSB)) +
    theme_fivethirtyeight()+
    scale_y_continuous(labels = percent, name = 'SB Success Rate') +
    scale_x_continuous(labels = function(x) paste(x, 'mph'), name = '\nPitch Speed')+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16))+
    ggtitle('Smoothed SB Success Rate vs. Pitch Speed')


#SB by Pitch Location

#Predicted gam probabilities
#Methodology from https://gist.github.com/bayesball
x = seq(-4.7,4.7,length.out = 100)
z = seq(-1.75, 6,length.out =100)
zone = expand.grid(px = x, pz = z, BAT_HAND_CD = c('L','R'))
pSB = predict(loc_mod ,zone , type='response')
zone = mutate(zone, pSB = pSB, BAT_HAND_CD = ifelse(BAT_HAND_CD=='R', 'Right Handed','Left Handed'))

#17 inches + radius of the ball
kzone_side = ((1.57*2 + 17) / 12) / 2
kzone_bot = mean(SB.pitchfx$sz_bot)
kzone_top = mean(SB.pitchfx$sz_top)

strikzone <- data.frame(
    x=c(kzone_side,kzone_side,-kzone_side,-kzone_side,kzone_side ),
    y=c(kzone_bot , kzone_top,kzone_top, kzone_bot ,kzone_bot )
)

#Plot pitch location smoothed probability
ggplot(zone, aes(x = px, y = pz)) + 
    geom_tile(aes(fill = pSB)) +
    facet_wrap(~BAT_HAND_CD)+
    scale_fill_distiller(palette='Spectral', label = percent, name = 'SB Probability\n') + 
    scale_x_continuous(name = "Catcher's Point of View")+
    scale_y_continuous(name = "")+
    geom_path(data = strikzone , aes(x, y), size = 1) +
    geom_point(data = filter(SB.pitchfx, pz<=6) %>%
                   mutate(BAT_HAND_CD = ifelse(BAT_HAND_CD=='R','Right Handed','Left Handed')), aes(px, pz), alpha = .05) +
    geom_abline(slope = 0, intercept = 0, size = 1) +
    annotate('text', label = 'Ground', x = -3.5, y = .2, size = 4) +
    theme_fivethirtyeight()+
    theme(legend.position = 'right',legend.direction='vertical', text = element_text(size = 16),panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_text())+ 
    ggtitle('Smoothed SB Probability', subtitle = "by Pitch Location and Batter Handedness")
    

#Description Table
group_by(SB.pitchfx, des_short) %>% 
    summarise(SB_Prob = mean(LeadSB)) %>%
    write.csv('SB Description Table.csv', row.names=F)


#SRAA vs pSRAA distribution
PlayerMetricsCompare = bind_rows(select(CatcherMetrics, Name, SRAA, PSRAA, Attempts = n) %>% mutate(Position='Catcher'),
                          select(PitcherMetrics, Name, SRAA, PSRAA, Attempts = n) %>% mutate(Position='Pitcher'),
                          select(RunnerMetrics, Name, SRAA, PSRAA, Attempts = n) %>% mutate(Position='Runner')) 

PlayerMetrics =  PlayerMetricsCompare %>%   gather(Metric, Value, -Name, -Position,-Attempts)

ggplot(PlayerMetrics)+
    geom_density(aes(x = Value, fill = Metric), alpha = .3)+
    facet_wrap(~Position)+
    theme_fivethirtyeight()+
    scale_x_continuous(label = percent, name = "Rate Above Average")+
    scale_y_continuous( name = "Density")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16))+
    ggtitle('SRAA and pSRAA Distributions by Position')

#SRAA vs pSRAA change
PlayerMetricsCompare = mutate(PlayerMetricsCompare, Change = PSRAA-SRAA)
ggplot(PlayerMetricsCompare )+
    geom_abline(slope = 1, intercept = 0, lty = 5, alpha = .5)+
    geom_point(aes(x = SRAA, y = PSRAA, colour = Change, size =Attempts), alpha =.8) +
    scale_colour_distiller(palette = 'RdBu', label = percent, name = 'Change ')+
    facet_wrap(~Position)+
    theme_fivethirtyeight()+
    scale_x_continuous(label = percent, name = "\nSRAA")+
    scale_y_continuous(label = percent, name = "pSRAA")+
    theme(text = element_text(size = 16),
          axis.title = element_text(size = 16),
          legend.key.width = unit(1, "cm"))+
    ggtitle('SRAA vs. pSRAA by Position')


#Run Value
# 2016 CS/SB Run Values from http://www.fangraphs.com/guts.aspx?type=cn
# SB = .2
# CS = -.410
PlayerMetricsCompare = PlayerMetricsCompare %>% 
    mutate(pSRrAA = PSRAA*(.2-(-.410))*Attempts)


#Top 10 
CTop10 = PlayerMetricsCompare %>%
    filter(Position=='Catcher') %>%
    arrange(pSRrAA) %>%
    filter(row_number()<=10) %>%
    select( Catcher = Name, pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA), pSRrAA = round(pSRrAA, 2))
PTop10 =PlayerMetricsCompare %>%
    filter(Position=='Pitcher') %>%
    arrange(pSRrAA) %>%
    filter(row_number()<=10) %>%
    select( Pitcher = Name,pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA), pSRrAA = round(pSRrAA, 2))
RTop10 = PlayerMetricsCompare %>%
    filter(Position=='Runner') %>%
    arrange(-pSRrAA) %>%
    filter(row_number()<=10) %>%
    select( Runner = Name, pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA), pSRrAA = round(pSRrAA, 2))

CBot10 = PlayerMetricsCompare %>%
    filter(Position=='Catcher') %>%
    arrange(pSRrAA) %>%
    filter(row_number()>=n()-9) %>%
    select( Catcher = Name, pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA),pSRrAA = round(pSRrAA, 2))
PBot10 =PlayerMetricsCompare %>%
    filter(Position=='Pitcher') %>%
    arrange(pSRrAA) %>%
    filter(row_number()>=n()-9) %>%
    select( Pitcher = Name, pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA),pSRrAA = round(pSRrAA, 2))
RBot10 = PlayerMetricsCompare %>%
    filter(Position=='Runner') %>%
    arrange(-pSRrAA) %>%
    filter(row_number()>=n()-9) %>%
    select( Runner = Name, pSRAA = PSRAA,pSRrAA) %>%
    mutate( pSRAA = percent_format()( pSRAA), pSRrAA = round(pSRrAA, 2))

FinalTop10 = bind_cols(CTop10, PTop10, RTop10)
FinalBottom10 = bind_cols(CBot10, PBot10, RBot10)
write.csv(FinalTop10, 'data/Top10.csv', row.names = F)
write.csv(FinalBottom10, 'data/Bottom10.csv', row.names = F)


