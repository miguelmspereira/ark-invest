#Libraries
library(data.table)
library(tidyverse)
`%notin%` <- Negate(`%in%`)


###############################################################################
#Load template
arkk<-fread('ARK_INNOVATION_template.csv') %>% select(fund, company, ticker, prev.weights, final.weights)
arkg<-fread('ARK_GENOMIC_REVOLUTION_template.csv') %>% select(fund, company, ticker, prev.weights, final.weights)

#Read file
arkk.new<-fread('ARK_INNOVATION_ETF_ARKK_HOLDINGS.csv')
arkg.new<-fread('ARK_GENOMIC_REVOLUTION_MULTISECTOR_ETF_ARKG_HOLDINGS.csv')



###############################################################################
###############################---ARKK---######################################
#Search for new companies
arkk.new$company[arkk.new$company %notin% arkk$company]

arkk.new.companies<-arkk.new[arkk.new$company %notin% arkk$company,] %>%
  select(company,`weight(%)`,fund,ticker)
arkk.new.companies$prev.weights<-arkk.new.companies$`weight(%)`
arkk.new.companies$final.weights<-arkk.new.companies$`weight(%)`
arkk.new.companies

#Merge
arkk.merge<-merge(
  arkk.new %>% select(company,`weight(%)`),
  arkk,
  by='company',
  sort=F
)

arkk.merge<-rbind(
  arkk.merge,
  arkk.new.companies
)
arkk.merge$`weight(%)`[which(arkk.merge$company=='PACCAR INC')]<-arkk.new$`weight(%)`[which(arkk.new$company=='PACCAR INC')]
#Getting the new weights
arkk.merge$prev.weights<-arkk.merge$`weight(%)`
arkk.merge$prev.weights[which(arkk.merge$final.weights==0)]<-0

weights<-arkk.merge$prev.weights*100/sum(arkk.merge$prev.weights)

total<-100-0.5*length(which(weights<0.5 & weights>0))

weights5<-weights
weights5[which(weights<0.5 & weights>0)]<-0.5

final.weights<-weights5*total/sum(weights5[-which(weights<0.5 & weights>0)])
final.weights[which(weights<0.5 & weights>0)]<-0.5
sum(final.weights)

rounded.weights<-round(final.weights,1)

if(sum(rounded.weights)>100){
  subtract.length<-(sum(rounded.weights)-100)/0.1
  rounded.weights[1:subtract.length]<-rounded.weights[1:subtract.length]-0.1
}

if(sum(rounded.weights)<100){
  add.length<-(100-sum(rounded.weights))/0.1
  rounded.weights[1:add.length]<-rounded.weights[1:add.length]+0.1
}

#Update data.frame
arkk.final<-arkk.merge
arkk.final$final.weights<-rounded.weights
arkk.final<-arkk.final[order(arkk.final$final.weights,decreasing = T),]
write.csv(arkk.final, 'ARK_INNOVATION_template.csv', quote = F, row.names = F)



###############################################################################
###############################---ARKG---######################################
#Search for new companies
arkg.new$company[arkg.new$company %notin% arkg$company]

arkg.new.companies<-arkg.new[arkg.new$company %notin% arkg$company,] %>%
  select(company,`weight(%)`,fund,ticker)
arkg.new.companies$prev.weights<-arkg.new.companies$`weight(%)`
arkg.new.companies$final.weights<-arkg.new.companies$`weight(%)`
arkg.new.companies

#Merge
arkg.merge<-merge(
  arkg.new %>% select(company,`weight(%)`),
  arkg,
  by='company',
  sort=F
)

arkg.merge<-rbind(
  arkg.merge,
  arkg.new.companies
)


#arkg.merge$prev.weights[which(arkg.merge$company=='RECURSION PHARMACEUTICALS-A')]<-0
#Getting the new weights
arkg.merge$prev.weights<-arkg.merge$`weight(%)`
arkg.merge$prev.weights[which(arkg.merge$final.weights==0)]<-0

weights<-arkg.merge$prev.weights*100/sum(arkg.merge$prev.weights)

total<-100-0.5*length(which(weights<0.5 & weights>0))

weights5<-weights
weights5[which(weights<0.5 & weights>0)]<-0.5

final.weights<-weights5*total/sum(weights5[-which(weights<0.5 & weights>0)])
final.weights[which(weights<0.5 & weights>0)]<-0.5
sum(final.weights)

rounded.weights<-round(final.weights,1)

if(sum(rounded.weights)>100){
  subtract.length<-(sum(rounded.weights)-100)/0.1
  rounded.weights[1:subtract.length]<-rounded.weights[1:subtract.length]-0.1
}

if(sum(rounded.weights)<100){
  add.length<-(100-sum(rounded.weights))/0.1
  rounded.weights[1:add.length]<-rounded.weights[1:add.length]+0.1
}

#Update data.frame
arkg.final<-arkg.merge
arkg.final$final.weights<-rounded.weights
arkg.final<-arkg.final[order(arkg.final$final.weights,decreasing = T),]
write.csv(arkg.final, 'ARK_GENOMIC_REVOLUTION_template.csv', quote = F, row.names = F)
sum(rounded.weights)

