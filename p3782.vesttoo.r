#load libraries
library(haven)
library(dplyr)
library(tidyverse)
library(psych)
library(GPArotation)

cb <- function(arg1){
  write.table(arg1,"clipboard",sep="\t",row.names=FALSE)
}

coal <- function(arg1){
  return(coalesce(arg1,0))
}

pcts <- function(arg1,arg2,arg3){
  arg1 %>% filter(!is.na({{arg2}})) %>% group_by({{arg2}},{{arg3}}) %>% summarise(n=n()) %>% mutate(pct=100*(n/sum(n)))}

#load data
setwd("C:/Work/p3782.VesttooAudienceSeg")
df <- read_sav("vesttoo_final.sav")
df <- rename_with(df,tolower)
df <- df %>% mutate(future=(a1ar3+a1ar4+a1ar5+a1ar7)/4,tech=(a1ar2+a1br2)/2,ai=a1br4,roi=(a1cr5+((a4r7*4)-3))/2,newinv=(a1cr4+a1cr6)/2,subs=a1br1,ilsfam=a1br5,wordofmouth=a1br3,compete=a1ar6,decider=a1cr2,newmedia=c1r3+c1r6+c1r8,noncor=((a4r8)*3)-3)
#clustv1 <- c('a1ar6','a1ar7','a1br1','a1br3','a1br4','a1br5','a1cr1','a1cr2','a1cr5','a1cr6','a2r1')
clustv1 <- c('a1ar3','a1ar7','ilsfam','decider','newmedia')
clust1 <- df %>% select(clustv1)
k1<-kmeans(clust1, centers=5, iter.max = 1000, nstart=5)
k1
df$cluster1 <- k1$cluster
clust_stats_1<-df %>% select(cluster1,ilsfam,decider,a1ar3,a1ar7,newmedia,contains('a1'), contains('a2'),contains('a3'),contains('a4'),contains('a5'),contains('c1r'),contains('c7r'),contains('c8r')) %>% select(-contains('oe'),-a4r96,-noanswera4_r99) %>% group_by(cluster1) %>% summarise(across(everything(),mean))
write_csv(clust_stats_1,'clust1.csv')

#'roi','tech','ai','future','subs''wordofmouth','newinv'
clustv2 <- c('ai','ilsfam','newinv')
clust2 <- df %>% select(clustv2)
k2<-kmeans(clust2, centers=5, iter.max = 1000, nstart=5)
k2
df$cluster2 <- k2$cluster
clust_stats_2<-df %>% select(cluster2,ai,ilsfam,newinv,contains('a1'), contains('a2'),contains('a3'),contains('a4'),contains('a5'),contains('c1r'),contains('c7r'),contains('c8r')) %>% select(-contains('oe'),-a4r96,-noanswera4_r99) %>% group_by(cluster2) %>% summarise(across(everything(),mean))
write_csv(clust_stats_2,'clust2.csv')

clustv3 <- c('a1br4','a1br5','a1ar7','a1cr4','a1cr6','a5r7','a5r8','a1br2','a4r7','a4r8','a3r8','a1cr7','a4r3','a1cr5')
clust3 <- df %>% select(clustv3) %>% mutate(a5r7=2*(a5r7-1)+1,a5r8=2*(a5r8-1)+1,a3r8=2*(a3r8-1)+1,a4r3=a4r3*3,a4r7=a4r7*3,a4r8=a4r8*3)
clust3 <- clust3 %>% mutate(valueprop=(a4r8+a3r8)/1.5,interestednew=(a1cr4+a1cr6+(10-a5r8))/3,techai=(a1br2+a1br4),a1br5=as.double(a1br5),a1ar7=as.double(a1ar7)) %>% select(-a4r7,-a5r7,-a1cr5,-a4r3,-a1cr7) %>% select(-a1cr4,-a1cr6,-a5r8,-a1br4,-a1br2,-a4r8,-a3r8)
k3<-kmeans(clust3, centers=5, iter.max = 1000, nstart=5)
k3
df$cluster3 <- k3$cluster
clust_stats_3<-df %>% select(cluster3,contains('a1'), contains('a2'),contains('a3'),contains('a4'),contains('a5'),contains('c1r'),contains('c7r'),contains('c8r')) %>% select(-contains('oe'),-a4r96,-noanswera4_r99) %>% group_by(cluster3) %>% summarise(across(everything(),mean))
write_csv(clust_stats_3,'clust3.csv')

View(df %>% select(cluster3,contains('b1')) %>% group_by(cluster3) %>% summarise(across(everything(),mean)))

library(factoextra)
library(cluster)
clustm <- pam(clust3,k=5,metric='manhattan',stand=FALSE)
clustm
fviz_nbclust(clust3,pam,method='wss')
gap_stat <- clusGap(clust3,FUN=pam,K.max=8,B=50)
fviz_gap_stat(gap_stat)

write_csv(df,'vesttoo_with_clusters.csv')
write_sav(df,'vesttoo_with_clusters.sav')

#cb(k$centers)
cor(clust1)
cor(clust2)
#clust2 <- df %>% select(contains('a1a')) #%>% select(-contains('oe'),-a4r96,-noanswera4_r99)
#clust2 <- df %>% select(contains('a1'), contains('a4'),contains('a5')) %>% select(-contains('oe'),-a4r96,-noanswera4_r99) #mutate(across(everything(),coal))
#View(cor(clust2))

#f <- fa(clust2,nfactors=8, rotate='oblimin',fm='minres')
#f
#print(f$loadings,cutoff=0.3)

#c1r1,c1r4,c1r7,c1r10
#clust %>% select(!where(is.numeric))
#clust %>% summarise(across(everything(),sum(nullcount)))
