### function to calculate change in risk
#link 29 threats to 22 activities
#


actions<-function(pathredo="data/mpa_risk_df.csv",removed=NA,mpa_id=NA) {

  
  net_activities<-c("agricultural effluent","aquaculture","commercial & industrial dvlpt","crops",
                    "domestic waste water","dredge","gillnet","housing & urban dvlpt","jigger","livestock farming",
                    "longline","oil & gas production","pole and line","pot","recreational activities","roads & rail",
                    "seine","shipping","tourism & recreation dvlpt","trap","trawl","work disturbance","excess energy")
  
  mpa.impact.df <<- fread(pathredo)
  activities<-names(mpa.impact.df)[4:32]
  
  relationship_db<-data.frame(activities=sort(activities),netactivities=net_activities[c(1,2,3,4,2,5,6,23,7,8,18,9,10,11,2,12,13,14,15,16,2,17,18,2,19,20,21,2,22)])
  
  mpa.impact<-mpa.impact.df[mpa.impact.df$mpa==mpa_id,]
  
  mpa.managed.df<-mpa.impact[,4:32] %>% select(-c(relationship_db$activities[which(relationship_db$netactivities==removed)]))
  
#complexity
complexity<-(-rowSums((mpa.managed.df/mpa.impact.df$tot.sp[mpa.impact.df$mpa==mpa_id])*log((mpa.managed.df/mpa.impact.df$tot.sp[mpa.impact.df$mpa==mpa_id]),2),na.rm=T))
#richness
richness<-rowSums(mpa.managed.df!=0)

list(newrichness=richness,newcomplexity=complexity)
}
