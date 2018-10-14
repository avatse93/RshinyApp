US_heatmap_sentiment <- function(final_data){
  library(maps)
  library(mapproj)
  library(ggplot2)
  source("abbr2state.r")
  all_states <- map_data("state")
  final_data$region <-abbr2state(final_data$State) 
  state_compliant_agg <- as.data.frame(final_data)%>%group_by(region)%>%
    summarize(sum(Sentiment))
  names(state_compliant_agg)<-c("region","Sentiment")
  
  Total<- merge(all_states,state_compliant_agg,by="region",all.x=TRUE)
  Total <- Total[Total$region!="district of columbia",] 
  Total <- Total[order(Total$order),]
  
  ggplot(Total, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=Sentiment))+
    geom_path()+ 
    scale_fill_gradientn(colours=rev(hsv(1, seq(0,1,length.out = 12))),na.value="grey90")+
    coord_map()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "white"),
    axis.title = element_blank(),axis.text = element_blank())+ ggtitle("Sentiment analysis by states")}