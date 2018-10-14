US_heatmap <- function(final_data){
  library(maps)
  library(mapproj)
  library(ggplot2)
  source("abbr2state.r")
  all_states <- map_data("state")
  head(all_states)
  final_data$region <-abbr2state(final_data$State) 
  state_compliant_agg <- as.data.frame(final_data%>% group_by(region)%>% summarize(n()))
  names(state_compliant_agg)<-c("region","Number of Complaint")
  
  Total<- merge(all_states,state_compliant_agg,by="region",all.x=TRUE)
  Total <- Total[Total$region!="district of columbia",] 
  Total <- Total[order(Total$order),]
  
  ggplot(Total, aes(x=long,y=lat,group=group))+
    geom_polygon(aes(fill=`Number of Complaint`))+
    geom_path()+ 
    scale_fill_gradientn(colours=hsv(1, seq(0,1,length.out = 12)),na.value="grey90")+ #heat.colors(10)
    coord_map()+
    theme_minimal()+labs(colour = "Number of Complaints")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "white"),
          axis.title = element_blank(),axis.text = element_blank())+ ggtitle("Number of Complaints by states")
}