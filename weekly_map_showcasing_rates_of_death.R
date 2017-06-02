library(maps)
library(ggplot2)
library(RColorBrewer)
library(plyr)

All_multi= read.csv(file="ALL_multiple_1.csv", header = TRUE, sep = ",")
All_multi = na.omit(All_multi)

new.variable <- as.vector(All_multi$state) # creates a vector of all the values under "State"
list_names <- unique(new.variable) #Keeps only the unique values 

state_averages = data.frame(matrix(ncol=3,nrow=1)) #Makes an empty dataframe 
colnames(state_averages) = c("state","Week","Average") #assign names for columns 

for (i in list_names)
{
  State_name <- i
  State_all_years <- All_multi[All_multi$state == State_name,] #Subset main dataset for each state, determined by function value 
  keep_states = c("state", "Week", "Percent.of.Deaths.Due.to.Pneumonia.and.Influenza")
  State_all_years <- State_all_years[keep_states] #Keep only relevant columns 
  State_all_years<- rename(State_all_years, c("Percent.of.Deaths.Due.to.Pneumonia.and.Influenza"="Percent_Deaths")) #rename column with condensed title

  for(number in 1:52){
    Current_week <- State_all_years[State_all_years$Week == number,] #Pull the week from all the years 
    Weekly_average<- mean(Current_week$Percent_Deaths) #Average the values from the same week 
    state <- State_name
    state_averages <- rbind(state_averages, c(state,number,Weekly_average)) #Bind the week number and average to empty dataset
  }
}

state_averages = state_averages[-1,] #remove empty first column 

map_function <- function(week_num)
{
  Current <- state_averages[state_averages$Week == week_num,]  #Pulls all of the state data for that week of the year 
  
  #Found online
  us.state.map <- map_data('state')  #Creates the position of the states
  states <- levels(as.factor(us.state.map$region)) #Creates list of all the states 
  df <- data.frame(region = states, value = runif(length(states), min=0, max=100),stringsAsFactors = FALSE)
  
  map.data <- merge(us.state.map, df, by='region', all=T)
  map.data <- map.data[order(map.data$order),] #merges the states with a value
  
  state.percent <- data.frame(region = tolower(Current$state), percent = as.numeric(Current$Average))
  #simplifies imported data to just state names and percents of death
  state.percent$percent = round(state.percent$percent) #Rounds percents to simplify scale 
  state.percent$survivors = 100-state.percent$percent
  
  
  map.county <- merge(x = map.data, y = state.percent, all.y=TRUE, all.x=TRUE) #merges map data and imported data
  
  
  ggplot(map.county, aes(x = long, y = lat, group=group, fill=as.factor(survivors))) +
    geom_polygon(colour = "white", size = 0.1) + #Maps it based on the combined data, colored based on percent, white outline
    scale_fill_brewer(palette = "RdYlGn")
  
  my_map <- ggplot(map.county, aes(x = long, y = lat, group=group, fill=as.factor(survivors))) +
    geom_polygon(colour = "white", size = 0.1) + #Maps it based on the combined data, colored based on percent, white outline
    scale_fill_brewer(palette = "RdYlGn", guide=FALSE) + ggtitle(week_num) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
    )
  
  return(my_map)
  plot(my_map)
  
}

map_function(1)

for (sequence in 1:52)
{ 
  jpeg(file=paste("maps4_",sequence,".jpg",sep=""), quality=100)
  plot(map_function(sequence))
  dev.off()
} 
print("Finished")
