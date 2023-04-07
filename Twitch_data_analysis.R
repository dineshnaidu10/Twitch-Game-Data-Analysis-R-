
#Read the data from the csv file
twitch_data <- read.csv("Twitch_game_data.csv", header = TRUE)

#Select and save all data that were ranked Number 1 in a new variable
first_rank_game <- subset(twitch_data, Rank == 1)

#Plot pie chart of most top ranked game using lessR package
PieChart(Game, data = first_rank_game, hole = 0, color = "black",
                        lwd = 1,
                        lty = 1, main = "Most Top Ranked Game")



#Select and save all data that were ranked Number 1 for each 
#year in a new variable
rank_2016 <- subset(twitch_data, Rank == 1 & Year == 2016)
rank_2017 <- subset(twitch_data, Rank == 1 & Year == 2017)
rank_2018 <- subset(twitch_data, Rank == 1 & Year == 2018)
rank_2019 <- subset(twitch_data, Rank == 1 & Year == 2019)
rank_2020 <- subset(twitch_data, Rank == 1 & Year == 2020)
rank_2021 <- subset(twitch_data, Rank == 1 & Year == 2021)

#Compute percentages for top ranked game by year and save in a vector
Percentage <- c(round(max(table(rank_2016$Game)/length(rank_2016$Game))*100, digits = 2),
                round(max(table(rank_2017$Game)/length(rank_2017$Game))*100, digits = 2),
                round(max(table(rank_2018$Game)/length(rank_2018$Game))*100, digits = 2),
                round(max(table(rank_2019$Game)/length(rank_2019$Game))*100, digits = 2),
                round(max(table(rank_2020$Game)/length(rank_2020$Game))*100, digits = 2),
                round(max(table(rank_2021$Game)/length(rank_2021$Game))*100, digits = 2))

#Get the name of top ranked game by year and save in a vector
Game <- c(max(rank_2016$Game), max(rank_2017$Game), max(rank_2018$Game), max(rank_2019$Game),
          max(rank_2020$Game), max(rank_2021$Game))

#Save the years into a vector
Year <- c(2016, 2017, 2018, 2019, 2020, 2021)

#Code block to derive the bar chart of top ranked game by year using plotly package
data <- data.frame(Game, Percentage, Year)

fig <- plot_ly(data, x = ~Year, y = ~Percentage, type = 'bar',
               
               text = Game, textposition = 'auto',
               
               marker = list(color = 'rgb(158,202,225)',
                             
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = "Top Ranked Game of Each Year",
                      
                      xaxis = list(title = "Year"),
                      
                      yaxis = list(title = "Percentage(%)"))
fig

#Numerical Analysis
install.packages("ggplot2")
install.packages("tidyr")
install.packages("reshape2")
install.packages("dplyr")

#Histogram
#Firstly grouping of twitch average viewer ratio by month in a data frame
group <- aggregate(x=twitch$Avg_viewer_ratio, by=list(twitch$Month), FUN=sum)
group

#Calculation of relative frequency
frequency <- group$x/sum(group$x)
frequency

#Plotting of histogram
x <- c("January", "February", "March", "April", "May", "June", "July", "August",
       "September", "October", "November", "December")

y <- c(frequency)

relative_frequency <- data.frame(x,y)
relative_frequency
names(relative_frequency) <- c("Month", "Relative Frequency")
ggplot(data=relative_frequency, aes(x=Month, y=frequency, fill=Month))+
  geom_bar(stat="identity", width=0.5)+theme_minimal()+
  scale_x_discrete(limits=month.name, guide=guide_axis(n.dodge=2))+
  ggtitle("Histogram of Relative Frequency of Average Viewers Ratio by Month")

#Box Plot

#Filtering Data
LOL <- twitch %>% filter(Game=="League of Legends")
LOL

#Creating New Data Frame
data.frame(LOL$Year, LOL$Month, LOL$Avg_viewer_ratio)

#Plotting of box plot
h <- ggplot(data=LOL, aes(x=Year, y=Avg_viewer_ratio, group=1))+geom_boxplot()      +ggtitle("Boxplot of Average Viewer Ratio by Year")+coord_flip()
h

#Central Tendency
median(LOL$Avg_viewer_ratio)
mean(LOL$Avg_viewer_ratio)
summary(LOL)

#Dot Plot
#Filtering data with selective game and year grouped by year
new_data <- twitch %>% filter(Year=='2021', Game=='VALORANT') %>% group_by(Month)

#Renaming value in month column with name of the months
new_data$Month <- factor(month, name, levels=month.name)

#Plotting of dot chart
dotchart(new_data$Peak_channels, labels=new_data$Month, pch=21, bg="green",
         pt.cek=1.5,
         title("Dotplot of valorant Game with Channel Count in 2021"),
         xlab="Peak Channels")



Jan_2021 <- subset(twitch_data, Month == 1 & Year == 2021)
summary(Jan_2021)

sd(Jan_2021$Hours_watched)
sd(twitch_data$Hours_watched)
mean(twitch_data$Hours_watched)

sample_15 <- sample(twitch_data$Hours_watched, 15, replace = FALSE, prob = NULL)
t.test(sample_15, mu=4500000, alternative = "greater")

anova_data <- subset(twitch_data, Year == 2019 | Year == 2020 | Year == 2021)
anova_model <- aov(anova_data$Peak_viewers ~ factor(anova_data$Year), data = anova_data)
summary(anova_model)

TukeyHSD(anova_model)







