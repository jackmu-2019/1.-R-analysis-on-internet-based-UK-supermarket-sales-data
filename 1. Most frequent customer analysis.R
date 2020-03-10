#To import essential libraries before start of analysis
library(tidyverse)
library(ggplot2)

#To extract only name and id of personal data to match up with id of purchase data, to assign the name of customer to each purchase
personal_name <- personal %>% select(id, name)

#To left-join purchase data with the names of personal_name data using the common id from both lists
purchases <- left_join(purchases, personal_name, by="id")
View(purchases)

#Group by name on the list of purchases and count the total number of frequency of purchase for each name
purchases %>% group_by(name) %>% summarize(frequency=n())

#Arrange the frequency list in descending order to show the top 10 most frequent customers on the list
purchases %>% group_by(name) %>% summarize(frequency=n()) %>% arrange(desc(frequency))

#Group the frequency data in to a new list called purchases_frequency
purchases_frequency <- purchases %>% group_by(name) %>% summarize(frequency=n()) %>% arrange(desc(frequency))

#Extract only the top 10 customers from the purchases_frequency list for visualization
top10purchases_frequency <- purchases_frequency[1:10,]
View(top10purchases_frequency)

#To ensure the presenting of frequency on the graph is also in descending order
top10purchases_frequency$name <- factor(top10purchases_frequency$name, levels = top10purchases_frequency$name[order(-top10purchases_frequency$frequency)])

#To plot the graph of top 10 data by ggplot2
ggplot(data=top10purchases_frequency, aes(x=name, y=frequency))+geom_bar(stat="identity", width=0.7, fill="steelblue")+geom_text(aes(label=frequency), vjust=-0.3, size=3.5)+theme_minimal()+labs(title="Top 10 most frequent customers")+ theme(plot.title = element_text(hjust=0.5))+ theme(axis.text.x = element_text(angle = -30))

