#To import essential libraries before start of analysis
library(tidyverse)
library(ggplot2)

#To create a list called purchases_2015 to show only the purchases of products in 2015 from the purchases data
purchases_2015 <- purchases %>% select(product,year) %>% filter(str_detect(year, "2015"))
View(purchases_2015)

#To group the data in purchases_2015 by product and count the sum of frequency of purchases for each product
purchases_2015 %>% group_by(product) %>% summarise(frequency=n())

#To create a new list called frequencyofpurchases_2015 to show the sum of frequency of purchases for each product in 2015 in ascending order
frequencyofpurchases_2015 <- purchases_2015 %>% group_by(product) %>% summarise(frequency=n()) %>% arrange(frequency)
frequencyofpurchases_2015

#To select the lowest 10 products of purchase for visualization
frequencyofpurchases_2015 <- frequencyofpurchases_2015[1:10,]
View(frequencyofpurchases_2015)

#To ensure the presenting of frequency of purchases on the graph is also in ascending order
frequencyofpurchases_2015$product <- factor(frequencyofpurchases_2015$product, levels = frequencyofpurchases_2015$product[order(frequencyofpurchases_2015$frequency)])

#To plot the graph of lowest 10 purchase data by ggplot2
ggplot(data=frequencyofpurchases_2015, aes(x=product, y=frequency))+geom_bar(stat="identity", width=0.7, fill="steelblue")+geom_text(aes(label=frequency), vjust=-0.3, size=3.5)+theme_minimal()+labs(title="Lowest 10 products sale in 2015")+ theme(plot.title = element_text(hjust=0.5))+ theme(axis.text.x = element_text(angle = -30))
