#To import essential libraries before start of analysis
library(tidyverse)
library(ggplot2)

#To inner-join purchases and products data to label the products with cost in the list of purchases
purchases %>% inner_join(products, by="product")

#To create a list of data called year_purchases with only year and cost to present for each puchased item
year_purchases <- purchases %>% inner_join(products, by="product") %>% select(year,cost)
View(year_purchases)

#To group the year_purchases data by year and add the sum of sale for each year by the costs of items
year_purchases %>% group_by(year) %>% summarize(profit=sum(cost))

#To create a new list of data called year_sale to present the total sale for each year
year_sale <- year_purchases %>% group_by(year) %>% summarize(profit=sum(cost))
View(year_sale)

#To plot the graph of Profits in different years to compare the amount of sales
ggplot(data=year_sale, aes(x=year, y=profit))+geom_bar(stat="identity", width=0.6, fill="steelblue")+geom_text(aes(label=profit), vjust=-0.3, size=3.5)+theme_minimal()+labs(title="Profits in different years")+ theme(plot.title = element_text(hjust=0.5))

