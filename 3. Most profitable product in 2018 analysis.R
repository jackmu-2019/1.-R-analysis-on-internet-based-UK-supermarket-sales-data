#To import essential libraries before start of analysis
library(tidyverse)
library(ggplot2)

#To select the purchases only in 2018
purchases %>% filter(str_detect(year,"2018"))

#To create a new list called products_2018 with only products of purchase in 2018
products_2018 <- purchases %>% filter(str_detect(year,"2018")) %>% select(product,year)
View(products_2018)

#To update the products_2018 list with the counts of number of purchases of each kind of product in 2018
products_2018 <- products_2018 %>% group_by(product) %>% summarize(count=n())
products_2018

#To inner-join the products_2018 list with the original product list to label the products with their own costs
products_2018 %>% inner_join(products, by="product")

#To create a new list of sales_2018 to find the total sale of each product in 2018 by mutiplying the count with the cost
sales_2018 <- products_2018 %>% inner_join(products, by="product")
sales_2018$sale <- sales_2018$count * sales_2018$cost
View(sales_2018)

#To arrange the sales_2018 data in descending order of sale, and to show only the product name and their respective sales
sales_2018 <- sales_2018 %>% select(product, sale) %>% arrange(desc(sale))
sales_2018

#To extract the top 10 sales data from sales_2018 list
top10sales_2018 <- sales_2018[1:10,]

#To ensure the presenting of sales on the graph is also in descending order
top10sales_2018$product <- factor(top10sales_2018$product, levels = top10sales_2018$product[order(-top10sales_2018$sale)])

#To plot the graph of top 10 data by ggplot2
ggplot(data=top10sales_2018, aes(x=product, y=sale))+geom_bar(stat="identity", width=0.7, fill="steelblue")+geom_text(aes(label=sale), vjust=-0.3, size=3.5)+theme_minimal()+labs(title="Top 10 most profitable products in 2018")+ theme(plot.title = element_text(hjust=0.5))+ theme(axis.text.x = element_text(angle = -30))
