#To import essential libraries before start of analysis
library(tidyverse)
library(ggplot2)

#To create a new list called personal_postcode that only contains id and postcode from the personal data
personal_postcode <- personal %>% select(id,postcode)
View(personal_postcode)

#To left-join postcode data from personal_postcode to purchases data to label the id with postcodes
purchases <- left_join(purchases,personal_postcode, by="id")
View(purchases)

#To create a list of 2017 purchases from purchases data
purchases_2017 <- purchases %>% filter(str_detect(year,"2017"))
View(purchases_2017)

#To inner-join purchase data in 2017 with cost in products data to label the costs of products in the purchases
purchases_2017 <- purchases_2017 %>% inner_join(products, by="product")
View(purchases_2017)

#To group the purchases in 2017 by postcode and sum up the cost for the total amount of purchases for each postcode
purchases_2017 %>% group_by(postcode) %>% summarize(amount=sum(cost))
purchases_postcode_2017 <- purchases_2017 %>% group_by(postcode) %>% summarize(amount=sum(cost))
View(purchases_postcode_2017)

#To arrange the total amount of purchases by postcodes in descending order to show the top 10 postcodes
purchases_postcode_2017 <- purchases_postcode_2017 %>% group_by(postcode) %>% arrange(desc(amount))
purchases_postcode_2017

#To select the top 10 postcodes from the purchases_postcode_2017 list for visualization
top10purchases_postcode_2017 <- purchases_postcode_2017[1:10,]
top10purchases_postcode_2017

#To ensure the presenting of amount of purchase on the graph is also in descending order
top10purchases_postcode_2017$postcode <- factor(top10purchases_postcode_2017$postcode, levels = top10purchases_postcode_2017$postcode[order(-top10purchases_postcode_2017$amount)])

#To plot the graph of top 10 postcode data by ggplot2
ggplot(data=top10purchases_postcode_2017, aes(x=postcode, y=amount))+geom_bar(stat="identity", width=0.7, fill="steelblue")+geom_text(aes(label=amount), vjust=-0.3, size=3.5)+theme_minimal()+labs(title="Top 10 best purchase postcodes in 2017")+ theme(plot.title = element_text(hjust=0.5))+ theme(axis.text.x = element_text(angle = -30))
