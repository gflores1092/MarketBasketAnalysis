# 1. SETUP
# Load packages
library(tidyverse)
library(lubridate)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(recommenderlab)
# 2. DATA (Add attributes)
# Read data and save as products.csv and attributes.csv
products <- read.table('C:/Users/Gerardo Flores/Documents/products.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
attributes <- read.table('C:/Users/Gerardo Flores/Documents/attributes.csv', header=TRUE, sep=',', stringsAsFactors=FALSE, allowEscapes=TRUE, quote="\"")
# Transform data to transactions (Only run A or B)
# A. WITH ATTRIBUTES
products <- products %>% 
            group_by(order_id) %>%
            select(product_name) %>%
            mutate(product_name=str_to_lower(product_name)) %>%
            mutate(product_name=str_replace_all(product_name,"\\(|\\)|\\*|\\<|\\:|\\-|\\=", " ")) %>%
            mutate(product_name=str_squish(product_name)) 
attributes <- attributes %>%
              group_by(order_id) %>%
              select(attribute_name) %>%
              mutate(attribute_name=str_to_lower(attribute_name)) %>%
              mutate(attribute_name=str_replace_all(attribute_name,"\\(|\\)|\\*|\\<|\\:|\\-|\\=", " ")) %>%
              mutate(attribute_name=str_squish(attribute_name)) %>%
              rename("product_name"="attribute_name")
baskets <- rbind(products,attributes) %>%
           mutate(product_name=paste(product_name, collapse=",")) %>%
           distinct(order_id, .keep_all=TRUE) %>%
           ungroup() %>%
           select(product_name) %>%
           as.data.frame()
# B. WITHOUT ATTRIBUTES
baskets <- products %>% 
           group_by(order_id) %>%
           select(product_name) %>%
           mutate(product_name=str_to_lower(product_name)) %>%
           mutate(product_name=str_replace_all(product_name,"\\(|\\)|\\*|\\<|\\:|\\-|\\=", " ")) %>%
           mutate(product_name=str_squish(product_name)) %>%
           mutate(product_name=paste(product_name, collapse=",")) %>%
           distinct(order_id, .keep_all=TRUE) %>%
           ungroup() %>%
           select(product_name) %>%
           as.data.frame()
# Convert to transactions
items <- strsplit(as.character(baskets$product_name), ",")
transactions <- as(items, "transactions")  # Ignore warning
# 3. TRANSACTIONS
# Inspect transactions (most frequent products, most frequent basket sizes)
summary(transactions)
# Rules (change support and confidence)
rules <- apriori(transactions, parameter=list(support=0.01, confidence=0.005))
rules_conf <- sort(rules, by="confidence", decreasing=TRUE)
# Inspect rules (arranged by confidence) https://towardsdatascience.com/association-rules-2-aa9a77241654
# Support: This measure gives an idea of how frequent an itemset is in all the transactions.
# Confidence: This measure defines the likeliness of occurrence of consequent on the cart given that the cart already has the antecedents.
# Lift: Lift controls for the support (frequency) of consequent while calculating the conditional probability of occurrence of {Y} given {X}.
inspect(rules_conf)
# Rules to data frame
rules_df <- as(rules_conf, "data.frame") %>%
            separate(rules, c("lhs_rule","rhs_rule"),"=>")
# Export Rules as CSV
write_csv(rules_df, "C:/Users/Gerardo Flores/Documents/rules.csv")
# 4. PLOTS
# Absolute frequency (Top 20)
itemFrequencyPlot(transactions, topN=20, type="absolute", col=brewer.pal(8, 'Pastel2'), main="Most common products (n)")
# Relative frequency (Top 20)
itemFrequencyPlot(transactions, topN=20, type="relative", col=brewer.pal(8, 'Pastel2'), main="Productos más comunes (%)")
# Confidence (Top 10)
top10Rules <- head(rules, n=10, by="confidence")
plot(top10Rules, method="graph",  engine="htmlwidget")
# Scatterplot
#plot(rules, measure=c("support", "confidence"), shading="lift", control=list(col=rainbow(7)))
# Grouped matrix
#plot(rules, method="grouped", control=list(k=20))
# Paracoord (check)
#top100Rules <- head(rules, n=100, by="confidence")
#plot(top100Rules, method="paracoord", control=list(alpha=0.8, reorder=TRUE))
# Doubledecker
#top1Rule <- head(rules, n=1, by="confidence")
#plot(top1Rule, method="doubledecker", data=transactions)
