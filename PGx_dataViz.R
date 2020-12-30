
library(readxl)
library(readr)
library(dplyr) 
library(magrittr) 
library(janitor) 
library(tidyr) 
library(purrr) 
library(tidyverse) 
library(reshape2)
library(here)


pgx_data <- read_csv("pgx_data.csv")
names(pgx_data)[1] <- "Index"

card_layout_fields <- read_csv("data/card_layout.csv")


images <- read_csv("base64_icons.csv")
names(images)[1] <- "Index"



library(gridExtra)
library(grid)
library(gtable)



cards <- pgx_data %>% group_by(`Sample ID`) %>% select(3:10) %>% unique()


sample1 <- cards %>% filter(`Sample ID`== "R208")
sample1 %<>% group_by(Gene) %>% unique()
sample1_genes <- sample1 %>% select(Gene, Variant1, Variant2, Variant3, allele_count) %>% unique()
sample1_drugs <- sample1 %>% select(7:9) %>% unique()


drugs <- rbind(sample1_drugs[2]) 
Drugs <- drugs %>% dplyr::summarise(Drugs = paste(Drug, collapse = ", "))


drugs_wrapped <- strwrap(Drugs, width = 65, simplify = FALSE) # modify 30 to your needs
drugs_new <- sapply(drugs_wrapped, paste, collapse = "\n")
d <- data.frame(drugs_new)
names(d) <- "Drugs"



card_poc <- as.data.frame(card_layout[5:9])


# Card Front
card_lin1 <- card_poc[1:2]

vcu_icon <- images[15,3]
pgx_icon <- images[9,3]

card_lin2 <- card_poc[3:4]
card_lin3 <- card_poc[5]


# Card Back
genes_pos1 <- sample1_genes
drugs_pos2 <- Drugs


a <- tableGrob(card_lin1)
b <- tableGrob(card_lin2)
c <- tableGrob(card_lin3)
vcu <- tableGrob(vcu_icon)
pgx <- tableGrob(pgx_icon)
genes_card <- tableGrob(genes_pos1)
drugs_card <- tableGrob(drugs_pos2)



grid.newpage()


p1_a <- gtable_combine(a,b, along = 1)
graphic <- gtable_combine(vcu, pgx, along = 1)
p1 <- gtable_combine(graphic, c, along = 1)

p2 <- gtable_combine(genes_card, drugs_card, along = 1)

#grid.arrange()

grid.draw(p1)

grid.draw(p2)




