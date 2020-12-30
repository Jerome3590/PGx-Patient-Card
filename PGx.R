library(readxl) 
library(readr)
library(dplyr) 
library(magrittr) 
library(janitor) 
library(tidyr) 
library(purrr) 
library(tidyverse) 
library(reshape2)


# Step 1

pgx_data <- read_excel("data/pgx_analysis_v1.xlsx")


# Step 2
# Drop any extra empty columns
pgx <- janitor::remove_empty(pgx_data, which = "cols") 

# Drop NAs and INFs
is.na(pgx)<-sapply(pgx, is.infinite) 
pgx[is.na(pgx)] <- 0


pgx_data_long <- gather(pgx, allele, allele_count, 2:53, factor_key=TRUE)


# Drop Zeros
pgx_data_long <- pgx_data_long[pgx_data_long$allele_count != 0, ]

# Remove Spaces
pgx_data_long$allele <- gsub(" ", "", pgx_data_long$allele)


pgx_data_long %<>% separate(allele, c("Gene", "Variant1", "Variant2", "Variant3"), sep = "([*,])")


variants <- pgx_data_long$Variant1 %>% as.data.frame()
names(variants) <- "Value"


for (i in 1:nrow(variants)) {
  if (str_detect(variants$Value[i], "[1:9]")) {
    variants$new <- paste0("*", variants$Value[i])
  } else {
      variants$new <- variants$Value[i]
  }
}



library(rvest)

pgx_fda <- read_html("https://www.fda.gov/medical-devices/precision-medicine/table-pharmacogenetic-associations")


pgx_tables <- pgx_fda %>% html_table(trim = TRUE, fill = FALSE)


pgx_associations1 <- pgx_tables[[1]]

pgx_associations2 <- pgx_tables[[2]]

pgx_associations3 <- pgx_tables[[3]]
