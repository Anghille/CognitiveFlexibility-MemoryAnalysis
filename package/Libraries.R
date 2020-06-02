wants <- c("psych","reshape","reshape2","car","carData","moments","tidyverse","stringr",
           "plotly","readxl","GGally","grid","gridExtra","data.table","esquisse","Hmisc",
           "psychTools","stargazer","polycor","lavaan","lavaanPlot","ggpubr","corrplot",
           "outliers","GPArotation","jtools","DT","kableExtra","knitr","tidygraph","ggraph","semPlot","solitude","reticulate")

# IF R CRASHES, 
# REMOVE "JTOOLS" FROM THE "wants" VECTOR AND INSTALL/CALL IT AFTER 
# BY UNCOMMENTING THE LAST 2 LINES OF THIS CHUNK

has <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
invisible(lapply(wants, library, character.only = TRUE))
rm(has, wants)

# Uncomment it if needed (see last comment for the "why")
# install.packages("jtools")
# library(jtools)


stocks = data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm = stocks %>%
  reshape2::melt(id.vars = "time", variable.name = "stock", value.name = "price")
stocksm %>% reshape2::dcast(time ~ stock)


# dcast and melt are complements
df = data.frame(x = c("a", "b"), y = c(3, 4), z = c(5, 6))
df %>%
  reshape2::dcast(z ~ x, value.var = "y") %>%
  reshape2::melt(id.vars = "z", variable.name = "x", value.name = "y", na.rm = TRUE)
#