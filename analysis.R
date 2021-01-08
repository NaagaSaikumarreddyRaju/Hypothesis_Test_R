install.packages("tidyverse")
df <- read.csv("master.csv")
males_suicides=df[df['sex'] == 'male','suicides.100k.pop']
females_suicides=df[df['sex'] == 'female','suicides.100k.pop']
t.test(x=males_suicides,y=females_suicides,conf.level = 0.95,var.equal = TRUE)