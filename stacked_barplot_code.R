# Load required libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)


## STACKED BAR CHART - STAGE GROUPS
# set working directory
setwd("C:/Users/sweta/OneDrive - Emory University/SOM BMI/SEER Lung Cancer Project/Data Files")

# Load the dataset
seer_data <- read.csv("data_n18595.csv")
cat("Initial number of rows:", nrow(seer_data), "\n")

#Reformat Data frame
seer_data2<-expand.grid(
  Age_group=c("18-29","30-39","40-50"),
  Histology=c("Adenocarcinoma", "Large Cell Carcinoma", "Other/Unspecified","Squamous Cell Carcinoma"),
  Stage=c("Stage 1", "Stage 2", "Stage 3", "Stage 4"))
seer_data3<-data.frame(seer_data2)
seer_data3$Count<-c(18, 118, 1363, 202, 338, 747, 0, 20, 245, 1, 10, 195, 10, 54, 507, 54, 96, 212, 1, 3, 58, 6, 9, 165, 20, 168, 1400, 32, 76, 386, 4, 22, 161, 10, 42, 725, 146, 902, 6035, 32, 138, 1018, 16, 73, 529, 18, 71, 951)

seer_data3<-seer_data3%>%
  group_by(Age_group, Histology)%>%
  mutate(TotalCases=sum(Count),
         Percentage=(Count/TotalCases)*100)

#Change Age_group labels
#Compute total cases per Age Group
seer_data3 <- seer_data3 %>%
  mutate(Age_group_label = paste0(Age_group, "\n(n=", TotalCases, ")"))

seer_data4<- seer_data3%>% mutate(x_label = case_when(Histology == "Adenocarcinoma" & Age_group== "18-29" ~ "18-29 \n(n=194)", Histology  == "Adenocarcinoma" & Age_group == "30-39" ~ "30-39 \n(n=1242)", Histology  == "Adenocarcinoma" & Age_group== "40-50" ~ "40-50 \n(n=9305)", Histology  == "Large Cell Carcinoma" & Age_group== "18-29" ~ "18-29 \n(n=320)",Histology  == "Large Cell Carcinoma" & Age_group== "30-39" ~ "30-39 \n(n=648)", Histology  == "Large Cell Carcinoma" & Age_group== "40-50" ~ "40-50 \n(n=2363)", Histology  == "Other/Unspecified" & Age_group== "18-29" ~ "18-29 \n(n=21)", Histology  == "Other/Unspecified" & Age_group== "30-39" ~ "30-39 \n(n=118)", Histology=="Other/Unspecified" & Age_group=="40-50" ~ "40-50 \n(n=993)", Histology =="Squamous Cell Carcinoma" & Age_group =="18-29" ~ "18-29 \n(n=35)", Histology=="Squamous Cell Carcinoma" & Age_group =="30-39" ~ "30-39 \n(n=132)", Histology=="Squamous Cell Carcinoma" & Age_group=="40-50" ~ "40-50 \n(n=2036)", TRUE ~ Age_group) ) 

#Plot

ggplot(seer_data4, aes(x = x_label, y = Percentage, fill = Stage)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ Histology, scales = "free_x") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Age Group",
    y = "Cases (%)",
    fill = "",
    title = "NSCLC Stage by Histologic Findings, Overall and by Age"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_manual(values = c("Stage 4" = "deeppink", "Stage 3" = "purple", "Stage 2" = "darkorange", "Stage 1" = "aquamarine4"))
