# Load required libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(tidyr)

# Set Working Directory
setwd("C:/Users/sweta/OneDrive - Emory University/SOM BMI/SEER Lung Cancer Project/Data Files")

# Load the dataset
seer_data <- read.csv("raw_data.csv")
cat("Initial number of rows:", nrow(seer_data), "\n")

# Filter dataset for patients diagnosed in 2010 or later
filtered_data <- subset(seer_data, Year.of.diagnosis >= 2010)
cat("Number of rows after filtering by Year.of.diagnosis:", nrow(filtered_data), "\n")

# Update 'ICD.O.3.Hist.behav' column to keep only the code before the slash ('/')
filtered_data$ICD.O.3.Hist.behav <- sub("/.*", "", filtered_data$ICD.O.3.Hist.behav)

# Define lists of ICD codes for different lung cancer types
squamous_cell_codes <- c(8051:8052, 8070:8076, 8078, 8083:8084, 8090, 8094, 8123)
adenocarcinoma_codes <- c(8015, 8050, 8140:8141, 8143:8145, 8147, 8190, 8201, 8211,
                          8250:8255, 8260, 8290, 8310, 8320, 8323, 8333, 8401, 8440,
                          8470:8471, 8480:8481, 8490, 8503, 8507, 8550, 8570:8572, 8574, 8576,
                          8012:8014, 8021, 8034, 8082)
large_cell_codes <- c(8046, 8003:8004, 8022, 8030:8033, 8035, 8120, 8200, 8240:8241,
                      8243:8246, 8249, 8430, 8525, 8560, 8562, 8575)
small_cell_codes <- c(8002, 8041:8045)

# Map histologic codes to broader categories
filtered_data$Histology_Group <- with(filtered_data,
                                      ifelse(ICD.O.3.Hist.behav %in% squamous_cell_codes, "Squamous Cell Carcinoma",
                                             ifelse(ICD.O.3.Hist.behav %in% adenocarcinoma_codes, "Adenocarcinoma",
                                                    ifelse(ICD.O.3.Hist.behav %in% large_cell_codes, "Large Cell Carcinoma",
                                                           ifelse(ICD.O.3.Hist.behav %in% small_cell_codes, "Small Cell Lung Cancer",
                                                                  "Other/Unspecified")))))

# Filter out Small Cell Lung Cancer to keep only non-small cell lung cancer cases
nsclc_data <- subset(filtered_data, Histology_Group != "Small Cell Lung Cancer")
cat("Number of rows after filtering for NSCLC:", nrow(nsclc_data), "\n")

# Categorize age groups
nsclc_data$age_group <- ifelse(nsclc_data$Age.recode.with.single.ages.and.90. <= 50, "18-50", ">50")

#Survival years
mort<-nsclc_data%>%
  filter(SEER.cause.specific.death.classification=="Dead (attributable to this cancer dx)")%>%
  mutate(Survival.years=(as.numeric(Survival.months)/12))


#estimated death year
mort<-mutate(mort, Year.of.death.est=Year.of.diagnosis+Survival.years)
mort<-mutate(mort, Year.of.death.est.round=round(mort$Year.of.death.est,digits=0))

# Categorize age groups
mort$age_group <- ifelse(mort$Age.recode.with.single.ages.and.90. <= 50, "18-50", ">50")

# Calculate annual incidence counts for each year and age group
annual_incidence <- nsclc_data %>%
  group_by(year = Year.of.diagnosis, age_group) %>%
  summarise(count = n(), .groups = 'drop')

## MORTALITY: Calculate annual mortality counts for each year and age group
mort$Survival.years<-as.numeric(mort$Survival.years)
annual_mortality <- mort %>%
  group_by(year=Year.of.death.est.round, age_group) %>%
  summarise(count = sum(n()), .groups = 'drop')

View(annual_mortality)

# Population data for age groups (in millions)
population_stats <- data.frame(
  year = 2010:2021,
  pop_18_50 = c(129.91, 131.08, 132.25, 133.45, 134.65, 135.86, 137.08, 138.31, 139.56, 140.82, 142.08, 143.36),
  pop_over_50 = c(179.39, 181.01, 182.64, 184.28, 185.94, 187.61, 189.30, 191.01, 192.72, 194.46, 196.21, 197.98)
)

# Merge annual incidence with population data
annual_incidence <- merge(annual_incidence, population_stats, by = "year")

## MORTALITY: Merge annual mortality with population data
annual_mortality <- merge(annual_mortality, population_stats, by = "year")


# Calculate incidence rate per 100,000
annual_incidence <- annual_incidence %>%
  mutate(
    incidence_rate = case_when(
      age_group == "18-50" ~ (count / (pop_18_50 * 1e6)) * 100000,
      age_group == ">50" ~ (count / (pop_over_50 * 1e6)) * 100000,
      TRUE ~ NA_real_
    )
  )

## MORTALITY: Calculate 5-year mortality rate per 100,000
annual_mortality <- annual_mortality %>%
  mutate(
    mortality_rate = case_when(
      age_group == "18-50" ~ (count / (pop_18_50 * 1e6)) * 100000,
      age_group == ">50" ~ (count / (pop_over_50 * 1e6)) * 100000,
      TRUE ~ NA_real_
    )
  )

# Reshape data for plotting
incidence_plot_data <- annual_incidence %>%
  select(year, age_group, incidence_rate)

#MORTALITY: Reshape data for plotting
mortality_plot_data <- annual_mortality %>%
  select(year, age_group, mortality_rate)

#combine incidence and mortality

#At this point, I created a separate .csv file to store just the incidence and mortality rates and based on the calculations above.
#I then went into the file and edited the formatting to ease the process of creating the plot.

data<-cbind(incidence_plot_data,mortality_plot_data)
write.csv(data,"C:/Users/sweta/OneDrive - Emory University/SOM BMI/SEER Lung Cancer Project/Data Files/Incidence and Yearly Mortality.csv")


#edit csv in excel
#After writing the .csv in the last line of code, I manually went into the file and edited the formatting.

data<-read.csv("Incidence and Yearly Mortality.csv")

#Plot
ggplot(data, aes(x = year, y = rate, color = age_group, linetype=type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks=seq(2010,2021,by=1))+
  labs(
    title = "Annual Incidence and Mortality Rates per 100,000 by Age Group (2010-2021)",
    x = "Year",
    y = "Incidence and Mortality Rates per 100,000",
    color = "Age Group",
    linetype="Rate"
  ) +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
