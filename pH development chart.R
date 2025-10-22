

##1.load packages####
# install.packages("ggrepel")
# install.packages("extrafont")
# install.packages("tidyr")
# install.packages("ggtext")
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("cowplot")
library(tidyverse)
library(extrafont)
library(ggrepel)
library(tidyr)
library(RColorBrewer)
library(readxl)
library(dplyr)
library(cowplot)

display.brewer.all(colorblindFriendly = T)



##2. Set working directory####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##3. Load the .csv file from your working directory, make sure you file is saved in the same folder with this R script####
# Run <- read.csv("Run.csv", sep = ",")
Run <- read_excel("Run.xls")


##4.Prepare the data sheet, you will have one data.frame for overview of ph, and another data.frame for calculating NaOH dosage.####
Run <- Run[, !names(Run) %in% c("pH cha.6","SP1 target pH","SP2 target pH","SP3 target pH","SP4 target pH")]
colnames(Run) <- c("Time","Chamber1","Chamber2","Chamber3","Chamber4","Chamber5", "target_ph", "SP1", "SP2", "SP3", "SP4", "SP5")
Run[, 1] <- 1:nrow(Run)
development_ph <- Run[, 1:7]
NaOH_dose <- Run[, c(1, 8:12)]


##5.1Filter the development data####
chamber_cols <- c("Chamber1", "Chamber2", "Chamber3", "Chamber4", "Chamber5")
rows_to_keep <- rowSums(is.na(development_ph[, chamber_cols])) == 0
development_ph_filter <- development_ph[rows_to_keep, ]

##5.2Filter the NaOH data, fill the concentration and dose volunme set up of your project####

Concentration <- 1 #1M NaOH
Dose_vol <- 5 #ul

sp_cols <- c("SP1", "SP2", "SP3", "SP4", "SP5")

NaOH_dose <- NaOH_dose %>%
  mutate(across(all_of(sp_cols), ~ ifelse(. %in% c(2, 3), 0, .)))



##6 Plot the ph development chart####
#Transpose
pH_long <- development_ph_filter %>%
  pivot_longer(
    
    Chamber1:target_ph,
    names_to = "variable", values_to = "pH"
  )

#plotting
p_ph_development <-ggplot(pH_long, aes(x = Time, y = pH, group = variable)) +
                           geom_line(aes(color = variable), size=1.5) +
                            theme(legend.position = "right")+
                            theme_bw()+
                            ggtitle("pH development run XX")+
                            labs(y="pH in the fermentation sample", x="pH log during 24 hours")+
                            scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("Chamber 1", 
                                                                                              "Chamber 2",
                                                                                              "Chamber 3",
                                                                                              "Chamber 4",
                                                                                              "Chamber 5",
                                                                                              
                                                                                              "Target pH"))
p_ph_development

  
##7.NaOH dosage overtime####




p1 <- ggplot(data=NaOH_dose, aes(x=Time, y=SP1)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_blank()
  ) +
  xlab(" ")+  
  scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("SP1" ))+
  coord_cartesian(ylim = c(0.5, 1))


p2 <- ggplot(data=NaOH_dose, aes(x=Time, y=SP2)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank()) +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_blank()
  ) +
  xlab(" ")+
  scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("SP2" ))+
  coord_cartesian(ylim = c(0.5, 1))

p3 <- ggplot(data=NaOH_dose, aes(x=Time, y=SP3)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_blank()
  ) +
  xlab(" ")+
  scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("SP3" ))+
  coord_cartesian(ylim = c(0.5, 1))



p4 <- ggplot(data=NaOH_dose, aes(x=Time, y=SP4)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x  = element_blank(),
        axis.text.y = element_blank()
        ) +
  xlab(" ")+
  scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("SP4" ))+
  coord_cartesian(ylim = c(0.5, 1))




p5 <- ggplot(data=NaOH_dose, aes(x=Time, y=SP5)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()
        )+
  scale_colour_brewer(palette="Dark2", name = "Chamber", labels = c("SP5" ))+
  coord_cartesian(ylim = c(0.5, 1))

p_NaOH_development <- plot_grid(p1, p2, p3, p4, p5, nrow = 5) 
p_NaOH_development

##8. NaOH dosage calculation#####
totals <- sapply(sp_cols, function(sp) {
  sum(NaOH_dose[[sp]]) * Concentration * Dose_vol
})

Sum_NaOH <- data.frame(
  Sample = chamber_cols,
  Total = totals
)

Sum_NaOH

##9. Resurlt summary#################################
p_ph_development
p_NaOH_development
Sum_NaOH
