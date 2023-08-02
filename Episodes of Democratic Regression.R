#' Episodes of Democratic Regression (EDR)
#' 
#' An extended subset of the Episodes of Regime Transformation Datset (ERT)
#' @author Kevin Walz (kwalz@uni-mainz.de)



###' Load and install packages according to ERT-Readme (https://github.com/vdeminstitute/ERT#readme)
#' install.packages("devtools") #, lib.loc="C:/Program Files/R/R-4.1.3/library")
#' you should only install this once
install.packages("devtools")
devtools::install_github("vdeminstitute/ERT")
load("vdem.RData")
load("codebook.RData")

###' Import libraries according to ERT-Package Description(https://raw.githubusercontent.com/vdeminstitute/ERT/master/DESCRIPTION)

#' install.packages("dplyr")
library(dplyr) #' overlap to stats:: filter, lag and base:: intersect, setdiff, setequal, union
library(Rcpp) 
library(hablar) #' overlap to dplyr::na_if
library(tidyr)
library(plm) #' overlap dplyr::between, lag, lead

library(stringr)
library(ggplot2)
library(purrr)
library(tibble) #' overlap to hablar::num
library(tidyselect)

###' Test the standard functions of the ERT-Package
get_eps()
find_overlap()
plot_all()
plot_episodes(country = c("Belgium"),
                   years = c(1910,2010))

###' Build the ERT-Dataset by using get_eps()
episodes <- get_eps()

###' Now we want to generate a specific subset of the ERT-Data
###' We are interested in all Autocratization Episodes which begin in the democratic spectrum. We additionally consider
###' regime change from democracy to autocracy as ultimate end of any Autocratization Episode, as this determines the outcome significantly.
###' We use the variable "autocratization episode identifier" (aut_ep_id)  to select all autocratization episodes. 
###' By referring to the "Regimes of the World" (v2x_regime) variable and "RoW regime change event" (row_regch_event)
###' we exclude all the Autocratization Episodes in the autocratic spectrum and cut any Episode by the year of regime change from democracy
###' to autocracy

###' Subset Episodes of Autocratization
episodes_aut <- subset (episodes, select = c(country_text_id, country_name, year, v2x_regime, v2x_polyarchy, row_regch_event, aut_ep, aut_ep_start_year, aut_pre_ep_year, aut_ep_censored), aut_ep_id != "NA"| dem_ep_id == "NA"| aut_pre_ep_year == 1)

###' Subset Episodes of Democratic Regression
episodes_demreg <- subset (episodes_aut, v2x_regime >= 2 | row_regch_event < 0 )
view(episodes_demreg)

###' After reviewing the generated dataset, one observation stands out:
###' For some reason North Macedonia (1999, row = 471) is still partof the dataset. 
###' This is because the datapoint resembles the unique example of a year before an autocratization episode starts *within* the autocratic spectrum.
###' And the regime_changing_event lies in the year before the episode officially starts!
###' Therefore I manually tidy the data and delete this observation
episodes_demreg <- episodes_demreg [-471,]

###' Viewing the new Dataset we recognize, that there are episodes in which only the data for one year of the episode itself rests
###' e.g. Suriname or Ghana. These cases remain in the data because of a regime changing event. 
###' Therefore, it is plausible to consider these data as representing highly disruptive events from the year before the episode starts
###' and the very first year of the "episode"



###' Now we include additional variables for further analyses in episodes_demreg

###' First, we generate variable capturing the start an episode of democratic regression
###' For demreg_start_year we can simply copy the values of aut_ep_start_year as the starting point of the episodes are the same
episodes_demreg$demreg_start_year <- episodes_demreg$aut_ep_start_year

###' Going on from that we can generate individual IDs for all the episodes. 
###' demreg_id depends on country_text_id and the demreg_start_year
episodes_demreg$demreg_id <- as.numeric(as.factor(with(episodes_demreg, paste (country_text_id, demreg_start_year, sep = "_"))))


###' Now that we can distinguish episodes, we can generate ep_year for numbering years of each episode
ep_year =c("NA")
episodes_demreg$ep_year <- ep_year 
for (i in unique(episodes_demreg$demreg_id)) episodes_demreg$ep_year[episodes_demreg$demreg_id == i] <- seq_len(sum(episodes_demreg$demreg_id == i))
###' The numbering starts with '1'. This is not intuitive as the first row of each episode captures the last year before the start of an episode according to the operationalization of the ERT.
###' Therefore, we subtract '1' from all values of ep_year. 
episodes_demreg$ep_year = as.numeric(episodes_demreg$ep_year)
episodes_demreg$ep_year <- episodes_demreg$ep_year - 1
###' The numbering now starts with '0' for the last year before the episode starts

###' In the next step, we generate a variable capturing the total duration of each episode
episodes_demreg <- episodes_demreg %>%
  group_by (demreg_id) %>%
  mutate(ep_dur = n())
###' Similar to ep_year, we have the problem, that the values of ep_dur are not completely in line with the operationalization of the ERT
###' Analogously, we have to subtract '1' from all the values of ep_dur
episodes_demreg$ep_dur <- episodes_demreg$ep_dur - 1


###' Besides the duration of an episode, I consider the slopes of democratic regression as a second usable variable for distinguishing between fast and slow processes
###' Therefore, now we generate a variable capturing the decline of democratic quality (v2x_polyarchy) by year (decline_year)
episodes_demreg$decline_year <- ave(episodes_demreg$v2x_polyarchy, episodes_demreg$demreg_id, FUN = function (x) c(NA, diff(x)))

###' Now, we also want to generate a variable capturing the average decline_year for each episode
episodes_demreg <- episodes_demreg %>%
  group_by (demreg_id) %>%
  mutate (avg_decline_year := mean(decline_year, na.rm = T))

###' Now we want to categorize episodes considering whether they are rather slow or rather fast (cut-off ep_dur = 5 )
episodes_demreg <- mutate (episodes_demreg,
                           ep_progress = case_when(
                             ep_dur <= 5 ~ "fast",
                             ep_dur > 5 ~ "slow",
                           ))

###' It is helpful to build a abs-variable for row_regch_event before we can generate a variable capturing the outcome of an episode 
episodes_demreg <- episodes_demreg %>%
  group_by(demreg_id) %>%
  mutate (abs_row_regch_event := abs(row_regch_event))

###' We also need a variable capturing the outcome of an episode (ep_outcome)
episodes_demreg <- episodes_demreg %>%
  group_by (demreg_id) %>%
  mutate (ep_outcome = case_when(
    sum(abs_row_regch_event, na.rm = T) > 0 & sum(aut_ep_censored) == 0 | sum(row_regch_event) < 0 ~"regime change",
    sum(abs_row_regch_event, na.rm = T) >= 0 & sum(row_regch_event, na.rm = T) == 0 & sum(abs_row_regch_event, na.rm = T) < 2 & sum(aut_ep_censored) == 0  ~ "no regime change",
    sum(abs_row_regch_event, na.rm = T) >= 0 & sum(abs_row_regch_event, na.rm = T) <= 2 & sum(aut_ep_censored) > 0 ~ "outcome censored"
  ))

###' Based on these variable we can also classify all the variables according to the two-level-conceptualization of autocratization (ep_type)
episodes_demreg <- episodes_demreg %>%
  group_by(demreg_id) %>%
  mutate (ep_type = case_when(
    ep_progress == "fast" & ep_outcome == "regime change" ~ "substantial disruption",
    ep_progress == "fast" & ep_outcome == "no regime change" ~ "partial disruption",
    ep_progress == "slow" & ep_outcome == "regime change" ~ "substantial erosion",
    ep_progress == "slow" & ep_outcome == "no regime change" ~ "partial erosion",
    ep_outcome == "outcome censored" ~ "classification censored"
  ))





###' Now we can save the 'Episodes of Democratic Regression Dataset' as .Rdata
save (episodes_demreg, file = "EDR.Rdata") 

##' Focusing the plots on decline_year some outliers are noticeable
###' The most unambigous one attach the Episodes of Czechia (1930-1939, demreg_id: 20) and the Netherlands (1940, demreg_id: 70)
###' Obviously these episodes are strongly associated with war and/or annexation by the Third Reich, a totalitarian regime
###' I argue, that we should exclude those and very similar cases that underlie the very special factor "interstate war and annexion" 
###' in the context of World War I and II from the further analyses.
###' Based on online research this concerns at least 7 further episodes:
###' France (1936-1940, demreg_id 32), 
###' Belgium (1914, demreg_id: 7),
###' Belgium (1940, demreg_id: 8),
###' Denmark (1940-1943, demreg_id: 22),
###' Luxembourg (1940, demreg_id: 52),
###' Norway (1940, demreg_id: 71),
###' Finland (1940, demreg_id: 28)
###' We tidy up the dataset accordingly:
episodes_demreg_2 <- episodes_demreg %>%
  filter(!demreg_id == 20, !demreg_id == 70, !demreg_id == 32, !demreg_id == 7, !demreg_id == 8, !demreg_id == 22,!demreg_id == 52, !demreg_id == 71, !demreg_id == 28)

###' We can know save the tidied dataset as .Rda
save (episodes_demreg_2, file = "EDR_tidy.Rdata") 


###' Now we can continue with the Descriptive Analysis of the Episodes of Democratic Regression
##' We start with a borad descriptive overview of the dataset 
summary(episodes_demreg_2)
n_distinct(episodes_demreg_2$demreg_id)

##' Histogram of year-to-year decline rates
###'Histograms for the variables in focus
episodes_demreg_2 %>%
  ggplot(aes(decline_year)) +
  geom_histogram()

##' Now we can count the number of Episodes classified as slow or fast...
episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_progress == "slow") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_progress == "fast") %>%
  count(demreg_id)

##'... considering the outcome...
episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_outcome == "regime change") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_outcome == "no regime change") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_outcome == "outcome censored") %>%
  count(demreg_id)

##'...and, finally, considering the four subtypes

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_type == "substantial disruption") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_type == "partial disruption") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_type == "partial erosion") %>%
  count(demreg_id)

episodes_demreg_2 %>%
  group_by(demreg_id) %>%
  filter (ep_type == "substantial erosion") %>%
  count(demreg_id)


###'Boxplots for decline rates and duration 
boxplot(episodes_demreg_2$decline_year)
boxplot(episodes_demreg_2$avg_decline_year)
boxplot(episodes_demreg_2$ep_dur)

###' Basic distribution Plots for the variables in focus
##' Episodes and year-to-year declines
episodes_demreg_2 %>%
  ggplot(aes(x = ep_year, y = decline_year, col=ep_progress)) +
  theme_minimal() +
  labs(title = "Episodes and year-to-year declines",
       x = "Year of the Episode", 
       y = "Decline per year (EDI)", 
       col = "Progression") +
  scale_color_manual(values = c ("#67a9cf", "#ef8a62"),
                     limits = c("slow", "fast")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_point()

##' Episode duration and average declines
#' Depending on Progression
episodes_demreg_2 %>% 
  ggplot(aes(x = ep_dur, y = avg_decline_year, col=ep_progress)) +
  theme_minimal() +
  labs(title = "Episode duration and average declines",
       x = "Duration of the Episode (years)",
       y = "Average decline per year (EDI)", 
       col = "Progression") +
  scale_color_manual(values = c ("#67a9cf", "#ef8a62"),
                     limits = c("slow", "fast")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_point()

#'...and depending on subtype
episodes_demreg_2 %>% 
  filter(!ep_type == "classification censored") %>%
  ggplot(aes(x = ep_dur, y = avg_decline_year, col=ep_type)) +
  theme_minimal() +
  labs(title = "Episode duration and average declines",
       x = "Duration of the Episode (years)",
       y = "Average decline per year (EDI)", 
       col = "Type") +
  scale_color_manual(values = c ("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                     limits = c("substantial disruption", "partial disruption", "substantial erosion", "partial erosion")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_point()

###' Now we focus on potential patterns of Courses of Episodes associated with
###'  different progression forms and subtypes
##' We start with Courses of Episodes depending on progression
ggplot(episodes_demreg_2,
       aes (x = ep_year, y = v2x_polyarchy,
            group = demreg_id,
            col = ep_progress)) +
  theme_minimal() +
  labs(title = "Courses of Episodes depending on progression",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Progression") +
  scale_color_manual(values = c ("#67a9cf", "#ef8a62"),
                     limits = c("slow", "fast")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_line()

##' And continue with Courses of Episodes depending on (sub-)type
episodes_demreg_2 %>%
  filter(!ep_type == "classification censored") %>%
  ggplot (aes(x = ep_year, y = v2x_polyarchy,group = demreg_id, col = ep_type)) +
  theme_minimal() +
  labs(title = "Courses of Episodes depending on type",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Type") +
  scale_color_manual(values = c ("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                     limits = c("substantial disruption", "partial disruption", "substantial erosion", "partial erosion")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_line()

###' Now we want to look at diachronous comparison of the time periods before and since 1989/90
###' And check, whether we can observe a 'change of nature'
##' Episodes of Democratic Regression before 1989
episodes_demreg_2 %>%
  filter(year < 1989, !ep_type == "classification censored") %>%
  ggplot (aes(x = ep_year, y = v2x_polyarchy,group = demreg_id, col = ep_type)) +
  theme_minimal() +
  labs(title = "Episodes of Democratic Regression before 1989",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Type") +
  scale_color_manual(values = c ("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                     limits = c("substantial disruption", "partial disruption", "substantial erosion", "partial erosion")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_line()

##' Episodes of Democratic Regression since 1989
episodes_demreg_2 %>%
  filter(year >= 1989, !ep_type == "classification censored") %>%
  ggplot (aes(x = ep_year, y = v2x_polyarchy,group = demreg_id, col = ep_type)) +
  theme_minimal() +
  labs(title = "Episodes of Democratic Regression since 1989",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Type") +
  scale_color_manual(values = c ("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                     limits = c("substantial disruption", "partial disruption", "substantial erosion", "partial erosion")) +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust=0.5, size = 16),
  ) +
  geom_line()


###'Finally, we also want to have an overview of all the Episodes of Democratic Regression observed
##' Depending on the Type
episodes_demreg_2 %>%
  filter(!ep_type == "classification censored") %>%
  ggplot(aes (x = ep_year, y = v2x_polyarchy, group = demreg_id, col = ep_type)) +
  labs(title = "Episodes of Democratic Regression",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Type") +
  scale_color_manual(values = c ("#e41a1c", "#377eb8", "#4daf4a", "#984ea3"),
                     limits = c("substantial disruption", "partial disruption", "substantial erosion", "partial erosion")) +
  geom_line() +
  facet_wrap(~ country_name ) +
  theme_minimal() +
  theme (text = element_text(size = 10),
         axis.text.x =  
           element_text (hjust = 0.1),
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         plot.title = element_text(hjust=0.5, size = 14)) 


##' And depending on the progression
episodes_demreg_2 %>%
  ggplot(aes (x = ep_year, y = v2x_polyarchy, group = demreg_id, col = ep_progress)) +
  labs(title = "Episodes of Democratic Regression",
       x = "Year of the Episode",
       y = "Electoral Democracy Index",
       col = "Progression") +
  scale_color_manual(values = c ("#67a9cf", "#ef8a62"),
                     limits = c("slow", "fast")) +
  geom_line() +
  facet_wrap(~ country_name ) +
  theme_minimal() +
  theme (text = element_text(size = 10),
         axis.text.x =  
           element_text (hjust = 0.1),
         legend.text = element_text(size = 10),
         legend.title = element_text(size = 10),
         plot.title = element_text(hjust=0.5, size = 14)) 
  