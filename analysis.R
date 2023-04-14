# Regression Analyses for Bioarchaeology Studies
# Thomas A. Leibundgut
# 30.05.2022

#############################################################################
#                                   0. Setup                                #
#############################################################################

library("tidyverse")  # if error, run "install.packages("tidyverse")"
library("readxl")  # ditto
library("xtable")  # ditto
library("stargazer")  # ditto

# set working directory to where _Analysis.xlsx is saved
setwd("D:/Uni/3-PhD/1-Dissertation/5-BioArch/Analysis")

# open the prepared sheet R from _Analysis.xlsx
bioarch <- read_excel("_Analysis.xlsx", sheet = "R", na = "NA")

# Show a summary of the data, particularly if types and NAs are correct
bioarch_smmry <- summary(bioarch)
bioarch_smmry  # summary statistics from here

#############################################################################
#                           1. Variable Creation                            #
#############################################################################

# site-type dummies
bioarch %>%
  count(sitetype)  # only instances with 5 or more manifestations
bioarch$town <- ifelse(bioarch$sitetype == "town" & 
											 bioarch$site != "Roma", 1, 0)
bioarch$port <- ifelse(bioarch$sitetype == "port-city", 1, 0)
bioarch$fort <- ifelse(bioarch$sitetype == "fort", 1, 0)
bioarch$roma <- ifelse(bioarch$site == "Roma" & !is.na (bioarch$site), 1, 0)
bioarch$mine <- ifelse(bioarch$sitetype == "mine", 1, 0)
bioarch$rural <- ifelse(bioarch$sitetype == "rural", 1, 0)
bioarch$villa <- ifelse(bioarch$sitetype == "villa", 1, 0)
bioarch$village <- ifelse(bioarch$sitetype == "village", 1, 0)

# province dummies
bioarch %>%
  count(province)# only instances with 5 or more manifestations
bioarch$Italia <- ifelse(bioarch$province == "Aemilia (Italia 8)" |
                         bioarch$province == "Apulia et Calabria (Italia 2)" |
                         bioarch$province == "Latium et Campania (Italia 1)", 
                         1, 0)
bioarch$Britannia <- ifelse(bioarch$province == "Britannia", 1, 0)
bioarch$ItaliaRoma <- ifelse(bioarch$Italia == 1 | 
                             bioarch$province == "Roma", 1, 0)
bioarch$Hispania <- ifelse(bioarch$province == "Tarraconensis" |
                           bioarch$province == "Baetica" |
                           bioarch$province == "Lusitania", 1, 0)
bioarch$Raetia <- ifelse(bioarch$province == "Raetia", 1, 0)
bioarch$Tarraconensis <- ifelse(bioarch$province == "Tarraconensis", 1, 0)
bioarch$Aegyptus <- ifelse(bioarch$province == "Aegyptus", 1, 0)
bioarch$Aquitania <- ifelse(bioarch$province == "Aquitania", 1, 0)
bioarch$Lugdunensis <- ifelse(bioarch$province == "Lugdunensis", 1, 0)
bioarch$Narbonensis <- ifelse(bioarch$province == "Narbonensis", 1, 0)
bioarch$Belgica <- ifelse(bioarch$province == "Belgica", 1, 0)
bioarch$Gallia <- ifelse(bioarch$province == "Aquitania" | 
                         bioarch$province == "Lugdunensis" |
                         bioarch$province == "Narbonensis" | 
                         bioarch$province == "Belgica", 1, 0)
bioarch$Arabia <- ifelse(bioarch$province == "Arabia Petraea", 1, 0)
bioarch$Baetica <- ifelse(bioarch$province == "Baetica", 1, 0)
bioarch$Dalmatia <- ifelse(bioarch$province == "Dalmatia", 1, 0)
bioarch$GermaniaI <- ifelse(bioarch$province == "Germania Inferior", 1, 0)
bioarch$GermaniaS <- ifelse(bioarch$province == "Germania Suiperior", 1, 0)
bioarch$Germania <- ifelse(bioarch$province == "Germania Inferior" |
                           bioarch$province == "Germania Suiperior", 1, 0)
bioarch$Lusitania <- ifelse(bioarch$province == "Lusitania", 1, 0)

# bodypart dummies
bioarch %>%
  count(bodypart)
bioarch$bone <- ifelse(bioarch$bodypart == "bone" | 
                       bioarch$bodypart == "bone and dentine collagen" | 
                       bioarch$bodypart == "bones & teeth" | 
                       bioarch$bodypart == "enamel, bone" | 
                       bioarch$bodypart == "ribs", 1, 0)
bioarch$enamel <- ifelse(bioarch$bodypart == "enamel" | 
                         bioarch$bodypart == "bones & teeth" | 
                         bioarch$bodypart == "enamel, bone" | 
                         bioarch$bodypart == "enamel, dentine" | 
                         bioarch$bodypart == "teeth" |
                         bioarch$bodypart == "teeth (M1 and M3)", 1, 0)

# isotope dummies
bioarch %>%
  count(isotopes)
bioarch$Oxygen <- ifelse(bioarch$isotopes == "(C, N,) O" |
												 bioarch$isotopes == "C, N, O" |
												 bioarch$isotopes == "C, N, O, Sr" |
												 bioarch$isotopes == "O" |
												 bioarch$isotopes == "O, C" |
												 bioarch$isotopes == "O, N" |
												 bioarch$isotopes == "Pb, Sr, O" |
												 bioarch$isotopes == "Sr (O, C)" |
												 bioarch$isotopes == "Sr, O", 1, 0)
bioarch$Strontium <- ifelse(bioarch$isotopes == "C, N, O , Sr" |
														bioarch$isotopes == "C, N, O, Sr" |
														bioarch$isotopes == "C, N, S, Sr, Ca" |
														bioarch$isotopes == "C, Sr" |
														bioarch$isotopes == "Pb, Sr, O" |
														bioarch$isotopes == "Sr" |
														bioarch$isotopes == "Sr (O, C)" |
														bioarch$isotopes == "Sr, O" |
														bioarch$isotopes == "Sr, O, N, C" |
														bioarch$isotopes == "Sr, O, Pb" |
														bioarch$isotopes == "Sr, Pb", 1, 0)

# time period dummies
bioarch %>%
  count(date_from)
bioarch %>%
  count(date_to)
bioarch$republic <- ifelse(bioarch$date_to < 0, 1, 0)
bioarch$principate <- ifelse(bioarch$date_from > -50 & 
														 bioarch$date_to < 250, 1, 0)
bioarch$LA <- ifelse(bioarch$date_from > 200, 1, 0)
bioarch$republic_inc <- ifelse(bioarch$date_from < 0, 1, 0)
bioarch$principate_inc <- ifelse((bioarch$date_from > -50 & 
																	bioarch$date_from < 250) |
                                 (bioarch$date_to > -50 & 
                                 	bioarch$date_to < 250), 1, 0)
bioarch$LA_inc <- ifelse(bioarch$date_to > 200, 1, 0)

# proportion-variables
bioarch <- bioarch %>%
  mutate(ratio_tot = men / women,
         mig_perc = non_locals / N,
         mig_f_pc = mig_women / N,
         mig_m_pc = mig_men / N,
         f_am_mi = mig_women / non_locals,
         mig_am_f = mig_women / women,
         m_am_mi = mig_men / non_locals,
         mig_am_m = mig_men / men,
         ratio_mig = mig_women / mig_men)

# Cleanup after divisions
# replace infinite values (result of division by zero) with NA
bioarch <- do.call(data.frame,
                lapply(bioarch,
                       function(x) replace(x, is.infinite(x), NA)))
# replace NaN values (result of 0 / 0) with NA
bioarch <- do.call(data.frame,
                lapply(bioarch,
                       function(x) replace(x, is.nan(x), NA)))
# change the newly created dataframe back into a tibble
bioarch <- as_tibble(bioarch)

#############################################################################
#                           2. Summary Statistics                           #
#############################################################################

# Create Table "Result Analysis Using Different Criteria"
criteria <- c("N <= 40", "N > 40", "town == 1", "town == 0", "port == 1", 
              "port == 0", "fort == 1", "fort == 0", "Britannia == 1", 
              "Britannia == 0", "Italia == 1", "Italia == 0", "roma == 1", 
              "roma == 0", "ItaliaRoma == 1", "ItaliaRoma == 0", 
              "Hispania == 1", "Hispania == 0", "enamel == 1", "enamel == 0", 
              "bone == 1", "bone == 0", "Strontium == 1", "Strontium == 0", 
              "Oxygen == 1", "Oxygen == 0", "republic == 1", "republic == 0", 
              "principate == 1", "principate == 0", "LA == 1", "LA == 0", 
              "republic_inc == 1", "republic_inc == 0", "principate_inc == 1", 
              "principate_inc == 0", "LA_inc == 1", "LA_inc == 0")

# Set up basic case ("total")
cases <- bioarch %>%
  summarise(
    case = "total",
    N = sum(N, na.rm = TRUE),
    f = sum(women, na.rm = TRUE),
    f_pc = f / N * 100,
    m = sum(men, na.rm = TRUE),
    m_pc = m / N * 100,
    ratio_tot = m / f,
    mig = sum(non_locals, na.rm = TRUE),
    mig_pc = mig / N * 100,
    f_mig = sum(mig_women, na.rm = TRUE),
    f_mig_pc = f_mig / N * 100,
    m_mig = sum(mig_men, na.rm = TRUE),
    m_mig_pc = m_mig / N * 100,
    f_am_mig = f_mig / mig * 100,
    mig_am_f = f_mig / f * 100,
    m_am_mig = m_mig / mig * 100,
    mig_am_m = m_mig / m * 100,
    ratio_mig = m_mig / f_mig
  )

# add mean case
cases <- add_row(cases, bioarch %>%
          summarise(
            case = "mean",
            N = mean(N, na.rm = TRUE),
            f = mean(women, na.rm = TRUE),
            f_pc = f / N * 100,
            m = mean(men, na.rm = TRUE),
            m_pc = m / N * 100,
            ratio_tot = m / f,
            mig = mean(non_locals, na.rm = TRUE),
            mig_pc = mig / N * 100,
            f_mig = mean(mig_women, na.rm = TRUE),
            f_mig_pc = f_mig / N * 100,
            m_mig = mean(mig_men, na.rm = TRUE),
            m_mig_pc = m_mig / N * 100,
            f_am_mig = f_mig / mig * 100,
            mig_am_f = f_mig / f * 100,
            m_am_mig = m_mig / mig * 100,
            mig_am_m = m_mig / m * 100,
            ratio_mig = m_mig / f_mig
          )
)

# add median case
cases <- add_row(cases, bioarch %>%
                   summarise(
                     case = "median",
                     N = median(N, na.rm = TRUE),
                     f = median(women, na.rm = TRUE),
                     f_pc = f / N * 100,
                     m = median(men, na.rm = TRUE),
                     m_pc = m / N * 100,
                     ratio_tot = m / f,
                     mig = median(non_locals, na.rm = TRUE),
                     mig_pc = mig / N * 100,
                     f_mig = median(mig_women, na.rm = TRUE),
                     f_mig_pc = f_mig / N * 100,
                     m_mig = median(mig_men, na.rm = TRUE),
                     m_mig_pc = m_mig / N * 100,
                     f_am_mig = f_mig / mig * 100,
                     mig_am_f = f_mig / f * 100,
                     m_am_mig = m_mig / mig * 100,
                     mig_am_m = m_mig / m * 100,
                     ratio_mig = m_mig / f_mig
                   )
)

# add all other cases by looping over condition in 'criteria'
for (i in criteria){
  cases <- add_row(cases, bioarch %>%
            filter(eval(parse(text = i))) %>%
            summarise(
              case = i,
              N = sum(N, na.rm = TRUE),
              f = sum(women, na.rm = TRUE),
              f_pc = f / N * 100,
              m = sum(men, na.rm = TRUE),
              m_pc = m / N * 100,
              ratio_tot = m / f,
              mig = sum(non_locals, na.rm = TRUE),
              mig_pc = mig / N * 100,
              f_mig = sum(mig_women, na.rm = TRUE),
              f_mig_pc = f_mig / N * 100,
              m_mig = sum(mig_men, na.rm = TRUE),
              m_mig_pc = m_mig / N * 100,
              f_am_mig = f_mig / mig * 100,
              mig_am_f = f_mig / f * 100,
              m_am_mig = m_mig / mig * 100,
              mig_am_m = m_mig / m * 100,
              ratio_mig = m_mig / f_mig
            )
  )
}

# create Latex table with some basic formatting
# 1. create copy
cases_output <- cases
# 2. rename columns (xxx = \begin{tabular}[b]{@{}r@{}}, yyy = \end{tabular})
cases_output <- rename(cases_output,					 
       "f (%)" = "f_pc",
       "m (%)" = "m_pc",
       "m:f" = "ratio_tot",
       "mig (%)" = "mig_pc",
       "f mig" = "f_mig",
       "f mig (%)" = "f_mig_pc",
       "m mig" = "m_mig",
       "m mig (%)" = "m_mig_pc",
       "xxx f \\ am. \\ mig yyy" = "f_am_mig",
       "xxx mig \\ am. \\ f yyy" = "mig_am_f",
       "xxx m \\ am. \\ mig yyy" = "m_am_mig",
       "xxx mig \\ am. \\ m yyy" = "mig_am_m",
       "xxx m:f \\ (mig) yyy" = "ratio_mig")
# 3. create formatted table
print(xtable(cases_output, type = "latex",
             caption = "Result Analysis Using Different Criteria",
             label = "tab:analysis",
             align = "rl|rrrrrrrrrrrrrrrrr",
             display = c("d", "s", "d", "d", "f", "d", "f", "f", 
                         "d", "f", "d", "f", "d", "f", "f", 
                         "f", "f", "f", "f")),
      include.rownames = FALSE,
			floating = FALSE,
      tabular.environment = "longtable",
      file = "cases.tex")

#############################################################################
#                           3. Regression Analyses                          #
#############################################################################

# 1. create regressions for time periods
periods <- lm(f_am_mi ~ republic + principate + LA + republic_inc + 
                principate_inc + LA_inc, data = bioarch)
rep <- lm(f_am_mi ~ republic, data = bioarch)
rep_inc <- lm(f_am_mi ~ republic_inc, data = bioarch)
princ <- lm(f_am_mi ~ principate, data = bioarch)
princ_inc <- lm(f_am_mi ~ principate_inc, data = bioarch)
LatAnt <- lm(f_am_mi ~ LA, data = bioarch)
LatAnt_inc <- lm(f_am_mi ~ LA_inc, data = bioarch)

# create stargazer table with all time period regressions
stargazer(rep, rep_inc, princ, princ_inc, LatAnt, LatAnt_inc, 
         periods,  # specify models to include in table
         type = "latex",  # specify encoding type
         align = TRUE,
         omit.stat = c("ser","f"),  # omit Residual Std. Error, F-Statistic  
         out = "time_periods.tex",  # specify output file
         title = "Regression Analysis of Time Periods",  # define title
         label = "reg:periods",  # define label
         column.sep.width = "-25pt",  # reduce column separator width
         font.size = "small",  # define font size
         no.space = TRUE,  # no white lines between independent variables
         float = TRUE,  # make floating table
         table.placement = "tb",  # set position to top or bottom of page
         covariate.labels = c("Republic", # set model...
                              "Rep. (incl.)",  #...
                              "Principate",   #...
                              "Princ. (incl.)",  #...
                              "Late Ant.",  #...
                              "LA (incl.)",  #...
                              "Constant"),   #...labels
         dep.var.labels = ("Percentage of Women Among Migrants"),  # 
         star.char = c("+", "*", "**", "***"),  # use good significance levels
         star.cutoffs = c(.1, .05, .01, .001),  # ditto
         notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust ...
         notes.append = FALSE)  # ...note explaining the significance levels

# 2. create regressions for site types
# NB: here, Gabii is town but in Stata part of Rome; hence slight differences

sites <- lm(f_am_mi ~ fort + port + town + village + roma + mine,
            data = bioarch)
forts <- lm(f_am_mi ~ fort, data = bioarch)
ports <- lm(f_am_mi ~ port, data = bioarch)
towns <- lm(f_am_mi ~ town, data = bioarch)
villages <- lm(f_am_mi ~ village, data = bioarch)
rome <- lm(f_am_mi ~ roma, data = bioarch)
mines <- lm(f_am_mi ~ mine, data = bioarch)
rurals <- lm(f_am_mi ~ rural, data = bioarch)  # not defined b/c singular.
villas <- lm (f_am_mi ~ villa, data = bioarch)  # not defined b/c singular.

# create stargazer table with all site type regressions
stargazer(forts, ports, towns, villages, rome, mines, 
          sites,  # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "site_types.tex",  # specify output file
          title = "Regression Analysis of Site Types",  # define title
          label = "reg:sites",  # define label
          column.sep.width = "-28pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Fort", "Port",  # set model...
                               "Town",   #...
                               "Village",  #...
                               "Rome",  #...
                               "Mine",  #...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good significance lvls
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# 3. create regressions for provinces
provinces <- lm(f_am_mi ~ Italia + roma + ItaliaRoma + Britannia + 
                  Tarraconensis + Hispania + Raetia + Aquitania + 
                  Dalmatia + GermaniaI,
            data = bioarch)
provinces1 <- lm(f_am_mi ~ Italia + roma + ItaliaRoma + Britannia + Raetia,
                 data = bioarch)
provinces2 <- lm(f_am_mi ~ Tarraconensis + Hispania + Aquitania + 
                 Dalmatia + GermaniaI,
                 data = bioarch)
Italia <- lm(f_am_mi ~ Italia, data = bioarch)
Roma <- lm(f_am_mi ~ roma, data = bioarch)
ItaliaRoma <- lm(f_am_mi ~ ItaliaRoma, data = bioarch)
Britannia <- lm(f_am_mi ~ Britannia, data = bioarch)
Tarraconensis <- lm(f_am_mi ~ Tarraconensis, data = bioarch)
Hispania <- lm(f_am_mi ~ Hispania, data = bioarch)
Raetia <- lm(f_am_mi ~ Raetia, data = bioarch)
Aegyptus <- lm (f_am_mi ~ Aegyptus, data = bioarch)  # not defined b/c sing.
Aquitania <- lm (f_am_mi ~ Aquitania, data = bioarch)
Arabia <- lm (f_am_mi ~ Arabia, data = bioarch)
Baetica <- lm (f_am_mi ~ Baetica, data = bioarch)  # not defined b/c sing.
Dalmatia <- lm (f_am_mi ~ Dalmatia, data = bioarch)
GermaniaI <- lm (f_am_mi ~ GermaniaI, data = bioarch)
GermaniaS <- lm (f_am_mi ~ GermaniaS, data = bioarch)  # not defined b/c sing.
Germania <- lm (f_am_mi ~ Germania, data = bioarch)
Lusitania <- lm (f_am_mi ~ Lusitania, data = bioarch)  # not defined b/c sing.

# create stargazer table with all province regressions
stargazer(Italia, Roma, ItaliaRoma, Britannia, Tarraconensis, Hispania, 
          Raetia, Aquitania, Dalmatia, GermaniaI, 
          provinces,  # specify models to include in table
          type = "text",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "provinces.txt",  # specify output file
          title = "Regression Analysis of Provinces",  # define title
          label = "reg:provinces",  # define label
          column.sep.width = "-23pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Italia", "Rome",  # set model...
                               "Italia w/ Rome",   #...
                               "Britannia",  #...
                               "Tarraconensis",  #...
                               "Hispania",  #...
                               "Raetia",  #...
                               "Aquitania",  #...
                               "Dalmatia",  #...
                               "Germania Inf.",  #...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# create stargazer table with 1. half of province regressions so it fits page
stargazer(Italia, Roma, ItaliaRoma, Britannia, Raetia, 
          provinces1,  # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "provinces1.tex",  # specify output file
          title = "Regression Analysis of Provinces (1 of 2)",  # define title
          label = "reg:provinces1",  # define label
          column.sep.width = "-22pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Italia", "Rome",  # set model...
                               "Italia + Rome",   #...
                               "Britannia",  #...
                               "Raetia",  #...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adj...
          notes.append = FALSE)  # ...note explaining the significance levels

# create stargazer table with 2. half of province regressions so it fits page
stargazer(Tarraconensis, Hispania, Aquitania, Dalmatia, GermaniaI, 
          provinces2,  # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "provinces2.tex",  # specify output file
          title = "Regression Analysis of Provinces (2 of 2)",  # define title
          label = "reg:provinces2",  # define label
          column.sep.width = "-22pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Tarraconensis",  # set model...
                               "Hispania",  #...
                               "Aquitania",  #...
                               "Dalmatia",  #...
                               "Germania Inf.",  #...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# 4. create regressions for body part
bodyparts <- lm(f_am_mi ~ bone + enamel,
                data = bioarch)
bone <- lm(f_am_mi ~ bone, data = bioarch)
enamel <- lm(f_am_mi ~ enamel, data = bioarch)

# create stargazer table with all body part regressions
stargazer(bone, enamel, bodyparts,  # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "bodyparts.tex",  # specify output file
          title = "Regression Analysis of Body Parts",  # define title
          label = "reg:bodyparts",  # define label
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Bone", "Enamel",  # set model...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# 5. create regressions for isotopes
isotopes <- lm(f_am_mi ~ Oxygen + Strontium,
              data = bioarch)
Oxygen <- lm(f_am_mi ~ Oxygen, data = bioarch)
Strontium <- lm(f_am_mi ~ Strontium, data = bioarch)

# create stargazer table with all isotope regressions
stargazer(Oxygen, Strontium, isotopes,  # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "isotopes.tex",  # specify output file
          title = "Regression Analysis of Isotopes",  # define title
          label = "reg:isotopes",  # define label
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Oxygen", "Strontium",  # set model...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# create stargazer table with all body part and isotope regressions
stargazer(bone, enamel, bodyparts, Oxygen, Strontium, 
					isotopes,  # specify models to include in table
					type = "latex",  # specify encoding type
					align = TRUE,
					omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
					out = "bodytopes.tex",  # specify output file
					title = "Regression Analysis of Body Parts and Isotopes",  # title
					label = "reg:bodytopes",  # define label
					column.sep.width = "-25pt",  # reduce column separator width
					font.size = "small",  # define font size
					no.space = TRUE,  # no white lines between independent variables
					float = TRUE,  # make floating table
					table.placement = "tb",  # set position to top or bottom of page
					covariate.labels = c("Bone", "Enamel",  # set model...
															 "Oxygen",  #...
															 "Strontium",  #...
															 "Constant"),   #...labels
					dep.var.labels = ("Percentage of Women Among Migrants"),  # 
					star.char = c("+", "*", "**", "***"),  # use good sign. levels
					star.cutoffs = c(.1, .05, .01, .001),  # ditto
					notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
					notes.append = FALSE)  # ...note explaining the significance levels

# 6. create regression for group size
groups <- lm(f_am_mi ~ N + men + women + non_locals, data = bioarch)
samplesize <- lm(f_am_mi ~ N, data = bioarch)
men <- lm(f_am_mi ~ men, data = bioarch)
women <- lm(f_am_mi ~ women, data = bioarch)
nonlocals <- lm(f_am_mi ~ non_locals, data = bioarch)

cor.test(bioarch$N, bioarch$women)  # calculating correlation

# create stargazer table with group size regressions
stargazer(samplesize, men, women, nonlocals, groups,  # specify models
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "groupsize.tex",  # specify output file
          title = "Regression Analysis of Group Sizes",  # define title
          label = "reg:groupsizes",  # define label
          column.sep.width = "-16pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("N",  # set model...
                               "Number of Men",  # ...
                               "Number of Women",  # ...
                               "Number of Nonlocals",  # ...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# 7. create regression for publication date
pubdate <- lm(f_am_mi ~ pub_year, data = bioarch)

# create stargazer table with publication date regression
stargazer(pubdate, # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "pubdate.tex",  # specify output file
          title = "Regression Analysis of Publication Date",  # define title
          label = "reg:publication",  # define label
          font.size = "small",  # define font size
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("Publication Date",  # set model...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# create stargazer table with all body part, pubdate & isotope regressions
stargazer(bone, enamel, bodyparts, Oxygen, Strontium, isotopes,
					pubdate,  # specify models to include in table
					type = "latex",  # specify encoding type
					align = TRUE,
					omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
					out = "bodytopedate.tex",  # specify output file
					title = 
					"Regression Analysis of Body Parts, Isotopes, and Publication Date",
					label = "reg:bodytopedate",  # define label
					column.sep.width = "-22pt",  # reduce column separator width
					font.size = "small",  # define font size
					no.space = TRUE,  # no white lines between independent variables
					float = TRUE,  # make floating table
					table.placement = "tb",  # set position to top or bottom of page
					covariate.labels = c("Bone", "Enamel",  # set model...
															 "Oxygen",  #...
															 "Strontium",  #...
															 "Pub. Date",  #...
															 "Constant"),   #...labels
					dep.var.labels = ("Percentage of Women Among Migrants"),  # 
					star.char = c("+", "*", "**", "***"),  # use good sign. levels
					star.cutoffs = c(.1, .05, .01, .001),  # ditto
					notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
					notes.append = FALSE)  # ...note explaining the significance levels

# 8. create kitchen-sink regressions for everything that was significant
kitchen <- lm(f_am_mi ~ N + women + port, data = bioarch)
Nwomen <- lm(f_am_mi ~ N + women, data = bioarch)
Nports <- lm(f_am_mi ~ N + port, data = bioarch)
womenports <- lm(f_am_mi ~ women + port, data = bioarch)

# create stargazer table with kitchen sink regressions
stargazer(Nwomen, ports, Nports, womenports,
          kitchen, # specify models to include in table
          type = "latex",  # specify encoding type
          align = TRUE,
          omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
          out = "kitchen.tex",  # specify output file
          title = "Regression Analysis of Significant Variables",  # title
          label = "reg:kitchen",  # define label
          column.sep.width = "-13pt",  # reduce column separator width
          font.size = "small",  # define font size
          no.space = TRUE,  # no white lines between independent variables
          float = TRUE,  # make floating table
          table.placement = "tb",  # set position to top or bottom of page
          covariate.labels = c("N",  # set model...
                               "Number of Women",  #...
                               "Port",  #...
                               "Constant"),   #...labels
          dep.var.labels = ("Percentage of Women Among Migrants"),  # 
          star.char = c("+", "*", "**", "***"),  # use good sign. levels
          star.cutoffs = c(.1, .05, .01, .001),  # ditto
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
          notes.append = FALSE)  # ...note explaining the significance levels

# 9. test the port-variable against other possible influences
port_republic <- lm(f_am_mi ~ port + republic, data = bioarch)

port_rome <- lm(f_am_mi ~ port + roma, data = bioarch)
summary(port_rome)

port_dalmatia  <- lm(f_am_mi ~ port + Dalmatia, data = bioarch)

port_enamel  <- lm(f_am_mi ~ port + enamel, data = bioarch)

port_strontium  <- lm(f_am_mi ~ port + Strontium, data = bioarch)

sink <- lm(f_am_mi ~ port + republic + roma + Dalmatia + enamel + Strontium, 
					 data = bioarch)

# create stargazer table with kitchen sink regressions
stargazer(ports, port_republic, port_rome, port_dalmatia, port_enamel, 
					port_strontium, sink, # specify models to include in table
					type = "latex",  # specify encoding type
					align = TRUE,
					omit.stat = c("ser","f"),  # omit Residual Std Error, F-Statistic  
					out = "sink.tex",  # specify output file
					title = "Regression Analysis of Significant Variables",  # title
					label = "reg:sink",  # define label
					column.sep.width = "-26pt",  # reduce column separator width
					font.size = "small",  # define font size
					no.space = TRUE,  # no white lines between independent variables
					float = TRUE,  # make floating table
					table.placement = "tb",  # set position to top or bottom of page
					covariate.labels = c("Port",  # set model...
															 "Republic",  #...
															 "Rome",  #...
															 "Dalmatia",  #...
															 "Enamel",  #...
															 "Strontium",  #...
															 "Constant"),   #...labels
					dep.var.labels = ("Percentage of Women Among Migrants"),  # 
					star.char = c("+", "*", "**", "***"),  # use good sign. levels
					star.cutoffs = c(.1, .05, .01, .001),  # ditto
					notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),  # adjust...
					notes.append = FALSE)  # ...note explaining the significance levels

#############################################################################
#                           4. Chi-Squared Analyses                         #
#############################################################################

chi2 <- cases  # copy tibble

# remove unused columns
chi2 <- select(chi2, -c(f_mig_pc, m_mig_pc, mig_am_f, mig_am_m))

# rearrange columns
chi2 <- chi2 %>%
	relocate(f_am_mig, .after = f_mig)

# Calculate chi2 and p, & delete helper columns right after calculations
chi2 <- chi2 %>%
	mutate(
		e_f = (f + m) * (f + f_mig) / (f + m + f_mig + m_mig),
		e_m = (f + m) * (m + m_mig) / (f + m + f_mig + m_mig),
		e_fmi = (f_mig + m_mig) * (f + f_mig) / (f + m + f_mig + m_mig), 
		e_mmi = (f_mig + m_mig) * (m + m_mig) / (f + m + f_mig + m_mig),
		a = (f - e_f)^2 / e_f,
		b = (m - e_m)^2 / e_m,
		c = (f_mig - e_fmi)^2 / e_fmi,
		d = (m_mig - e_mmi)^2 / e_mmi,
		chi2 = a + b + c + d,
		p = pchisq(chi2, 1, lower.tail = FALSE),
		e_f = NULL,
		e_m = NULL,
		e_fmi = NULL, 
		e_mmi = NULL,
		a = NULL,
		b = NULL,
		c = NULL,
		d = NULL,
	) 

# create Latex table with some basic formatting
# 1. create copy
chi2_output <- chi2
# 2. rename columns 
chi2_output <- rename(chi2_output,					 
											 "f (%)" = "f_pc",
											 "m (%)" = "m_pc",
											 "m:f" = "ratio_tot",
											 "mig (%)" = "mig_pc",
											 "f mig" = "f_mig",
											 "m mig" = "m_mig")
# 3. create formatted table
print(xtable(chi2_output, type = "latex",
						 caption = "Chi2-Analyses of Gender Ratios",
						 label = "tab:chi2",
						 align = "rl|rrrrrrrrrrrrrrr",
						 display = c("d", "s", "d", "d", "f", "d", "f", "f", 
						 						"d", "f", "d", "f", "d", "f", "f", 
						 						"f", "f"),
						 digits = c(0, 0, 0, 0, 2, 0, 2, 2,
						 					  0, 2, 0, 2, 0, 2, 2, 
						 					  2, 3)),
			include.rownames = FALSE,
			floating = FALSE,
			tabular.environment = "longtable",
			file = "chi2.tex")
