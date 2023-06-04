library(tidyverse)
library(readxl)

file_path_p <- "C:/Users/LOKI/Downloads/Git hub rep/Energy -Renewable/Table_1.2_Primary_Energy_Production_by_Source.xlsx"
file_path_c <- "C:/Users/LOKI/Downloads/Git hub rep/Energy -Renewable/Table_1.3_Primary_Energy_Consumption_by_Source.xlsx"
file_path_e <- "C:/Users/LOKI/Downloads/Git hub rep/Energy -Renewable/Table_11.1_Carbon_Dioxide_Emissions_From_Energy_Consumption_by_Source.xlsx"


df_p <- tibble(read_excel(file_path_p, sheet="Annual Data", na = c("Not Available"),
                                col_names = FALSE, skip = 12)) %>%
  set_names(c('Year','Coal','Natural Gas (Dry)','Crude Oil','Natural Gas Plant Liquids','Total Fossil Fuels',
              'Nuclear Electric Power','Hydroelectric Power','Geothermal Energy', 'Solar Energy','Wind Energy',
              'Biomass Energy','Total Renewable Energy','Total Primary Energy'))


df_c <- tibble(read_excel(file_path_c, sheet="Annual Data", na = c("Not Available"),
                                col_names = FALSE, skip = 12)) %>%
  set_names(c('Year','Coal','Natural Gas','Petroleum','Total Fossil Fuels','Nuclear Electric Power',
              'Hydroelectric Power','Geothermal Energy', 'Solar Energy','Wind Energy',
              'Biomass Energy','Total Renewable Energy','Total Primary Energy'))


df_e <- tibble(read_excel(file_path_e, sheet="Annual Data", na = c("Not Available"),
                          col_names = FALSE, skip = 12)) %>%
  set_names(c('Year','Coal','Natural Gas','Aviation','FuelOil','Hydrocarbon',
              'Jet Fuel','Kerosene', 'Lubricants','Motor Gasoline',
              'PetroCoke','Residual Fuel','OtherPetro','Petro','Total'))


#-----------------------------------------------------------------------

all_p <- df_p %>%
  group_by(Year) %>%
  dplyr::summarize(Renewable = sum(`Total Renewable Energy`, na.rm=TRUE),
                   NonRenewable = sum(`Total Fossil Fuels`, na.rm=TRUE),
                   Total_Production = sum(`Nuclear Electric Power`+`Total Primary Energy`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

all_p %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = NonRenewable, color = "Non Renewable"), linewidth = 1.2) +
  geom_line(aes(y = Renewable, color = "Renewable"), linewidth = 1.2) +
  labs(title = "Past Energy Generation in US", x = "Year", y = "Quadrillion Btu", color = "Source Type") + theme_bw()

all_p %>%
  tidyr::gather(key = "variable", value = "value", -Year, -Total_Production) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Total Energy generation in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

all_p %>%
  tidyr::gather(key = "variable", value = "value", -Year, -Total_Production) %>%
  transform(perc = value / ave(value, FUN = sum)) %>%
  ggplot(aes(x = Year, y = perc, fill = variable)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "% Share of Total Energy generation in Quadrillion BTU",
       x = "Year", y = "", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------

NonRenew <- df_p %>%
  group_by(Year) %>%
  dplyr::summarize(Coal = sum(`Coal`, na.rm=TRUE),
                   NGD = sum(`Natural Gas (Dry)`, na.rm=TRUE),
                   CrO = sum(`Crude Oil`, na.rm=TRUE),
                   NGL = sum(`Natural Gas Plant Liquids`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

NonRenew %>%
  tidyr::gather(key = "variable", value = "value", -Year) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Total Non Renewable Energy Generation in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------

renewable_p <- df_p %>%
  group_by(Year) %>%
  dplyr::summarize(Hydro = sum(`Hydroelectric Power`, na.rm=TRUE),
                   Geo = sum(`Geothermal Energy`, na.rm=TRUE),
                   Solar = sum(`Solar Energy`, na.rm=TRUE),
                   Wind = sum(`Wind Energy`, na.rm=TRUE),
                   Bio = sum(`Biomass Energy`, na.rm=TRUE),
                   Nuc = sum(`Nuclear Electric Power`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

renewable_p %>%
  tidyr::gather(key = "variable", value = "value", -Year) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Total Renewable Energy Generation in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

all_c <- df_c %>%
  group_by(Year) %>%
  dplyr::summarize(Fossil_Fuels = sum(`Total Fossil Fuels`, na.rm=TRUE),
                   Renewable = sum(`Total Renewable Energy`, na.rm=TRUE),
                   Nuclear = sum(`Nuclear Electric Power`, na.rm=TRUE),
                   Total = sum(`Total Primary Energy`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

all_c %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Fossil_Fuels, color = "Fossil Fuels"), size = 1.2) +
  geom_line(aes(y = Renewable, color = "Renewable"), size = 1.2) +
  geom_line(aes(y = Nuclear, color = "Nuclear"), size = 1.2) +
  labs(title = "Past Energy Consumption in US", x = "Year", y = "Quadrillion Btu", color = "Source Type") + theme_bw()

all_c %>%
  tidyr::gather(key = "variable", value = "value", -Year, -Total) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Total Energy Consumption in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

all_c %>%
  tidyr::gather(key = "variable", value = "value", -Year, -Total) %>%
  transform(perc = value / ave(value, FUN = sum)) %>%
  ggplot(aes(x = Year, y = perc, fill = variable)) +
  geom_area(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "% Share of Total Energy Consumption in Quadrillion BTU",
       x = "Year", y = "", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------

fossil_c <- df_c %>%
  group_by(Year) %>%
  dplyr::summarize(Coal = sum(`Coal`, na.rm=TRUE),
                   NGD = sum(`Natural Gas`, na.rm=TRUE),
                   Pet = sum(`Petroleum`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

fossil_c %>%
  tidyr::gather(key = "variable", value = "value", -Year) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Fossil Fuel Consumption in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------

renewable_c <- df_c %>%
  group_by(Year) %>%
  dplyr::summarize(Hydro = sum(`Hydroelectric Power`, na.rm=TRUE),
                   Geo = sum(`Geothermal Energy`, na.rm=TRUE),
                   Solar = sum(`Solar Energy`, na.rm=TRUE),
                   Wind = sum(`Wind Energy`, na.rm=TRUE),
                   Bio = sum(`Biomass Energy`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

renewable_c %>%
  tidyr::gather(key = "variable", value = "value", -Year) %>%
  ggplot(aes(x = Year, y = value, fill = variable)) +
  geom_area(position = "stack") +
  labs(title = "Renewable Energy Consumption in Quadrillion BTU",
       x = "Year", y = "Energy", fill = "Source Type") + theme_bw()

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#Emmission

all_e <- df_e %>%
  group_by(Year) %>%
  dplyr::summarize(Total_Emissions = sum(`Total`, na.rm=TRUE)) %>%
  arrange(Year) %>% print(n=50)

#-----------------------------------------------------------------------

temp <- all_p %>%
  select(1:4) %>% set_names('Year','Renewable_Production','Non-Renewable_Production','Total_Production') %>%
  left_join (all_e %>% select(1,2) %>% set_names('Year','Total_Emmission'),
             join_by (Year==Year)) %>% na.omit() %>% as.data.frame()

temp %>% ggplot(aes(x = Year)) +
  geom_line(aes(y = Total_Emmission, color = "Total_Emmission"), size = 1.2) +
  labs(title = "US History", x = "Year", y = "Quadrillion Btu", color = "Source Type") + theme_bw()

temp %>% ggplot(aes(x = Year)) +
  geom_line(aes(y = Total_Production, color = "Total_Production"), size = 1.2) +
  labs(title = "US History", x = "Year", y = "Quadrillion Btu", color = "Source Type") + theme_bw()


#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  geom_line(aes(y = Total_Production, color = "Total_Production")) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission")) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission") +
  theme_classic() +
  theme(legend.position = "top")

#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  # geom_line(aes(y = Total_Production, color = "Total_Production")) +
  geom_line(aes(y = `Non-Renewable_Production`, color = "Non-Renewable_Production")) +
  geom_line(aes(y = Renewable_Production, color = "Renewable_Production")) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission")) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission") +
  theme_classic() +
  theme(legend.position = "top")

#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission")) +
  labs(x = "Year", y = "Total_Production", colour="") +
  theme_classic()+
  theme(legend.position = "top")

#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production", color="") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') + 
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 1997, linetype='dashed', color='navy') +
  annotate("text", x = 1997, y = 85, label = "Kyoto \nProtocol", color='black') +
  geom_vline(xintercept = 2012, linetype='dashed', color='navy') +
  annotate("text", x = 2012, y = 85, label = "Target \nYear 2012", color='black') +
  geom_segment(aes(x = 1990, y = 5037.901/75, xend = 2012, yend = 5037.901*0.95/75), linetype = "dashed", color='red')+
  annotate("text", x = 2012, y = 5037.901*0.95/75, label = "5% reduction \nto 1990 levels", color='red', hjust=-0.1)

#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') + 
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2012, linetype='dashed', color='navy') +
  annotate("text", x = 2012, y = 85, label = "Doha \nAmmendment", color='black') +
  geom_vline(xintercept = 2020, linetype='dashed', color='navy') +
  annotate("text", x = 2020, y = 85, label = "Target \nYear 2020", color='black') +
  geom_segment(aes(x = 1990, y = 5037.901/75, xend = 2020, yend = 5037.901*0.82/75), linetype = "dashed", color='red')+
  annotate("text", x = 2020, y = 5037.901*0.82/75, label = "18% reduction \nto 1990 levels", color='red', hjust=1.1, vjust=-1)

#-----------------------------------------------------------------------

ggplot(temp, aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') +
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2018, linetype='dashed', color='navy') +
  annotate("text", x = 2018, y = 85, label = "Paris \nAccords", color='black') +
  geom_vline(xintercept = 2030, linetype='dashed', color='navy') +
  annotate("text", x = 2030, y = 85, label = "Target \nYear 2030", color='black') +
  geom_segment(aes(x = 1990, y = 5037.901/75, xend = 2030, yend = 5037.901*0.60/75), linetype = "dashed", color='red')+
  annotate("text", x = 2020, y = 5037.901*0.60/75, label = "40% reduction \nto 1990 levels", color='red', hjust=-0.3, vjust=-1.1)
  # geom_point(data = temp[which.max(temp$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75), color = "red", size = 3) +
  # geom_text(data = temp[which.max(temp$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75, label = paste0("Max value: ", Total_Emmission," in ",Year)), hjust = 1.2, vjust = 0.5, size = 4, color = "red")

#-----------------------------------------------------------------------

temp %>%
  ggplot(aes(x = Year)) +
  # geom_line(aes(y = Total_Production, color = "Total_Production")) +
  # geom_line(aes(y = Fossil_Fuels, color = "Fossil_Fuels")) +
  # geom_line(aes(y = Renewable, color = "Renewable")) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission", colour="") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') +
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2018, linetype='dashed', color='navy') +
  annotate("text", x = 2018, y = 85, label = "Paris \nAccords", color='black') +
  geom_vline(xintercept = 2030, linetype ='dashed', color='navy') +
  annotate("text", x = 2030, y = 85, label = "Target \nYear", color='black') +
  # geom_segment(aes(x = 1990, y = 5037.901/75, xend = 2030, yend = 5037.901*0.60/75), linetype = "dashed") +
  geom_hline(yintercept = 5037.901*0.60/75, linetype='dashed', color='red')+
  # geom_point(data = temp_p[which.max(temp_p$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75), color = "red", size = 3) +
  # geom_text(data = temp_p[which.max(temp_p$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75, label = paste0("Max value: ", Total_Emmission," in ",Year)), hjust = 1.2, vjust = 0.5, size = 4, color = "red") +
  # geom_point(data = temp_p[which.min(temp_p$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75), color = "red", size = 3) +
  # geom_text(data = temp_p[which.min(temp_p$Total_Emmission), ], aes(x = Year, y = Total_Emmission/75, label = paste0("Max value: ", Total_Emmission," in ",Year)), hjust = 1.2, vjust = 0.5, size = 4, color = "red")+
  geom_polygon(data = data.frame(Year = c(2021, 2030, 2030), Total = c(4902.501/75, 1.1*4187/75, 0.9*4187/75)),
               aes(x = Year, y = Total), fill = "red",alpha = 0.3)+
  annotate("text", x = 2023, y = 68, label = "Forecast", color='red')+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 4187/75), linetype = "dashed", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 1.1*4187/75), linetype = "dotted", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.9*4187/75), linetype = "dotted", color = "red")+
  annotate("text", x = 2028, y = 42, label = "Target", color='red')

#-----------------------------------------------------------------------

temp %>%
  ggplot(aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission", colour="") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') +
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2018, linetype='dashed', color='navy') +
  annotate("text", x = 2018, y = 85, label = "Paris \nAccords", color='black') +
  geom_vline(xintercept = 2030, linetype ='dashed', color='navy') +
  annotate("text", x = 2030, y = 85, label = "Target \nYear", color='black') +
  geom_hline(yintercept = 5037.901*0.60/75, linetype='dashed', color='red')+
  geom_polygon(data = data.frame(Year = c(2021, 2030, 2030), Total = c(4902.501/75, 0.9*1.1*4187/75, 0.9*0.9*4187/75)),
               aes(x = Year, y = Total), fill = "orange",alpha = 0.3)+
  annotate("text", x = 2023, y = 68, label = "Forecast 2", color='red')+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.9*4187/75), linetype = "dashed", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.9*1.1*4187/75), linetype = "dotted", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.9*0.9*4187/75), linetype = "dotted", color = "red")+
  annotate("text", x = 2028, y = 42, label = "Target", color='red')

#-----------------------------------------------------------------------

temp %>%
  ggplot(aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission", colour="") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') +
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2018, linetype='dashed', color='navy') +
  annotate("text", x = 2018, y = 85, label = "Paris \nAccords", color='black') +
  geom_vline(xintercept = 2030, linetype ='dashed', color='navy') +
  annotate("text", x = 2030, y = 85, label = "Target \nYear", color='black') +
  geom_hline(yintercept = 5037.901*0.60/75, linetype='dashed', color='red')+
  geom_polygon(data = data.frame(Year = c(2021, 2030, 2030), Total = c(4902.501/75, 0.8*1.1*4187/75, 0.8*0.9*4187/75)),
               aes(x = Year, y = Total), fill = "yellow",alpha = 0.3)+
  annotate("text", x = 2023, y = 68, label = "Forecast 3", color='red')+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.8*4187/75), linetype = "dashed", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.8*1.1*4187/75), linetype = "dotted", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.8*0.9*4187/75), linetype = "dotted", color = "red")+
  annotate("text", x = 2028, y = 42, label = "Target", color='red')

#-----------------------------------------------------------------------

temp %>%
  ggplot(aes(x = Year)) +
  scale_y_continuous(name = "Total_Production", sec.axis = sec_axis(~ . * 75, name = "Total_Emmission")) +
  geom_line(aes(y = Total_Emmission / 75, color = "Total_Emmission"), linewidth=1.2) +
  labs(x = "Year", y = "Total_Production, Total_Consumption and Total_Emmission", colour="") +
  theme_classic() +
  theme(legend.position = "top") +
  geom_vline(xintercept = 1990, linetype='dashed', color='grey') +
  annotate("text", x = 1990, y = 85, label = "Reference", color='grey') +
  geom_vline(xintercept = 2018, linetype='dashed', color='navy') +
  annotate("text", x = 2018, y = 85, label = "Paris \nAccords", color='black') +
  geom_vline(xintercept = 2030, linetype ='dashed', color='navy') +
  annotate("text", x = 2030, y = 85, label = "Target \nYear", color='black') +
  geom_hline(yintercept = 5037.901*0.60/75, linetype='dashed', color='red')+
  geom_polygon(data = data.frame(Year = c(2021, 2030, 2030), Total = c(4902.501/75, 0.7*1.1*4187/75, 0.7*0.9*4187/75)),
               aes(x = Year, y = Total), fill = "lightgreen",alpha = 0.3)+
  annotate("text", x = 2023, y = 68, label = "Forecast 4", color='red')+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.7*4187/75), linetype = "dashed", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.7*1.1*4187/75), linetype = "dotted", color = "red")+
  geom_segment(aes(x = 2021, y = 4902.501/75, xend = 2030, yend = 0.7*0.9*4187/75), linetype = "dotted", color = "red")+
  annotate("text", x = 2028, y = 42, label = "Target", color='red')
