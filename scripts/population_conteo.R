install.packages('here')
library(here)
library(haven)
library(foreign)
path<-here("data", "municipal_population", "inegi","population_conteo")


# path<-here("data", "municipal_population", "pop2000_and_2010.dta")
# pop2000_2010<- read_dta(path)
# pop2000 <- subset(pop2000_2010, yearcenso==2000)
# pop2010 <- subset(pop2000_2010, yearcenso==2010)
# 
# pop2010 %>%
#   select(yearcenso, municipio,pob)
# 
# pop2000 %>%
#   select(yearcenso, municipio,pob)

install.packages('xlsx', type='source', repos='http://cran.rstudio.com') 
path<-here("data", "inequality_poverty","gini.xlsx")
library(openxlsx)
gini2010 <- read.xlsx(path, sheet = "2010")
gini2015 <- read.xlsx(path, sheet = "2015")

  colnames(gini2010)[3] <- "id_municipal_gini"
  colnames(gini2010)[4]<- "name_muni_gini"
  colnames(gini2010)[5] <- "gini"
  colnames(gini2010)[6] <- "income_ratio"
  colnames(gini2015)[3] <- "id_municipal_gini"
  colnames(gini2015)[4]<- "name_muni_gini"
  colnames(gini2015)[5] <- "gini"
  colnames(gini2015)[6] <- "income_ratio"
  gini2010<-select( gini2010, -c("Clave.de.entidad", "Entidad.federativa") )
  gini2015<-select( gini2015, -c("Clave.de.entidad", "Entidad.federativa") )
  
# for (datagini in (dfs)) {
#   colnames(datagini)[3] <- "id_municipal_gini"
#   colnames(datagini)[4]<- "name_muni_gini"
#   colnames(datagini)[5] <- "gini"
#   colnames(datagini)[6] <- "income_ratio"
# }
  
  
  ###### REZAGO #########
  
  path<-here("data", "inequality_poverty","IRS_2000_2015_vf.xlsx")
  irs <- read.xlsx(path, sheet = "Municipios", startRow = 5)
  colnames(irs)[5] <- "pop2000"
  colnames(irs)[6] <- "pop2005"
  colnames(irs)[7] <- "pop2010"
  colnames(irs)[8] <- "pop2015"
  irs <- subset(irs[c(-9:-20)])
  colnames(irs)[9] <- "pop_no_healthserv00"
  colnames(irs)[10] <- "pop_no_healthserv05"
  colnames(irs)[11] <- "pop_no_healthserv10"
  colnames(irs)[12] <- "pop_no_healthserv15"
  irs <- subset(irs[c(-13:-40)])
  colnames(irs)[13] <- "irs00"
  colnames(irs)[14] <- "irs05"
  colnames(irs)[15] <- "irs10"
  colnames(irs)[16] <- "irs15"
  colnames(irs)[17] <- "degree00"
  colnames(irs)[18] <- "degree05"
  colnames(irs)[19] <- "degree10"
  colnames(irs)[20] <- "degree15"
  colnames(irs)[21] <- "rank00"
  colnames(irs)[22] <- "rank05"
  colnames(irs)[23] <- "rank10"
  colnames(irs)[24] <- "rank15"
  colnames(irs)[1] <- "cve_edo"
  colnames(irs)[2] <- "state_name"
  colnames(irs)[3] <- "municipal_id"
  colnames(irs)[4] <- "municipal_name"
  
irs$pop2000<-as.numeric(irs$pop2000, na.rm=TRUE) 
irs$pop2005<-as.numeric(irs$pop2005) 
irs$pop2010<-as.numeric(irs$pop2010) 
irs$pop2015<-as.numeric(irs$pop2015) 
irs$pop_no_healthserv00<- as.numeric(irs$pop_no_healthserv00)
irs$pop_no_healthserv05<- as.numeric(irs$pop_no_healthserv05)
irs$pop_no_healthserv10<- as.numeric(irs$pop_no_healthserv10)
irs$pop_no_healthserv15<- as.numeric(irs$pop_no_healthserv15)
irs$irs00<-as.numeric(irs$irs00) 
irs$irs05<-as.numeric(irs$irs05) 
irs$irs10<-as.numeric(irs$irs10) 
irs$irs15<-as.numeric(irs$irs15) 

                  