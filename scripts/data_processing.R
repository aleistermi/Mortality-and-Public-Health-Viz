## Pre-processing data ####
install.packages("foreign")
install.packages('utf8')
install.packages('gtools')
install.packages('dplyr')
install.packages('stringr')
install.packages('chron')
install.packages("here")
library(foreign)
library(utf8)
library(gtools)
library(stringr)
library(chron)
library(dplyr)
library(hms)
library(car)
library(stringi)
library(anytime)
library(here)
## Data Cleaning ####
# I create dataset paths and names. Later come back and create strings with a loop! Or a function...
dataset_paths<- list('defunciones_base_datos_2017_dbf','defunciones_base_datos_2016_dbf', 'defunciones_base_datos_2015_dbf', 
                            'defunciones_base_datos_2014_dbf', 'defunciones_base_datos_2013_dbf', 'defunciones_base_datos_2012_dbf', 
                            'defunciones_base_datos_2011_dbf','defunciones_base_datos_2010_dbf', 'defunciones_base_datos_2009_dbf',
                            'defunciones_base_datos_2008_dbf', 'defunciones_base_datos_2007_dbf', 'defunciones_base_datos_2006_dbf',
                            'defunciones_base_datos_2005_dbf', 'defunciones_base_datos_2004_dbf', 'defunciones_base_datos_2003_dbf',
                            'defunciones_base_datos_2002_dbf', 'defunciones_base_datos_2001_dbf', 'defunciones_base_datos_2000_dbf',
                            'defunciones_base_datos_1999_dbf', 'defunciones_base_datos_1998_dbf','defunciones_base_datos_1997_dbf','defunciones_base_datos_1996_dbf', 
                           'defunciones_base_datos_1995_dbf', 'defunciones_base_datos_1994_dbf','defunciones_base_datos_1993_dbf','defunciones_base_datos_1992_dbf', 
                           'defunciones_base_datos_1991_dbf','defunciones_base_datos_1990_dbf')


dataset_names<- list('DEFUN17.dbf', 'DEFUN16.dbf', 'DEFUN15.DBF', 'DEFUN14.dbf', 'DEFUN13.DBF', 'DEFUN12.dbf',
                     'DEFUN11.dbf', 'DEFUN10.dbf', 'DEFUN09.dbf', 'DEFUN08.dbf', 'DEFUN07.dbf', 'DEFUN06.dbf', 'DEFUN05.dbf',
                     'DEFUN04.dbf', 'DEFUN03.dbf', 'DEFUN02.dbf', 'DEFUN01.dbf', 'DEFUN00.dbf', 'DEFUN99.DBF', 'DEFUN98.dbf',
                     'DEFUN97.dbf','DEFUN96.dbf','DEFUN95.dbf', 'DEFUN94.dbf', 'DEFUN93.dbf', 'DEFUN92.dbf', 
                     'DEFUN91.dbf','DEFUN90.dbf')
     
# dataset_names_90_97<- list('DEFUN97.dbf','DEFUN96.dbf','DEFUN95.dbf', 'DEFUN94.dbf', 'DEFUN93.dbf', 'DEFUN92.dbf', 
#                            'DEFUN91.dbf','DEFUN90.dbf')

death_certificates<- data.frame()
path<-here::here("data", dataset_paths[1], "CAPGPO.dbf")
CAPGPO<-read.dbf(path, as.is = FALSE)

i<-1
#for (element in list(1:19))
while (i<21) {  ## PATHS TO CREATE DE DATAFRAME ## 21
                            path<-here::here("data", dataset_paths[i], dataset_names[i])
                            data_element<-read.dbf(path, as.is = FALSE)
                            data_element$year_dataset<- (as.numeric(str_sub(path,-6, end=-5)))
                            ######
                            #  path<-here("data", dataset_paths[1], dataset_names[1])
                            # data_element<-read.dbf(path, as.is = FALSE)
                            #  backup<-data_element
                            #####
                            ## LOCATION RECODE ##
                            #First I need to make variables as character, otherwise R reads the factors as numbers. What a pain.
                            data_element$municipality_ocur<- as.character(data_element$MUN_OCURR)
                            data_element$municipality_regis<- as.character(data_element$MUN_REGIS)
                            data_element$municipality_resid <- as.character(data_element$MUN_RESID)
                            data_element$state_ocurrence<- as.character(data_element$ENT_OCURR)
                            data_element$state_regis<- as.character(data_element$ENT_REGIS)
                            data_element$state_resid <- as.character(data_element$ENT_RESID)                            
                            
                            data_element$munici_regis<-ifelse(stri_length(data_element$municipality_regis)==2,
                                                                paste("0", data_element$municipality_regis,sep=""), 
                                                                ifelse(stri_length(data_element$municipality_regis)==1,
                                                                       paste("00", data_element$municipality_regis, sep=""),
                                                                       data_element$municipality_regis))
                            data_element$state_registration<-ifelse(stri_length(data_element$state_regis)==1,
                                                              paste("0", data_element$state_regis,sep=""),
                                                             data_element$state_regis)
                            
                            data_element$state_mun_regis = paste(data_element$state_registration, data_element$munici_regis, sep="")
                            
                            data_element$munici_ocur<-ifelse(stri_length(data_element$municipality_ocur)==2,
                                                              paste("0", data_element$municipality_ocur,sep=""), 
                                                              ifelse(stri_length(data_element$municipality_ocur)==1,
                                                                     paste("00", data_element$municipality_ocur, sep=""),data_element$municipality_ocur))
                            data_element$state_ocur<-ifelse(stri_length(data_element$state_ocurrence)==1,
                                                            paste("0", data_element$state_ocurrence,sep=""),
                                                            data_element$state_ocurrence) 
                            data_element$state_mun_OCUR = paste((data_element$state_ocur), (data_element$munici_ocur), sep="")
                            

                            
                            data_element$state_address<-ifelse(stri_length(data_element$state_resid)==1,
                                                             paste("0", data_element$state_resid,sep=""),
                                                             data_element$state_resid)
                            
                            data_element$munici_resid<-ifelse(stri_length(data_element$municipality_resid)==2,
                                                             paste("0", data_element$municipality_resid,sep=""), 
                                                             ifelse(stri_length(data_element$municipality_resid)==1,
                                                                    paste("00", data_element$municipality_resid, sep=""),data_element$municipality_resid))
                            
                            data_element$state_mun_address = paste(data_element$state_address, data_element$munici_resid, sep="")
                            
                            ## SEX ##
                            data_element$sex = data_element$SEXO
                            data_element$sex <- ifelse(data_element$sex==1,
                                                      "Male",
                                                              ifelse(data_element$sex==2,
                                                                     "Female","Not specified"))
                    
                            ## AGE ##
                              # first two intermediate vars to recode it
                            data_element$age_code1 = str_sub(data_element$EDAD, 0,2)
                            data_element$age_code2 = str_sub(data_element$EDAD, 3,)
                              # now we create age for hours, minutes, days and years
                            data_element = data_element %>% 
                              mutate(age_hours = if_else( age_code1=="10" & age_code2!="97" & age_code2!="98", as.numeric(age_code2), NULL))
                            data_element = data_element %>% 
                              mutate(age_minutes = if_else( age_code1=="10" & age_code2=="97", 1, 0))
                            data_element = data_element %>% 
                              mutate(age_days = if_else( age_code1=="20" & age_code2!="98", as.numeric(age_code2), NULL))
                            data_element = data_element %>% 
                              mutate(age_months = if_else( age_code1=="30" & age_code2!="98", as.numeric(age_code2), NULL))
                            data_element = data_element %>% 
                              mutate(age_years = if_else( age_code1=="40" & EDAD!="4998", as.numeric(str_sub(data_element$EDAD, 2,)), NULL))
                            data_element<-select (data_element,-c(age_code1,age_code2))
                            
                            # data_element$age_hours[data_element$age_code1=="10" & data_element$age_code2!="97" & data_element$age_code2!="98"]<- as.numeric(data_element$age_code2)
                            # data_element$age_minutes<-if_else(data_element$age_code1=="10" & data_element$age_code2==97,1,0,missing = NULL)
                            # data_element$age_days[data_element$age_code1=="20" & data_element$age_code2!="98"]<- (data_element$age_code2)
                            ##DATE##
                             ## DATE WHEN DEATH HAPPENED
                            data_element = data_element %>% 
                              mutate(day_death = if_else( DIA_OCURR!="99", as.integer(DIA_OCURR), NULL))
                            data_element = data_element %>% 
                              mutate(month_death = if_else( MES_OCURR!="99", as.integer(MES_OCURR), NULL))
                            data_element = data_element %>% 
                              mutate(year_death = if_else( ANIO_OCUR!="99", as.integer(ANIO_OCUR), NULL))
                            data_element$date_death <- as.Date(paste(data_element$year_death, data_element$month_death, data_element$day_death, sep="-"))
                            # data_element$day_death<-as.character(data_element$day_death)
                            #   data_element$month_death<-as.character(data_element$month_death)
                            #   data_element$year_death<-as.character(data_element$year_death)
                              # data_element = data_element %>%
                              # mutate(date_death = as.Date(paste( data_element$year_death, data_element$month_death ,
                              #                                     data_element$day_death ,  sep = "/") , format= "%m/%d/%Y"  ))
                            
                              #data_element$date_death <- paste(data_element$year_death, data_element$month_death, data_element$day_death, sep="-") %>% ymd() %>% as.Date()
                              
                              data_element = data_element %>% 
                              mutate(day_birth = if_else( DIA_NACIM!="99", as.numeric(DIA_NACIM), NULL))
                            data_element = data_element %>% 
                              mutate(month_birth = if_else( MES_NACIM!="99", as.numeric(MES_NACIM), NULL))
                            data_element = data_element %>% 
                              mutate(year_birth = if_else( ANIO_NACIM!="99", as.numeric(ANIO_NACIM), NULL))
                            data_element = data_element %>% 
                              mutate(dob = as.Date( paste(  
                                data_element$month_birth , 
                                data_element$day_birth , 
                                data_element$year_birth,  sep = "/"), format= "%m/%d/%Y"
                              ))
                            
                            ## DATE WHEN DEATH WAS REGISTERED --> THIS VARIABLE DOES NOT EXIST FOR YEARS BEFORE 2001
                            if (as.integer(str_sub(path,-6, end=-5))>2001){
                            data_element = data_element %>% 
                              mutate(day_registered_death = if_else( DIA_REGIS!="99", as.integer(DIA_REGIS), NULL))
                            data_element = data_element %>% 
                              mutate(month_registered_death = if_else( MES_REGIS!="99", as.integer(MES_REGIS), NULL))
                            data_element = data_element %>% 
                              mutate(year_registered_death = if_else( ANIO_REGIS!="99", as.integer(ANIO_REGIS), NULL))
                            data_element = data_element %>% 
                              mutate(date_registered_death  = as.Date( paste(  
                                data_element$month_registered_death , 
                                data_element$day_registered_death, data_element$year_registered_death,  sep = "/"), format= "%m/%d/%Y"
                              ))
                            }
                            ## DATE OF BIRTH
                            data_element = data_element %>% 
                              mutate(day_birth = if_else( DIA_NACIM!="99", as.integer(DIA_NACIM), NULL))
                            data_element = data_element %>% 
                              mutate(month_birth = if_else( MES_NACIM!="99", as.integer(MES_NACIM), NULL))
                            data_element = data_element %>% 
                              mutate(year_birth = if_else( ANIO_NACIM!="99", as.integer(ANIO_NACIM), NULL))
                            data_element = data_element %>% 
                                          mutate(dob = as.Date( paste(  
                                          data_element$month_birth , 
                                          data_element$day_birth , 
                                          data_element$year_birth,  sep = "/"), format= "%m/%d/%Y"
                            ))
                            
                            ##OCCUPATION THIS CLASSIFICATION IS VALID STARTING IN 2013
                            data_element = data_element %>% 
                              mutate(occupation_raw = OCUPACION)
                            data_element$occupation[data_element$OCUPACION == '1' | data_element$OCUPACION == '21'] <- 'White Collar'   # Funcionarios, directores y jefes                         
                            data_element$occupation[data_element$OCUPACION == '2' | data_element$OCUPACION == '11' | data_element$OCUPACION == '12' |
                                                      data_element$OCUPACION == '13' | data_element$OCUPACION == '14'] <- 'Professional-Technical worker'   # Profesionistas y técnicos, educacion, arte, deportes, espectaculos
                            data_element$occupation[data_element$OCUPACION == '3' | data_element$OCUPACION == '61' | data_element$OCUPACION == '62'] <- 'Admin Work'   # Trabajadores auxiliares en actividades administrativas; nivel intermedio y nivel inferior (old classification) 
                            data_element$occupation[data_element$OCUPACION == '4' | data_element$OCUPACION == '71' ] <- 'Salesman'   #  Comerciantes, empleados en ventas y agentes de ventas
                            data_element$occupation[data_element$OCUPACION == '5' | data_element$OCUPACION == '83'] <- 'Surveillance-Personal Services'   # Trabajadores en servicios personales y vigilancia/Trabajadores en fuerzas armadas, proteccion y vigilancia
                            data_element$occupation[data_element$OCUPACION == '6' | data_element$OCUPACION == '41'] <- 'Primary Sector Worker' # Trabajadores en actividades agrícolas, ganaderas, forestales, caza y pesca
                            data_element$occupation[data_element$OCUPACION == '7' | data_element$OCUPACION == '54' | data_element$OCUPACION == '9'] <- 'Artisan/Auxiliary Workers' # Trabajadores artesanales /Ayudantes en el proceso de producción industrial y artesanal(old)
                            data_element$occupation[data_element$OCUPACION == '8' | data_element$OCUPACION == '51'| data_element$OCUPACION == '52' | data_element$OCUPACION == '53'] <- 'Industrial/Manufacturing/Transport worker' #Operadores de maquinaria industrial, ensambladores, choferes y conductores de transporte
                            data_element$occupation[data_element$OCUPACION == '81'] <- 'Personal Services at businesses' # Trabajadores en actividades elementales y de apoyo
                            data_element$occupation[data_element$OCUPACION == '10'] <- 'Looking for Job' #Busca trabajo
                            data_element$occupation[data_element$OCUPACION == '11' | data_element$OCUPACION == '2'] <- 'Not Working'
                            data_element$occupation[data_element$OCUPACION == '72'] <- 'Informal seller (Vend. Amb)'
                            data_element$occupation[data_element$OCUPACION == '82'] <- 'Domestic/House Services'
                            
                            data_element = data_element %>% 
                              mutate(occupation = if_else( OCUPACION=="97"| OCUPACION=="98" | OCUPACION=="99" , "NA", occupation))

                            ### SCHOOL
                            data_element = data_element %>% 
                              mutate(schooling_raw = ESCOLARIDA)
                            if (data_element$year_dataset>=2012){
                            data_element$school_category[data_element$ESCOLARIDA == '1'] <- 'No school'   
                            data_element$school_category[data_element$ESCOLARIDA == '2'] <- 'Pre-elementary'   
                            data_element$school_category[data_element$ESCOLARIDA == '3'] <- 'Incomplete Elementary'   
                            data_element$school_category[data_element$ESCOLARIDA == '4'] <- 'Complete Elementary'   
                            data_element$school_category[data_element$ESCOLARIDA == '5'] <- 'Incomplete Middle'   
                            data_element$school_category[data_element$ESCOLARIDA == '6'] <- 'Complete Middle'   
                            data_element$school_category[data_element$ESCOLARIDA == '7'] <- 'Incomplete High-School'   
                            data_element$school_category[data_element$ESCOLARIDA == '8'] <- 'High School'   
                            data_element$school_category[data_element$ESCOLARIDA == '9'] <- 'Professional/Graduate'   
                            data_element$school_category[data_element$ESCOLARIDA == '10'] <- 'Professional/Graduate' #This is graduate/postf
                            data_element = data_element %>% 
                              mutate(school_category = if_else( ESCOLARIDA=="88"| ESCOLARIDA=="99" , "NA", school_category)) 
                            }
                            if (data_element$year_dataset<2012){
                              data_element$school_category[data_element$ESCOLARIDA == '1'] <- 'No school'   
                              data_element$school_category[data_element$ESCOLARIDA == '3' | data_element$ESCOLARIDA == '2'] <- 'Incomplete Elementary'   
                              data_element$school_category[data_element$ESCOLARIDA == '4'] <- 'Complete Elementary'    
                              data_element$school_category[data_element$ESCOLARIDA == '5'] <- 'Complete Middle'   
                              data_element$school_category[data_element$ESCOLARIDA == '6'] <- 'High School'   
                              data_element$school_category[data_element$ESCOLARIDA == '7'] <- 'Professional/Graduate'  
                              data_element = data_element %>% 
                                mutate(school_category = if_else( ESCOLARIDA=="8"| ESCOLARIDA=="9" , "NA", school_category)) 
                            }
                            
                            ### MARITAL STATUS
                            
                              data_element = data_element %>% 
                              mutate(marital_status_raw = EDO_CIVIL)
                              if (data_element$year_dataset<2004) {
                                data_element$marital_status[data_element$EDO_CIVIL == '1'] <- 'single'
                                data_element$marital_status[data_element$EDO_CIVIL == '2'] <- 'divorced'
                                data_element$marital_status[data_element$EDO_CIVIL == '3'] <- 'widow'
                                data_element$marital_status[data_element$EDO_CIVIL == '4'] <- 'free_union'
                                data_element$marital_status[data_element$EDO_CIVIL == '5'] <- 'married'
                                data_element$marital_status[data_element$EDO_CIVIL == '6'] <- 'separated'
                                data_element$marital_status[data_element$EDO_CIVIL == '8'] <- 'NA'
                                data_element$marital_status[data_element$EDO_CIVIL == '9'] <- 'NA'
                              }
                              if (data_element$year_dataset>2004 & data_element$year_dataset<2012) {
                                data_element$marital_status[data_element$EDO_CIVIL == '1'] <- 'single'
                                data_element$marital_status[data_element$EDO_CIVIL == '2'] <- 'widow'
                                data_element$marital_status[data_element$EDO_CIVIL == '3'] <- 'divorced'
                                data_element$marital_status[data_element$EDO_CIVIL == '4'] <- 'free_union'
                                data_element$marital_status[data_element$EDO_CIVIL == '5'] <- 'married'
                                data_element$marital_status[data_element$EDO_CIVIL == '8'] <- 'NA'
                                data_element$marital_status[data_element$EDO_CIVIL == '9'] <- 'NA'
                              }
                              if (data_element$year_dataset>2012) {
                                data_element$marital_status[data_element$EDO_CIVIL == '1'] <- 'single'
                                data_element$marital_status[data_element$EDO_CIVIL == '2'] <- 'divorced'
                                data_element$marital_status[data_element$EDO_CIVIL == '3'] <- 'widow'
                                data_element$marital_status[data_element$EDO_CIVIL == '4'] <- 'free_union'
                                data_element$marital_status[data_element$EDO_CIVIL == '5'] <- 'married'
                                data_element$marital_status[data_element$EDO_CIVIL == '6'] <- 'separated'
                                data_element$marital_status[data_element$EDO_CIVIL == '8'] <- 'NA'
                                data_element$marital_status[data_element$EDO_CIVIL == '9'] <- 'NA'
                              }
                              
                            ## DEATH CLASSIFICATION FROM INEGI FOR NON-NATURAL DEATHS
                            data_element = data_element %>% 
                              mutate(motivation_raw = PRESUNTO)
                            data_element$non_natural_causes[data_element$PRESUNTO==1]<- "Accident" 
                            data_element$non_natural_causes[data_element$PRESUNTO==2]<- "Homicide"
                            data_element$non_natural_causes[data_element$PRESUNTO==3]<-"Suicide"       
                            data_element$non_natural_causes[data_element$PRESUNTO==4]<-"Don't know"        
                            data_element$non_natural_causes[data_element$PRESUNTO==5]<-"War Operations"
                            data_element$non_natural_causes[data_element$PRESUNTO==8]<-"N.A/Natural Death"
                            
                            ## AT WORK
                            data_element$at_work[data_element$OCURR_TRAB==1]<-1
                            data_element$at_work[data_element$OCURR_TRAB==2]<-0
                            data_element$at_work[data_element$OCURR_TRAB==8 | data_element$OCURR_TRAB==9]<-"NA"
                            
                            # Place of death for accident or violent situation
                            data_element = data_element %>% 
                              mutate(place_death_raw = LUGAR_OCUR)
                            if (data_element$year_dataset>2003) {
                            data_element$place_of_death[data_element$LUGAR_OCUR==0]<-"Household"
                            data_element$place_of_death[data_element$LUGAR_OCUR==1]<-"Household" # Institucion residencial
                            data_element$place_of_death[data_element$LUGAR_OCUR==2]<-"School/Office"
                            data_element$place_of_death[data_element$LUGAR_OCUR==3]<-"Sport Areas"
                            data_element$place_of_death[data_element$LUGAR_OCUR==4]<-"Road"
                            data_element$place_of_death[data_element$LUGAR_OCUR==5]<-"Comercial Area"
                            data_element$place_of_death[data_element$LUGAR_OCUR==6]<-"Industrial Area"
                            data_element$place_of_death[data_element$LUGAR_OCUR==7]<-"Farm"
                            data_element$place_of_death[data_element$LUGAR_OCUR==8]<-"Other"
                            data_element$place_of_death[data_element$LUGAR_OCUR==9]<-"DN"
                            data_element$place_of_death[data_element$LUGAR_OCUR==88]<-"N.A (natural death)"
                            }
                            if (data_element$year_dataset<=2003) {
                              data_element$place_of_death[data_element$LUGAR_OCUR==1]<-"Household" # Institucion residencial
                              data_element$place_of_death[data_element$LUGAR_OCUR==2]<-"School/Office"
                              data_element$place_of_death[data_element$LUGAR_OCUR==3]<-"Public Areas"
                              data_element$place_of_death[data_element$LUGAR_OCUR==4]<-"Recreational Areas"
                              data_element$place_of_death[data_element$LUGAR_OCUR==5]<-"Comercial Area"
                              data_element$place_of_death[data_element$LUGAR_OCUR==6]<-"Roads"
                              data_element$place_of_death[data_element$LUGAR_OCUR==7]<-"Other"
                              data_element$place_of_death[data_element$LUGAR_OCUR==9]<-"N.A"
                              data_element$place_of_death[data_element$LUGAR_OCUR==8]<-"N.A"
                            }
                            
                            #NECROPSY
                            data_element = data_element %>% 
                              mutate(necropsy_raw = NECROPSIA)
                            data_element$place_of_death[data_element$NECROPSIA==9]<-"Not specified"
                            data_element$place_of_death[data_element$NECROPSIA==1]<-1
                            data_element$place_of_death[data_element$NECROPSIA==2]<-0
                            
                            # Hospital where death occured
                            data_element = data_element %>% 
                              mutate(hospital_raw = SITIO_OCUR)
                            if (data_element$year_dataset>=2004) {
                            data_element$sitio_dissag[data_element$SITIO_OCUR==1]<-"SSA"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==2]<-"IMSS Oportunidades"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==3]<-"IMSS"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==4]<-"ISSSTE"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==5]<-"PEMEX"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==6]<-"SEDENA"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==7]<-"MARINA"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==8]<-"MARINA"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==9]<-"Other public hospital"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==10]<-"Public place (road)" # via publica
                            data_element$sitio_dissag[data_element$SITIO_OCUR==11]<-"Household"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==12]<-"Other place"
                            data_element$sitio_dissag[data_element$SITIO_OCUR==99]<-"DN"
                            }
                            # GENERIC VERSION
                            if (data_element$year_dataset>=2004) {
                            data_element$sitio_generic[data_element$SITIO_OCUR==1]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==2]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==3]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==4]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==5]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==6]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==7]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==8]<-"Public Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==9]<-"Private Medical Center"
                            data_element$sitio_generic[data_element$SITIO_OCUR==10]<-"Public place" # via publica
                            data_element$sitio_generic[data_element$SITIO_OCUR==11]<-"Household"
                            data_element$sitio_generic[data_element$SITIO_OCUR==12]<-"Other place"
                            data_element$sitio_generic[data_element$SITIO_OCUR==99]<-"DN"
                            }
                            
                            if (data_element$year_dataset<2004) {
                              data_element$sitio_generic[data_element$SITIO_OCUR==1]<-"Public Medical Center"
                              data_element$sitio_generic[data_element$SITIO_OCUR==2]<-"Private Medical Center"
                              data_element$sitio_generic[data_element$SITIO_OCUR==3]<-"Household"
                              data_element$sitio_generic[data_element$SITIO_OCUR==4]<-"Other place"
                              data_element$sitio_generic[data_element$SITIO_OCUR==9]<-"DN"
                            }
                            
                            
                            ## PREGNANCY CONDITIONS #especifica si la muerte ocurrió durante el embarazo, el parto, el puerperio,43 días a 11 meses después del parto, no estuvo embarazada durante los 11 meses previos a la muerte, estuvo embarazada un año o más antes de la muerte.
                            data_element = data_element %>% 
                              mutate(pregnancy_raw = SITIO_OCUR)
                            data_element$pregnancy_condition[data_element$EMBARAZO==1]<-"pregnant"
                            data_element$pregnancy_condition[data_element$EMBARAZO==2]<-"birth"
                            data_element$pregnancy_condition[data_element$EMBARAZO==3]<-"puerperium"
                            data_element$pregnancy_condition[data_element$EMBARAZO==4]<-"post_birth" # 43 days<x<11 months
                            data_element$pregnancy_condition[data_element$EMBARAZO==5]<-"not_pregnant_11monthsbef"
                            data_element$pregnancy_condition[data_element$EMBARAZO==8]<-"N.A" #no aplica
                            data_element$pregnancy_condition[data_element$EMBARAZO==9]<-"Not specified"
             
                            ## related to pregnancy
                            data_element = data_element %>% 
                              mutate(pregnancy_related_raw = REL_EMBA)
                            data_element$pregnancy_related[data_element$REL_EMBA==1]<-"yes"
                            data_element$pregnancy_related[data_element$REL_EMBA==2]<-"no"
                            data_element$pregnancy_related[data_element$REL_EMBA==8]<-"N.A"
                            data_element$pregnancy_related[data_element$REL_EMBA==9]<-"not specified"
                            
                            ### TIME ###
                            
                            data_element = data_element %>% 
                              mutate(hour = if_else( HORAS!="99", as.numeric(HORAS), NULL))
                            data_element = data_element %>% 
                              mutate(minutes = if_else( MINUTOS!="99", as.numeric(MINUTOS), NULL))
                            data_element = data_element %>% 
                              mutate(secs = 0)
                            data_element = data_element %>% 
                              mutate(time_death = hms(secs,minutes,hour))
                            
                            # DERECHOHABIENCIA
                            if (data_element$year_dataset<2004) {
                            data_element$derechohabiencia[data_element$SITIO_OCUR==1]<-"None"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==2]<-"IMSS"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==3]<-"ISSSTE"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==5]<-"ARMY"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==4]<-"PEMEX"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==6]<-"OTHER"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==23]<-"IMSS" #"IMSS-ISSSTE"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==24]<-"IMSS" #"IMSS-PEMEX"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==26]<-"IMSS" # "IMSS-OTHER"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==34]<-"ISSSTE" #"ISSSTE-PEMEX" via publica
                            data_element$derechohabiencia[data_element$SITIO_OCUR==35]<-"ISSSTE" #"ISSSTE-ARMY"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==36]<-"ISSSTE" #"ISSSTE-OTHER"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==45]<-"PEMEX" #"PEMEX-ARMY"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==46]<-"PEMEX" #"PEMEX-OTHER"
                            data_element$derechohabiencia[data_element$SITIO_OCUR==56]<-"ARMY" #ARMY OTHER
                            data_element$derechohabiencia[data_element$SITIO_OCUR==99]<-"NA"
                            }
                            if (data_element$year_dataset>=2004) {
                              data_element$derechohabiencia[data_element$SITIO_OCUR==1]<-"None"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==2]<-"IMSS"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==3]<-"ISSSTE"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==5]<-"ARMY"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==4]<-"PEMEX"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==6]<-"ARMY" #SEMAR
                              data_element$derechohabiencia[data_element$SITIO_OCUR==7]<-"SEGURO POPULAR" 
                              data_element$derechohabiencia[data_element$SITIO_OCUR==8]<-"OTRA" #"IMSS-PEMEX"
                              data_element$derechohabiencia[data_element$SITIO_OCUR==9]<-"IMSS-OPORTUNIDADES" # 
                
                              data_element$derechohabiencia[data_element$SITIO_OCUR==99]<-"NA"
                            }
                            
                            ##NEED TO DOUBLE CHECK THIS PART##
                            ### FAMILY VIOLENCE ### if homicide, family violence condition
                            if (as.numeric(str_sub(path,-6, end=-5))>1999){
                              data_element$family_violence[data_element$VIO_FAMI==1]<-"yes"
                            data_element$family_violence[data_element$VIO_FAMI==2]<-"no"
                            data_element$family_violence[data_element$VIO_FAMI==8]<-"NA"
                            data_element$family_violence[data_element$VIO_FAMI==9]<-"not specified"
                            }
                            ### Urban rural ###
                            if (as.numeric(str_sub(path,-6, end=-5))>2001){
                            data_element$urban[data_element$AREA_UR==1]<-1
                            data_element$urban[data_element$AREA_UR==2]<-0 #With an else if better...
                            data_element$urban[data_element$AREA_UR==9]<-NA
                              }
                            ## AGE GROUP ##
                            data_element = data_element %>% 
                              mutate(age_group = EDAD_AGRU)
                            
                            # DATE DEATH CERTIFICATE #
                            if (as.numeric(str_sub(path,-6, end=-5))>2002){
                            data_element = data_element %>% 
                              mutate(day_cert = if_else( DIA_CERT!="99", as.numeric(DIA_CERT), NULL))
                            data_element = data_element %>% 
                              mutate(month_cert = if_else( MES_CERT!="99", as.numeric(MES_CERT), NULL))
                            data_element = data_element %>% 
                              mutate(year_cert = if_else( ANIO_CERT!="99", as.numeric(ANIO_CERT), NULL))
                            data_element = data_element %>% 
                              mutate(death_certificate_date = as.Date( paste(  
                                                           data_element$month_cert , 
                                                           data_element$day_cert , data_element$year_cert,  sep = "/"), format= "%m/%d/%Y"
                                                           ))
                            }
                            # INDIGENOUS#
                            if (as.numeric(str_sub(path,-6, end=-5))>2011){
                                print(as.numeric(str_sub(path,-6, end=-5)))
                                data_element$indigenous[data_element$LENGUA==1]<-1
                            data_element$indigenous[data_element$LENGUA==2]<-0 #With an else if better...
                            data_element$indigenous[data_element$LENGUA==9]<-"DN"
                            data_element$indigenous[data_element$LENGUA==8]<-"N.A"
                            }
                            # ECONOMIC ACTIVITY#
                            if (as.numeric(str_sub(path,-6, end=-5))>2011){
                            data_element$economic_activity[data_element$COND_ACT==1]<-1
                            data_element$economic_activity[data_element$COND_ACT==2]<-0 #With an else if better...
                            data_element$economic_activity[data_element$COND_ACT==9]<-"DN"
                            data_element$economic_activity[data_element$COND_ACT==8]<-"N.A"
                            }
                            #LATER INCLUDE FAMILY RELATIONSHIP OF AGRESSOR (Par_Agre)
                            #############
                            if (as.numeric(str_sub(path,-6, end=-5))>2011){
                            #PLACE WHERE AGRESSION TOOK PLACE #
                            data_element$state_mun_injury = paste(data_element$ENT_OCULES, data_element$MUN_OCULES, sep="")
                            }
                            ## MATERNITY DEATH
                            #data_element$maternity_death <- data_element$

                            ### CAUSE OF DEATH
                            data_element$GPO<-data_element$GRUPO
                            data_element <- merge(x = data_element, y = CAPGPO, by = "GPO")
                            data_element$cause_of_death=""
                            data_element$cause_of_death[data_element$CAP == '01' & data_element$GPO !="13"] <- 'INFECTION NO HIV'                           
                            data_element$cause_of_death[data_element$CAP == '01' & data_element$GPO =="13"] <- 'HIV'                            
                            data_element$cause_of_death[data_element$CAP == '02'] <- 'CANCER'                            
                            data_element$cause_of_death[data_element$CAP == '03'] <- 'BLOOD DISEASE'                            
                            data_element$cause_of_death[data_element$CAP == '04'] <- 'ENM'   # Endocrine, Nutrional and Metabolic diseases                         
                            data_element$cause_of_death[data_element$CAP == '05'] <- 'MENTAL ILLNESS'                            
                            data_element$cause_of_death[data_element$CAP == '06'] <- 'NERVOUS SYSTEM ILLNESS'                           
                            data_element$cause_of_death[data_element$CAP == '07'] <- 'EYE ILLNESS'                           
                            data_element$cause_of_death[data_element$CAP == '08'] <- 'EAR ILLNESS'                           
                            data_element$cause_of_death[data_element$CAP == '09'] <- 'CIRCULATORY SYSTEM'                         
                            data_element$cause_of_death[data_element$CAP == '10'] <- 'RESPIRATORY SYSTEM'                            
                            data_element$cause_of_death[data_element$CAP == '11'] <- 'DIGESTIVE SYSTEM'                          
                            data_element$cause_of_death[data_element$CAP == '12'] <- 'SKIN ILLNESS'                           
                            data_element$cause_of_death[data_element$CAP == '13'] <- 'OSTEOMUSCULAR CONJ' #osteomuscular and conjunctive tissue disorders
                            data_element$cause_of_death[data_element$CAP == '14'] <-'GENITOURINARY'
                            data_element$cause_of_death[data_element$CAP == '15' & data_element$GPO =="01"] <-'ABORTION PREGNANCY'
                            data_element$cause_of_death[data_element$CAP == '15' & data_element$GPO !="01"] <-'PREGNANCY RELATED_NA'
                            data_element$cause_of_death[data_element$CAP == '16' ] <-'PERINATAL PERIOD ILLNESS'
                            data_element$cause_of_death[data_element$CAP == '17' ] <-'CONGENITAL MALF'
                            data_element$cause_of_death[data_element$CAP == '18' ] <-'ABNORMAL CLINIC LAB' #Síntomas, signos y hallazgos anormales clínicos y de laboratorio, no clasificados en otra parte 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO !="01" ] <-'PEDESTRIAN ACCIDENT' 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO !="02" ] <-'BYCICLIST ACCIDENT' 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO !="03" ] <-'MOTORCYCLIST ACCIDENT' 
                            data_element$cause_of_death[data_element$CAP == '20' & (data_element$GPO =="04"| data_element$GPO =="05"| 
                                                                                    data_element$GPO =="06"|  data_element$GPO =="07"|
                                                                                    data_element$GPO =="08"|  data_element$GPO =="09")  ] <-'ROAD ACCIDENT'
                            data_element$cause_of_death[data_element$CAP == '20' & (data_element$GPO =="10"| data_element$GPO =="11"| 
                                                                                      data_element$GPO =="12")] <-'AIR, WATER, OTHER TRANSPORT ACC' 
                            data_element$cause_of_death[data_element$CAP == '20' & (data_element$GPO =="13"| data_element$GPO =="14"| 
                                                                                      data_element$GPO =="15"|  data_element$GPO =="16"|
                                                                                      data_element$GPO =="17"|  data_element$GPO =="18"| 
                                                                                      data_element$GPO =="19"| data_element$GPO =="20"|
                                                                                      data_element$GPO =="21"| data_element$GPO =="22"|
                                                                                      data_element$GPO =="23"| data_element$GPO =="24"|
                                                                                      data_element$GPO =="25")] <-'OTHER ACCIDENT'                           
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO =="26" ] <-'SUICIDES' 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO =="27" | data_element$GPO =="28" ] <-'HOMICIDES' 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO =="29" ] <-'WAR OPERATIONS' 
                            data_element$cause_of_death[data_element$CAP == '20' & data_element$GPO =="30" ] <-'DRUG_USE_LEGAL' 
                            data_element$cause_of_death[data_element$CAP == '20' & (data_element$GPO =="31"| data_element$GPO =="32"| data_element$GPO =="33"| data_element$CAUSA_DEF == 'Y95X') ] <-'MEDICAL ASSIST' 
                            data_element$cause_of_death[data_element$CAP == '20' & (data_element$GPO =="34") ] <-'AFTERMATH' 
                            data_element$cause_of_death[data_element$CAUSA_DEF == 'Y900'| data_element$CAUSA_DEF == 'Y901'|
                                                          data_element$CAUSA_DEF == 'Y900'|data_element$CAUSA_DEF == 'Y900'|
                                                          data_element$CAUSA_DEF == 'Y902'|data_element$CAUSA_DEF == 'Y903'|
                                                          data_element$CAUSA_DEF == 'Y904'|data_element$CAUSA_DEF == 'Y905'|
                                                          data_element$CAUSA_DEF == 'Y906'|data_element$CAUSA_DEF == 'Y907'|
                                                          data_element$CAUSA_DEF == 'Y908'|data_element$CAUSA_DEF == 'Y909'|
                                                          data_element$CAUSA_DEF == 'Y910'|data_element$CAUSA_DEF == 'Y911'|
                                                          data_element$CAUSA_DEF == 'Y912'|data_element$CAUSA_DEF == 'Y913'|
                                                          data_element$CAUSA_DEF == 'Y914'|data_element$CAUSA_DEF == 'Y915'|
                                                          data_element$CAUSA_DEF == 'Y916'| data_element$CAUSA_DEF == 'Y917'|
                                                          data_element$CAUSA_DEF == 'Y918'|data_element$CAUSA_DEF == 'Y919'] <-'ALCOHOL CONSUMPTION' 
                            data_element$cause_of_death[data_element$CAUSA_DEF == 'Y96X'] <-'WORK RELATED' 
                            data_element$cause_of_death[data_element$CAUSA_DEF == 'Y97X'|data_element$CAUSA_DEF == 'Y98X'] <-'POLLUTION LIFES RELATED' 
                            ## I included Y95X in hospital related (affeccion nocosomial)
                            
                            # data_element$state_register = data_element$ENT_REGIS
                            # data_element$state_living = data_element$ENT_REGIS
                            # data_element$mun_register = data_element$MUN_REGIS
                            # data_element$mun_living = data_element$MUN_RESID                            
                            #data_element <- merge(x = data_element, y = pop2010 , by = "state_mun_address")
                            data_element$counter=1 #Final counter of deaths
                            data_element %>% select (-c(data_element$GRUPO, data_element$ENT_REGIS, 
                                                        data_element$MUN_REGIS, data_element$ENT_RESID,
                                                        data_element$MUN_RESID, data_element$TLOC_RESID, 
                                                        data_element$LOC_RESID, data_element$ENT_OCURR,
                                                        data_element$MUN_OCURR, data_element$TLOC_OCURR,
                                                        data_element$LOC_OCURR,data_element$LISTA_MEX,
                                                        data_element$EDAD, data_element$ASIST_MEDI, 
                                                        data_element$COND_CERT, data_element$EDAD_AGRU,
                                                        data_element$COND_ACT, data_element$PAR_AGRE,
                                                        data_element$DIS_RE_OAX))
                            i<-i+1
                            print(paste("The colnames for year data element", as.numeric(str_sub(path,-6, end=-5)), "are", list(colnames(data_element)), sep=" "))
                            print(paste("The colnames for death_certificates", as.numeric(str_sub(path,-6, end=-5)), "are", list(colnames(death_certificates)), sep=" "))
                            
                            death_certificates = smartbind(death_certificates, data_element)
                            print(path)}
 ##CODE ENDS HERE ####
write.csv(death_certificates, "all_deaths.csv")

# rm(CAPGPO, CATMINDE,dataset_names_90_97, dataset_names_98_17,dataset_paths, 
#    dataset_paths_90_97,dataset_names_98_17, DETALLADA, GPOLIMEX, LISTA1, LISTAMEX, lobasmin, mergetry, 
#    unique_mun, unique_state)
rm(CAPGPO, data_element, dataset_names,dataset_paths)

############################## TESTING DATASETS WITH CODES #############################

path<-here("data", 'defunciones_base_datos_2003_dbf', 'DEFUN03.dbf')
#path<-here("data", dataset_paths[21], dataset_names[21])
print(path)
#print(element)
data_element<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[21], "LISBASMIN.dbf")
libasmin<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "CATMINDE.dbf")
CATMINDE<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "CAPGPO.dbf")
CAPGPO<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "GPOLIMEX.dbf")
GPOLIMEX<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "LISTA1.dbf")
LISTA1<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "LISTAMEX.dbf")
LISTAMEX<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[21], "DETALLADA.dbf")
DETALLADA<-read.dbf(path, as.is = FALSE)

path<-here("data", dataset_paths[18], "ENTMUN.dbf")
ENTMUN<-read.dbf(path, as.is = FALSE)

path<-here("data", dataset_paths[1], "CATEMLDE17.dbf")
CATEMLDE17<-read.dbf(path, as.is = FALSE)
path<-here("data", dataset_paths[1], "PARENTESCO.dbf")
PARENTESCO<-read.dbf(path, as.is = FALSE)
############################## TESTING DATASETS WITH CODES #############################
death_certificates$GPO<-death_certificates$GRUPO
# Left outer join
death_certificates <- merge(x = death_certificates, y = CAPGPO, by = "GPO")

path<-here("data", dataset_paths[1], dataset_names[1])
data_element<-read.dbf(path, as.is = FALSE)
data_element$GPO<-data_element$GRUPO
data_element <- merge(x = data_element, y = CAPGPO, by = "GPO")

data_element= subset(data_element, select = -c(var) )


while (i<21) { (path<-here("data", dataset_paths_98_17[i], dataset_names_98_17[i])) 
  print(as.numeric(str_sub(path,-6, end=-5))) 
  i<i+1
  }
rm(CAPGPO, CATMINDE, data_17, data_97, data_element,dataset_names, dataset_paths, dataset_names_90_97, dataset_names_98_17, dataset_paths_90_97, dataset_paths_98_17,DETALLADA, GPOLIMEX,LISTA1,LISTAMEX, mergetry)
