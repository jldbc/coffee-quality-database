library(dplyr)
library(stringr)
library(digest)

df = read.csv('/Users/jledoux/Documents/projects/coffee-quality-database/data/arabica_ratings_raw.csv')

df$X = NULL
df$X.1 = NULL
df$view_certificate_1 = NULL
df$view_certificate_2 = NULL
df$Cupping.Protocol.and.Descriptors = NULL
df$View.Green.Analysis.Details = NULL
df$Request.a.Sample = NULL
df$NA. = NULL
df$NA.1 = NULL
df$NA.2 = NULL
df$NA.3 = NULL
df$NA..1 = NULL
df$NA..2 = NULL
df$NA..3 = NULL
df$Status = NULL
df$Total.Cup.Points = df$quality_score
df$quality_score = NULL
df$Notes = NULL

# columns
# [1] "quality_score"         "Species"               "Owner"                 "Country.of.Origin"    
# [5] "Farm.Name"             "Lot.Number"            "Mill"                  "ICO.Number"           
# [9] "Company"               "Altitude"              "Region"                "Producer"             
# [13] "Number.of.Bags"        "Bag.Weight"            "In.Country.Partner"    "Harvest.Year"         
# [17] "Grading.Date"          "Owner.1"               "Variety"               "Status"               
# [21] "Processing.Method"     "Aroma"                 "Flavor"                "Aftertaste"           
# [25] "Acidity"               "Body"                  "Balance"               "NA.1"                 
# [29] "Uniformity"            "Clean.Cup"             "Sweetness"             "Cupper.Points"        
# [33] "Total.Cup.Points"      "NA.2"                  "Moisture"              "Category.One.Defects" 
# [37] "Quakers"               "Color"                 "Category.Two.Defects"  "NA.3"                 
# [41] "Expiration"            "Certification.Body"    "Certification.Address" "Certification.Contact"
# [45] "X.1"                   "Notes" 

# row X is a test row. get rid of it. 


##############################################
# ALTITUDE
##############################################
# several different encoding chemes here, usually depicting a range 
# 1: get unit (m / ft)
# 2: locate numbers in strings, split to two columns 
# 3: third column as the middle of the range, we'll call this the altitude 
df$Altitude = tolower(df$Altitude)
unique(df$Altitude)
df$unit_of_measurement = 'm'

# first, there are some garbage values in this column. drop them. 
drops = c('unkown','TEST', 'none', 'n/a', 'Huanuco', '-(average)', '-')
df[df$Altitude %in% drops,'Altitude'] = ''

# now get units. default is meters, find non-meters and change their value 
# remaining units: (none), 公尺, P.S.N.M, feet, msnm, mts, ft, m.s.n.m., P.S.N.M, MASL, mals, F, Feet, Pies, meters above sea level

# units in dataset:
# msnm: meters above sea level 
# 公尺: meters 
# P.S.N.M: feet above sea lvel (pies sobre el nivel del mar)
# mts: meters 
# MASL: meters above sea level 
# mals: meters above sea level 
# Pies: feet 

# everything besides the feet-related ones can stay in meters
# unknowns to be forced to empty strings 

unique(df$Altitude)
feets = c('feet','ft',' f', 'pies', 'P.S.N.M:', 'psnm')

# first check to be sure this isn't replacing any of the wrong values 
fun = function(x) {df[grep(x, df$Altitude),'Altitude']}
sapply(feets, fun)

feets = c('feet','ft',' f', 'pies')
fun = function(x) {
  match_vals = df[grep(x, df$Altitude),'Altitude']
  df[df$Altitude %in% match_vals,'unit_of_measurement'] <<- 'ft' # assign value, sticks outside of function bc double arrow
  return(df)
}
df2 = sapply(feets, fun)
sapply(feets, fun)

# of those with no declared unit, can we assign one? 
table(df[df$unit_of_measurement=='ft','Country.of.Origin'])
table(df[df$Country.of.Origin=='Guatemala','unit_of_measurement'])

# always assign ft if a us state/territory, guatemala is a bit of a mixed bag 
df[df$Country.of.Origin %in% c('United States (Hawaii)', 'United States (Puerto Rico)'),'unit_of_measurement'] = 'ft'

# grab lower and upper bound numbers for altitude 
df$Altitude = gsub(",","",df$Altitude) # commas mess up the string splitting
df$alt = sapply(df$Altitude, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]]) #[0-9]+
Split <- strsplit(as.character(df$alt), ", ", fixed = TRUE)
df$alt_low <- sapply(Split, "[", 1)
df$alt_high <- sapply(Split, "[", 2)
df$alt_low = sapply(df$alt_low, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]])
df$alt_high = sapply(df$alt_high, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]])

#remove commas and spaces, make numeric
df$alt_low = gsub('\\.','',df$alt_low)
df$alt_high = gsub('\\.','',df$alt_high)

# replace zeros with blanks
df[df$alt_low==0,'alt_low'] = NA

# if no available high alt, replace alt high with alt low 
df[is.na(df$alt_high),'alt_high'] = df[is.na(df$alt_high),'alt_low']

# mean alt column to deal with farms where we're provided a range 
df$alt_low = as.numeric(df$alt_low)
df$alt_high = as.numeric(df$alt_high)
df$alt_mean = rowMeans(df[,c('alt_low','alt_high')],na.rm=TRUE)

# if ht. > 3500 it's probably feet. most points in guatemala are < 3500 m 
df[df$Country.of.Origin=='Guatemala',]
df[(df$Country.of.Origin=='Guatemala') & (df$Altitude>3500),'unit_of_measurement'] = 'ft'

# standardize units of measurement  - 1 ft = 0.3048 m 
df[df$unit_of_measurement=='ft',c('alt_low','alt_high','alt_mean')] = df[df$unit_of_measurement=='ft',c('alt_low','alt_high','alt_mean')]*0.3048

# did it work? 
head(df[,c('Altitude','alt_low','alt_high','alt_mean','unit_of_measurement')], 100)


##############################
# MOISTURE: make it a decimal 
##############################
df$Moisture = substr(df$Moisture,1,nchar(as.character(df$Moisture))-2)
df$Moisture = as.numeric(df$Moisture) / 100


##############################
# DEFECTS: make numeric 
##############################
df$Category.One.Defects = as.numeric(substr(df$Category.One.Defects,1,nchar(as.character(df$Category.One.Defects))-13))
df$Category.Two.Defects = as.numeric(substr(df$Category.Two.Defects,1,nchar(as.character(df$Category.Two.Defects))-13))

##############################
# PII - anonymize this 
##############################
# Certification.Address    Certification.Contact
df$Certification.Contact = sapply(df$Certification.Contact, function(x) sha1(x))
df$Certification.Address = sapply(df$Certification.Address, function(x) sha1(x))

##############################################
#     OWNER
##############################################

# Owner - see if any are miscoded 
sort(unique(tolower(df$Owner)))
df$Owner = tolower(df$Owner)

# the following my be duplicates. investigate and fix if needed
#[283] Taylor Winch (Coffee) Ltd.                        
#[284] Taylor Winch (T) Ltd  
# might be different, not obvious from looking them both up
df %>% filter(Owner %in% c('taylor winch (coffee) ltd.', 'taylor winch (t) ltd'))

#[180] Klem Organics                                     
#[181] KlemOrganics 
# same
df[df$Owner=='klemorganics','Owner'] = 'klem organics'


#[152] JESUS CARLOS CADENA VALDIVIA                      
#[153] JESUS CARLOS CARDENAS VALDIVIA
# same 
df[df$Owner=='jesus carlos cadena valdiva','Owner'] = 'jesus carlos cadenas valdiva'

#[101] "federacion nacional de cafeteros"                  
#[102] "federación nacional de cafeteros" 
# same
df[df$Owner=='federación nacional de cafeteros','Owner'] = 'federacion nacional de cafeteros'


#Country.of.Origin
sort(unique(tolower(df$Country.of.Origin)))

##############################################
#     FARM NAME
##############################################

sort(unique(tolower(df$Farm.Name)))
df$Farm.Name = tolower(df$Farm.Name)

# [61] "capoeirinha"                                                                            
# [62] "capoeirinha farm"                                                                       
# [63] "capoerinha farm"
df %>% filter(Farm.Name %in% c('capoeirinha', 'capoeirinha farm','capoerinha farm'))
df[df$Farm.Name %in% c('capoeirinha farm','capoerinha farm'),'Farm.Name'] = 'capoeirinha'

# [90] "conquista / morito"                                                                     
# [91] "conquista/ morito"                                                                      
# [92] "conquista/morito"  
df[df$Farm.Name %in% c('conquista/ morito','conquista/morito'),'Farm.Name'] = 'conquista / morito'

# [127] "el desmoronado"                                                                         
# [128] "el desmoronado, talpan de allende jalisco"    
# different owner, keeping them separate 
df %>% filter(Farm.Name %in% c('el desmoronado', 'el desmoronado, talpan de allende jalisco'))

# 
# [138] "el majahual"                                                                            
# [139] "el majahual tempisque" 
df %>% filter(Farm.Name %in% c('el majahual', 'el majahual tempisque'))
df[df$Farm.Name %in% c('el majahual', 'el majahual tempisque'),'Farm.Name'] = 'el majahual'


# 166] "fazenda capoeirinha"                                                                    
# [167] "fazenda capoeirnha" 
df[df$Farm.Name %in% c('fazenda capoeirinha'),'Farm.Name'] = 'fazenda capoeirnha'


# [170] "fazenda do sertao"                                                                      
# [171] "fazenda do sertão"  
df[df$Farm.Name %in% c('fazenda do sertão'),'Farm.Name'] = 'fazenda do sertao'

# [187] "fazendas klem        "                                                                  
# [188] "fazendas klem ltda" 
df %>% filter(Farm.Name %in% c('fazendas klem        ', 'fazendas klem ltda'))
df[df$Farm.Name %in% c('fazendas klem        '),'Farm.Name'] = 'fazendas klem ltda'
df[df$Farm.Name %in% c('fazendas klem        '),'Region'] = 'BRAZIL MATAS DE MINAS'

# 204] "finca huehuetecpam"                                                                     
# [205] "finca huehuetecpan"  
df[df$Farm.Name %in% c('finca huehuetecpam'),'Farm.Name'] = 'finca huehuetecpan'


# [217] "finca los barreales"                                                                    
# [218] "finca los barreales 1112lban"                                                           
# [219] "finca los barreales 1112lbga7250412"
# different - multiple farms owned by the same group
df %>% filter(Farm.Name %in% c('finca los barreales 1112lbga7250412', 'finca los barreales 1112lban','finca los barreales'))

#
# 223] "finca monte grande"                                                                     
# [224] "finca montegrande"  
df[df$Farm.Name %in% c('finca montegrande'),'Farm.Name'] = 'finca monte grande'

# 
# 249] "guo xin ka fei 國昕咖啡"                                                                
# [250] "guo xing ka fei wang 國姓咖啡王咖啡莊園"                                                
# [251] "guoxing farm coffee 國姓農場咖啡 
df %>% filter(Farm.Name %in% c('guo xin ka fei 國昕咖啡', 'guo xing ka fei wang 國姓咖啡王咖啡莊園',
                               'guoxing farm coffee 國姓農場咖啡'))

# 
# [298] "kona pacific farmers co-op"                                                             
# [299] "kona pacific farmers cooperative"   
df[df$Farm.Name %in% c('kona pacific farmers co-op'),'Farm.Name'] = 'kona pacific farmers cooperative'

# 
# 
# 314] "la esmeralda"                                                                           
# [315] "la esperanza"                                                                           
# [316] "la esperanza y anexos"                                                                  
# [317] "la esperanza, margarita nuyes" 
# 
# 
# 336] "la union monte verde"                                                                   
# [337] "la union monteverde"    
df[df$Farm.Name %in% c('la union monteverde'),'Farm.Name'] = 'la union monte verde'

# 359] "los barreales 1112lbcat_s110312"                                                        
# [360] "los barreales 1112lbgr7230412"    
#
# 
# 380] "matsuzawa coffee"                                                                       
# [381] "matsuzawa coffee farm" 
df[df$Farm.Name %in% c('matsuzawa coffee farm'),'Farm.Name'] = 'matsuzawa coffee'

# 
# 
# "n/a"  
df[df$Farm.Name %in% c("n/a"),'Farm.Name'] = ''

# 
# 427] "pereira estate coffee"                                                                  
# [428] "pereira estate coffeee"
df[df$Farm.Name %in% c("pereira estate coffeee"),'Farm.Name'] = 'pereira estate coffee'

# 
# 429] "phone number | 0911-51-08-01, email | at"                                               
# [430] "phone number | 0911-513824 email: gmt.ft"  
# 
# 
# [467] "santa maria"                                                                            
# [468] "santa maria temaxcalapa" 
df %>% filter(Farm.Name %in% c('santa maria', 'santa maria temaxcalapa'))

# 
# [481] "seid damtew coffee export and plantation"                                               
# [482] "seid damtew coffee planation" 
df %>% filter(Farm.Name %in% c('seid damtew coffee planation', 'seid damtew coffee export and plantation'))
df[df$Farm.Name %in% c("seid damtew coffee export and plantation"),'Farm.Name'] = 'seid damtew coffee planation'

# 
# 
# [534] "varias"                                                                                 
# [535] "varias comuidades"                                                                      
# [536] "varias comunidades"                                                                     
# [537] "varias counidades"                                                                      
# [538] "varias fincas"                                                                          
# [539] "varios"                                                                                 
# [540] "varios farms"                                                                           
# [541] "various"                                                                                
# [542] "various small farms"                                                                    
# [543] "various smallholders"                                                                   
# [544] "various smallholders from the municipality of tuba"                                     
# [545] "vary"  
various = c('vary','various smallholders from the municipality of tuba','various smallholders',
            'various small farms','various','varios farms','varios','varias fincas','varias counidades',
            'varias comunidades','varias comuidades','varias')
df %>% filter(Farm.Name %in% various)
df[df$Farm.Name %in% various,'Farm.Name'] = 'various'


##############################################
#     MILL
##############################################

sort(unique(tolower(df$Mill)))
df$Mill = tolower(df$Mill)
# 
# [1] ""                                                                                       
# [2] "-"                                                                                      
# [3] "--" 
df[df$Mill %in% c('-','--'),'Mill'] = ''

# 
# [10] "agroindustrial unidas de mexico, s.a. de c.v. sucursal tuxtla"                          
# [11] "agroindustrias unidas de mexico"                                                        
# [12] "agroindustrias unidas de mexico s.a. de c.v."                                           
# [13] "agroindustrias unidas de mexico, s.s. de c.v. sucursal tuxtla" 
df[df$Mill %in% c('agroindustrial unidas de mexico, s.a. de c.v. sucursal tuxtla','agroindustrias unidas de mexico s.a. de c.v.',
                       'agroindustrias unidas de mexico, s.s. de c.v. sucursal tuxtla'),'Mill'] = 'agroindustrias unidas de mexico'

# 
# 
# [22] "angel albino corzo, chiapas"                                                            
# [23] "angel albio corzo, chiapas"                                                             
# [24] "angel de albino de corzo"   
df[df$Mill %in% c('angel albio corzo, chiapas','angel de albino de corzo'),'Mill'] = 'angel albino corzo, chiapas'

# 
# [40] "baishencun coffee farm百勝村咖啡莊園"                                                   
# [41] "baishengcun coffee 百勝村咖啡莊園" 
df[df$Mill %in% c("baishencun coffee farm百勝村咖啡莊園"),'Mill'] = 'baishengcun coffee 百勝村咖啡莊園'

# 
# 
# [44] "beneficio 2000"                                                                         
# [45] "beneficio 2000 km 25.5 carretera a el salvador, guatemala phone (502) 6661-2800"        
# [46] "beneficio 2000 km. 25.5 carretera a el salvador, guatemala"  
df[df$Mill %in% c("beneficio 2000 km 25.5 carretera a el salvador, guatemala phone (502) 6661-2800", 
                  "beneficio 2000 km. 25.5 carretera a el salvador, guatemala"),'Mill'] = 'beneficio 2000'

# 
# [47] "beneficio atlantic condega"                                                             
# [48] "beneficio atlantic condega." 
df[df$Mill %in% c("beneficio atlantic condega."),'Mill'] = 'beneficio atlantic condega'

# 
# [50] "beneficio atlantic sebaco"                                                              
# [51] "beneficio atlanticsebaco" 
df[df$Mill %in% c("beneficio atlanticsebaco"),'Mill'] = 'beneficio atlantic sebaco'

# 
# 60] "beneficio exportacafe agua santa"                                                       
# [61] "beneficio exportcafe agua santa" 
df[df$Mill %in% c("beneficio exportcafe agua santa"),'Mill'] = 'beneficio exportacafe agua santa'

# 
# 
# 
# [63] "beneficio ixchel"                                                                       
# [64] "beneficio ixchell"  
df[df$Mill %in% c("beneficio ixchell"),'Mill'] = 'beneficio ixchel'

# 
# 
# [69] "beneficio montañas"                                                                     
# [70] "beneficio montañas del diamante" 
df[df$Mill %in% c("beneficio montañas"),'Mill'] = 'beneficio montañas del diamante'

# [74] "beneficio san carlos"                                                                   
# [75] "beneficio san carlos. matagalpa"  
df[df$Mill %in% c("beneficio san carlos. matagalpa"),'Mill'] = 'beneficio san carlos'

# 
# [81] "beneficio serben"                                                                       
# [82] "beneficio serben km 21.5 carretera a villa canales" 
df[df$Mill %in% c("beneficio serben km 21.5 carretera a villa canales"),'Mill'] = "beneficio serben"

# 
# "beneficio siembras vision (154)"                                                        
# [84] "beneficio siembras visión (154)" 
df[df$Mill %in% c("beneficio siembras visión (154)"),'Mill'] = 'beneficio siembras vision (154)'

# 
# 
# [91] "bonanza - armenia"                                                                      
# [92] "bonanza-armenia" 
df[df$Mill %in% c("bonanza-armenia"),'Mill'] = 'bonanza - armenia'

# 
# [101] "cafe altura de san ramon"                                                               
# [102] "cafe de altura san ramon"
df[df$Mill %in% c("cafe de altura san ramon"),'Mill'] = 'cafe altura de san ramon'

# 
# 105] "cafe gourmet de sierra azual sc"                                                        
# [106] "cafe gourmet de sierra azul sc" 
df[df$Mill %in% c("cafe gourmet de sierra azual sc"),'Mill'] = 'cafe gourmet de sierra azul sc'

# 
# 111] "cafetal el equimite, rancho agroecológico"                                              
# [112] "cafetal el equimite, rancho agroecológico s.p.r. de r.l." 
df[df$Mill %in% c("cafetal el equimite, rancho agroecológico s.p.r. de r.l."),'Mill'] = 'cafetal el equimite, rancho agroecológico'

# 
# [128] "central kenya coffee mills"                                                             
# [129] "central kenya mill"
df[df$Mill %in% c("central kenya mill"),'Mill'] = 'central kenya coffee mills'

# 
# [132] "cigrah s.a de c.v"                                                                      
# [133] "cigrah s.a de c.v."                                                                     
# [134] "cigrah s.a. de .v."                                                                     
# [135] "cigrah s.a. de c.v."
df[df$Mill %in% c("cigrah s.a de c.v",'cigrah s.a. de .v.','cigrah s.a. de c.v.'),'Mill'] = 'cigrah s.a de c.v.'

# 
# 
# "cosautlan de carvajal, ver"                                                             
# [166] "cosautlan de carvajal, veracruz, méxico"
df[df$Mill %in% c("cosautlan de carvajal, ver"),'Mill'] = 'cosautlan de carvajal, veracruz, méxico'

# 
# 
# [170] "d.a.e"                                                                                  
# [171] "dae"                                                                                    
# [172] "dae ltd
df[df$Mill %in% c("d.a.e","dae"),'Mill'] = 'dae ltd'

# 
# 177] "dragon coffee 龍咖啡"                                                                   
# [178] "dragon 龍咖啡" 
df[df$Mill %in% c("dragon 龍咖啡"),'Mill'] = 'dragon coffee 龍咖'

# 
# 
# [216] "exclusive"                                                                              
# [217] "exclusive coffees s.a," 
df[df$Mill %in% c("exclusive"),'Mill'] = 'exclusive coffees s.a,'


# 222] "falcafe s.a. de c.v."                                                                   
# [223] "falcafe s.a. de c.v. coatepec veracruz" 
df[df$Mill %in% c("falcafe s.a. de c.v. coatepec veracruz"),'Mill'] = 'alcafe s.a. de c.v'

# 
# 
# [233] "finca los barreales"                                                                    
# [234] "finca los barreales, teocelo, veracruz"  
df[df$Mill %in% c("finca los barreales, teocelo, veracruz"),'Mill'] = 'finca los barreales'

# 
# [245] "great lakes"                                                                            
# [246] "great lakes coffee"                                                                     
# [247] "great lakes coffee uganda"
df[df$Mill %in% c("great lakes", "great lakes coffee uganda"),'Mill'] = 'great lakes coffee'

# 
# 
# [252] "guo xin ka fei 國昕咖啡"                                                                
# [253] "guo xing ka fei wang 國姓咖啡王咖啡莊園"
df[df$Mill %in% c("guo xing ka fei wang 國姓咖啡王咖啡莊園"),'Mill'] = 'guo xin ka fei 國昕咖啡'

# 
# [262] "humedo: finca santo tomas pachuj y seco: beneficio palinsa"                             
# [263] "humedo: finca sto tomas pachuj, seco: beneficio palinsa.
df[df$Mill %in% c("humedo: finca sto tomas pachuj, seco: beneficio palinsa."),'Mill'] = 'humedo: finca santo tomas pachuj y seco: beneficio palinsa'
# 
# [285] "kawacom"                                                                                
# [286] "kawacom (u) ltd"                                                                        
# [287] "kawacom uganda limited"                                                                 
# [288] "kawacom uganda ltd"                                                                     
# [289] "kawacom(u)ltd"  
df[df$Mill %in% c("kawacom (u) ltd", 'kawacom uganda limited', 'kawacom uganda ltd', 'kawacom(u)ltd'),'Mill'] = 'kawacom'

# 
# 
# [336] "mzuzu coffee coop union"                                                                
# [337] "mzuzu coffee coop union        "                                                        
# [338] "mzuzu coffee planters coop union" 
df[df$Mill %in% c("mzuzu coffee coop union        ", 'mzuzu coffee planters coop union'),'Mill'] = 'mzuzu coffee coop union'

# 
# [367] "productores de cafe especiales \"el triunfo\", s.c., chiapas"                           
# [368] "productores de cafes especiales s.c."                                                   
# [369] "productores de cafes especiales sc" 
df[df$Mill %in% c('productores de cafe especiales \"el triunfo\", s.c., chiapas', 'productores de cafes especiales s.c.'),'Mill'] = 'productores de cafes especiales sc'

# 
# 
# [380] "rafiki (coffee) limited"                                                                
# [381] "rafiki coffee limited."                                                                 
# [382] "rafiki ltd"      
df[df$Mill %in% c("rafiki ltd", 'rafiki coffee limited.'),'Mill'] = 'rafiki (coffee) limited'

# 
# [436] "trilladora agricola"                                                                    
# [437] "trilladora agricola - bucaramanga"                                                      
# [438] "trilladora agricola bucaramanga"                                                        
# [439] "trilladora agrícola de santander"                                                       
# [440] "trilladora agricola de santander-bucaramanga"
df[df$Mill %in% c("trilladora agricola - bucaramanga", 'trilladora agricola bucaramanga',
                  'trilladora agrícola de santander','trilladora agricola de santander-bucaramanga'),'Mill'] = 'trilladora agricola'

# 
# 
# [442] "trilladora boananza"                                                                    
# [443] "trilladora bonanaza"                                                                    
# [444] "trilladora bonanza" 
df[df$Mill %in% c("trilladora bonanaza", 'trilladora bonanza'),'Mill'] = 'trilladora boananza'

# 
# 
# 
# 5] "trilladora bonanza - armenia quindio"                                                   
# [446] "trilladora bonanza-armenia"                                                             
# [447] "trilladora bonanza-armenia quindio"                                                     
# [448] "trilladora bonanza-calarca-quindio"   
# [453] "trrilladora bonanza-armenia"                                                            
df[df$Mill %in% c("trilladora bonanza-armenia", 'trilladora bonanza-armenia quindio',
                  'trilladora bonanza-calarca-quindio', 'trrilladora bonanza-armenia'),'Mill'] = 'trilladora bonanza - armenia quindioa'

# 
# 
# 
# [472] "xicotepec de juarez"                                                                    
# [473] "xicotepec de juarez, puebla"  
df[df$Mill %in% c("xicotepec de juarez, puebla"),'Mill'] = 'xicotepec de juarez'

# 
# [505] "黑咖啡道"                                                                               
# [506] "黑咖啡道咖啡"  
df[df$Mill %in% c("黑咖啡道"),'Mill'] = '黑咖啡道咖啡'



##############################################
#     COMPANY NAME 
##############################################
sort(unique(tolower(df$Company)))
df$Company = tolower(df$Company)

# [298] "宸嶧國際"                                                                      
# [299] "宸嶧國際        " 
df %>% filter(Company %in% c())
df[df$Company %in% c("宸嶧國際        "),'Company'] = '宸嶧國際'

# 288] "waelti -schoenfeld"                                                            
# [289] "waelti schoenfeld exportadores de cafe, s.a."   
df %>% filter(Company %in% c('waelti -schoenfeld', 'waelti schoenfeld exportadores de cafe, s.a.'))
df[df$Company %in% c("waelti -schoenfeld"),'Company'] = 'waelti schoenfeld exportadores de cafe, s.a.'

# looks to be different owners / farms, can't assume it's same company 
# 279] "unión de ejidos san fernando"                                                  
# [280] "union de ejidos san fernando de ri"   
df %>% filter(Company %in% c('unión de ejidos san fernando', 'union de ejidos san fernando de ri'))

# [277] "unicafe"                                                                       
# [278] "unicafe, s.a" 
df %>% filter(Company %in% c('unicafe', 'unicafe, s.a'))
df[df$Company %in% c('unicafe, s.a'),'Company'] = 'unicafe'

# [273] "unex (guatemala), s.a"                                                         
# [274] "unex (guatemala), s.a."                                                        
# [275] "unex (guatemala),s.a"                                                          
# [276] "unex guatemala, s.a."   
df[df$Company %in% c('unex (guatemala), s.a', "unex (guatemala), s.a.",  "unex (guatemala),s.a"   ),'Company'] = "unex guatemala, s.a."

# [256] "taylor winch (coffee) ltd"                                                     
# [257] "taylor winch (coffee) ltd."
df[df$Company %in% c('taylor winch (coffee) ltd.'),'Company'] = 'taylor winch (coffee) ltd'

# [241] "siembras vision, s.a"                                                          
# [242] "siembras vision, s.a."                                                         
# [243] "siembras visión, s.a."      
df[df$Company %in% c('siembras vision, s.a', 'siembras visión, s.a.'),'Company'] = 'siembras vision, s.a.'

# [225] "red on tree co., ltd."                                                         
# [226] "red on tree co., ltd.        "   
df[df$Company %in% c('red on tree co., ltd.        '),'Company'] = 'red on tree co., ltd.'

# [258] "taylor winch (t) ltd"                                                          
# [259] "taylor winch(t) ltd"  
df[df$Company %in% c('taylor winch(t) ltd'),'Company'] = 'taylor winch (t) ltd'

# [215] "productos y servicios chilindrón s.a de c.v"                                   
# [216] "productos y servicios chilindron s.a. de c.v."  
df[df$Company %in% c('productos y servicios chilindrón s.a de c.v'),'Company'] = 'productos y servicios chilindron s.a. de c.v.'

# [205] "outspan guatemala s.a."                                                        
# [206] "outspan guatemala, s. a."                                                      
# [207] "outspan guatemala, s.a."   
df[df$Company %in% c('outspan guatemala, s. a.', 'outspan guatemala, s.a.'),'Company'] = 'outspan guatemala s.a.'

# [188] "mzuzu coffee planters coop union"                                              
# [189] "mzuzu coffee planters coop union        " 
df[df$Company %in% c('mzuzu coffee planters coop union        '),'Company'] = 'mzuzu coffee planters coop union'

# [181] "mercon guatemala s.a."                                                         
# [182] "mercon guatemala, s.a."    
df[df$Company %in% c('mercon guatemala, s.a.'),'Company'] = "mercon guatemala s.a."

# [174] "kyagalanyi coffee limited"                                                     
# [175] "kyagalanyi coffee ltd"   
df[df$Company %in% c('kyagalanyi coffee limited'),'Company'] = "kyagalanyi coffee ltd"

# [169] "klem organics coffee"                                                          
# [170] "klem organics coffee        "  
df[df$Company %in% c('klem organics coffee        '),'Company'] = "klem organics coffee"

# see if below two are the same, or split by country 
# [164] "kawacom ltd"                                                                   
# [165] "kawacom uganda ltd" 
df %>% filter(Company %in% c('kawacom ltd', 'kawacom uganda ltd'))
df[df$Company %in% c('kawacom ltd', 'kawacom uganda ltd'),'Company'] = 'kawacom uganda ltd'

# both in uganda so probably same 
# [140] "great lakes coffee"                                                            
# [141] "great lakes coffee uganda"    
df %>% filter(Company %in% c('great lakes coffee','great lakes coffee uganda'))
df[df$Company %in% c('great lakes coffee','great lakes coffee uganda'),'Company'] = 'great lakes coffee'

# 66] "carcafe ltda"                                                                  
# [67] "carcafe ltda ci" 
df[df$Company %in% c('carcafe ltda ci'),'Company'] = 'carcafe ltda'

# [119] "exportcafe"                                                                    
# [120] "exportcafe, s.a"                                                               
# [121] "exportcafe, s.a." 
df[df$Company %in% c('exportcafe, s.a', 'exportcafe, s.a.'),'Company'] = 'exportcafe'

# 57] "cafes tomari sa de c.v."                                                       
# [58] "cafes tomari sa de cv"   
df[df$Company %in% c('cafes tomari sa de c.v.'),'Company'] = 'cafes tomari sa de cv'

# [38] "cafcom"                                                                        
# [39] "cafcom, s. a." 
df[df$Company %in% c('cafcom, s. a.'),'Company'] = 'cafcom'

# [32] "c dorman ltd"                                                                  
# [33] "c dormans kenya ltd"                                                           
# [34] "c. dorman limited"                                                             
# [35] "c.dorman ltd"  
df %>% filter(Company %in% c('c dorman ltd','c dormans kenya ltd', 'c. dorman limited', 'c.dorman ltd'))
df[df$Company %in%c('c dorman ltd','c dormans kenya ltd', 'c. dorman limited', 'c.dorman ltd'),'Company'] = 'c dorman ltd'

# [23] "blossom valley宸嶧國際"                                                        
# [24] "blossom valley宸嶧國際        " 
df[df$Company %in%c('blossom valley宸嶧國際        '),'Company'] = 'blossom valley宸嶧國際'


##############################################
#     REGION
##############################################
sort(unique(tolower(df$Region)))
df$Region = tolower(df$Region)


# [419] "台南市東山區 (dongshan dist., tainan city)"                                  
# [420] "台南市東山區 (dongshan dist., tainan city)"  
df[df$Region %in% c('台南市東山區 (dongshan dist., tainan city)'),'Region'] = '台南市東山區 (dongshan dist., tainan city)'

# [401] "yunnan"                                                                      
# [402] "yunnan province pu'er city lancang county"                                   
# [403] "yunnan province pu'er city menglian county"                                  
# [404] "yunnan province pu'er city menglian county nayun town"                       
# [405] "yunnan province pu'er city mojiang county"                                   
# [406] "yunnan province xishuangbanna city menghai county"                           
# [407] "yunnan province xishuangbanna city puwen county"                             
# [408] "yunnan,china"  
df[df$Region %in% c('yunnan', "yunnan province pu'er city lancang county", "yunnan province pu'er city menglian county",
                    "yunnan province pu'er city menglian county nayun town", "yunnan province pu'er city mojiang county",
                    "yunnan province xishuangbanna city menghai county", "yunnan province xishuangbanna city puwen county",
                    "yunnan,china"),'Region'] = 'yunnan'

 
# [387] "west valley"                                                                 
# [388] "west valley / central valley blend"  
df[df$Region %in% c('west valley', 'west valley / central valley blend'),'Region'] = 'west valley'
 
# "n/a"   
# unkown
df[df$Region %in% c('n/a','unkown'),'Region'] = ''

 
# [361] "teocelo"                                                                     
# [362] "teocelo, veracruz" 
# [379] "veracruz"                                                                    
# [148] "huatusco, veracruz"                                                          
# [146] "huatusco"                                                                    
# [147] "huatusco, ver"
# [94] "cosautlan de carvajal"                                                       
# [95] "cosautlan de carvajal, ver."
df[df$Region %in% c('teocelo','teocelo, veracruz','veracruz', 'huatusco, veracruz',
                    'huatusco', 'huatusco, ver', 'cosautlan de carvajal', 'cosautlan de carvajal, ver.'),'Region'] = 'veracruz'


# [355] "tarrazu"                                                                     
# [356] "tarrazú"                                                                     
# [357] "tarrazu, san jose, costa rica" 
df[df$Region %in% c('tarrazu', 'tarrazú', 'tarrazu, san jose, costa rica'),'Region'] = 'tarrazu'


# [347] "sumatra brastagi"                                                            
# [348] "sumatra brastagi - indonesia"  
df[df$Region %in% c('sumatra brastagi', 'sumatra brastagi - indonesia'),'Region'] = 'sumatra brastagi'


# [344] "sul de minas"                                                                
# [345] "sul de minas - carmo de minas" 
df[df$Region %in% c('sul de minas', 'sul de minas - carmo deminas'),'Region'] = 'sul de minas'


# [342] "south of mzuzu"                                                              
# west of mzuzu
# [256] "north of mzuzu"                                                              
df[df$Region %in% c('south of mzuzu', 'west of mzuzu', 'north of mzuzu'),'Region'] = 'mzuzu'


# [338] "south minas"                                                                 
# [339] "south minas gerais"                                                          
# [340] "south of minas"                                                              
# [341] "south of minas gerais" 
df[df$Region %in% c('south minas', 'south minas gerais', 'south of minas', 'south of minas gerais'),'Region'] = 'south of minas'

# [332] "sipi, mt elgon"                                                              
# [333] "sipi, mt. elgon" 
df[df$Region %in% c('sipi, mt. elgon'),'Region'] = 'sipi, mt elgon'

# [307] "san marcos"                                                                  
# [308] "san marcos ocotepeque"                                                       
# [309] "san marcos, ocotepeque" 
df[df$Region %in% c('san marcos', 'san marcos ocotepeque', 'san marcos, ocotepeque'),'Region'] = 'san marcos'

 
# [303] "san ignacio"                                                                 
# [304] "san ignacio, jaen, cajamarca"  
df[df$Region %in% c('san ignacio, jaen, cajamarca'),'Region'] = 'san ignacio'


# 291] "pu'er city, yunnan, china"                                                   
# [292] "puer"       
df[df$Region %in% c('puer', "pu'er city, yunnan, china"),'Region'] = 'yunnan'


# 283] "pitalito"                                                                    
# [284] "pitalito - huila"                                                            
# [285] "pitalito huila"                                                              
# [286] "pitalito-huila" 
df[df$Region %in% c('pitalito', 'pitalito - huila','pitalito huila', 'pitalito-huila'),'Region'] = 'pitalito'

# [261] "nyeri"                                                                       
# [262] "nyeri county"                                                                
# [263] "nyeri province"                                                              
# [264] "nyeri province, kenya"  
df[df$Region %in% c('nyeri', 'nyeri county', 'nyeri province', 'nyeri province, kenya', 'pitalito-huila'),'Region'] = 'nyeri'


# 259] "nuevo oriente"                                                               
# [260] "nuevo oriente aldea el limon"  
df[df$Region %in% c('nuevo oriente', 'nuevo oriente aldea el limon'),'Region'] = 'nuevo oriente'

# [243] "nantou guoxing 南投縣國姓鄉"                                                 
# [244] "nantou guoxing 南投縣國姓鄉        "                                         
# [245] "nantou lugu bai he lin南投縣鹿谷鄉白鶴林"                                    
# [246] "nantou xinyi 南投縣信義鄉"                                                   
# [247] "nantou 南投日月潭"  
df[df$Region %in% c('nantou guoxing 南投縣國姓鄉', 'nantou guoxing 南投縣國姓鄉        ',
                    'nantou lugu bai he lin南投縣鹿谷鄉白鶴林', 'nantou xinyi 南投縣信義鄉', 
                    'nantou 南投日月潭'),'Region'] = 'nantou'


# [228] "monte carmelo"                                                               
# [229] "monte carmelo - cerrado"                                                     
# [230] "monte carmelo, cerrado" 
df[df$Region %in% c('monte carmelo', 'monte carmelo - cerrado', 'monte carmelo, cerrado'),'Region'] = 'monte carmelo'

# [231] "moshi"                                                                       
# [232] "moshi rural" 
df[df$Region %in% c('moshi rural'),'Region'] = 'moshi'

# 219] "mecatlán"                                                                    
# [220] "menglian" 
df[df$Region %in% c('mecatlán'),'Region'] = 'menglian'


# [217] "mbinga"                                                                      
# [218] "mbinga, ruvuma"  
df[df$Region %in% c('mbinga, ruvuma'),'Region'] = 'mbinga'


# [211] "matagalpa"                                                                   
# [212] "matagalpa - nicaragua"                                                       
# [213] "matagalpa, jinotega and nueva segovia"  
df[df$Region %in% c('matagalpa - nicaragua', 'matagalpa, jinotega and nueva segovia'),'Region'] = 'matagalpa'


# [199] "limu"                                                                        
# [200] "limu (south nation nationalities peoples state region)"                      
# [201] "limu (southwest ethiopia)" 
df[df$Region %in% c('limu (south nation nationalities peoples state region)', 'limu (southwest ethiopia)'),'Region'] = 'limu'


# [184] "kiambu"                                                                      
# [185] "kiambu/meru"  
df[df$Region %in% c('kiambu/meru'),'Region'] = 'kiambu'

# 167] "jinotega"                                                                    
# [168] "jinotega province"                                                           
# [169] "jinotega, nicaragua" 
df[df$Region %in% c('jinotega province', 'jinotega, nicaragua'),'Region'] = 'jinotega'

# [155] "ijen east java"                                                              
# [156] "ijen highland, east java" 
df[df$Region %in% c('ijen east java', 'ijen highland, east java'),'Region'] = 'ijen'

# [141] "guoxing township, nantou county 南投縣國姓鄉"                                
# [142] "guoxing township, nantou county 南投縣國姓鄉        " 
df[df$Region %in% c('guoxing township, nantou county 南投縣國姓鄉', 'guoxing township, nantou county 南投縣國姓鄉        '),'Region'] = 'natou county'

# [138] "guji-hambela/alaka"                                                          
# [139] "guji-hambela/bishan fugu"                                                    
# [140] "guji-hambela/goyo"  
df[df$Region %in% c('guji-hambela/alaka', 'guji-hambela/bishan fugu', 'guji-hambela/goyo'),'Region'] = 'guji-hambela'

# [135] "guayata"                                                                     
# [136] "guayatá"  
df[df$Region %in% c('guayatá'),'Region'] = 'guayata'

# [132] "grama vale"                                                                  
# [133] "grama valley"
df[df$Region %in% c('grama vale'),'Region'] = 'grama valley' 

# [119] "el bálsamo - quezaltepec"                                                    
# [120] "el balsamo, quezaltepec" 
df[df$Region %in% c('el bálsamo - quezaltepec'),'Region'] = 'el balsamo, quezaltepec'

# [112] "dongshan dist., tainan city 台南市東山區"                                    
# [113] "dongshan dist., tainan city 臺南市東山區"  
df[df$Region %in% c("dongshan dist., tainan city 臺南市東山區", "dongshan dist., tainan city 臺南市東山區"),'Region'] = "dongshan dist., tainan city 臺南市東山區"

# [107] "doi chaang village, chiang rai, thialand"                                    
# [108] "doi chaang villiang, chiang rai, thialand" 
df[df$Region %in% c('doi chaang villiang, chiang rai, thialand'),'Region'] = 'doi chaang village, chiang rai, thialand'

# [85] "coaepec"                                                                     
# [86] "coatepec"    
df[df$Region %in% c('coaepec'),'Region'] = 'coatepec'

# [63] "cerrado"                                                                     
# [64] "cerrado - campos altos"                                                      
# [65] "cerrado - monte carmelo"                                                     
# [66] "cerrado - monte carmelo - minas gerais"                                      
# [67] "cerrado mineiro"                                                             
# [68] "cerrado of minas gerais"   
df[df$Region %in% c('cerrado - campos altos', 'cerrado - monte carmelo', 'cerrado - monte carmelo = minas gerais',
                    'cerrado mineiro', 'cerrado of minas gerais'),'Region'] = 'cerrado'

# [60] "central region"                                                              
# [61] "central valley"                                                              
# [62] "central valley - west valley"  
df[df$Region %in% c('central valley', 'central valley - west valley'),'Region'] = 'central valley'

# 58] "central kenya"                                                               
# [59] "central kenya - nyeri"  
# [40] "blend - central & western kenya"                                             
df[df$Region %in% c('central kenya - nyeri', 'blend - central & western kenya'),'Region'] = 'central kenya'

# [54] "carmo de minas"                                                              
# [55] "carmo de minas micro region" 
df[df$Region %in% c('carmo de minas micro region'),'Region'] = 'carmo de minas'

# [43] "boquete"                                                                     
# [44] "boquete, panama" 
df[df$Region %in% c('boquete, panama'),'Region'] = 'boquete'

# [34] "bali"                                                                        
# [35] "bali kintamani" 
df[df$Region %in% c('bali kintamani'),'Region'] = 'bali'

# [21] "apaneca"                                                                     
# [22] "apaneca - ilamatepec"                                                        
# [23] "apaneca ilamatepec mountain range"                                           
# [24] "apaneca llamantepec"                                                         
# [25] "apaneca-ilamatepec" 
df[df$Region %in% c('apaneca - ilamatepec', 'apaneca ilamatepec mountain range',
                    'apaneca llamantepec', 'apaneca-ilamatepec'),'Region'] = 'apaneca'

# [16] "antioquia"                                                                   
# [17] "antioquía-betulia"                                                           
# [18] "antioquía-caicedo"                                                           
# [19] "antioquía-giraldo"                                                           
# [20] "antioquía-urrao"
df[df$Region %in% c('antioquía-betulia', 'antioquía-caicedo',
                    'antioquía-giraldo', 'antioquía-urrao'),'Region'] = 'antioquia'
# [3] "acatenango"                                                                  
# [4] "acatenango, chimaltenango"
df[df$Region %in% c('acatenango, chimaltenango'),'Region'] = 'acatenango'

df = df[df$Farm.Name != 'test',]

df$alt = NULL
df$altitude_low_meters = df$alt_low
df$altitude_high_meters = df$alt_high
df$altitude_mean_meters = df$alt_mean
df$alt_low = NULL
df$alt_high = NULL
df$alt_mean = NULL
df$Notes = NULL

write.csv(df,file='/Users/jledoux/Documents/projects/coffee-quality-database/data/arabica_data_cleaned.csv')

########################################
# repeat for robustas
########################################
df = read.csv('/Users/jledoux/Documents/projects/coffee-quality-database/data/robusta_ratings_raw.csv')


df$X = NULL
df$X.1 = NULL
df$view_certificate_1 = NULL
df$view_certificate_2 = NULL
df$Cupping.Protocol.and.Descriptors = NULL
df$View.Green.Analysis.Details = NULL
df$Request.a.Sample = NULL
df$NA. = NULL
df$NA.1 = NULL
df$NA.2 = NULL
df$NA.3 = NULL
df$NA..1 = NULL
df$NA..2 = NULL
df$NA..3 = NULL
df$Status = NULL
df$Total.Cup.Points = df$quality_score
df$quality_score = NULL
df$Notes = NULL

# columns
# [1] "quality_score"         "Species"               "Owner"                 "Country.of.Origin"    
# [5] "Farm.Name"             "Lot.Number"            "Mill"                  "ICO.Number"           
# [9] "Company"               "Altitude"              "Region"                "Producer"             
# [13] "Number.of.Bags"        "Bag.Weight"            "In.Country.Partner"    "Harvest.Year"         
# [17] "Grading.Date"          "Owner.1"               "Variety"               "Status"               
# [21] "Processing.Method"     "Aroma"                 "Flavor"                "Aftertaste"           
# [25] "Acidity"               "Body"                  "Balance"               "NA.1"                 
# [29] "Uniformity"            "Clean.Cup"             "Sweetness"             "Cupper.Points"        
# [33] "Total.Cup.Points"      "NA.2"                  "Moisture"              "Category.One.Defects" 
# [37] "Quakers"               "Color"                 "Category.Two.Defects"  "NA.3"                 
# [41] "Expiration"            "Certification.Body"    "Certification.Address" "Certification.Contact"
# [45] "X.1"                   "Notes" 

# row X is a test row. get rid of it. 


##############################################
# ALTITUDE
##############################################
# several different encoding chemes here, usually depicting a range 
# 1: get unit (m / ft)
# 2: locate numbers in strings, split to two columns 
# 3: third column as the middle of the range, we'll call this the altitude 
df$Altitude = tolower(df$Altitude)
unique(df$Altitude)
df$unit_of_measurement = 'm'

# first, there are some garbage values in this column. drop them. 
drops = c('unkown','TEST', 'none', 'n/a', 'Huanuco', '-(average)', '-')
df[df$Altitude %in% drops,'Altitude'] = ''

# now get units. default is meters, find non-meters and change their value 
# remaining units: (none), 公尺, P.S.N.M, feet, msnm, mts, ft, m.s.n.m., P.S.N.M, MASL, mals, F, Feet, Pies, meters above sea level

# units in dataset:
# msnm: meters above sea level 
# 公尺: meters 
# P.S.N.M: feet above sea lvel (pies sobre el nivel del mar)
# mts: meters 
# MASL: meters above sea level 
# mals: meters above sea level 
# Pies: feet 

# everything besides the feet-related ones can stay in meters
# unknowns to be forced to empty strings 

unique(df$Altitude)
feets = c('feet','ft',' f', 'pies', 'P.S.N.M:', 'psnm')

# first check to be sure this isn't replacing any of the wrong values 
fun = function(x) {df[grep(x, df$Altitude),'Altitude']}
sapply(feets, fun)

feets = c('feet','ft',' f', 'pies')
fun = function(x) {
  match_vals = df[grep(x, df$Altitude),'Altitude']
  df[df$Altitude %in% match_vals,'unit_of_measurement'] <<- 'ft' # assign value, sticks outside of function bc double arrow
  return(df)
}
df2 = sapply(feets, fun)
sapply(feets, fun)

# of those with no declared unit, can we assign one? 
table(df[df$unit_of_measurement=='ft','Country.of.Origin'])
table(df[df$Country.of.Origin=='Guatemala','unit_of_measurement'])

# always assign ft if a us state/territory, guatemala is a bit of a mixed bag 
df[df$Country.of.Origin %in% c('United States (Hawaii)', 'United States (Puerto Rico)'),'unit_of_measurement'] = 'ft'

# grab lower and upper bound numbers for altitude 
df$Altitude = gsub(",","",df$Altitude) # commas mess up the string splitting
df$alt = sapply(df$Altitude, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]]) #[0-9]+
Split <- strsplit(as.character(df$alt), ", ", fixed = TRUE)
df$alt_low <- sapply(Split, "[", 1)
df$alt_high <- sapply(Split, "[", 2)
df$alt_low = sapply(df$alt_low, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]])
df$alt_high = sapply(df$alt_high, function(x) str_extract_all(x, "\\d+\\.*\\d*")[[1]])

#remove commas and spaces, make numeric
df$alt_low = gsub('\\.','',df$alt_low)
df$alt_high = gsub('\\.','',df$alt_high)

# replace zeros with blanks
df[df$alt_low==0,'alt_low'] = NA

# if no available high alt, replace alt high with alt low 
df[is.na(df$alt_high),'alt_high'] = df[is.na(df$alt_high),'alt_low']

# mean alt column to deal with farms where we're provided a range 
df$alt_low = as.numeric(df$alt_low)
df$alt_high = as.numeric(df$alt_high)
df$alt_mean = rowMeans(df[,c('alt_low','alt_high')],na.rm=TRUE)

# if ht. > 3500 it's probably feet. most points in guatemala are < 3500 m 
df[df$Country.of.Origin=='Guatemala',]
df[(df$Country.of.Origin=='Guatemala') & (df$Altitude>3500),'unit_of_measurement'] = 'ft'

# standardize units of measurement  - 1 ft = 0.3048 m 
df[df$unit_of_measurement=='ft',c('alt_low','alt_high','alt_mean')] = df[df$unit_of_measurement=='ft',c('alt_low','alt_high','alt_mean')]*0.3048

# did it work? 
head(df[,c('Altitude','alt_low','alt_high','alt_mean','unit_of_measurement')], 100)


##############################
# MOISTURE: make it a decimal 
##############################
df$Moisture = substr(df$Moisture,1,nchar(as.character(df$Moisture))-2)
df$Moisture = as.numeric(df$Moisture) / 100


##############################
# DEFECTS: make numeric 
##############################
df$Category.One.Defects = as.numeric(substr(df$Category.One.Defects,1,nchar(as.character(df$Category.One.Defects))-13))
df$Category.Two.Defects = as.numeric(substr(df$Category.Two.Defects,1,nchar(as.character(df$Category.Two.Defects))-13))

##############################
# PII - anonymize this 
##############################
# Certification.Address    Certification.Contact
df$Certification.Contact = sapply(df$Certification.Contact, function(x) sha1(x))
df$Certification.Address = sapply(df$Certification.Address, function(x) sha1(x))

##############################################
#     OWNER
##############################################

# Owner - see if any are miscoded 
sort(unique(tolower(df$Owner)))
df$Owner = tolower(df$Owner)

# the following my be duplicates. investigate and fix if needed
#[283] Taylor Winch (Coffee) Ltd.                        
#[284] Taylor Winch (T) Ltd  
# might be different, not obvious from looking them both up
df %>% filter(Owner %in% c('taylor winch (coffee) ltd.', 'taylor winch (t) ltd'))

#[180] Klem Organics                                     
#[181] KlemOrganics 
# same
df[df$Owner=='klemorganics','Owner'] = 'klem organics'


#[152] JESUS CARLOS CADENA VALDIVIA                      
#[153] JESUS CARLOS CARDENAS VALDIVIA
# same 
df[df$Owner=='jesus carlos cadena valdiva','Owner'] = 'jesus carlos cadenas valdiva'

#[101] "federacion nacional de cafeteros"                  
#[102] "federación nacional de cafeteros" 
# same
df[df$Owner=='federación nacional de cafeteros','Owner'] = 'federacion nacional de cafeteros'


#Country.of.Origin
sort(unique(tolower(df$Country.of.Origin)))

##############################################
#     FARM NAME
##############################################

sort(unique(tolower(df$Farm.Name)))
df$Farm.Name = tolower(df$Farm.Name)

# [61] "capoeirinha"                                                                            
# [62] "capoeirinha farm"                                                                       
# [63] "capoerinha farm"
df %>% filter(Farm.Name %in% c('capoeirinha', 'capoeirinha farm','capoerinha farm'))
df[df$Farm.Name %in% c('capoeirinha farm','capoerinha farm'),'Farm.Name'] = 'capoeirinha'

# [90] "conquista / morito"                                                                     
# [91] "conquista/ morito"                                                                      
# [92] "conquista/morito"  
df[df$Farm.Name %in% c('conquista/ morito','conquista/morito'),'Farm.Name'] = 'conquista / morito'

# [127] "el desmoronado"                                                                         
# [128] "el desmoronado, talpan de allende jalisco"    
# different owner, keeping them separate 
df %>% filter(Farm.Name %in% c('el desmoronado', 'el desmoronado, talpan de allende jalisco'))

# 
# [138] "el majahual"                                                                            
# [139] "el majahual tempisque" 
df %>% filter(Farm.Name %in% c('el majahual', 'el majahual tempisque'))
df[df$Farm.Name %in% c('el majahual', 'el majahual tempisque'),'Farm.Name'] = 'el majahual'


# 166] "fazenda capoeirinha"                                                                    
# [167] "fazenda capoeirnha" 
df[df$Farm.Name %in% c('fazenda capoeirinha'),'Farm.Name'] = 'fazenda capoeirnha'


# [170] "fazenda do sertao"                                                                      
# [171] "fazenda do sertão"  
df[df$Farm.Name %in% c('fazenda do sertão'),'Farm.Name'] = 'fazenda do sertao'

# 204] "finca huehuetecpam"                                                                     
# [205] "finca huehuetecpan"  
df[df$Farm.Name %in% c('finca huehuetecpam'),'Farm.Name'] = 'finca huehuetecpan'


# [217] "finca los barreales"                                                                    
# [218] "finca los barreales 1112lban"                                                           
# [219] "finca los barreales 1112lbga7250412"
# different - multiple farms owned by the same group
df %>% filter(Farm.Name %in% c('finca los barreales 1112lbga7250412', 'finca los barreales 1112lban','finca los barreales'))

#
# 223] "finca monte grande"                                                                     
# [224] "finca montegrande"  
df[df$Farm.Name %in% c('finca montegrande'),'Farm.Name'] = 'finca monte grande'

# 
# 249] "guo xin ka fei 國昕咖啡"                                                                
# [250] "guo xing ka fei wang 國姓咖啡王咖啡莊園"                                                
# [251] "guoxing farm coffee 國姓農場咖啡 
df %>% filter(Farm.Name %in% c('guo xin ka fei 國昕咖啡', 'guo xing ka fei wang 國姓咖啡王咖啡莊園',
                               'guoxing farm coffee 國姓農場咖啡'))

# 
# [298] "kona pacific farmers co-op"                                                             
# [299] "kona pacific farmers cooperative"   
df[df$Farm.Name %in% c('kona pacific farmers co-op'),'Farm.Name'] = 'kona pacific farmers cooperative'

# 
# 
# 314] "la esmeralda"                                                                           
# [315] "la esperanza"                                                                           
# [316] "la esperanza y anexos"                                                                  
# [317] "la esperanza, margarita nuyes" 
# 
# 
# 336] "la union monte verde"                                                                   
# [337] "la union monteverde"    
df[df$Farm.Name %in% c('la union monteverde'),'Farm.Name'] = 'la union monte verde'

# 359] "los barreales 1112lbcat_s110312"                                                        
# [360] "los barreales 1112lbgr7230412"    
#
# 
# 380] "matsuzawa coffee"                                                                       
# [381] "matsuzawa coffee farm" 
df[df$Farm.Name %in% c('matsuzawa coffee farm'),'Farm.Name'] = 'matsuzawa coffee'

# 
# 
# "n/a"  
df[df$Farm.Name %in% c("n/a"),'Farm.Name'] = ''

# 
# 427] "pereira estate coffee"                                                                  
# [428] "pereira estate coffeee"
df[df$Farm.Name %in% c("pereira estate coffeee"),'Farm.Name'] = 'pereira estate coffee'

# 
# 429] "phone number | 0911-51-08-01, email | at"                                               
# [430] "phone number | 0911-513824 email: gmt.ft"  
# 
# 
# [467] "santa maria"                                                                            
# [468] "santa maria temaxcalapa" 
df %>% filter(Farm.Name %in% c('santa maria', 'santa maria temaxcalapa'))

# 
# [481] "seid damtew coffee export and plantation"                                               
# [482] "seid damtew coffee planation" 
df %>% filter(Farm.Name %in% c('seid damtew coffee planation', 'seid damtew coffee export and plantation'))
df[df$Farm.Name %in% c("seid damtew coffee export and plantation"),'Farm.Name'] = 'seid damtew coffee planation'

# 
# 
# [534] "varias"                                                                                 
# [535] "varias comuidades"                                                                      
# [536] "varias comunidades"                                                                     
# [537] "varias counidades"                                                                      
# [538] "varias fincas"                                                                          
# [539] "varios"                                                                                 
# [540] "varios farms"                                                                           
# [541] "various"                                                                                
# [542] "various small farms"                                                                    
# [543] "various smallholders"                                                                   
# [544] "various smallholders from the municipality of tuba"                                     
# [545] "vary"  
various = c('vary','various smallholders from the municipality of tuba','various smallholders',
            'various small farms','various','varios farms','varios','varias fincas','varias counidades',
            'varias comunidades','varias comuidades','varias')
df %>% filter(Farm.Name %in% various)
df[df$Farm.Name %in% various,'Farm.Name'] = 'various'


##############################################
#     MILL
##############################################

sort(unique(tolower(df$Mill)))
df$Mill = tolower(df$Mill)
# 
# [1] ""                                                                                       
# [2] "-"                                                                                      
# [3] "--" 
df[df$Mill %in% c('-','--'),'Mill'] = ''

# 
# [10] "agroindustrial unidas de mexico, s.a. de c.v. sucursal tuxtla"                          
# [11] "agroindustrias unidas de mexico"                                                        
# [12] "agroindustrias unidas de mexico s.a. de c.v."                                           
# [13] "agroindustrias unidas de mexico, s.s. de c.v. sucursal tuxtla" 
df[df$Mill %in% c('agroindustrial unidas de mexico, s.a. de c.v. sucursal tuxtla','agroindustrias unidas de mexico s.a. de c.v.',
                  'agroindustrias unidas de mexico, s.s. de c.v. sucursal tuxtla'),'Mill'] = 'agroindustrias unidas de mexico'

# 
# 
# [22] "angel albino corzo, chiapas"                                                            
# [23] "angel albio corzo, chiapas"                                                             
# [24] "angel de albino de corzo"   
df[df$Mill %in% c('angel albio corzo, chiapas','angel de albino de corzo'),'Mill'] = 'angel albino corzo, chiapas'

# 
# [40] "baishencun coffee farm百勝村咖啡莊園"                                                   
# [41] "baishengcun coffee 百勝村咖啡莊園" 
df[df$Mill %in% c("baishencun coffee farm百勝村咖啡莊園"),'Mill'] = 'baishengcun coffee 百勝村咖啡莊園'

# 
# 
# [44] "beneficio 2000"                                                                         
# [45] "beneficio 2000 km 25.5 carretera a el salvador, guatemala phone (502) 6661-2800"        
# [46] "beneficio 2000 km. 25.5 carretera a el salvador, guatemala"  
df[df$Mill %in% c("beneficio 2000 km 25.5 carretera a el salvador, guatemala phone (502) 6661-2800", 
                  "beneficio 2000 km. 25.5 carretera a el salvador, guatemala"),'Mill'] = 'beneficio 2000'

# 
# [47] "beneficio atlantic condega"                                                             
# [48] "beneficio atlantic condega." 
df[df$Mill %in% c("beneficio atlantic condega."),'Mill'] = 'beneficio atlantic condega'

# 
# [50] "beneficio atlantic sebaco"                                                              
# [51] "beneficio atlanticsebaco" 
df[df$Mill %in% c("beneficio atlanticsebaco"),'Mill'] = 'beneficio atlantic sebaco'

# 
# 60] "beneficio exportacafe agua santa"                                                       
# [61] "beneficio exportcafe agua santa" 
df[df$Mill %in% c("beneficio exportcafe agua santa"),'Mill'] = 'beneficio exportacafe agua santa'

# 
# 
# 
# [63] "beneficio ixchel"                                                                       
# [64] "beneficio ixchell"  
df[df$Mill %in% c("beneficio ixchell"),'Mill'] = 'beneficio ixchel'

# 
# 
# [69] "beneficio montañas"                                                                     
# [70] "beneficio montañas del diamante" 
df[df$Mill %in% c("beneficio montañas"),'Mill'] = 'beneficio montañas del diamante'

# [74] "beneficio san carlos"                                                                   
# [75] "beneficio san carlos. matagalpa"  
df[df$Mill %in% c("beneficio san carlos. matagalpa"),'Mill'] = 'beneficio san carlos'

# 
# [81] "beneficio serben"                                                                       
# [82] "beneficio serben km 21.5 carretera a villa canales" 
df[df$Mill %in% c("beneficio serben km 21.5 carretera a villa canales"),'Mill'] = "beneficio serben"

# 
# "beneficio siembras vision (154)"                                                        
# [84] "beneficio siembras visión (154)" 
df[df$Mill %in% c("beneficio siembras visión (154)"),'Mill'] = 'beneficio siembras vision (154)'

# 
# 
# [91] "bonanza - armenia"                                                                      
# [92] "bonanza-armenia" 
df[df$Mill %in% c("bonanza-armenia"),'Mill'] = 'bonanza - armenia'

# 
# [101] "cafe altura de san ramon"                                                               
# [102] "cafe de altura san ramon"
df[df$Mill %in% c("cafe de altura san ramon"),'Mill'] = 'cafe altura de san ramon'

# 
# 105] "cafe gourmet de sierra azual sc"                                                        
# [106] "cafe gourmet de sierra azul sc" 
df[df$Mill %in% c("cafe gourmet de sierra azual sc"),'Mill'] = 'cafe gourmet de sierra azul sc'

# 
# 111] "cafetal el equimite, rancho agroecológico"                                              
# [112] "cafetal el equimite, rancho agroecológico s.p.r. de r.l." 
df[df$Mill %in% c("cafetal el equimite, rancho agroecológico s.p.r. de r.l."),'Mill'] = 'cafetal el equimite, rancho agroecológico'

# 
# [128] "central kenya coffee mills"                                                             
# [129] "central kenya mill"
df[df$Mill %in% c("central kenya mill"),'Mill'] = 'central kenya coffee mills'

# 
# [132] "cigrah s.a de c.v"                                                                      
# [133] "cigrah s.a de c.v."                                                                     
# [134] "cigrah s.a. de .v."                                                                     
# [135] "cigrah s.a. de c.v."
df[df$Mill %in% c("cigrah s.a de c.v",'cigrah s.a. de .v.','cigrah s.a. de c.v.'),'Mill'] = 'cigrah s.a de c.v.'

# 
# 
# "cosautlan de carvajal, ver"                                                             
# [166] "cosautlan de carvajal, veracruz, méxico"
df[df$Mill %in% c("cosautlan de carvajal, ver"),'Mill'] = 'cosautlan de carvajal, veracruz, méxico'

# 
# 
# [170] "d.a.e"                                                                                  
# [171] "dae"                                                                                    
# [172] "dae ltd
df[df$Mill %in% c("d.a.e","dae"),'Mill'] = 'dae ltd'

# 
# 177] "dragon coffee 龍咖啡"                                                                   
# [178] "dragon 龍咖啡" 
df[df$Mill %in% c("dragon 龍咖啡"),'Mill'] = 'dragon coffee 龍咖'

# 
# 
# [216] "exclusive"                                                                              
# [217] "exclusive coffees s.a," 
df[df$Mill %in% c("exclusive"),'Mill'] = 'exclusive coffees s.a,'


# 222] "falcafe s.a. de c.v."                                                                   
# [223] "falcafe s.a. de c.v. coatepec veracruz" 
df[df$Mill %in% c("falcafe s.a. de c.v. coatepec veracruz"),'Mill'] = 'alcafe s.a. de c.v'

# 
# 
# [233] "finca los barreales"                                                                    
# [234] "finca los barreales, teocelo, veracruz"  
df[df$Mill %in% c("finca los barreales, teocelo, veracruz"),'Mill'] = 'finca los barreales'

# 
# [245] "great lakes"                                                                            
# [246] "great lakes coffee"                                                                     
# [247] "great lakes coffee uganda"
df[df$Mill %in% c("great lakes", "great lakes coffee uganda"),'Mill'] = 'great lakes coffee'

# 
# 
# [252] "guo xin ka fei 國昕咖啡"                                                                
# [253] "guo xing ka fei wang 國姓咖啡王咖啡莊園"
df[df$Mill %in% c("guo xing ka fei wang 國姓咖啡王咖啡莊園"),'Mill'] = 'guo xin ka fei 國昕咖啡'

# 
# [262] "humedo: finca santo tomas pachuj y seco: beneficio palinsa"                             
# [263] "humedo: finca sto tomas pachuj, seco: beneficio palinsa.
df[df$Mill %in% c("humedo: finca sto tomas pachuj, seco: beneficio palinsa."),'Mill'] = 'humedo: finca santo tomas pachuj y seco: beneficio palinsa'
# 
# [285] "kawacom"                                                                                
# [286] "kawacom (u) ltd"                                                                        
# [287] "kawacom uganda limited"                                                                 
# [288] "kawacom uganda ltd"                                                                     
# [289] "kawacom(u)ltd"  
df[df$Mill %in% c("kawacom (u) ltd", 'kawacom uganda limited', 'kawacom uganda ltd', 'kawacom(u)ltd'),'Mill'] = 'kawacom'

# 
# 
# [336] "mzuzu coffee coop union"                                                                
# [337] "mzuzu coffee coop union        "                                                        
# [338] "mzuzu coffee planters coop union" 
df[df$Mill %in% c("mzuzu coffee coop union        ", 'mzuzu coffee planters coop union'),'Mill'] = 'mzuzu coffee coop union'

# 
# [367] "productores de cafe especiales \"el triunfo\", s.c., chiapas"                           
# [368] "productores de cafes especiales s.c."                                                   
# [369] "productores de cafes especiales sc" 
df[df$Mill %in% c('productores de cafe especiales \"el triunfo\", s.c., chiapas', 'productores de cafes especiales s.c.'),'Mill'] = 'productores de cafes especiales sc'

# 
# 
# [380] "rafiki (coffee) limited"                                                                
# [381] "rafiki coffee limited."                                                                 
# [382] "rafiki ltd"      
df[df$Mill %in% c("rafiki ltd", 'rafiki coffee limited.'),'Mill'] = 'rafiki (coffee) limited'

# 
# [436] "trilladora agricola"                                                                    
# [437] "trilladora agricola - bucaramanga"                                                      
# [438] "trilladora agricola bucaramanga"                                                        
# [439] "trilladora agrícola de santander"                                                       
# [440] "trilladora agricola de santander-bucaramanga"
df[df$Mill %in% c("trilladora agricola - bucaramanga", 'trilladora agricola bucaramanga',
                  'trilladora agrícola de santander','trilladora agricola de santander-bucaramanga'),'Mill'] = 'trilladora agricola'

# 
# 
# [442] "trilladora boananza"                                                                    
# [443] "trilladora bonanaza"                                                                    
# [444] "trilladora bonanza" 
df[df$Mill %in% c("trilladora bonanaza", 'trilladora bonanza'),'Mill'] = 'trilladora boananza'

# 
# 
# 
# 5] "trilladora bonanza - armenia quindio"                                                   
# [446] "trilladora bonanza-armenia"                                                             
# [447] "trilladora bonanza-armenia quindio"                                                     
# [448] "trilladora bonanza-calarca-quindio"   
# [453] "trrilladora bonanza-armenia"                                                            
df[df$Mill %in% c("trilladora bonanza-armenia", 'trilladora bonanza-armenia quindio',
                  'trilladora bonanza-calarca-quindio', 'trrilladora bonanza-armenia'),'Mill'] = 'trilladora bonanza - armenia quindioa'

# 
# 
# 
# [472] "xicotepec de juarez"                                                                    
# [473] "xicotepec de juarez, puebla"  
df[df$Mill %in% c("xicotepec de juarez, puebla"),'Mill'] = 'xicotepec de juarez'

# 
# [505] "黑咖啡道"                                                                               
# [506] "黑咖啡道咖啡"  
df[df$Mill %in% c("黑咖啡道"),'Mill'] = '黑咖啡道咖啡'



##############################################
#     COMPANY NAME 
##############################################
sort(unique(tolower(df$Company)))
df$Company = tolower(df$Company)

# [298] "宸嶧國際"                                                                      
# [299] "宸嶧國際        " 
df %>% filter(Company %in% c())
df[df$Company %in% c("宸嶧國際        "),'Company'] = '宸嶧國際'

# 288] "waelti -schoenfeld"                                                            
# [289] "waelti schoenfeld exportadores de cafe, s.a."   
df %>% filter(Company %in% c('waelti -schoenfeld', 'waelti schoenfeld exportadores de cafe, s.a.'))
df[df$Company %in% c("waelti -schoenfeld"),'Company'] = 'waelti schoenfeld exportadores de cafe, s.a.'

# looks to be different owners / farms, can't assume it's same company 
# 279] "unión de ejidos san fernando"                                                  
# [280] "union de ejidos san fernando de ri"   
df %>% filter(Company %in% c('unión de ejidos san fernando', 'union de ejidos san fernando de ri'))

# [277] "unicafe"                                                                       
# [278] "unicafe, s.a" 
df %>% filter(Company %in% c('unicafe', 'unicafe, s.a'))
df[df$Company %in% c('unicafe, s.a'),'Company'] = 'unicafe'

# [273] "unex (guatemala), s.a"                                                         
# [274] "unex (guatemala), s.a."                                                        
# [275] "unex (guatemala),s.a"                                                          
# [276] "unex guatemala, s.a."   
df[df$Company %in% c('unex (guatemala), s.a', "unex (guatemala), s.a.",  "unex (guatemala),s.a"   ),'Company'] = "unex guatemala, s.a."

# [256] "taylor winch (coffee) ltd"                                                     
# [257] "taylor winch (coffee) ltd."
df[df$Company %in% c('taylor winch (coffee) ltd.'),'Company'] = 'taylor winch (coffee) ltd'

# [241] "siembras vision, s.a"                                                          
# [242] "siembras vision, s.a."                                                         
# [243] "siembras visión, s.a."      
df[df$Company %in% c('siembras vision, s.a', 'siembras visión, s.a.'),'Company'] = 'siembras vision, s.a.'

# [225] "red on tree co., ltd."                                                         
# [226] "red on tree co., ltd.        "   
df[df$Company %in% c('red on tree co., ltd.        '),'Company'] = 'red on tree co., ltd.'

# [258] "taylor winch (t) ltd"                                                          
# [259] "taylor winch(t) ltd"  
df[df$Company %in% c('taylor winch(t) ltd'),'Company'] = 'taylor winch (t) ltd'

# [215] "productos y servicios chilindrón s.a de c.v"                                   
# [216] "productos y servicios chilindron s.a. de c.v."  
df[df$Company %in% c('productos y servicios chilindrón s.a de c.v'),'Company'] = 'productos y servicios chilindron s.a. de c.v.'

# [205] "outspan guatemala s.a."                                                        
# [206] "outspan guatemala, s. a."                                                      
# [207] "outspan guatemala, s.a."   
df[df$Company %in% c('outspan guatemala, s. a.', 'outspan guatemala, s.a.'),'Company'] = 'outspan guatemala s.a.'

# [188] "mzuzu coffee planters coop union"                                              
# [189] "mzuzu coffee planters coop union        " 
df[df$Company %in% c('mzuzu coffee planters coop union        '),'Company'] = 'mzuzu coffee planters coop union'

# [181] "mercon guatemala s.a."                                                         
# [182] "mercon guatemala, s.a."    
df[df$Company %in% c('mercon guatemala, s.a.'),'Company'] = "mercon guatemala s.a."

# [174] "kyagalanyi coffee limited"                                                     
# [175] "kyagalanyi coffee ltd"   
df[df$Company %in% c('kyagalanyi coffee limited'),'Company'] = "kyagalanyi coffee ltd"

# [169] "klem organics coffee"                                                          
# [170] "klem organics coffee        "  
df[df$Company %in% c('klem organics coffee        '),'Company'] = "klem organics coffee"

# see if below two are the same, or split by country 
# [164] "kawacom ltd"                                                                   
# [165] "kawacom uganda ltd" 
df %>% filter(Company %in% c('kawacom ltd', 'kawacom uganda ltd'))
df[df$Company %in% c('kawacom ltd', 'kawacom uganda ltd'),'Company'] = 'kawacom uganda ltd'

# both in uganda so probably same 
# [140] "great lakes coffee"                                                            
# [141] "great lakes coffee uganda"    
df %>% filter(Company %in% c('great lakes coffee','great lakes coffee uganda'))
df[df$Company %in% c('great lakes coffee','great lakes coffee uganda'),'Company'] = 'great lakes coffee'

# 66] "carcafe ltda"                                                                  
# [67] "carcafe ltda ci" 
df[df$Company %in% c('carcafe ltda ci'),'Company'] = 'carcafe ltda'

# [119] "exportcafe"                                                                    
# [120] "exportcafe, s.a"                                                               
# [121] "exportcafe, s.a." 
df[df$Company %in% c('exportcafe, s.a', 'exportcafe, s.a.'),'Company'] = 'exportcafe'

# 57] "cafes tomari sa de c.v."                                                       
# [58] "cafes tomari sa de cv"   
df[df$Company %in% c('cafes tomari sa de c.v.'),'Company'] = 'cafes tomari sa de cv'

# [38] "cafcom"                                                                        
# [39] "cafcom, s. a." 
df[df$Company %in% c('cafcom, s. a.'),'Company'] = 'cafcom'

# [32] "c dorman ltd"                                                                  
# [33] "c dormans kenya ltd"                                                           
# [34] "c. dorman limited"                                                             
# [35] "c.dorman ltd"  
df %>% filter(Company %in% c('c dorman ltd','c dormans kenya ltd', 'c. dorman limited', 'c.dorman ltd'))
df[df$Company %in%c('c dorman ltd','c dormans kenya ltd', 'c. dorman limited', 'c.dorman ltd'),'Company'] = 'c dorman ltd'

# [23] "blossom valley宸嶧國際"                                                        
# [24] "blossom valley宸嶧國際        " 
df[df$Company %in%c('blossom valley宸嶧國際        '),'Company'] = 'blossom valley宸嶧國際'


##############################################
#     REGION
##############################################
sort(unique(tolower(df$Region)))
df$Region = tolower(df$Region)


# [419] "台南市東山區 (dongshan dist., tainan city)"                                  
# [420] "台南市東山區 (dongshan dist., tainan city)"  
df[df$Region %in% c('台南市東山區 (dongshan dist., tainan city)'),'Region'] = '台南市東山區 (dongshan dist., tainan city)'

# [401] "yunnan"                                                                      
# [402] "yunnan province pu'er city lancang county"                                   
# [403] "yunnan province pu'er city menglian county"                                  
# [404] "yunnan province pu'er city menglian county nayun town"                       
# [405] "yunnan province pu'er city mojiang county"                                   
# [406] "yunnan province xishuangbanna city menghai county"                           
# [407] "yunnan province xishuangbanna city puwen county"                             
# [408] "yunnan,china"  
df[df$Region %in% c('yunnan', "yunnan province pu'er city lancang county", "yunnan province pu'er city menglian county",
                    "yunnan province pu'er city menglian county nayun town", "yunnan province pu'er city mojiang county",
                    "yunnan province xishuangbanna city menghai county", "yunnan province xishuangbanna city puwen county",
                    "yunnan,china"),'Region'] = 'yunnan'


# [387] "west valley"                                                                 
# [388] "west valley / central valley blend"  
df[df$Region %in% c('west valley', 'west valley / central valley blend'),'Region'] = 'west valley'

# "n/a"   
# unkown
df[df$Region %in% c('n/a','unkown'),'Region'] = ''


# [361] "teocelo"                                                                     
# [362] "teocelo, veracruz" 
# [379] "veracruz"                                                                    
# [148] "huatusco, veracruz"                                                          
# [146] "huatusco"                                                                    
# [147] "huatusco, ver"
# [94] "cosautlan de carvajal"                                                       
# [95] "cosautlan de carvajal, ver."
df[df$Region %in% c('teocelo','teocelo, veracruz','veracruz', 'huatusco, veracruz',
                    'huatusco', 'huatusco, ver', 'cosautlan de carvajal', 'cosautlan de carvajal, ver.'),'Region'] = 'veracruz'


# [355] "tarrazu"                                                                     
# [356] "tarrazú"                                                                     
# [357] "tarrazu, san jose, costa rica" 
df[df$Region %in% c('tarrazu', 'tarrazú', 'tarrazu, san jose, costa rica'),'Region'] = 'tarrazu'


# [347] "sumatra brastagi"                                                            
# [348] "sumatra brastagi - indonesia"  
df[df$Region %in% c('sumatra brastagi', 'sumatra brastagi - indonesia'),'Region'] = 'sumatra brastagi'


# [344] "sul de minas"                                                                
# [345] "sul de minas - carmo de minas" 
df[df$Region %in% c('sul de minas', 'sul de minas - carmo deminas'),'Region'] = 'sul de minas'


# [342] "south of mzuzu"                                                              
# west of mzuzu
# [256] "north of mzuzu"                                                              
df[df$Region %in% c('south of mzuzu', 'west of mzuzu', 'north of mzuzu'),'Region'] = 'mzuzu'


# [338] "south minas"                                                                 
# [339] "south minas gerais"                                                          
# [340] "south of minas"                                                              
# [341] "south of minas gerais" 
df[df$Region %in% c('south minas', 'south minas gerais', 'south of minas', 'south of minas gerais'),'Region'] = 'south of minas'

# [332] "sipi, mt elgon"                                                              
# [333] "sipi, mt. elgon" 
df[df$Region %in% c('sipi, mt. elgon'),'Region'] = 'sipi, mt elgon'

# [307] "san marcos"                                                                  
# [308] "san marcos ocotepeque"                                                       
# [309] "san marcos, ocotepeque" 
df[df$Region %in% c('san marcos', 'san marcos ocotepeque', 'san marcos, ocotepeque'),'Region'] = 'san marcos'


# [303] "san ignacio"                                                                 
# [304] "san ignacio, jaen, cajamarca"  
df[df$Region %in% c('san ignacio, jaen, cajamarca'),'Region'] = 'san ignacio'


# 291] "pu'er city, yunnan, china"                                                   
# [292] "puer"       
df[df$Region %in% c('puer', "pu'er city, yunnan, china"),'Region'] = 'yunnan'


# 283] "pitalito"                                                                    
# [284] "pitalito - huila"                                                            
# [285] "pitalito huila"                                                              
# [286] "pitalito-huila" 
df[df$Region %in% c('pitalito', 'pitalito - huila','pitalito huila', 'pitalito-huila'),'Region'] = 'pitalito'

# [261] "nyeri"                                                                       
# [262] "nyeri county"                                                                
# [263] "nyeri province"                                                              
# [264] "nyeri province, kenya"  
df[df$Region %in% c('nyeri', 'nyeri county', 'nyeri province', 'nyeri province, kenya', 'pitalito-huila'),'Region'] = 'nyeri'


# 259] "nuevo oriente"                                                               
# [260] "nuevo oriente aldea el limon"  
df[df$Region %in% c('nuevo oriente', 'nuevo oriente aldea el limon'),'Region'] = 'nuevo oriente'

# [243] "nantou guoxing 南投縣國姓鄉"                                                 
# [244] "nantou guoxing 南投縣國姓鄉        "                                         
# [245] "nantou lugu bai he lin南投縣鹿谷鄉白鶴林"                                    
# [246] "nantou xinyi 南投縣信義鄉"                                                   
# [247] "nantou 南投日月潭"  
df[df$Region %in% c('nantou guoxing 南投縣國姓鄉', 'nantou guoxing 南投縣國姓鄉        ',
                    'nantou lugu bai he lin南投縣鹿谷鄉白鶴林', 'nantou xinyi 南投縣信義鄉', 
                    'nantou 南投日月潭'),'Region'] = 'nantou'


# [228] "monte carmelo"                                                               
# [229] "monte carmelo - cerrado"                                                     
# [230] "monte carmelo, cerrado" 
df[df$Region %in% c('monte carmelo', 'monte carmelo - cerrado', 'monte carmelo, cerrado'),'Region'] = 'monte carmelo'

# [231] "moshi"                                                                       
# [232] "moshi rural" 
df[df$Region %in% c('moshi rural'),'Region'] = 'moshi'

# 219] "mecatlán"                                                                    
# [220] "menglian" 
df[df$Region %in% c('mecatlán'),'Region'] = 'menglian'


# [217] "mbinga"                                                                      
# [218] "mbinga, ruvuma"  
df[df$Region %in% c('mbinga, ruvuma'),'Region'] = 'mbinga'


# [211] "matagalpa"                                                                   
# [212] "matagalpa - nicaragua"                                                       
# [213] "matagalpa, jinotega and nueva segovia"  
df[df$Region %in% c('matagalpa - nicaragua', 'matagalpa, jinotega and nueva segovia'),'Region'] = 'matagalpa'


# [199] "limu"                                                                        
# [200] "limu (south nation nationalities peoples state region)"                      
# [201] "limu (southwest ethiopia)" 
df[df$Region %in% c('limu (south nation nationalities peoples state region)', 'limu (southwest ethiopia)'),'Region'] = 'limu'


# [184] "kiambu"                                                                      
# [185] "kiambu/meru"  
df[df$Region %in% c('kiambu/meru'),'Region'] = 'kiambu'

# 167] "jinotega"                                                                    
# [168] "jinotega province"                                                           
# [169] "jinotega, nicaragua" 
df[df$Region %in% c('jinotega province', 'jinotega, nicaragua'),'Region'] = 'jinotega'

# [155] "ijen east java"                                                              
# [156] "ijen highland, east java" 
df[df$Region %in% c('ijen east java', 'ijen highland, east java'),'Region'] = 'ijen'

# [141] "guoxing township, nantou county 南投縣國姓鄉"                                
# [142] "guoxing township, nantou county 南投縣國姓鄉        " 
df[df$Region %in% c('guoxing township, nantou county 南投縣國姓鄉', 'guoxing township, nantou county 南投縣國姓鄉        '),'Region'] = 'natou county'

# [138] "guji-hambela/alaka"                                                          
# [139] "guji-hambela/bishan fugu"                                                    
# [140] "guji-hambela/goyo"  
df[df$Region %in% c('guji-hambela/alaka', 'guji-hambela/bishan fugu', 'guji-hambela/goyo'),'Region'] = 'guji-hambela'

# [135] "guayata"                                                                     
# [136] "guayatá"  
df[df$Region %in% c('guayatá'),'Region'] = 'guayata'

# [132] "grama vale"                                                                  
# [133] "grama valley"
df[df$Region %in% c('grama vale'),'Region'] = 'grama valley' 

# [119] "el bálsamo - quezaltepec"                                                    
# [120] "el balsamo, quezaltepec" 
df[df$Region %in% c('el bálsamo - quezaltepec'),'Region'] = 'el balsamo, quezaltepec'

# [112] "dongshan dist., tainan city 台南市東山區"                                    
# [113] "dongshan dist., tainan city 臺南市東山區"  
df[df$Region %in% c("dongshan dist., tainan city 臺南市東山區", "dongshan dist., tainan city 臺南市東山區"),'Region'] = "dongshan dist., tainan city 臺南市東山區"

# [107] "doi chaang village, chiang rai, thialand"                                    
# [108] "doi chaang villiang, chiang rai, thialand" 
df[df$Region %in% c('doi chaang villiang, chiang rai, thialand'),'Region'] = 'doi chaang village, chiang rai, thialand'

# [85] "coaepec"                                                                     
# [86] "coatepec"    
df[df$Region %in% c('coaepec'),'Region'] = 'coatepec'

# [63] "cerrado"                                                                     
# [64] "cerrado - campos altos"                                                      
# [65] "cerrado - monte carmelo"                                                     
# [66] "cerrado - monte carmelo - minas gerais"                                      
# [67] "cerrado mineiro"                                                             
# [68] "cerrado of minas gerais"   
df[df$Region %in% c('cerrado - campos altos', 'cerrado - monte carmelo', 'cerrado - monte carmelo = minas gerais',
                    'cerrado mineiro', 'cerrado of minas gerais'),'Region'] = 'cerrado'

# [60] "central region"                                                              
# [61] "central valley"                                                              
# [62] "central valley - west valley"  
df[df$Region %in% c('central valley', 'central valley - west valley'),'Region'] = 'central valley'

# 58] "central kenya"                                                               
# [59] "central kenya - nyeri"  
# [40] "blend - central & western kenya"                                             
df[df$Region %in% c('central kenya - nyeri', 'blend - central & western kenya'),'Region'] = 'central kenya'

# [54] "carmo de minas"                                                              
# [55] "carmo de minas micro region" 
df[df$Region %in% c('carmo de minas micro region'),'Region'] = 'carmo de minas'

# [43] "boquete"                                                                     
# [44] "boquete, panama" 
df[df$Region %in% c('boquete, panama'),'Region'] = 'boquete'

# [34] "bali"                                                                        
# [35] "bali kintamani" 
df[df$Region %in% c('bali kintamani'),'Region'] = 'bali'

# [21] "apaneca"                                                                     
# [22] "apaneca - ilamatepec"                                                        
# [23] "apaneca ilamatepec mountain range"                                           
# [24] "apaneca llamantepec"                                                         
# [25] "apaneca-ilamatepec" 
df[df$Region %in% c('apaneca - ilamatepec', 'apaneca ilamatepec mountain range',
                    'apaneca llamantepec', 'apaneca-ilamatepec'),'Region'] = 'apaneca'

# [16] "antioquia"                                                                   
# [17] "antioquía-betulia"                                                           
# [18] "antioquía-caicedo"                                                           
# [19] "antioquía-giraldo"                                                           
# [20] "antioquía-urrao"
df[df$Region %in% c('antioquía-betulia', 'antioquía-caicedo',
                    'antioquía-giraldo', 'antioquía-urrao'),'Region'] = 'antioquia'
# [3] "acatenango"                                                                  
# [4] "acatenango, chimaltenango"
df[df$Region %in% c('acatenango, chimaltenango'),'Region'] = 'acatenango'

df = df[df$Farm.Name != 'test',]

df$alt = NULL
df$altitude_low_meters = df$alt_low
df$altitude_high_meters = df$alt_high
df$altitude_mean_meters = df$alt_mean
df$alt_low = NULL
df$alt_high = NULL
df$alt_mean = NULL
df$Notes = NULL

write.csv(df,file='/Users/jledoux/Documents/projects/coffee-quality-database/data/robusta_data_cleaned.csv')

