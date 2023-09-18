######****** Data Processing ******######

### Form A has a record of all the neonates who are admitted in NICU ###
## Read FormA - total rows = 17980 ##
form_a = read.csv('FormA.csv')

## Remove duplicates - total rows = 17659 ##
form_a = unique(form_a)

## Remove unwanted columns, remaining columns = 46 ##
form_a = form_a[,c(1, 2, 4:8, 11, 14, 16:22, 24, 25, 27:37, 40:42, 46:51, 62:64, 67, 
                   68, 71, 157, 158)]

## Total number of rows returned = 17604 ##
form_a = unique(form_a)

## Total number of rows returned = 15425, removed CNBC ##
form_a = form_a[-which(form_a$CCODE == 1),]

## Creating a key by combining ccode and enrolment number ##
ccode_enrlno_form_a = paste(form_a$CCODE, form_a$ENRLNO, sep = '.')
form_a$ccode_enrlno = ccode_enrlno_form_a

## Removing second entry of a neonate ##
form_a = form_a[-which(duplicated(ccode_enrlno_form_a)),]


### FormC has the record of all the neonates who were suspected with Sepsis ###
## Read FORM C - total rows = 7445 ##
form_c = read.csv('FORMC.csv')

## Remove duplicates - total rows = 7350 ##
form_c = unique(form_c)

## Remove unwanted columns ##
form_c = form_c[,1:31]

## Total number of rows returned = 7300 ##
form_c = unique(form_c)

## Total number of rows returned = 5312, removed CNBC ##
form_c = form_c[-which(form_c$CCODE == 1),]

## Creating a key by combining ccode and enrolment number ##
ccode_enrlno_form_c = paste(form_c$CCODE, form_c$ENRLNO, sep = '.')
form_c$ccode_enrlno = ccode_enrlno_form_c

## Creating a key by combining ccode, enrolment number and episode number ##
ccode_enrlno_epsno_form_c = paste(ccode_enrlno_form_c, form_c$EPISODENO, sep = '.')
form_c$ccode_enrlno_episode = ccode_enrlno_epsno_form_c

## Removing second entry of a neonate ##
form_c = form_c[-which(duplicated(form_c$ccode_enrlno_episode)),]

## Read ANNEXURE - total rows = 11906 ##
annexure = read.csv('ANEXURE.csv')

## Remove unwanted columns ##
annexure = annexure[,c(1:4, 8, 18, 23:90)]

## Total number of rows returned = 11906 ##
annexure = unique(annexure)

## Total number of rows returned = 8079, removed CNBC ##
annexure = annexure[-which(annexure$CCODE == 1),]

## Creating a key by merging enrolment number and episode number ##
ccode_enrlno_episode_ann = paste(annexure$CCODE, annexure$ENRLNO, sep = '.')
ccode_enrlno_episode_ann = paste(ccode_enrlno_episode_ann, annexure$EPISODENO, sep = '.')
annexure$ccode_enrlno_episode = ccode_enrlno_episode_ann

## Merging formC and annexure returns 8037 rows ##
formc_annexure = merge(form_c, annexure, by = 'ccode_enrlno_episode')

## Merging formA, with the above generated formc_annexure returns 8037 rows ##
final_Data = merge(form_a, formc_annexure, by = 'ccode_enrlno')

## Cleaning obstretic problem to convert it into one hot encoding ##
final_Data$OBSPBM1 = as.character(final_Data$OBSPBM1)
final_Data$OBSPBM1[which(final_Data$OBSPBM1 == 'NAEMIA')] = 'Anaemia'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == 'A')] = 'Anaemia'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == 'N')] = 'NONE'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == 'Ã')] = 'NONE'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == '')] = 'NONE'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == ' ')] = 'NONE'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == 'G')] = 'GDM'
final_Data$OBSPBM1[which(substr(final_Data$OBSPBM1,1,1) == 'P')] = 'PIH'
final_Data$OBSPBM1 = as.factor(final_Data$OBSPBM1)

final_Data$OBSPBM2 = as.character(final_Data$OBSPBM2)
final_Data$OBSPBM2[which(substr(final_Data$OBSPBM2,1,1) == 'A')] = 'Anaemia'
final_Data$OBSPBM2[which(substr(final_Data$OBSPBM2,1,1) == 'P')] = 'PIH'
final_Data$OBSPBM2[which(substr(final_Data$OBSPBM2,1,1) == 'G')] = 'GDM'
final_Data$OBSPBM2[which(final_Data$OBSPBM2 %in% c('Anaemia', 'PIH', 'GDM') == FALSE)] = 'NONE'
final_Data$OBSPBM2 = as.factor(final_Data$OBSPBM2)

final_Data$OBSPBM3 = as.character(final_Data$OBSPBM3)
final_Data$OBSPBM3[which(substr(final_Data$OBSPBM3,1,1) == 'A')] = 'Anaemia'
final_Data$OBSPBM3[which(substr(final_Data$OBSPBM3,1,1) == 'G')] = 'GDM'
final_Data$OBSPBM3[which(final_Data$OBSPBM3 %in% c('Anaemia', 'PIH', 'GDM') == FALSE)] = 'NONE'
final_Data$OBSPBM3 = as.factor(final_Data$OBSPBM3)

final_Data$OBSPBM4 = as.character(final_Data$OBSPBM4)
final_Data$OBSPBM4[which(substr(final_Data$OBSPBM4,1,1) == 'A')] = 'Anaemia'
final_Data$OBSPBM4[which(substr(final_Data$OBSPBM4,1,1) == 'P')] = 'PIH'
final_Data$OBSPBM4[which(substr(final_Data$OBSPBM4,1,1) == 'G')] = 'GDM'
final_Data$OBSPBM4[which(final_Data$OBSPBM4 %in% c('Anaemia', 'PIH', 'GDM') == FALSE)] = 'NONE'
final_Data$OBSPBM4 = as.factor(final_Data$OBSPBM4)

## Converting OBSPBM to one-hot enocdes ##
obspbm = c('PIH', 'GDM', 'Anaemia')
for (i in 1:3) {
  final_Data = cbind(final_Data, "N")
  colnames(final_Data)[ncol(final_Data)] = obspbm[i]
}

for (i in 1:nrow(final_Data)) {
  col_index = which(colnames(final_Data) == as.character(final_Data$OBSPBM1[i]))
  if(length(col_index) != 0){
    final_Data[,col_index] = as.character(final_Data[,col_index])
    final_Data[i,col_index] = "Y"
  }
  col_index = which(colnames(final_Data) == as.character(final_Data$OBSPBM2[i]))
  if(length(col_index) != 0){
    final_Data[,col_index] = as.character(final_Data[,col_index])
    final_Data[i,col_index] = "Y"
  }
  col_index = which(colnames(final_Data) == as.character(final_Data$OBSPBM3[i]))
  if(length(col_index) != 0){
    final_Data[i,col_index] = "Y"
  }
  col_index = which(colnames(final_Data) == as.character(final_Data$OBSPBM4[i]))
  if(length(col_index) != 0){
    final_Data[i,col_index] = "Y"
  }
}

final_Data$OBSPBM1 = NULL
final_Data$OBSPBM2 = NULL
final_Data$OBSPBM3 = NULL
final_Data$OBSPBM4 = NULL

## Cleaning antibiotic before delivery to convert it into one hot encoding ##
## Converting ANTIB4D to one-hot encodes ##
antibiotic_dictionary = t(as.data.frame(c('1','Amikacin')))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('2', 'Amoxyclav'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('3', 'Ampicillin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('4', 'Cefoprazone+Sulbactam'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('5', 'Cefotaxime'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('6', 'Ciprofloxacin/norflox'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('7', 'Co-trimoxazole'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('8', 'Gentamycin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('9', 'Imipenem'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('10', 'Meropenem'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('11', 'Ertapenam'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('12', 'Linezolid'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('13', 'Netilmycin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('14', 'Oxacillin/Cefoxitin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('15', 'Penicillin G'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('16', 'Piperacillin +tazobactum'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('17', 'Teicoplanin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('18', 'Vancomycin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('19', 'Aztreonam'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('20', 'Cefuroxime'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('21', 'Cefazolin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('22', 'Cloxacillin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('23', 'Colistin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('24', 'Cefixime'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('25', 'Cefpodoxime'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('26', 'Ofloxacin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('27', 'Amphotericin'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('28', 'Fluconazole'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('29', 'Voraconazole'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('30', 'Ceftriaxone'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('31', 'Ceftazidime'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('32', 'Metronidazole'))
antibiotic_dictionary = rbind(antibiotic_dictionary,c('33', 'Others'))
colnames(antibiotic_dictionary) = c('Code','Antibiotic')
antibiotic_dictionary = as.data.frame(antibiotic_dictionary)
antibiotic_dictionary$Code = as.character(antibiotic_dictionary$Code)
antibiotic_dictionary$Antibiotic = as.character(antibiotic_dictionary$Antibiotic)

for (i in 1:33) {
  final_Data = cbind(final_Data, "N")
  colnames(final_Data)[ncol(final_Data)] = antibiotic_dictionary$Antibiotic[i]
}

for (i in 1:nrow(final_Data)) {
  antibiotic = antibiotic_dictionary$Antibiotic[which(antibiotic_dictionary$Code == as.character(final_Data$ANTIB4DEL_SP1[i]))]
  col_index = which(colnames(final_Data) == antibiotic)
  if(length(col_index) != 0){
    final_Data[,col_index] = as.character(final_Data[,col_index])
    final_Data[i,col_index] = "Y"
  }
  antibiotic = antibiotic_dictionary$Antibiotic[which(antibiotic_dictionary$Code == as.character(final_Data$ANTIB4DEL_SP2[i]))]
  col_index = which(colnames(final_Data) == antibiotic)
  if(length(col_index) != 0){
    final_Data[,col_index] = as.character(final_Data[,col_index])
    final_Data[i,col_index] = "Y"
  }
  antibiotic = antibiotic_dictionary$Antibiotic[which(antibiotic_dictionary$Code == as.character(final_Data$ANTIB4DEL_SP3[i]))]
  col_index = which(colnames(final_Data) == antibiotic)
  if(length(col_index) != 0){
    final_Data[,col_index] = as.character(final_Data[,col_index])
    final_Data[i,col_index] = "Y"
  }
}

final_Data$ANTIB4DEL_SP1 = NULL
final_Data$ANTIB4DEL_SP2 = NULL
final_Data$ANTIB4DEL_SP3 = NULL

## Cleans CCODE ##
final_Data$CCODE = as.character(final_Data$CCODE)
final_Data$CCODE[which(final_Data$CCODE == '2')] = 8.8 #'MAMC'
final_Data$CCODE[which(final_Data$CCODE == '3')] = 4.9 #'SJH'
final_Data$CCODE[which(final_Data$CCODE == '4')] = 2.1 #'AIIMS'
final_Data$CCODE = as.numeric(final_Data$CCODE)

## Cleans MAGE ##
final_Data$MAGE[which(final_Data$MAGE %in% c(88, 99))] = NA
final_Data = final_Data[-which(is.na(final_Data$MAGE)),]

## Cleans MEDU ##
final_Data$MEDU = as.character(final_Data$MEDU)
final_Data$MEDU = gsub(' ','',final_Data$MEDU)
final_Data$MEDU[which(final_Data$MEDU %in% c('','NK'))] = NA
final_Data$MEDU[which(final_Data$MEDU == 'GRADUATEORPOSTGARDUATE')] = 'GRADUATE OR POSTGARDUATE'
final_Data$MEDU[which(final_Data$MEDU == 'HIGHERSECONDARY')] = 'HIGHER SECONDARY'
final_Data$MEDU[which(is.na(final_Data$MEDU))] = "Not Available"
final_Data$MEDU = toTitleCase(final_Data$MEDU)
final_Data$MEDU = as.factor(final_Data$MEDU)

## Cleans FEDU ##
final_Data$FEDU = as.character(final_Data$FEDU)
final_Data$FEDU = gsub(' ','',final_Data$FEDU)
final_Data$FEDU[which(final_Data$FEDU %in% c('','NK'))] = NA
final_Data$FEDU[which(final_Data$FEDU == 'GRADUATEORPOSTGARDUATE')] = 'GRADUATE OR POSTGARDUATE'
final_Data$FEDU[which(final_Data$FEDU == 'HIGHERSECONDARY')] = 'HIGHER SECONDARY'
final_Data$FEDU[which(is.na(final_Data$FEDU))] = "Not Available"
final_Data$FEDU = toTitleCase(final_Data$FEDU)
final_Data$FEDU = as.factor(final_Data$FEDU)

## Cleans FOCCUP ##
final_Data$FOCCUP = as.character(final_Data$FOCCUP)
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[1])] = "Unskilled"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[2])] = "Skilled"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[3])] = "Semi-skilled"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[4])] = "Clerk or Shop owner"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[5])] = "Semi-professional"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[6])] = "Unemployed"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[7])] = "Professional"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[8])] = NA
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[9])] = NA
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[10])] = "Farmer"
final_Data$FOCCUP[which(final_Data$FOCCUP == unique(final_Data$FOCCUP)[11])] = "Skilled"
final_Data$FOCCUP[which(is.na(final_Data$FOCCUP))] = "Not Available"
final_Data$FOCCUP = as.factor(final_Data$FOCCUP)

## Cleans STATE ##
final_Data$STATE1 = as.character(final_Data$STATE1)
final_Data$STATE1 = tolower(final_Data$STATE1)
final_Data$STATE1 = gsub(' ', '', final_Data$STATE1)
final_Data$STATE1 = gsub('[.]','', final_Data$STATE1)
final_Data$STATE1 = gsub('\\d','', final_Data$STATE1)
final_Data$STATE1[which(final_Data$STATE1 %in% c('delhi', 'delh', 'delha',
                                                 'delho', 'delhu', 'delhi-',
                                                 'delhii', 'delhio', 'delhiu',
                                                 'dlhi', 'maujpurdelhi',
                                                 'mustafabad,delhi',
                                                 'mustafanaddelhi',
                                                 'najafgarhndelhi',
                                                 'narela,delhi', 'nd', 'nd-',
                                                 'ndelhi', 'ndelhi-', 
                                                 'nedelhi', 'newdehi',
                                                 'newdehi', 'newdelh', 
                                                 'newdelhhi', 'newdelhi',
                                                 'newdelhi-', 'newdlehi',
                                                 'newdlhi', 'nwdelhi',
                                                 'okhland', 'sadarbazar,delhi',
                                                 'samaspurdelhi', 
                                                 'sangamviharndelhi', 'seelampurdelhi',
                                                 'shahadradelhi', 'shahdradelhi',
                                                 'southdelhi', 'westdelhi', 
                                                 'badarp', 'badarpur', 'badarpurndelhi',
                                                 'baljeetnagar', 'collegedelhi',
                                                 'ghondachowkdelhi', 'gokulpuridelhi',
                                                 'itodelhi', 'badarpurnd', 'berinewdelhi',
                                                 'olddelhi'))] = 'Delhi'
final_Data$STATE1[which(final_Data$STATE1 %in% c('bihar'))] = 'Bihar'
final_Data$STATE1[which(final_Data$STATE1 %in% c('chattisgarh'))] = 'Chattisgarh'
final_Data$STATE1[which(final_Data$STATE1 %in% c('faridabad', 'fb', 'faridabad,hr-',
                                                 'faridabadharyana', 'gurgaon',
                                                 'gurgaonharyana', 'haaryana',
                                                 'harayana', 'hariyana',
                                                 'harya', 'haryana', 'hr', 
                                                 'naryana', 'sehadpur', 'haryan'))] = 'Haryana'
final_Data$STATE1[which(final_Data$STATE1 %in% c('gbnagarup', 'ghaziabad',
                                                 'ghaziabad,up', 'ghaziabad(up)',
                                                 'ghaziabadup', 'gzb', 'gzb,up',
                                                 'sahibabadup', 'up', 'upo',
                                                 'uttarpradesh', 'loniup',
                                                 'muzaffarnagar', 
                                                 'kanpur', 'utarpradesh'))] = 'Uttar Pradesh'
final_Data$STATE1[which(final_Data$STATE1 %in% c('himachalpradesh', 'hp'))] = 'Himachal Pradesh'
final_Data$STATE1[which(final_Data$STATE1 %in% c('j&k', 'jammu&kashmir',
                                                 'jammukashmir'))] = 'Jammu & Kashmir'
final_Data$STATE1[which(final_Data$STATE1 %in% c('jharkhand'))] = 'Jharkhand'
final_Data$STATE1[which(final_Data$STATE1 %in% c('mp'))] = 'Madhya Pradesh'
final_Data$STATE1[which(final_Data$STATE1 %in% c('punjab'))] = 'Punjab'
final_Data$STATE1[which(final_Data$STATE1 %in% c('rajasthan', 'rajsthan'))] =
  'Rajasthan'
final_Data$STATE1[which(final_Data$STATE1 %in% c('sikkim'))] = 'Sikkim'
final_Data$STATE1[which(final_Data$STATE1 %in% c('uk', 'uttarakhand'))] = 'Uttarakhand'
final_Data$STATE1[which(final_Data$STATE1 %in% c('uttaranchal'))] = 'Uttaranchal'
final_Data$STATE1[which(final_Data$STATE1 %in% c('westbengal'))] = 'West Bengal'
final_Data$STATE1[which(final_Data$STATE1 %in% c('maharashtra'))] = 'Maharashtra'
final_Data$STATE1[which(final_Data$STATE1 == "")] = NA
unique(final_Data$STATE1)
final_Data$STATE1[which(is.na(final_Data$STATE1))] = "Not Available"
final_Data$STATE1 = as.factor(final_Data$STATE1)

## Cleans BWT ##
final_Data$BWT[which(final_Data$BWT %in% c(8888,9999))] = NA
final_Data = final_Data[-which(is.na(final_Data$BWT)),]

## Cleans GESTATION ##
final_Data$GESTATION[which(final_Data$GESTATION %in% 99)] = NA

## Cleans GENDER ##
final_Data$GENDER = as.character(final_Data$GENDER)
final_Data$GENDER = gsub(' ','',final_Data$GENDER)
final_Data$GENDER[which(final_Data$GENDER == '')] = NA
final_Data = final_Data[-which(is.na(final_Data$GENDER)),]
final_Data$GENDER = toTitleCase(final_Data$GENDER)
final_Data$GENDER = as.factor(final_Data$GENDER)

## Cleans MULBIRTH ##
final_Data$MULBIRTH = as.character(final_Data$MULBIRTH)
final_Data$MULBIRTH = gsub(' ','',final_Data$MULBIRTH)
final_Data$MULBIRTH[which(final_Data$MULBIRTH == '')] = NA
final_Data$MULBIRTH[which(final_Data$MULBIRTH == "SINGLE")] = "Single"
final_Data$MULBIRTH[which(final_Data$MULBIRTH == "TWIN")] = "Twins"
final_Data$MULBIRTH[which(final_Data$MULBIRTH == "TRIPLET")] = "Triplets"
final_Data$MULBIRTH = as.factor(final_Data$MULBIRTH)

## Cleans APGAR1MIN ##
final_Data$APGAR1MIN[which(final_Data$APGAR1MIN %in% c(88,99))] = NA
final_Data$APGAR1MIN[which(is.na(final_Data$APGAR1MIN))] = 100

## Cleans APGAR5MIN ##
final_Data$APGAR5MIN[which(final_Data$APGAR5MIN %in% c(88,99))] = NA
final_Data$APGAR5MIN[which(is.na(final_Data$APGAR5MIN))] = 100

## Cleans WTNICU ##
final_Data$WTNICU[which(final_Data$WTNICU %in% c(8888,9999))] = NA
final_Data$WTNICU[which(is.na(final_Data$WTNICU))] = 9999

## Cleans NOCHECKUP ##
final_Data$NOCHECKUP[which(final_Data$NOCHECKUP %in% c(88,99))] = NA
final_Data$NOCHECKUP[which(is.na(final_Data$NOCHECKUP))] = 99

## Cleans PARITY ##
final_Data$PARITY[which(final_Data$PARITY %in% c(88,99))] = NA

## Cleans FEVERB4D ##
final_Data$FEVERB4D = as.character(final_Data$FEVERB4D)
final_Data$FEVERB4D = gsub(' ','',final_Data$FEVERB4D)
final_Data$FEVERB4D[which(final_Data$FEVERB4D == '')] = NA
final_Data$FEVERB4D[which(final_Data$FEVERB4D == 'n')] = 'N'
final_Data$FEVERB4D[which(final_Data$FEVERB4D == "N")] = "No"
final_Data$FEVERB4D[which(final_Data$FEVERB4D == "Y")] = "Yes"
final_Data$FEVERB4D = as.factor(final_Data$FEVERB4D)

## Cleans UTI ##
final_Data$UTI = as.character(final_Data$UTI)
final_Data$UTI[which(final_Data$UTI %in% c("N ", "N"))] = "No"
final_Data$UTI[which(final_Data$UTI %in% c("Y ", "Y"))] = "Yes"
final_Data$UTI[which(final_Data$UTI %in% c("NY", "NK", "  "))] = "Not known"
final_Data$UTI = as.factor(final_Data$UTI)

## Cleans ANTEST ##
final_Data$ANTEST = as.character(final_Data$ANTEST)
final_Data$ANTEST = gsub(' ','',final_Data$ANTEST)
final_Data$ANTEST[which(final_Data$ANTEST == '')] = 'NK'
final_Data$ANTEST[which(final_Data$ANTEST == 'NM')] = 'NK'
final_Data$ANTEST[which(final_Data$ANTEST == 'UY')] = 'Y'
final_Data$ANTEST[which(is.na(final_Data$ANTEST))] = "Not known"
final_Data$ANTEST[which(final_Data$GESTATION > 34)] = 'NK'
final_Data$ANTEST[which(final_Data$ANTEST == "N")] = "No"
final_Data$ANTEST[which(final_Data$ANTEST == "NK")] = "Not known"
final_Data$ANTEST[which(final_Data$ANTEST == "Y")] = "Yes"
final_Data$ANTEST = as.factor(final_Data$ANTEST)

## Cleans MEDSURILL ##
final_Data$MEDSURILL = as.character(final_Data$MEDSURILL)
final_Data$MEDSURILL[which(final_Data$MEDSURILL == "B")] = "N"
final_Data$MEDSURILL[which(final_Data$MEDSURILL == "N")] = "No"
final_Data$MEDSURILL[which(final_Data$MEDSURILL == "Y")] = "Yes"
final_Data$MEDSURILL = as.factor(final_Data$MEDSURILL)

## Cleans NOVAGEX ##
final_Data$NOVAGEX[which(final_Data$NOVAGEX %in% c(88,99))] = NA
final_Data$NOVAGEX[which(is.na(final_Data$NOVAGEX))] = 100

## Cleans DURLABOR ##
final_Data$DURLABOR[which(final_Data$DURLABOR %in% c(888,999))] = NA
final_Data$DURLABOR[which(is.na(final_Data$DURLABOR))] = 1000

## Cleans DURROFM ##
final_Data$DURROFM[which(final_Data$DURROFM %in% c(888,999))] = NA
final_Data$DURROFM[which(is.na(final_Data$DURROFM))] = 1000

## Cleans MODEDEL ##
final_Data$MODEDEL = as.character(final_Data$MODEDEL)
final_Data$MODEDEL = gsub(' ','',final_Data$MODEDEL)
final_Data$MODEDEL[which(final_Data$MODEDEL %in% c('','N','NK'))] = NA
final_Data$MODEDEL[which(final_Data$MODEDEL == 'CESAREANSECTION')] = 'CESAREAN SECTION'
final_Data$MODEDEL = toTitleCase(tolower(final_Data$MODEDEL))
final_Data$MODEDEL = as.factor(final_Data$MODEDEL)

## Cleans MECOSL ##
final_Data$MECOSL = as.character(final_Data$MECOSL)
final_Data$MECOSL = gsub(' ','',final_Data$MECOSL)
final_Data$MECOSL[which(final_Data$MECOSL %in% c('99',''))] = NA
final_Data$MECOSL[which(final_Data$MECOSL == "N")] = "No"
final_Data$MECOSL[which(final_Data$MECOSL == "Y")] = "Yes"
final_Data$MECOSL[which(final_Data$MECOSL == "NK")] = "Not known"
final_Data$MECOSL = as.factor(final_Data$MECOSL)

## Cleans FOULSL ##
final_Data$FOULSL = as.character(final_Data$FOULSL)
final_Data$FOULSL = gsub(' ','',final_Data$FOULSL)
final_Data$FOULSL[which(final_Data$FOULSL == '')] = NA
final_Data$FOULSL[which(final_Data$FOULSL == 'NB')] = 'N'
final_Data$FOULSL[which(final_Data$FOULSL == "N")] = "No"
final_Data$FOULSL[which(final_Data$FOULSL == "Y")] = "Yes"
final_Data$FOULSL[which(final_Data$FOULSL == "NK")] = "Not known"
final_Data$FOULSL = as.factor(final_Data$FOULSL)

## Cleans N4BAGMASK ##
final_Data$N4BAGMASK = as.character(final_Data$N4BAGMASK)
final_Data$N4BAGMASK = gsub(' ','',final_Data$N4BAGMASK)
final_Data$N4BAGMASK[which(final_Data$N4BAGMASK == "N")] = "No"
final_Data$N4BAGMASK[which(final_Data$N4BAGMASK == "Y")] = "Yes"
final_Data$N4BAGMASK[which(final_Data$N4BAGMASK == "NK")] = "Not known"
final_Data$N4BAGMASK = as.factor(final_Data$N4BAGMASK)

## Cleans N4CHESTCOMP ##
final_Data$N4CHESTCOMP = as.character(final_Data$N4CHESTCOMP)
final_Data$N4CHESTCOMP = gsub(' ','',final_Data$N4CHESTCOMP)
final_Data$N4CHESTCOMP[which(final_Data$N4CHESTCOMP == '')] = NA
final_Data$N4CHESTCOMP[which(final_Data$N4CHESTCOMP == "N")] = "No"
final_Data$N4CHESTCOMP[which(final_Data$N4CHESTCOMP == "Y")] = "Yes"
final_Data$N4CHESTCOMP[which(final_Data$N4CHESTCOMP == "NK")] = "Not known"
final_Data$N4CHESTCOMP[which(final_Data$N4CHESTCOMP == "NN")] = "No"
final_Data$N4CHESTCOMP = as.factor(final_Data$N4CHESTCOMP)

## Cleans PRELAFEED ##
final_Data$PRELAFEED = as.character(final_Data$PRELAFEED)
final_Data$PRELAFEED = gsub(' ','',final_Data$PRELAFEED)
final_Data$PRELAFEED[which(final_Data$PRELAFEED == 'YH')] = 'Y'
final_Data$PRELAFEED[which(final_Data$PRELAFEED == 'NN')] = 'N'
final_Data$PRELAFEED[which(final_Data$PRELAFEED == "N")] = "No"
final_Data$PRELAFEED[which(final_Data$PRELAFEED == "Y")] = "Yes"
final_Data$PRELAFEED = as.factor(final_Data$PRELAFEED)

## Cleans PRIORFEED ##
final_Data$PRIORFEED = as.character(final_Data$PRIORFEED)
final_Data$PRIORFEED = gsub(' ','',final_Data$PRIORFEED)
final_Data$PRIORFEED[which(final_Data$PRIORFEED %in% c("", "N", "E"))] = NA
final_Data$PRIORFEED[which(is.na(final_Data$PRIORFEED))] = "Not Available"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "1-25%BREASTMILK")] = "1-25% Breastmilk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "25-49%BREASTMILK")] = "25-49% Breastmilk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "50-74%BREASTMILK")] = "50-74% Breastmilk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "75-99%BREASTMILK")] = "75-99% Breastmilk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "EXCLUSIVELYBREASTMILK")] = "Exclusive breastmilk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "FULLYANIMALMILK")] = "Fully animal milk"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "FULLYFORMULA")] = "Fully formula"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "NILPERORAL")] = "Nil per oral"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "NOTAPPLICABLE")] = "Not applicable"
final_Data$PRIORFEED[which(final_Data$PRIORFEED == "Not Available")] = "Not applicable"
final_Data$PRIORFEED = as.factor(final_Data$PRIORFEED)

## Cleans MAJMALFORM ##
final_Data$MAJMALFORM = as.character(final_Data$MAJMALFORM)
final_Data$MAJMALFORM = gsub(' ','',final_Data$MAJMALFORM)
final_Data$MAJMALFORM[which(final_Data$MAJMALFORM == '')] = NA
final_Data$MAJMALFORM[which(final_Data$MAJMALFORM == 'B')] = 'N'
final_Data$MAJMALFORM[which(final_Data$MAJMALFORM == 'N')] = 'No'
final_Data$MAJMALFORM[which(final_Data$MAJMALFORM == "Y")] = "Yes"
final_Data$MAJMALFORM = as.factor(final_Data$MAJMALFORM)

## Cleans STOMACHWASH ##
final_Data$STOMACHW = as.character(final_Data$STOMACHW)
final_Data$STOMACHW = gsub(' ','',final_Data$STOMACHW)
final_Data$STOMACHW[which(final_Data$STOMACHW == "")] = NA
final_Data$STOMACHW[which(is.na(final_Data$STOMACHW))] = "Not Available"
final_Data$STOMACHW[which(final_Data$STOMACHW == 'N')] = 'No'
final_Data$STOMACHW[which(final_Data$STOMACHW == "Y")] = "Yes"
final_Data$STOMACHW = as.factor(final_Data$STOMACHW)

## Cleans AGESUHRS ##
final_Data$AGESUSHRS[which(final_Data$AGESUSHRS %in% c(8888,9999))] = NA
final_Data$AGESUSHRS[which(is.na(final_Data$AGESUSHRS))] = 9999

## Cleans DIFFFEED ##
final_Data$DIFFFEED = as.character(final_Data$DIFFFEED)
final_Data$DIFFFEED = gsub(' ','',final_Data$DIFFFEED)
final_Data$DIFFFEED = toupper(final_Data$DIFFFEED)
final_Data$DIFFFEED[which(final_Data$DIFFFEED == '')] = NA
final_Data$DIFFFEED[which(final_Data$DIFFFEED == 'T')] = 'Y'
final_Data$DIFFFEED[which(is.na(final_Data$DIFFFEED))] = 'Not Available'
final_Data$DIFFFEED[which(final_Data$DIFFFEED == 'N')] = 'No'
final_Data$DIFFFEED[which(final_Data$DIFFFEED == "Y")] = "Yes"
final_Data$DIFFFEED = as.factor(final_Data$DIFFFEED)

## Cleans CONVULSION ##
final_Data$CONVULSION = as.character(final_Data$CONVULSION)
final_Data$CONVULSION = gsub(' ','',final_Data$CONVULSION)
final_Data$CONVULSION[which(final_Data$CONVULSION == 'U')] = 'Y'
final_Data$CONVULSION[which(final_Data$CONVULSION == 'N')] = 'No'
final_Data$CONVULSION[which(final_Data$CONVULSION == "Y")] = "Yes"
final_Data$CONVULSION = as.factor(final_Data$CONVULSION)

## Cleans MOVESTIMUL ##
final_Data$MOVESTIMUL = as.character(final_Data$MOVESTIMUL)
final_Data$MOVESTIMUL = gsub(' ','',final_Data$MOVESTIMUL)
final_Data$MOVESTIMUL[which(final_Data$MOVESTIMUL == 'B')] = 'N'
final_Data$MOVESTIMUL[which(final_Data$MOVESTIMUL == 'N')] = 'No'
final_Data$MOVESTIMUL[which(final_Data$MOVESTIMUL == "Y")] = "Yes"
final_Data$MOVESTIMUL = as.factor(final_Data$MOVESTIMUL)

## Cleans FEVER ##
final_Data$FEVER = as.character(final_Data$FEVER)
final_Data$FEVER[which(final_Data$FEVER == 'N')] = 'No'
final_Data$FEVER[which(final_Data$FEVER == "Y")] = "Yes"
final_Data$FEVER = as.factor(final_Data$FEVER)

## Cleans COLD2TCH ##
final_Data$COLD2TCH = as.character(final_Data$COLD2TCH)
final_Data$COLD2TCH[which(final_Data$COLD2TCH == 'N')] = 'No'
final_Data$COLD2TCH[which(final_Data$COLD2TCH == "Y")] = "Yes"
final_Data$COLD2TCH = as.factor(final_Data$COLD2TCH)

## Cleans APNEA ##
final_Data$APNEA = as.character(final_Data$APNEA)
final_Data$APNEA[which(final_Data$APNEA == 'N')] = 'No'
final_Data$APNEA[which(final_Data$APNEA == "Y")] = "Yes"
final_Data$APNEA = as.factor(final_Data$APNEA)

## Cleans DIFFBREATH ##
final_Data$DIFFBREATH = as.character(final_Data$DIFFBREATH)
final_Data$DIFFBREATH = gsub(' ','',final_Data$DIFFBREATH)
final_Data$DIFFBREATH[which(final_Data$DIFFBREATH == 'U')] = "Y"
final_Data$DIFFBREATH[which(final_Data$DIFFBREATH == '')] = NA
final_Data$DIFFBREATH[which(is.na(final_Data$DIFFBREATH))] = 'No'
final_Data$DIFFBREATH[which(final_Data$DIFFBREATH == 'N')] = 'No'
final_Data$DIFFBREATH[which(final_Data$DIFFBREATH == "Y")] = "Yes"
final_Data$DIFFBREATH = as.factor(final_Data$DIFFBREATH)

## Cleans VOMITING ##
final_Data$VOMITING = as.character(final_Data$VOMITING)
final_Data$VOMITING[which(final_Data$VOMITING == 'N')] = 'No'
final_Data$VOMITING[which(final_Data$VOMITING == "Y")] = "Yes"
final_Data$VOMITING = as.factor(final_Data$VOMITING)

## Cleans DIARRHEA ##
final_Data$DIARRHEA = as.character(final_Data$DIARRHEA)
final_Data$DIARRHEA = gsub('_',NA,final_Data$DIARRHEA)
final_Data$DIARRHEA[which(is.na(final_Data$DIARRHEA))] = 'Not Available'
final_Data$DIARRHEA[which(final_Data$DIARRHEA == 'N')] = 'No'
final_Data$DIARRHEA[which(final_Data$DIARRHEA == "Y")] = "Yes"
final_Data$DIARRHEA = as.factor(final_Data$DIARRHEA)

## Cleans PUSUMBSIP ##
final_Data$PUSUMBSIP = as.character(final_Data$PUSUMBSIP)
final_Data$PUSUMBSIP = gsub(' ','',final_Data$PUSUMBSIP)
final_Data$PUSUMBSIP = toupper(final_Data$PUSUMBSIP)
final_Data$PUSUMBSIP[which(final_Data$PUSUMBSIP == 'N')] = 'No'
final_Data$PUSUMBSIP[which(final_Data$PUSUMBSIP == "Y")] = "Yes"
final_Data$PUSUMBSIP = as.factor(final_Data$PUSUMBSIP)

## Cleans EARDISC ##
final_Data$EARDISC = as.character(final_Data$EARDISC)
final_Data$EARDISC[which(final_Data$EARDISC == 'N')] = 'No'
final_Data$EARDISC[which(final_Data$EARDISC == "Y")] = "Yes"
final_Data$EARDISC = as.factor(final_Data$EARDISC)

## Cleans TEMPGT ##
final_Data$TEMPGT = as.character(final_Data$TEMPGT)
final_Data$TEMPGT[which(final_Data$TEMPGT == 'N')] = 'No'
final_Data$TEMPGT[which(final_Data$TEMPGT == "Y")] = "Yes"
final_Data$TEMPGT = as.factor(final_Data$TEMPGT)

## Cleans HEARTRATEGT ##
final_Data$HEARTRATEGT = as.character(final_Data$HEARTRATEGT)
final_Data$HEARTRATEGT[which(final_Data$HEARTRATEGT == 'N')] = 'No'
final_Data$HEARTRATEGT[which(final_Data$HEARTRATEGT == "Y")] = "Yes"
final_Data$HEARTRATEGT = as.factor(final_Data$HEARTRATEGT)

## Cleans CRT ##
final_Data$CRT = as.character(final_Data$CRT)
final_Data$CRT[which(final_Data$CRT == 'N')] = 'No'
final_Data$CRT[which(final_Data$CRT == "Y")] = "Yes"
final_Data$CRT = as.factor(final_Data$CRT)

## Cleans RESPERATE ##
final_Data$RESPRATE = as.character(final_Data$RESPRATE)
final_Data$RESPRATE[which(final_Data$RESPRATE == " ")] = NA
final_Data$RESPRATE[which(is.na(final_Data$RESPRATE))] = "No"
final_Data$RESPRATE[which(final_Data$RESPRATE == 'N')] = 'No'
final_Data$RESPRATE[which(final_Data$RESPRATE == "Y")] = "Yes"
final_Data$RESPRATE = as.factor(final_Data$RESPRATE)

## Cleans SCHESTIND ##
final_Data$SCHESTIND = as.character(final_Data$SCHESTIND)
final_Data$SCHESTIND[which(final_Data$SCHESTIND == 'N')] = 'No'
final_Data$SCHESTIND[which(final_Data$SCHESTIND == "Y")] = "Yes"
final_Data$SCHESTIND = as.factor(final_Data$SCHESTIND)

## Cleans LETHARGY ##
final_Data$LETHARGY = as.character(final_Data$LETHARGY)
final_Data$LETHARGY[which(final_Data$LETHARGY == 'N')] = 'No'
final_Data$LETHARGY[which(final_Data$LETHARGY == "Y")] = "Yes"
final_Data$LETHARGY = as.factor(final_Data$LETHARGY)

## Cleans CYANOSIS ##
final_Data$CYANOSIS = as.character(final_Data$CYANOSIS)
final_Data$CYANOSIS[which(final_Data$CYANOSIS == 'N')] = 'No'
final_Data$CYANOSIS[which(final_Data$CYANOSIS == "Y")] = "Yes"
final_Data$CYANOSIS = as.factor(final_Data$CYANOSIS)

## Cleans TEMPLT ##
final_Data$TEMPLT = as.character(final_Data$TEMPLT)
final_Data$TEMPLT[which(final_Data$TEMPLT == 'N')] = 'No'
final_Data$TEMPLT[which(final_Data$TEMPLT == "Y")] = "Yes"
final_Data$TEMPLT = as.factor(final_Data$TEMPLT)

## Cleans HEARTRATELT ##
final_Data$HEARTRATELT = as.character(final_Data$HEARTRATELT)
final_Data$HEARTRATELT[which(final_Data$HEARTRATELT == "M")] = "N"
final_Data$HEARTRATELT[which(final_Data$HEARTRATELT == 'N')] = 'No'
final_Data$HEARTRATELT[which(final_Data$HEARTRATELT == "Y")] = "Yes"
final_Data$HEARTRATELT = as.factor(final_Data$HEARTRATELT)

## Cleans GRUNTING ##
final_Data$GRUNTING = as.character(final_Data$GRUNTING)
final_Data$GRUNTING[which(final_Data$GRUNTING == 'N')] = 'No'
final_Data$GRUNTING[which(final_Data$GRUNTING == "Y")] = "Yes"
final_Data$GRUNTING = as.factor(final_Data$GRUNTING)

## Cleans BULFONTL ##
final_Data$BULFONTL = as.character(final_Data$BULFONTL)
final_Data$BULFONTL[which(final_Data$BULFONTL == 'N')] = 'No'
final_Data$BULFONTL[which(final_Data$BULFONTL == "Y")] = "Yes"
final_Data$BULFONTL = as.factor(final_Data$BULFONTL)

## Cleans ABDODISTEN ##
final_Data$ABDODISTEN = as.character(final_Data$ABDODISTEN)
final_Data$ABDODISTEN[which(final_Data$ABDODISTEN == 'N')] = 'No'
final_Data$ABDODISTEN[which(final_Data$ABDODISTEN == "Y")] = "Yes"
final_Data$ABDODISTEN = as.factor(final_Data$ABDODISTEN)

## Cleans UMBSEPSISC ##
final_Data$UMBSEPSISC = as.character(final_Data$UMBSEPSISC)
final_Data$UMBSEPSISC[which(final_Data$UMBSEPSISC == 'N')] = 'No'
final_Data$UMBSEPSISC[which(final_Data$UMBSEPSISC == "Y")] = "Yes"
final_Data$UMBSEPSISC = as.factor(final_Data$UMBSEPSISC)

## Cleans MULSPUS ##
final_Data$MULSPUS = as.character(final_Data$MULSPUS)
final_Data$MULSPUS[which(final_Data$MULSPUS == 'N')] = 'No'
final_Data$MULSPUS[which(final_Data$MULSPUS == "Y")] = "Yes"
final_Data$MULSPUS = as.factor(final_Data$MULSPUS)

## Cleans SEPSCREEN ##
final_Data$SEPSCREEN = as.character(final_Data$SEPSCREEN)
final_Data$SEPSCREEN[grep("DONE", final_Data$SEPSCREEN)] = "Not Done"
final_Data$SEPSCREEN[grep("NEGATIVE", final_Data$SEPSCREEN)] = "Negative"
final_Data$SEPSCREEN[grep("POSITIVE", final_Data$SEPSCREEN)] = "Positive"
final_Data$SEPSCREEN[which(final_Data$SEPSCREEN %in% c("Not Done", "Negative", "Positive") == FALSE)] = 'Not Available'
final_Data$SEPSCREEN = as.factor(final_Data$SEPSCREEN)

## Factorizes obstetric problem ##

## Cleans PIH ##
final_Data$PIH = as.character(final_Data$PIH)
final_Data$PIH[which(final_Data$PIH == 'N')] = 'No'
final_Data$PIH[which(final_Data$PIH == "Y")] = "Yes"
final_Data$PIH = as.factor(final_Data$PIH)

## Cleans GDM ##
final_Data$GDM = as.character(final_Data$GDM)
final_Data$GDM[which(final_Data$GDM == 'N')] = 'No'
final_Data$GDM[which(final_Data$GDM == "Y")] = "Yes"
final_Data$GDM = as.factor(final_Data$GDM)

## Cleans Anaemia ##
final_Data$Anaemia = as.character(final_Data$Anaemia)
final_Data$Anaemia[which(final_Data$Anaemia == 'N')] = 'No'
final_Data$Anaemia[which(final_Data$Anaemia == "Y")] = "Yes"
final_Data$Anaemia = as.factor(final_Data$Anaemia)

## Checking if maternal antibiotic was given ##
final_Data$antibiotics = 0
for (i in 1:nrow(final_Data)) {
  if("Y" %in% final_Data[i,150:182] == TRUE){
    final_Data$antibiotics[i] = 1
  }
}
final_Data[,150:182] = NULL
final_Data[,79:146] = NULL

## Removal of duplicates after data cleaning, total number of rows = 8031 ##
final_Data = unique(final_Data)

final_Data = final_Data[which(final_Data$AGESUSHRS < 72),]
## Removed neonates which has more than 1 episode## ## 4 cases without 1st episode ##
mul_ep = final_Data$ccode_enrlno[which(final_Data$EPISODENO.x > 1)]
length(unique(mul_ep))
## Total number of rows after the removal of neonates with more than 1st episode = 6974 ##
final_Data = final_Data[-which(final_Data$ccode_enrlno %in% unique(mul_ep)),]

## Extracting unique neonates with 1st sample and 1st episode and final decision is Sepsis ##
# sepsis 1150 samples #
sepsis = final_Data[which(final_Data$SGANYORG == "Y" & final_Data$CULPOSSEP == "Y"),]
sepsis$SAMPLENO = NULL
sepsis$NATURESPECI = NULL
# Total number of neonates after removing duplicate rows = 826 #
sepsis = unique(sepsis)
# Introducing outocme column as Sepsis #
sepsis$outcome = 'Sepsis'

## Extracting unique neonates with 1st sample and 1st episode and final decision is No Sepsis ##
# No sepsis 3305 sample #
nosepsis = final_Data[which(final_Data$CULPOSSEP == "N" & final_Data$CULNEGSEP == "N"),]
# Removing samples which are not 1st samples, total number of unique neonates = 2748 #
mul_non = nosepsis$ccode_enrlno[which(duplicated(nosepsis$ccode_enrlno))]
# 4 cases without sample 1 "3.7503" "3.7557" "3.7612" "3.7613" #
nosepsis_count = nosepsis[-which(nosepsis$SAMPLENO != 1 & nosepsis$ccode_enrlno %in% unique(mul_non)),]
nosepsis_count$SAMPLENO = NULL
nosepsis_count$NATURESPECI = NULL
nosepsis_count = unique(nosepsis_count)
nosepsis_count$outcome = 'NoSepsis'

## Extracting clinical sepsis samples ##
drop = final_Data[which(final_Data$CULPOSSEP == "N" & final_Data$CULNEGSEP == "Y" & final_Data$EPISODENO.x == 1),]

non = c(sepsis$ccode_enrlno, nosepsis$ccode_enrlno, drop$ccode_enrlno)
remaining = final_Data[which(final_Data$ccode_enrlno %in% non == FALSE),]
#remaining = remaining[-which(remaining$ccode_enrlno %in% c(sepsis$ccode_enrlno, nosepsis$ccode_enrlno, drop$ccode_enrlno)),]
length(unique(remaining$ccode_enrlno))

## Final dataset which is clearly separable into sepsis and no sepsis, total number of rows = 3574 ##
combineddata = rbind(sepsis, nosepsis_count)
dummyfinal = final_Data
final_Data = combineddata
final_Data$outcome = as.factor(final_Data$outcome)

## Reading visit data, total number of rows = 150052 ##
VISIT = read.csv("VISIT.csv")
## Filtering the visit data for the samples available in final data, total number of rows = 32388 ##
VISIT$ccode_enrlno = paste(as.character(VISIT$CCODE), as.character(VISIT$ENRLNO), sep = '.')
VISIT = VISIT[which(VISIT$ccode_enrlno %in% final_Data$ccode_enrlno),]

length(unique(VISIT$ccode_enrlno))
length(unique(final_Data$ccode_enrlno))

## Filtering out the samples if we don't have all visits data 24 hours before the age of suspicion ##
dummy = c(0,0,0)
for (i in 1:nrow(final_Data)){
  row = final_Data$ccode_enrlno[i]
  row = c(row, final_Data$AGESUSHRS[i])
  count = VISIT[which(VISIT$ccode_enrlno == final_Data$ccode_enrlno[i]),]
  row = c(row, nrow(count[which(count$AGEHRS <= (final_Data$AGESUSHRS[i]-24)),]))
  dummy = rbind(dummy, row)
}
dummy = as.data.frame(dummy)
dummy$V2 = as.numeric(as.character(dummy$V2))
dummy$count = (dummy$V2-24)/24
dummy$V3 = as.numeric(as.character(dummy$V3))
dummy$diff = dummy$count - dummy$V3
dummy = dummy[-1,]

## Calculating total number different visit parameters for the visits done before 24 hrs of suspicion ##
final_Data$piccline = 0
final_Data$umbilicalcatheter= 0
final_Data$cpap = 0
final_Data$imv = 0
final_Data$parentutn = 0
final_Data$ivcannula = 0
for (i in 1:nrow(final_Data)) {
  visit_count = round(final_Data$AGESUSHRS[i]/24)
  count = VISIT[which(VISIT$ccode_enrlno == final_Data$ccode_enrlno[i]),]
  dumb = count[which(count$AGEHRS < final_Data$AGESUSHRS[i]),]
  final_Data$piccline[i] = length(which(dumb$PICCLINE == "Y"))
  final_Data$umbilicalcatheter[i] = length(which(dumb$UMBCATHRS == "Y"))
  final_Data$cpap[i] = length(which(dumb$CPAP == "Y"))
  final_Data$imv[i] = length(which(dumb$IMV == "Y"))
  final_Data$parentutn[i] = length(which(dumb$PARENUTN == "Y"))
  final_Data$ivcannula[i] = length(which(dumb$IVCANNULA == "Y"))
}


######****** Machine Learning Modeling ******######

## Prepare data for eos ##
eosData = final_Data[which(final_Data$AGESUSHRS < 72),]
colnames(eosData) = c("ccode_enrlno", "Prevalence", "BHRN", "ENRLNO", "Mother's Age",
                      "Mother's Education", "Father's Education", "Father's occupation",
                      "State", "DOB", "Birth Weight", "Gestation", "Gender", "Multiple Births",
                      "Apgar score at 1 minute", "APGAR5MIN", "DOANICU", "WTNICU", "Place of delivery",
                      "Did baby cry at birth", "Number of antenatal visits", "Parity", 
                      "Maternal fever", "Maternal UTI", "Antenatal Steroid", 
                      "Medical or surgical illness", "Number of vaginal examination",
                      "Duration of labor", "Duration of rupture of membrane", 
                      "Mode of delivery", "Meconium stained liquor", 
                      "Foul smelling liquor", "Need for bag and mask at birth", 
                      "Need for chest compression at birth", "Prelactal feed",
                      "Priorfeed", "Major malformation", "Stomach wash", "CULPOSSEP", "CULNEGSEP",
                      "ccode_enrlno_episode", "ENRLNO.x", "EPISODENO.x", "CCODE.x", "BHRNO",
                      "Age at suspicion in hours", "Difficulty in feeding", 
                      "Convulsion", "Moves only when stimulated", "Fever",
                      "Cold to touch", "Apnea", "Difficulty in breathing", "Vomiting", "Diarrhea",
                      "Pus from umbilicus", "Ear discharge", "Temperature > 38 C", 
                      "Heart rate > 180/min", "Capillary refill > 3sec", "Respiratory rate > 60/min",
                      "Severe chest indrawing", "Lethargy", "Cyanosis", "Temperature < 36 C", 
                      "Heart rate < 100/min", "Grunting", "Bulging fontanelle", "Abdominal distension", 
                      "Umbilical sepsis", "Skin pustules", "Sepsis screen", "ENRLNO.y", "EPISODENO.y", 
                      "CCODE.y", "SGANYORG", "Pregnancy induced hypertension", "Gestational diabetes", 
                      "Maternal anaemia", "Antepartum antibiotics",  "outcome", 
                      "No. of PICC line days till 24 hours before suspicion", 
                      "No. of umbilical catheter days till 24 hours before suspicion",
                      "No. of cpap days till 24 hours before suspicion",
                      "No. of imv days till 24 hours before suspicion",
                      "No. of parental nutrition days till 24 hours before suspicion", 
                      "No. of ivcannula days till 24 hours before suspicion")

## Train and test split index ##
sample1 = read.csv('FinalSampling.csv')
sample1 = sample1$x

set.seed(2503)
sample1 = sample(1:nrow(eosData), 0.8*nrow(eosData), replace=FALSE)
train_new  <- eosData[sample1,]
test_new   <- eosData[-sample1,]

train_new = train_new[,c(2, 5:9, 11:15, 21:34, 37, 46:54, 57:67, 69, 71, 72, 77:80, 82:87, 81)]
test_new = test_new[,c(2, 5:9, 11:15, 21:34, 37, 46:54, 57:67, 69, 71, 72, 77:80, 82:87, 81)]

## Feature selection using Boruta - Don't rerun ##
bor_features1 <- list(list())
set.seed(123)
bor2 <- Boruta(outcome~. , data = train_new, doTrace = 2,ntree=10000,maxRuns = 2000)
cur_ind2 <- Boruta::getSelectedAttributes(bor2)
pdf("Boruta_final_eos_allvariables.pdf",height=8,width=16)
par(mar=c(6,2,2,2), crt = 45)
par(oma=c(6,2,2,2))
plot(bor2,xlab="",cex.axis = 0.5, las = 2)
dev.off()

## Over Sampling ##
colnames(train_new) = gsub(" ", "", colnames(train_new))
colnames(train_new) = gsub("'", "", colnames(train_new))
colnames(train_new) = gsub(">", "", colnames(train_new))
colnames(train_new) = gsub("<", "", colnames(train_new))
colnames(train_new) = gsub("/", "", colnames(train_new))
over_sample <-  ovun.sample(outcome~., data = train_new, seed=1, method="over")$data
colnames(over_sample) = colnames(eosData)[c(2, 5:9, 11:15, 21:34, 37, 46:54, 57:67, 69, 71, 72, 77:80, 82:87, 81)]
    
train <- over_sample[,c(cur_ind2[-c(22,14,13)], 'outcome')]
rand_over_sample=train %>% mutate_if(is.character, as.factor)
colnames(rand_over_sample) = gsub(" ", "", colnames(rand_over_sample))
colnames(rand_over_sample) = gsub("'", "", colnames(rand_over_sample))
colnames(rand_over_sample) = gsub(">", "", colnames(rand_over_sample))
colnames(rand_over_sample) = gsub("<", "", colnames(rand_over_sample))
colnames(rand_over_sample) = gsub("/", "", colnames(rand_over_sample))
## Building ML Model ##
r_modelh2o <- h2o.randomForest(colnames(rand_over_sample)[-31], 'outcome',
                                   as.h2o(rand_over_sample), nfolds = 10, 
                                   ntrees = 120, mtries = 5,
                                   seed = 111, max_depth = 8)

r_modelh2o <- h2o.xgboost(colnames(rand_over_sample)[-26], 'outcome',
                      as.h2o(rand_over_sample), nfolds = 10, 
                      ntrees = 20, learn_rate = 0.1,
                      seed = 111)
    
## Making predictions and comparing results ##
colnames(test_new) = gsub(" ", "", colnames(test_new))
colnames(test_new) = gsub("'", "", colnames(test_new))
colnames(test_new) = gsub(">", "", colnames(test_new))
colnames(test_new) = gsub("<", "", colnames(test_new))
colnames(test_new) = gsub("/", "", colnames(test_new))
newd = test_new[,names(rand_over_sample)]
pred_glmer = as.data.frame(h2o.predict(r_modelh2o, as.h2o(newd)))
pred_glmer$predict = 'NoSepsis'
pred_glmer$predict[which(pred_glmer$Sepsis > 0.37)] = 'Sepsis'
result = cbind(pred_glmer$predict, as.character(newd$outcome))

## Prediction Evaluation ##
dumb_randomforest = confusionMatrix(as.factor(result[,1]), as.factor(result[,2]), positive = "Sepsis", mode = "everything")
c(paste(col_ind[j,], collapse = ','), dumb_randomforest$overall[1], dumb_randomforest$overall[2],
  dumb_randomforest$byClass[1], dumb_randomforest$byClass[2],
  dumb_randomforest$byClass[3])


##### Prospective Validation #####

## Data processing ##
valid = read.csv('Data250222.csv')
eos_valid = valid[which(valid$AGESUSHRS < 72),]
eos_valid$BWT
eos_valid$ABDODISTEN = as.character(eos_valid$ABDODISTEN)
eos_valid$ABDODISTEN[which(eos_valid$ABDODISTEN == "N")] = "No"
eos_valid$ABDODISTEN[which(eos_valid$ABDODISTEN == "Y")] = "Yes"
eos_valid$ABDODISTEN = as.factor(eos_valid$ABDODISTEN)
eos_valid$GESTATION
eos_valid$APGAR1MIN
eos_valid$DURROFM
eos_valid$N4BAGMASK
eos_valid$AGESUSHRS
eos_valid$DIFFFEED
eos_valid$APNEA = as.character(eos_valid$APNEA)
eos_valid$APNEA[which(eos_valid$APNEA == "N")] = "No"
eos_valid$APNEA[which(eos_valid$APNEA == "Y")] = "Yes"
eos_valid$APNEA = as.factor(eos_valid$APNEA)
eos_valid$APNEA
eos_valid$TEMP
eos_valid$COLD2TCH
eos_valid$DIFFBREATH
eos_valid$VOMITING
eos_valid$HEARTRATE
eos_valid$CRT = as.character(eos_valid$CRT)
eos_valid$CRT[which(eos_valid$CRT == "N")] = "No"
eos_valid$CRT[which(eos_valid$CRT == "Y")] = "Yes"
eos_valid$CRT = as.factor(eos_valid$CRT)
eos_valid$CRT
eos_valid$LETHARGY = as.character(eos_valid$LETHARGY)
eos_valid$LETHARGY[which(eos_valid$LETHARGY == "N")] = "No"
eos_valid$LETHARGY[which(eos_valid$LETHARGY == "Y")] = "Yes"
eos_valid$LETHARGY = as.factor(eos_valid$LETHARGY)
eos_valid$LETHARGY
eos_valid$CYANOSIS
eos_valid$HEARTRATELT
eos_valid$SEPSCREEN
eos_valid$GRUNTING
eos_valid$DURLABOR
eos_valid$CPAP
eos_valid$IMV
eos_valid$IVCANNULA
eos_valid$PARENUTN
eos_valid$PICCLINE
eos_valid$UMBCTH
eos_valid$CCODE
eos_valid$COLD2TCH = as.character(eos_valid$COLD2TCH)
eos_valid$COLD2TCH = "No"
eos_valid$COLD2TCH[which(eos_valid$TEMP == "Less than 36.5°C")] = "Yes"
eos_valid$COLD2TCH = as.factor(eos_valid$COLD2TCH)
eos_valid$GDM = "No"
eos_valid$GDM[which(eos_valid$OBSPRBLM == "GDM")] = "Yes"
eos_valid$GDM[c(17,21,30,38)] = "Yes"
eos_valid$GDM = as.factor(eos_valid$GDM)
eos_valid$HEARTRATELT = as.character(eos_valid$HEARTRATELT)
eos_valid$HEARTRATELT = "No"
eos_valid$HEARTRATELT[which(eos_valid$HEARTRATE == "Less than 100/min")] = "Yes"
eos_valid$HEARTRATELT = as.factor(eos_valid$HEARTRATELT)
eos_valid$HEARTRATEGT = "No"
eos_valid$HEARTRATEGT[which(eos_valid$HEARTRATE == "Greater than 180/min")] = "Yes"
eos_valid$HEARTRATEGT = as.factor(eos_valid$HEARTRATEGT)
eos_valid$MODEDEL = as.character(eos_valid$MODEDEL)
eos_valid$MODEDEL = toTitleCase(tolower(eos_valid$MODEDEL))
eos_valid$MODEDEL = as.factor(eos_valid$MODEDEL)
eos_valid$RESPRATE = as.character(eos_valid$RESPRATE)
eos_valid$RESPRATE[which(eos_valid$RESPRATE == "N")] = "No"
eos_valid$RESPRATE[which(eos_valid$RESPRATE == "Y")] = "Yes"
eos_valid$RESPRATE = as.factor(eos_valid$RESPRATE)
eos_valid$TEMPGT = "No"
eos_valid$TEMPGT[which(eos_valid$TEMP == "Greater than 37.5°C")] = "Yes"
eos_valid = eos_valid[,c(3,4,6,8,11,12,15,17,19,21,22,28,31,33:35,37,40,46,49,56:62,72,82:84,77,13)]
colnames(eos_valid) = c("Prevalence", "Birth Weight", "Apgar score at 1 minute", "Mode of delivery",
                      "Capillary refill > 3sec", "Respiratory rate > 60/min", "Gestation",
                      "Duration of rupture of membrane", "Apnea", "Lethargy", "Abdominal distension", 
                      "Duration of labor", "Difficulty in feeding", "Cold to touch", 
                      "Difficulty in breathing", "Vomiting", "Grunting", "Sepsis screen", 
                      "Need for bag and mask at birth", "Age at suspicion in hours", "Cyanosis", 
                      "No. of ivcannula days till 24 hours before suspicion", 
                      "No. of PICC line days till 24 hours before suspicion", 
                      "No. of umbilical catheter days till 24 hours before suspicion",
                      "No. of cpap days till 24 hours before suspicion",
                      "No. of imv days till 24 hours before suspicion",
                      "No. of parental nutrition days till 24 hours before suspicion", 
                      "Heart rate < 100/min", "Gestational diabetes", "Heart rate > 180/min",  
                      "Temperature > 38 C", "outcome", "ENRLNO")
eos_valid$Prevalence = as.character(eos_valid$Prevalence)
eos_valid$Prevalence = 2.1
eos_valid$Steroids = "Not known"
eos_valid$outcome = as.character(eos_valid$outcome)
eos_valid$outcome[which(eos_valid$outcome == "Negative")] = "NoSepsis"
eos_valid$outcome[which(eos_valid$outcome == "Positive")] = "Sepsis"
eos_valid$outcome = as.factor(eos_valid$outcome)
colnames(eos_valid)[34] = 'Antenatal Steroid'
colnames(eos_valid) = gsub(" ", "", colnames(eos_valid))
colnames(eos_valid) = gsub("'", "", colnames(eos_valid))
colnames(eos_valid) = gsub(">", "", colnames(eos_valid))
colnames(eos_valid) = gsub("<", "", colnames(eos_valid))
colnames(eos_valid) = gsub("/", "", colnames(eos_valid))
eos_valid = unique(eos_valid)
validh2o = as.h2o(eos_valid[,-c(32,33)])
pred_valid = as.data.frame(h2o.predict(r_modelh2o, validh2o))
pred_valid$predict = 'NoSepsis'
pred_valid$predict[which(pred_valid$Sepsis > 0.35)] = 'Sepsis'
result_valid = cbind(pred_valid$predict, as.character(eos_valid$outcome), as.character(eos_valid$ENRLNO))

## Prediction Evaluation ##
dumb_randomforest_valid = confusionMatrix(as.factor(result_valid[,1]), as.factor(result_valid[,2]), positive = "Sepsis")
c(paste(col_ind[j,], collapse = ','), dumb_randomforest_valid$overall[1], dumb_randomforest_valid$overall[2],
  dumb_randomforest_valid$byClass[1], dumb_randomforest_valid$byClass[2],
  dumb_randomforest_valid$byClass[3])



##### Interpretability #####
explainer <- lime(rand_over_sample, r_modelh2o, n_bins = 5, n_permutations = 1000)

internew = eos_valid[which(eos_valid$ENRLNO %in% c("134R","450R","249R","35P","267R","444R","10R")),]
internew$ENRLNO = NULL
internew = internew[,colnames(rand_over_sample)]
internew = rbind(rand_over_sample[1,], internew)
internew$outcome = NULL
rownames(internew) = c("1", "10R", "134R", "267R", "249R", "35P", "444R", "450R")
# Explain new observation
explanation <- explain(internew, explainer, n_labels = 2, n_features = 35)
explainplot = explanation[which(explanation$label == "NoSepsis"),]
saveRDS(explanation, 'explainplot.rds')

pdf("35P")
plot_features(explanation)
dev.off()


"134R","450R","249R","35P","267R","444R","10R"



#########################################################33

    sens = c(sens, dumb_randomforest$byClass[1])
    spec = c(spec, dumb_randomforest$byClass[2])
    colnames(train_new)[which(colnames(train_new) %in% strsplit(vec[6406,1], ',')[[1]] == FALSE)]
    #dumb_ensemble
    #cur_ind[-34]
    vec = rbind(vec, c(paste(col_ind[j,], collapse = ','), dumb_randomforest$overall[1], dumb_randomforest$overall[2],
                       dumb_randomforest$byClass[1], dumb_randomforest$byClass[2],
                       dumb_randomforest$byClass[3]))








train = train_new[,c(cur_ind, 'outcome')]

## Over sampling ##
over_sample <-  ovun.sample(outcome~., data = train, seed=1, method="over")$data

## Generating random forest model ##
rand_over_sample=over_sample %>% mutate_if(is.character, as.factor)
r_model <- randomForest(outcome~., data = rand_over_sample,
                        ntree = 500,mtry=2,)

## Making predictions and comparing results ##
pred_glmer <- predict(r_model, newdata=(test_new[,names(train)]),  type = "prob")
pred_glmer = as.data.frame(pred_glmer)
pred_glmer$predict = 'NoSepsis'
pred_glmer$predict[which(pred_glmer$Sepsis > 0.30)] = 'Sepsis'
result = cbind(pred_glmer$predict, as.character(test_new$outcome))
#length(which(result[,1] == result[,2]))
dumb_randomforest = confusionMatrix(as.factor(result[,2]), as.factor(result[,1]), positive = "Sepsis")
#dumb_ensemble
#cur_ind[-34]
vec = rbind(vec, c(paste(col_ind[j,], collapse = ','), dumb_randomforest$overall[1], dumb_randomforest$overall[2],
                   dumb_randomforest$byClass[1], dumb_randomforest$byClass[2],
                   dumb_randomforest$byClass[3]))

###################################columns before boruta##########

before_boruta<-c("FEDU","FOCCUP","PDELIVERY","CRYBIRTH","WTNICU","PRELAFEED","PUSUMBSIP","PRIORFEED","STOMACHW","DIARRHEA","BULFONTL","UMBSEPSISC")


train_new<-train_new[,-which(colnames(train_new) %in% before_boruta)]
test_new<-test_new[,-which(colnames(test_new) %in% before_boruta)]





train_new = train_new[which(train_new$AGESUSHRS < 72),]
#################################################



clinical<-c("VOMITING","DIFFFEED","ABDODISTEN","DIFFBREATH","APNEA","RESPRATE","SCHESTIND","GRUNTING",
            "CYANOSIS","HEARTRATELT","HEARTRATEGT","TEMPLT","TEMPGT","CRT","CONVULSION","MOVESTIMUL",
            "LETHARGY","EARDISC","SEPSCREEN","MULSPUS", "FEVER","COLD2TCH",
            "piccline","umbilicalcatheter","cpap","imv","parentutn","PUSUMBSIP","AGESUSHRS","FOULSL",             
            "ivcannula")

train_cli<-train_new[,-which(colnames(train_new) %in% clinical)]


#################################################
col_drop_2<-c("outcome","ccode_enrlno")
col_drop_3<-c("ccode_enrlno")
np<-length(colnames(train_new) )

#saveRDS(parts,file =paste0("/storage/pradeep/aysuhi/test_out_case/stratified_results/parts.RDS"))



#parts<-readRDS("/storage/pradeep/aysuhi/test_out_case/stratified_results/parts.RDS")

bor_features <- list(list())
set.seed(123)
bor <- Boruta(outcome~. , data = train_new, doTrace = 2,ntree=10000,maxRuns = 2000)



#bor <- Boruta(outcome~. , data = train[,!(names(train) %in% col_drop_3)], doTrace = 2,ntree=10000,maxRuns = 2000)

#bor<-TentativeRoughFix(bor)



cur_ind <- Boruta::getSelectedAttributes(bor)
#bor_features[[i]] <- cur_ind
#bor_features<-readRDS("/storage/pradeep/aysuhi/test_out_case/without_validation/final_eos_los/bor_final.RDS")




pdf("Boruta_final_eos.pdf",height=7,width=28)
par(mar=c(5,2,2,2))
par(oma=c(5,2,2,2))
plot(bor,las=2,xlab="")
dev.off()

##################################################################################

new_train<- train_new[,!(names(train_new) %in% col_drop_3)]
new_train<-new_train[,which(colnames(new_train) %in% c(cur_ind,"outcome"))]


colnames(new_train)[which(colnames(new_train)=="outcome")]<-"label"
new_train$label<-as.factor(new_train$label)




w <- which( sapply( new_train, class ) == 'factor' )
new_train[w] <- lapply( new_train[w], function(x) as.numeric((x))-1 )

numericData <- new_train[sapply(new_train, is.numeric)]

descrCor <- cor(numericData)


# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

pdf("corr_after_boruta_eos.pdf")
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))
dev.off()

########################################################################################


nam_features<-cur_ind[!cur_ind %in% c("APGAR5MIN","RESPRATE","MOVESTIMUL","TEMPGT","TEMPLT","N4CHESTCOMP","ANTEST")]
nam_features<-c(nam_features,"outcome")

vec = c()

for (i in 2:37) {
  col_ind = combinations(n=37, r = i, v = cur_ind)
  for (j in nrow(col_ind):1) {
    train <- train_new[,c(col_ind[j,], 'outcome')]
    over_sample <-  ovun.sample(outcome~., data = train,
                                seed=1, method="over")$data
    
    rand_over_sample=over_sample %>% mutate_if(is.character, as.factor)
    r_model <- randomForest(outcome~., data = rand_over_sample,
                            ntree = 500,mtry=2)
    
    pred_glmer <- predict(r_model, newdata=(test_new[,names(train)]),  type = "prob")
    pred_glmer = as.data.frame(pred_glmer)
    pred_glmer$predict = 'NoSepsis'
    pred_glmer$predict[which(pred_glmer$Sepsis > 0.30)] = 'Sepsis'
    result = cbind(pred_glmer$predict, as.character(test_new$outcome))
    #length(which(result[,1] == result[,2]))
    dumb_randomforest = confusionMatrix(as.factor(result[,2]), as.factor(result[,1]), positive = "Sepsis")
    #dumb_ensemble
    #cur_ind[-34]
    vec = rbind(vec, c(paste(col_ind[j,], collapse = ','), dumb_randomforest$overall[1], dumb_randomforest$overall[2],
                        dumb_randomforest$byClass[1], dumb_randomforest$byClass[2],
                        dumb_randomforest$byClass[3]))
  }
}





test <- test_new[,which(colnames(test_new) %in% nam_features)]
#test$outcome<-as.numeric(test$outcome)-1

######################################################################################
# pro_range<-c(0.1,0.2,0.3,0.4,0.5)
# 
# for (i_pro in pro_range)
k=5
#
#parts <- createFolds(as.factor(train_new$outcome), k = k)
# parts<-createDataPartition(
#   as.factor(train$outcome),
#   times = 5,
#   p = 0.9,
#   list = TRUE
# )



test_eos = test[which(test$AGESUSHRS < 72),]

parts1 <- createFolds(as.factor(test_eos$outcome), k = k)


for(i in 1:5)
  
{
  # if(i !=5){
  print(i)
  #val_data <- train[-c(parts[[i]]),]
  train_data <- train
  
  test_data<-test[parts1[[i]],]
  
  
  train_data <- train_data[,which(colnames(train_data) %in% nam_features)]
  # val_data <- val_data[,which(colnames(val_data) %in% nam_features)]
  
  test_data <- test_data[,which(colnames(test_data) %in% nam_features)]
  
  if("steroid" %in% nam_features)
  {
    train_data$steroid <-as.factor(train_data$steroid)
    #  val_data$steroid <-as.factor(val_data$steroid)
    test_data$steroid <-as.factor(test_data$steroid)
  }
  
  if("CCODE" %in% nam_features)
  {
    
    train_data$CCODE <-as.factor(train_data$CCODE)
    #  val_data$CCODE <-as.factor(val_data$CCODE)
    test_data$CCODE <-as.factor(test_data$CCODE)
  }
  
  
  if("NoSepsis"%in% train_data$outcome)
  {
    train_data$outcome<-unfactor( train_data$outcome)
    train_data$outcome[grep("NoSepsis", train_data$outcome)] <- "0"
    train_data$outcome[grep("Sepsis", train_data$outcome)] <- "1"
    train_data$outcome<-as.factor( train_data$outcome)
  }
  
  
  if("NoSepsis"%in% test_data$outcome)
  {
    test_data$outcome<-unfactor( test_data$outcome)
    test_data$outcome[grep("NoSepsis", test_data$outcome)] <- "0"
    test_data$outcome[grep("Sepsis", test_data$outcome)] <- "1"
    test_data$outcome<-as.factor( test_data$outcome)
  }
  #test_data <- train[parts[[i]],]
  # }else{
  #   train_data <- train[-c(parts[[1]], parts[[5]]),]
  #   val_data <- train[parts[[1]],]
  #   test_data <- train[parts[[5]],]
  # }
  # 
  # col_drop_2<-c("outcome","ccode_enrlno")
  # col_drop_3<-c("ccode_enrlno")
  
  
  
  if(i==1)
  {
    over_sample <-  ovun.sample(outcome~., data = train,
                                seed=1, method="over")$data
    # 
    #under_sample <- SMOTE(outcome~ .,  train_data[,!(names(train_data) %in% col_drop_3)])
    #under_sample<-train_data
    #under_sample$outcome <- as.numeric(levels(under_sample$outcome))[under_sample$outcome]
    tr<-as.numeric(levels(over_sample$outcome))[over_sample$outcome]
    #under_sample$outcome<-NULL
  }
  
  
  
  
  
  
  
  # 
  
  # 
  
  
  
  
  
  for(mod in 1:4)
  {
    
    
    if(mod==1)
    {
      
      if(i==1)
      {
        
        #    # xg_data<-over_sample
        #    # xg_data$outcome<-NULL
        #    # 
        #    # #make grid
        #    # grid = expand.grid(eta = seq(0.1,0.4,0.1),gamma = seq(0,5,1))
        #    # 
        #    # #make parallel cluster and register
        #    # cl = makeCluster(detectCores()-1) #n-1 core cluster
        #    # registerDoParallel(cl)
        #    # 
        #    # 
        #    # #do search by parallel
        #    # grid_search = foreach(i = 1:nrow(grid),.combine = rbind,.packages = c('dplyr','xgboost')) %dopar% {
        #    #   model = xgb.cv(as.matrix(sapply(xg_data, as.numeric)),
        #    #                  label = tr,
        #    #                  nfold = 5, nrounds = 200, early_stopping_rounds = 20,# about folds and rounds
        #    #                  objective = 'binary:logistic',verbose = F, prediction = T,
        #    #                  params = grid[i,]
        #    #   )
        #    #   data.frame(train_rmse_last = unlist(model$evaluation_log[,2]) %>% last,
        #    #              test_rmse_last = unlist(model$evaluation_log[,4]) %>% last)
        #      
        #    }
        #    
        #    
        #    
        #   # stopCluster(cl) #end parallel cluster
        #    
        # #   grid_search[which.min(grid_search$test_rmse_last),]
        #    
        # #   grid[which.min(grid_search$test_rmse_last),]
        #    
        #    
        #    
        #    
        #    #==========================================================================
        #    # Variable importance using xgb.importance
        #    #==========================================================================
        #    
        #    
        #    #xgbooster model using grid search parameter
        #    
        #    
        xg_data<-over_sample
        
        xg_data$outcome<-NULL
        #    xgmodel = xgboost(as.matrix(sapply(xg_data, as.numeric)),
        #                    label = tr,
        #                    nrounds = 200, early_stopping_rounds = 20,# about folds and rounds
        #                    objective = 'binary:logistic',verbose = F,
        #                    params =)
        
        
        # xg_tune<- eztune(
        #   x = train_data[,!(names(train_data) %in% col_drop_2)],y=train_data$outcome,
        #   method = "gbm",
        #   fast = FALSE ,cross = 5)
        
        
        
        
        xgmodel <- xgboost(as.matrix(sapply(xg_data, as.numeric)),
                           label = tr,num_parallel_tree =100,
                           max_depth = 10, colsample_bytree = 0.5,
                           nrounds=500,
                           early_stopping_rounds=50,
                           seed = 1,objective = "binary:logistic")
        
        
        # max.depth = 10, 
        # eta = 0.01, 
        # nthread = 2, 
        # nround = 10000, 
        # watchlist = watchlist,
        
        
        
      }
      # pred_glmer <- predict(model, newdata=as.matrix(sapply(val_data[,!(names(val_data) %in% col_drop_2)], as.numeric)),  type = "prob")
      # 
      # pred_glmer_perf <- prediction(as.numeric(pred_glmer), as.numeric(val_data$outcome))
      # perf_glmer <- performance(pred_glmer_perf, "tpr", "fpr")
      
      
      
      # f1_orig<-as.numeric(unfactor(unlist(pred_glmer_perf@labels)))
      # f1_pred<-as.numeric(unlist(pred_glmer_perf@predictions))
      # last_ts<-max(f1_pred)
      # first_ts<-min(f1_pred)+0.1
      # f1_scores <- sapply(seq(first_ts, last_ts, .01), function(thresh) F1_Score(f1_orig, ifelse(f1_pred >= thresh, 1, 0), positive = 1))
      # cp_max_height_glmer=f1_scores[which.max(f1_scores)] #
      # 
      
      pro_range<-seq(0.10,0.49,by=0.01)
      
      
      
      for (i_pro in pro_range)
      {
        cp_max_height_glmer<-i_pro
        pred_glmer <- predict(xgmodel, newdata=as.matrix(sapply(test_data[,!(names(test_data) %in% col_drop_2)], as.numeric)),  type = "prob")
        #plot(pred_glmer)
        pred_glmer_perf <- prediction(as.numeric(pred_glmer), as.numeric(test_data$outcome))
        perf_glmer <- performance(pred_glmer_perf, "tpr", "fpr")
        
        
        pro<-data.frame(Predicted= as.numeric(pred_glmer ),Actual=test_data$outcome)
        
        pro$'Cutoff'<- cp_max_height_glmer
        # xg_p_threshold[[i]]<-cp_max_height_glmer
        # xg_p_5fold[[i]]<-pro
        # write.csv(xg_p_5fold[[i]],paste0("/storage/pradeep/ayus"))
        
        
        
        ########################
        AUC_glmer = (performance(pred_glmer_perf, "auc")@y.values[[1]])
        ##print(plot(perf_glmer,colorize =T, main=paste('ROC',i, 'Hyt', 'AUC=', round(AUC_glmer,2),sep = ' ')))
        #dev.off()
        AUCPR_glmer<-(performance(pred_glmer_perf, "aucpr")@y.values[[1]])
        
        result_vec <- as.numeric(pred_glmer > cp_max_height_glmer)
        result_vec <- as.factor(result_vec)
        
        con <- data.frame(actual=test_data$outcome,result_vec)
        ##print(paste("confusion_matrix...for_cutoff=",cp_max_height,sep=""))
        mat_con <- confusionMatrix(con$result_vec,reference = con$actual, positive = "1")
        print(confusionMatrix(con$result_vec,reference = con$actual, positive = "1"))
        # xg_spe_sen_5fold[[i]] <- mat_con$byClass
        # xg_Acc_5fold[[i]] <- mat_con$overall[1]
        # xg_AUC_5fold[[i]] <- AUC_glmer
        if(i_pro==0.1 & i==1)
        {
          all_metric<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
          all_metric$"Model"<-"XGB"
        }
        
        if(i_pro!=0.1)
        {
          all_me<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
          all_me$"Model"<-"XGB"
          all_metric<-rbind(all_metric,all_me)
        }
        
        
        
        if(i_pro==0.1 & i!=1)
        {
          all_me<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
          all_me$"Model"<-"XGB"
          all_metric<-rbind(all_metric,all_me)
        }
        
      }
      
      
      
    }
    
    
    if(mod==2)
    {
      
      
      
      if(i==1)
      {
        
        # ada_tune<- eztune(
        #   x = train_data[,!(names(train_data) %in% col_drop_2)],y=train_data$outcome,
        #   method = "ada",
        #   fast = FALSE ,cross = 5)
        
        adamodel <- ada(outcome~.,over_sample,iter=100, loss="logistic")
      }
      pro_range<-seq(0.10,0.49,by=0.01)
      
      for (i_pro in pro_range)
      {
        
        cp_max_height_glmer<-i_pro
        
        pred_glmer <- predict(adamodel, newdata=(test_data[,!(names(test_data) %in% col_drop_2)]),  type = "prob")
        #plot(pred_glmer)
        pred_glmer_perf <- prediction(as.numeric(pred_glmer[,2]), as.numeric(test_data$outcome))
        perf_glmer <- performance(pred_glmer_perf, "tpr", "fpr")
        
        
        pro<-data.frame(Predicted= as.numeric(pred_glmer ),Actual=test_data$outcome)
        
        pro$'Cutoff'<- cp_max_height_glmer
        # ad_p_threshold[[i]]<-cp_max_height_glmer
        # ad_p_5fold[[i]]<-pro
        #write.csv(p_5fold[[i]],paste0(main_path,"Modeling_30min_4hr_final/pro_fold/",i,".csv"))
        
        
        
        ########################
        AUC_glmer = (performance(pred_glmer_perf, "auc")@y.values[[1]])
        ##print(plot(perf_glmer,colorize =T, main=paste('ROC',i, 'Hyt', 'AUC=', round(AUC_glmer,2),sep = ' ')))
        #dev.off()
        AUCPR_glmer<-(performance(pred_glmer_perf, "aucpr")@y.values[[1]])
        
        result_vec <- as.numeric(pred_glmer[,2] > cp_max_height_glmer)
        result_vec <- as.factor(result_vec)
        
        con <- data.frame(actual=test_data$outcome,result_vec)
        ##print(paste("confusion_matrix...for_cutoff=",cp_max_height,sep=""))
        mat_con <- confusionMatrix(con$result_vec,reference = con$actual, positive = "1")
        print(confusionMatrix(con$result_vec,reference = con$actual, positive = "1"))
        # ad_spe_sen_5fold[[i]] <- mat_con$byClass
        # ad_Acc_5fold[[i]] <- mat_con$overall[1]
        # ad_AUC_5fold[[i]] <- AUC_glmer
        
        all_me<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
        all_me$"Model"<-"ADA"
        all_metric<-rbind(all_metric,all_me)
        
      }
    }
    if(mod==3)
    {
      
      if(i==1)
      {
        
        # obj <- tune.svm(outcome~., data = val_data[,!(names(val_data) %in% col_drop_3)], gamma = 2^(-1:1), cost = 2^(2:4))
        
        # obj<- eztune( x = train_data[,!(names(train_data) %in% col_drop_2)],y=train_data$outcome, fast = FALSE,cross=5)
        
        
        svmmodel <- svm(outcome~., data = over_sample,type = 'C-classification',
                        kernel = 'radial',cost=33,gamma=0.00390625,probability=TRUE)
        
      }
      pro_range<-seq(0.10,0.49,by=0.01)
      
      for (i_pro in pro_range)
      {
        cp_max_height_glmer<-i_pro
        
        pred_glmer <- predict(svmmodel, newdata=(test_data[,!(names(test_data) %in% col_drop_2)]),  probability=TRUE)
        #plot(pred_glmer)
        pred_glmer_perf <- prediction(as.numeric(attr(pred_glmer,"probabilities")[,2]), as.numeric(test_data$outcome))
        perf_glmer <- performance(pred_glmer_perf, "tpr", "fpr")
        
        
        pro<-data.frame(Predicted= as.numeric(pred_glmer ),Actual=test_data$outcome)
        
        pro$'Cutoff'<- cp_max_height_glmer
        # svm_p_threshold[[i]]<-cp_max_height_glmer
        # svm_p_5fold[[i]]<-pro
        # #write.csv(p_5fold[[i]],paste0(main_path,"Modeling_30min_4hr_final/pro_fold/",i,".csv"))
        
        
        
        ########################
        AUC_glmer = (performance(pred_glmer_perf, "auc")@y.values[[1]])
        ##print(plot(perf_glmer,colorize =T, main=paste('ROC',i, 'Hyt', 'AUC=', round(AUC_glmer,2),sep = ' ')))
        #dev.off()
        AUCPR_glmer<-(performance(pred_glmer_perf, "aucpr")@y.values[[1]])
        
        result_vec <- as.numeric(attr(pred_glmer,"probabilities")[,2] > cp_max_height_glmer)
        result_vec <- as.factor(result_vec)
        
        con <- data.frame(actual=test_data$outcome,result_vec)
        ##print(paste("confusion_matrix...for_cutoff=",cp_max_height,sep=""))
        mat_con <- confusionMatrix(con$result_vec,reference = con$actual, positive = "1")
        print(confusionMatrix(con$result_vec,reference = con$actual, positive = "1"))
        # svm_spe_sen_5fold[[i]] <- mat_con$byClass
        # svm_Acc_5fold[[i]] <- mat_con$overall[1]
        # svm_AUC_5fold[[i]] <- AUC_glmer
        # 
        all_me<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
        all_me$"Model"<-"SVM"
        all_metric<-rbind(all_metric,all_me)
        
      }
      
    }
    if(mod==4)
    {
      
      ## Set seed for reproducibility
      set.seed(123)
      
      
      
      if(i==1)
      {
        # bestmtry <- tuneRF((over_sample[,!(names(over_sample) %in% col_drop_2)]),over_sample$outcome,mtryStart = 1, 
        #                    ntreeTry=1000, 
        #                    stepFactor = 0.5, 
        #                    improve = 0.0001, 
        #                    trace=TRUE, 
        #                    plot = TRUE,
        #                    doBest = TRUE,
        #                    nodesize = 30, 
        #                    importance=TRUE,seed=2) 
        
        
        rand_over_sample=over_sample %>% mutate_if(is.character, as.factor)
        r_model <- randomForest(outcome~., data = rand_over_sample,
                                ntree = 500,mtry=2,)
        
        
        
      }
      
      
      pro_range<-seq(0.10,0.49,by=0.01)
      
      for (i_pro in pro_range)
      {
        cp_max_height_glmer<-i_pro
        
        test_data=test_data %>% mutate_if(is.character, as.factor)
        
        pred_glmer <- predict(r_model, newdata=(test_new[,names(train)]),  type = "prob")
        pred_glmer = as.data.frame(pred_glmer)
        pred_glmer$predict = 'NoSepsis'
        pred_glmer$predict[which(pred_glmer$Sepsis > 0.30)] = 'Sepsis'
        result = cbind(pred_glmer$predict, as.character(test_new$outcome))
        length(which(result[,1] == result[,2]))
        dumb_ensemble = confusionMatrix(as.factor(result[,2]), as.factor(result[,1]), positive = "Sepsis")
        h2orf1_clinical = c(dumb_randomforest$overall[1], dumb_randomforest$overall[2],
                            dumb_randomforest$byClass[1], dumb_randomforest$byClass[2],
                            dumb_randomforest$byClass[3]))
        #plot(pred_glmer)
        pred_glmer_perf <- prediction(as.numeric(pred_glmer[,2]), as.numeric(test_new$outcome))
        perf_glmer <- performance(pred_glmer_perf, "tpr", "fpr")
        
        
        pro<-data.frame(Predicted= as.numeric(pred_glmer ),Actual=test_data$outcome)
        
        pro$'Cutoff'<- cp_max_height_glmer
        # rm_p_threshold[[i]]<-cp_max_height_glmer
        # rm_p_5fold[[i]]<-pro
        #write.csv(p_5fold[[i]],paste0(main_path,"Modeling_30min_4hr_final/pro_fold/",i,".csv"))
        
        
        
        ########################
        AUC_glmer = (performance(pred_glmer_perf, "auc")@y.values[[1]])
        ##print(plot(perf_glmer,colorize =T, main=paste('ROC',i, 'Hyt', 'AUC=', round(AUC_glmer,2),sep = ' ')))
        #dev.off()
        
        result_vec <- as.numeric(pred_glmer[,2] > cp_max_height_glmer)
        result_vec <- as.factor(result_vec)
        
        con <- data.frame(actual=test_data$outcome,result_vec)
        ##print(paste("confusion_matrix...for_cutoff=",cp_max_height,sep=""))
        mat_con <- confusionMatrix(con$result_vec,reference = as.factor(con$actual), positive = "1")
        print(confusionMatrix(con$result_vec,reference = as.factor(con$actual), positive = "1"))
        
        AUCPR_glmer<-(performance(pred_glmer_perf, "aucpr")@y.values[[1]])
        
        
        all_me<- cbind(cp_max_height_glmer,as.data.frame(t(mat_con$byClass)),mat_con$overall[1],AUC_glmer,AUCPR_glmer,i)
        all_me$"Model"<-"Random"
        all_metric<-rbind(all_metric,all_me)
        
      }
    }
    
    
    
  }
  
}


# all_metric<-round(all_metric[,-c(1,16)], digits=2)
agg_tbl <- all_metric %>% group_by(Model, cp_max_height_glmer) %>% 
  summarise(across(everything(), list(mean = mean)))

dat_per_summ1<-round(agg_tbl[,-c(1)], digits=2)
dat_per_summ1$"Model"<-agg_tbl$Model


dat_per_summ1<-dat_per_summ1[,c("Sensitivity_mean","Specificity_mean","Pos Pred Value_mean" ,"Neg Pred Value_mean","Recall_mean","F1_mean","mat_con$overall[1]_mean","AUC_glmer_mean","AUCPR_glmer_mean","cp_max_height_glmer","Model")]

write.csv(dat_per_summ1,"/storage/pradeep/aysuhi/test_out_case/without_validation/final_eos_los/all_eos_eos_result.csv")




agg_tbl_2 <- all_metric %>% group_by(Model, cp_max_height_glmer) %>% 
  summarise(across(everything(), list(sd = sd)))

dat_per_sd1<-round(agg_tbl_2[,-c(1)], digits=2)
dat_per_sd1$"Model"<-agg_tbl_2$Model

dat_per_sd1<-dat_per_sd1[,c("Sensitivity_sd","Specificity_sd","Pos Pred Value_sd" ,"Neg Pred Value_sd","Recall_sd","F1_sd","mat_con$overall[1]_sd","AUC_glmer_sd","AUCPR_glmer_sd","cp_max_height_glmer","Model")]

write.csv(dat_per_sd1,"/storage/pradeep/aysuhi/test_out_case/without_validation/final_eos_los/all_eos_eos_result_sd.csv")
