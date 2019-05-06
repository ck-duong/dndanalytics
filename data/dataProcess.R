library(import5eChar) # github.com/oganm/import5eChar
library(purrr)
library(readr)
library(glue)
library(digest)
library(dplyr)
library(XML)
library(ogbox) # github.com/oganm/ogbox
library(wizaRd) # github.com/oganm/wizaRd
library(stringr)
library(memoise)
library(rgeolocate) # in case I deal with geo placement. not used now
library(here)
library(data.table)
library(randomNames) # add friendlier names. github.com/oganm/randomNames

usethis::use_data_raw()

# memoisation for quick access
if(file.exists('memoImportChar.rds')){
	memoImportChar = readRDS(here('memoImportChar.rds'))
} else {
	memoImportChar = memoise(importCharacter)
	saveRDS(memoImportChar,'memoImportChar.rds')
}

# get all char files saved everywhere. Yes I made a mess that I refused to fix...
charFiles = c(list.files('/srv/shiny-server/printSheetApp/chars/',full.names = TRUE),
			  list.files('/srv/shiny-server/interactiveSheet/chars/',full.names = TRUE),
			  list.files('/srv/shiny-server/chars',full.names = TRUE),
			  list.files('/srv/shiny-server/chars2', full.names = TRUE),
			  list.files('/srv/shiny-server/chars3', full.names = TRUE),
			  list.files('/srv/shiny-server/chars4', full.names = TRUE))




print('reading char files')

fileInfo = file.info(charFiles)
charFiles = charFiles[order(fileInfo$mtime)]
fileInfo = fileInfo[order(fileInfo$mtime),]

# use import5eChar to read the all of them
chars = charFiles %>% lapply(function(x){
	memoImportChar(file = x)
})
saveRDS(memoImportChar,here('memoImportChar.rds'))

# get date information. dates before 2018-04-16 are not reliable
# get user fingerprint and IP
fileData = charFiles %>% basename %>% strsplit('_')

# add file and user info to the characters
print('constructing char table')
chars = lapply(1:length(chars),function(i){
	char = chars[[i]]
	char$date = fileInfo$mtime[i]
	if(length(fileData[[i]]) == 1){
		char$ip = 'NULL'
		char$finger = 'NULL'
		char$hash = fileData[[i]]
	} else{
		char$finger = fileData[[i]][1]
		char$ip = fileData[[i]][2]
		char$hash = fileData[[i]][3]
	}
	char
})

# setting the names to character name and class. this won't be exposed to others
names(chars) = chars %>% map_chr(function(x){
	paste(x$Name,x$ClassField)
})

# create the table
charTable = chars %>% map(function(x){
	data.frame(ip = x$ip,
			   finger = x$finger,
			   hash = x$hash,
			   name = x$Name,
			   race = x$Race,
			   background = x$Background,
			   date = x$date,
			   class = paste(x$classInfo[,1],x$classInfo[,3],collapse='|'),
			   justClass =  x$classInfo[,'Class'] %>% paste(collapse ='|'),
			   subclass = x$classInfo[,'Archetype'] %>% paste(collapse ='|'),
			   level = x$classInfo[,'Level'] %>% as.integer() %>% sum,
			   feats = x$feats[x$feats !=''] %>% paste(collapse = '|'),
			   HP = x$currentHealth,
			   AC = AC(x),
			   Str = x$abilityScores['Str'],
			   Dex = x$abilityScores['Dex'],
			   Con = x$abilityScores['Con'],
			   Int = x$abilityScores['Int'],
			   Wis = x$abilityScores['Wis'],
			   Cha = x$abilityScores['Cha'],
			   alignment = x$Alignment,
			   skills = x$skillProf %>% which %>% names %>% paste(collapse = '|'),
			   weapons = x$weapons %>% map_chr('name') %>% gsub("\\|","",.)  %>% paste(collapse = '|'),
			   spells = glue('{x$spells$name %>% gsub("\\\\*|\\\\|","",.)}*{x$spells$level}') %>% glue_collapse('|') %>% {if(length(.)!=1){return('')}else{return(.)}},
			   day = x$date %>%  format('%m %d %Y'),
			   castingStat = names(x$abilityMods[x$castingStatCode+1]),
			   stringsAsFactors = FALSE)
}) %>% do.call(rbind,.)

# remove multiple occurances of the same file
charTable %<>% arrange(desc(date)) %>%  filter(!duplicated(hash))



if(file.exists('memoIPgeolocate.rds')){
	memoIPgeolocate = readRDS(here('memoIPgeolocate.rds'))
} else {
	memoIPgeolocate = memoise(ipapi::geolocate)
	saveRDS(memoIPgeolocate,'memoIPgeolocate.rds')
}




ipLocations = charTable$ip %>%
	lapply(memoIPgeolocate,.progress = FALSE) %>%
	rbindlist(fill = TRUE)

saveRDS(memoIPgeolocate,here('memoIPgeolocate.rds'))

charTable$country = ipLocations$country
charTable$countryCode = ipLocations$countryCode
# some experimentation with user location.
# file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
# results <- maxmind(charTable$ip, file, c("continent_name", "country_code", "country_name"))



# post processing -----
# the way races are encoded in the app is a little silly. sub-races are
# not recorded separately. essentially race information is lost other
# than a text field after it's effects are applied during creation.
# The text field is also not too consistent. For instance if you are a
# variant half elf it'll simply say "Variant" but if you are a variant human
# it'll only say human
# here, I define regex that matches races.
# kind of an overkill as only few races actually required special care
races = c(Aarakocra = 'Aarakocra',
		  Aasimar = 'Aasimar',
		  Bugbear= 'Bugbear',
		  Dragonborn = 'Dragonborn',
		  Dwarf = 'Dwarf',
		  Elf = '(?<!Half-)Elf|Drow',
		  Firbolg = 'Firbolg',
		  Genasi= 'Genasi',
		  Gith = 'Geth|Githzerai',
		  Gnome = 'Gnome',
		  Goblin='^Goblin$',
		  Goliath = 'Goliath',
		  'Half-Elf' = '(Half-Elf)|(^Variant$)',
		  'Half-Orc' = 'Half-Orc',
		  Halfling = 'Halfling',
		  Hobgoblin = 'Hobgoblin$',
		  Human = 'Human|Variant Human',
		  Kenku = 'Kenku',
		  Kobold = 'Kobold',
		  Lizardfolk = 'Lizardfolk',
		  Orc = '(?<!Half-)Orc',
		  'Yaun-Ti' = 'Serpentblood|Yuan-Ti',
		  Tabaxi = 'Tabaxi',
		  Tiefling ='Tiefling|Lineage',
		  Triton = 'Triton',
		  Turtle = 'Turtle|Tortle',
		  Vedalken = 'Violetken|Vedalken',
		  Minotaur = 'Minotaur',
		  Centaur = 'Centaur',
		  Loxodon = 'Elephantine|Luxodon',
		  `Simic hybrid` = 'Animal Hybrid|Simic Hybrid',
		  Warforged = 'Warforged|Envoy|Juggernaut|Juggeenaut',
		  Changeling = 'Changeling',
		  Eladrin = 'Eladrin')

align = list(NG = c('ng',
					'"good"',
					'good',
					'neuteral good',
					'neitral good',
					'neutral good',
					'nuetral goodt',
					'neutral/good',
					'neutral-good',
					'nuetral good',
					'nutral good',
					'n good',
					'\U0001f937 neutral good',
					'neutral goodsskkd',
					'n/g'),
			 CG = c('chaotic good',
			 	   'caótico neutro',
			 	   'cg',
			 	   'chacotic good',
			 	   'good chaotic'),
			 LG = c('lawful good',
			 	   'l/g',
			 	   'l-g',
			 	   'lg',
			 	   'lawfull good',
			 	   'lawful goodness',
			 	   'lawfully good'),
			 NN = c('neutral',
			 	   'neutral neutral',
			 	   'netral',
			 	   'n',
			 	   'true neutral',
			 	   'tn'),
			 CN = c('chaotic neutral',
			 	   'chaotic',
			 	   'cn',
			 	   'chaotic nuetral',
			 	   'neutral chaotic'),
			 LN = c('lawful neutral',
			 	   'lawful',
			 	   'lawful/neutral',
			 	   'leal e neutro',
			 	   'lawful - neutral',
			 	   'ln',
			 	   'lawful neutral (good-ish)',
			 	   'legal good'),
			 NE = c('neutral evil','ne'),
			 LE = c('lawful evil',
			 	   'le'),
			 CE = c('ce',
			 	   'chaotic evil'))

goodEvil = list(`E` = c('NE','LE','CE'),
				`N` = c('LN','CN','NN'),
				`G` = c('NG','LG','CG'))

lawfulChaotic = list(`C` = c('CN','CG','CE'),
					 `N` = c('NG','NE','NN'),
					 `L` = c('LG','LE','LN'))

# lists any alignment text I'm not processing
charTable$alignment  %>% {.[!tolower(trimws(.)) %in% unlist(align)]} %>% table %>% sort %>% names %>% tolower %>% trimws

checkAlignment = function(x,legend){
	x = names(legend)[findInList(tolower(trimws(x)),legend)]
	if(length(x) == 0){
		return('')
	} else{
		return(x)
	}
}


charTable %<>% mutate(processedAlignment = alignment %>% purrr::map_chr(checkAlignment,align),
					  good = processedAlignment %>% purrr::map_chr(checkAlignment,goodEvil) %>%
					  	factor(levels = c('E','N','G')),
					  lawful = processedAlignment %>%
					  	purrr::map_chr(checkAlignment,lawfulChaotic) %>% factor(levels = c('C','N','L')))

charTable %<>% mutate(processedRace = race %>% sapply(function(x){
	out = races %>% sapply(function(y){
		grepl(pattern = y, x,perl = TRUE,ignore.case = TRUE)
	}) %>% which %>% names

	if(length(out) == 0 | length(out)>1){
		out = ''
	}

	return(out)
}))

#  lists any race text I'm not processing
charTable$processedRace[charTable$processedRace == ""] %>% names %>% table %>% sort

# process spells -----
spells = wizaRd::spells

spells = c(spells, list('.' = list(level = as.integer(99))))
class(spells) = 'list'

legitSpells =spells %>% names


processedSpells = charTable$spells %>% sapply(function(x){
	if(x==''){
		return('')
	}
	spellNames = x %>% str_split('\\|') %>% {.[[1]]} %>% str_split('\\*') %>% map_chr(1)
	spellLevels =  x %>% str_split('\\|') %>% {.[[1]]} %>% str_split('\\*') %>% map_chr(2)

	distanceMatrix = adist(tolower(spellNames), tolower(legitSpells),costs = list(ins=2, del=2, sub=3), counts = TRUE)

	rownames(distanceMatrix) = spellNames
	colnames(distanceMatrix) = legitSpells

	predictedSpell = distanceMatrix %>% apply(1,which.min) %>% {legitSpells[.]}
	distanceScores =  distanceMatrix %>% apply(1,min)
	predictedSpellLevel = spells[predictedSpell] %>% purrr::map_int('level')

	ins = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'ins'] %>% as.matrix  %>% diag
	del = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'del'] %>% as.matrix %>% diag
	sub = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'sub'] %>% as.matrix %>% diag
	isItIn = predictedSpell %>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
	{sapply(1:length(.),function(i){
		all(sapply(.[[i]],grepl,x =spellNames[i],ignore.case=TRUE))
	})}

	spellFrame = data.frame(spellNames,predictedSpell,spellLevels,predictedSpellLevel,distanceScores,ins,del,sub,isItIn,stringsAsFactors = FALSE)

	spellFrame %<>% filter(as.integer(spellLevels)==predictedSpellLevel &( isItIn | (sub < 5 & del < 5 & ins < 5)))

	paste0(spellFrame$predictedSpell,'*',spellFrame$predictedSpellLevel,collapse ='|')
})
charTable$processedSpells = processedSpells

# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[43]}
# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[70]}
# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$spells[i],charTable$processedSpells[i])}) %>% {.>20} %>% {charTable$spells[.]} %>% {.[88]}

# download.file('https://www.dropbox.com/s/4f7zdx09nkfa9as/Core.xml?dl=1',destfile = 'Core.xml')
# allRules = xmlParse('Core.xml') %>% xmlToList()
# fightClubItems = allRules[names(allRules) == 'item']
# saveRDS(fightClubItems,'fightClubItems.rds')

# fightClubItems =  readRDS('fightClubItems.rds')
# names(fightClubItems) = allRules %>% map('name') %>% as.character
#
# fightClubItems %>% map_chr('type') %>% {. %in% 'M'} %>% {fightClubItems[.]} %>% map_chr('name')
# fightClubItems %>% map_chr('type') %>% {. %in% 'R'} %>% {fightClubItems[.]} %>% map_chr('name')

legitWeapons = c(# fightClubItems %>% map_chr('type') %>% {. %in% 'M'} %>% {fightClubItems[.]} %>% map_chr('name'),
	# fightClubItems %>% map_chr('type') %>% {. %in% 'R'} %>% {fightClubItems[.]} %>% map_chr('name'),
	'Crossbow, Light', 'Dart', 'Shortbow', 'Sling',
	'Blowgun', 'Crossbow, hand', 'Crossbow, Heavy', 'Longbow', 'Net',
	'Club','Dagger','Greatclub','Handaxe','Javelin','Light hammer','Mace','Quarterstaff','Sickle','Spear','Unarmed Strike',
	'Battleaxe','Flail','Glaive','Greataxe','Greatsword','Halberd','Lance','Longsword','Maul','Morningstar','Pike','Rapier','Scimitar','Shortsword','Trident','War pick','Warhammer','Whip')

processedWeapons = charTable$weapons %>% sapply(function(x){
	if(x==''){
		return('')
	}
	weaponNames = x %>% str_split('\\|') %>% {.[[1]]}

	distanceMatrix = adist(tolower(weaponNames), tolower(legitWeapons),costs = list(ins=2, del=2, sub=3), counts = TRUE)

	rownames(distanceMatrix) = weaponNames
	colnames(distanceMatrix) = legitWeapons

	predictedWeapon = distanceMatrix %>% apply(1,which.min) %>% {legitWeapons[.]}
	distanceScores =  distanceMatrix %>% apply(1,min)

	ins = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'ins'] %>% as.matrix  %>% diag
	del = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'del'] %>% as.matrix %>% diag
	sub = attributes(distanceMatrix)$counts[,distanceMatrix %>% apply(1,which.min),'sub'] %>% as.matrix %>% diag
	isItIn = predictedWeapon %>% str_split(' |/') %>% map(function(x){
		x[!x %in% c('and','or','of','to','the')]
	}) %>%
	{sapply(1:length(.),function(i){
		all(sapply(.[[i]],grepl,x =weaponNames[i],ignore.case=TRUE))
	})}

	weaponFrame = data.frame(weaponNames,predictedWeapon,distanceScores,ins,del,sub,isItIn,stringsAsFactors = FALSE)

	weaponFrame %<>% filter(isItIn|  (sub < 2 & del < 2 & ins < 2))

	paste0(weaponFrame$predictedWeapon %>% unique,collapse ='|')
})

charTable$processedWeapons = processedWeapons

# x = 1:nrow(charTable) %>% sapply(function(i){adist(charTable$weapons[i],charTable$processedWeapons[i])}) %>% {.>20} %>% {charTable$weapons[.]} %>% {.[10]}




# user id ------
# userID = c()
# pb = txtProgressBar(min = 0, max = nrow(charTable), initial = 0)
#
# for(i in 1:nrow(charTable)){
#     setTxtProgressBar(pb,i)
#     for (id in unique(userID)){
#         userChars = charTable[which(userID == id),]
#         ip = charTable$ip[i] %>% {if(is.na(.) || . =='NULL' || .==''){return("NANA")}else{.}}
#         finger = charTable$finger[i] %>% {if(is.na(.) || . =='NULL' ||. == ''){return("NANA")}else{.}}
#         hash = charTable$hash[i] %>% {if(is.na(.) || . =='NULL' || . == ''){return("NANA")}else{.}}
#
#         ipInUser = ip %in% userChars$ip
#         fingerInUser = finger %in% userChars$finger
#         hashInUser = hash %in% userChars$hash
#         if(ipInUser | fingerInUser | hashInUser){
#
#             userID = c(userID,id)
#             break
#         }
#
#     }
#
#     if(length(userID)!=i){
#         userID = c(userID, max(c(userID,0))+1)
#     }
# }
#
# charTable$userID = userID
#
#
# userID = c()
# pb = txtProgressBar(min = 0, max = nrow(charTable), initial = 0)
#
# for(i in 1:nrow(charTable)){
#     setTxtProgressBar(pb,i)
#     for (id in unique(userID)){
#         userChars = charTable[which(userID == id),]
#         ip = charTable$ip[i] %>% {if(is.na(.) || . =='NULL' || .==''){return("NANA")}else{.}}
#         finger = charTable$finger[i] %>% {if(is.na(.) || . =='NULL' ||. == ''){return("NANA")}else{.}}
#         hash = charTable$hash[i] %>% {if(is.na(.) || . =='NULL' || . == ''){return("NANA")}else{.}}
#
#         ipInUser = ip %in% userChars$ip
#         fingerInUser = finger %in% userChars$finger
#         hashInUser = hash %in% userChars$hash
#         if(fingerInUser | hashInUser){
#
#             userID = c(userID,id)
#             break
#         }
#
#     }
#
#     if(length(userID)!=i){
#         userID = c(userID, max(c(userID,0))+1)
#     }
# }
#
# charTable$userIDNoIP = userID

# group levels at common feat acquisition points. sorry fighters and rogues

charTable %<>% mutate(levelGroup = cut(level,
									   breaks = c(0,3,7,11,15,18,20),
									   labels  = c('1-3','4-7','8-11','12-15','16-18','19-20')))


# remove personal info -----------

shortestDigest = function(vector){
	digested = vector(mode = 'character',length = length(vector))
	digested[vector!='']  = vector[vector!=''] %>% map_chr(digest,'sha1')
	uniqueDigested =  digested[digested!=''] %>% unique
	collusionLimit = 1:40 %>% sapply(function(i){
		substr(uniqueDigested,40-i,40)%>% unique %>% length
	}) %>% which.max %>% {.+1}
	digested %<>%  substr(40-collusionLimit,40)
	return(digested)
}


charTable$name %<>% shortestDigest
charTable$ip %<>% shortestDigest
charTable$finger %<>% shortestDigest
charTable$hash %<>% shortestDigest
# unsecureFields = c('ip','finger','hash')
# charTable = charTable[!names(charTable) %in% unsecureFields]

# add friendly names ensure old names remain the same
# the hashes will actually change but their order of introduction shouldn't
set.seed(1)
uniqueNames = charTable %>% arrange(date) %$% name %>% unique
randomAlias = random_names(length(uniqueNames))
names(randomAlias) = uniqueNames
charTable %<>% mutate(alias = randomAlias[name])


dnd_chars_all = charTable
write_tsv(dnd_chars_all,path = here('data-raw/dnd_chars_all.tsv'))


# get unique table ----------------
getUniqueTable = function(charTable){
	# remove obvious duplicates. same name and class assumed to be dups
	# race is not considered in case same person is experimenting with different
	# races
	charTable %<>% filter(name !='')

	uniqueTable = charTable %>% arrange(desc(level)) %>%
		filter(!duplicated(paste(name,justClass))) %>%
		filter(!level > 20)

	# detect non unique characters that multiclassed
	multiClassed = uniqueTable %>% filter(grepl('\\|',justClass))
	singleClassed = uniqueTable %>% filter(!grepl('\\|',justClass))

	multiClassDuplicates = multiClassed$name %>% duplicated %>% which

	# this is somewhat of a heuristic since it only looks at total level and classes chosen
	# but as both name and class combination is the same its probably some guy experimenting
	# with different character ideas.
	multiClassDuplicates %>% sapply(function(x){

		thedup = multiClassed[x,]
		matches = multiClassed[-x,] %>% filter(name == thedup$name)

		higherLevel = thedup$level < matches$level
		dupClass = strsplit(thedup$justClass,'\\|')[[1]]
		matchClass = strsplit(matches$justClass,'\\|')

		matchClass %>% sapply(function(y){
			all(dupClass %in% y)
		}) -> classMatches

		any(classMatches & higherLevel)

	}) -> isMultiClassDuplicate
	if(length(multiClassDuplicates[isMultiClassDuplicate])>0){
		multiClassed = multiClassed[-multiClassDuplicates[isMultiClassDuplicate],]
	}

	matchingNames = multiClassed$name[multiClassed$name %in% singleClassed$name] %>%
		unique

	singleCharDuplicates = which(singleClassed$name %in% matchingNames)

	singleCharDuplicates %>% sapply(function(x){
		char = singleClassed[x,]
		print(char[['name']])
        multiChar = multiClassed %>%
        	filter(name %in% char[['name']] & grepl(char[['justClass']],justClass))
        if(nrow(multiChar) == 0){
        	return (FALSE)
        }

        isHigher = any(multiChar$level > char[['level']])
        if (nrow(multiChar)>1){
        	warning("multiple matches")
        }
        return(isHigher)
	}) -> isDuplicate
	if(length(singleCharDuplicates[isDuplicate])>0){
		singleClassed = singleClassed[-singleCharDuplicates[isDuplicate],]
	}

	uniqueTable = rbind(singleClassed,multiClassed)

	return(list(uniqueTable = uniqueTable,
				singleClassed = singleClassed,
				multiClassed = multiClassed))
}


# dnd_chars_all = read_tsv(here("data-raw/dnd_chars_all.tsv"),na = 'NA') # redundant

usethis::use_data(dnd_chars_all,overwrite = TRUE)

list[dnd_chars_unique,dnd_chars_singleclass,dnd_chars_multiclass] = getUniqueTable(dnd_chars_all)

write_tsv(dnd_chars_unique,path = here('data-raw/dnd_chars_unique.tsv'))

usethis::use_data(dnd_chars_unique,overwrite = TRUE)
usethis::use_data(dnd_chars_singleclass,overwrite = TRUE)
usethis::use_data(dnd_chars_multiclass,overwrite = TRUE)

# format

glue("
	 #' @format A data frame with 30 variables:
	 #' \\describe")

# github updates ------
# library(git2r)
# repo = repository(here('.'))
# add(repo, 'data-raw/.')
# token = readLines('data-raw/auth')
# Sys.setenv(GITHUB_PAT = token)
# cred = git2r::cred_token()
#
# ogbox::setDate(format(Sys.Date(),'%Y-%m-%d'))
# version = ogbox::getVersion()
# version %<>% strsplit('\\.') %>% {.[[1]]}
#
# ogbox::setVersion(paste(version[1],version[2],
# 						format(Sys.Date(),'%y.%m.%d') %>%
# 							gsub(pattern = '\\.0','.',x=.),sep='.'))
#
# add(repo,'DESCRIPTION')
#
#
# git2r::commit(repo,message = paste('Weekly auto update'))
# git2r::push(repo,credentials = cred)


