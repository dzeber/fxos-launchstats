#######################################################################
###  
###  Set constants and load info to be used in formatting tables.
###  
#######################################################################


## HTML source containing tables. 
html.files <- setNames(sprintf("~/fxos/launchstats/html/%s.html", 
    c("lh", "lr")), c("history", "future"))

## Lookup table for country codes. 
load("/usr/local/share/countrycodes/countrycodes.RData")
cc <- countries[, list(code, name)]
setkey(cc, name)

## Lookup table for locale codes. 
locale.codes <- c("brazilian" = "pt-br",
                "spanish" = "es",               
                "german" = "de",
                "greek" = "el",        
                "hungarian" = "hu",         
                "italian" = "it",     
                "serbian" = "sr",                
                "polish" = "pl",
                "sp" = "es",
                "bengali" = "bn",
                "portuguese" = "pt",
                "bulgarian" = "bg",
                "chinese" = "zh",
                "japanese" = "ja",
                "swahili" = "sw",
                "english" = "en",
                "arabic" = "ar",
                "french" = "fr",
                "russian" = "ru",
                "vietnamese" = "vi",
                "burmese" = "my",
                "urdu" = "ur",
                "taiwanese" = "zh-tw")
    
    