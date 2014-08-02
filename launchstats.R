#######################################################################
###  
###  Parse the launch stats tables from Mana and convert to CSV.
###  
#######################################################################

## Load info. 
source("launchstats-vars.R")

## Change to output directory.
setwd("~/fxos/launchstats")

library(XML)
library(data.table)

## HTML parsing. 
load.html <- function(filename) {
    con <- file(filename, encoding = "UTF-8")
    h <- readLines(con, encoding = "UTF-8")
    close(con)
    
    ## Try removing weird characters related to encoding issues. 
    h <- gsub(rawToChar(charToRaw("\xc2\xa0")), "", h)
    
    ## Trim whitespace and remove spaces from empty elements. 
    h <- paste0(h[!grepl("^\\s*$", h)], collapse = "")
    h <- gsub(">\\s+<", "><", h)

    htmlTreeParse(h, asText = TRUE, useInternalNodes = TRUE)
}

###############################

## Parse history table. 

lh <- load.html(html.files[["history"]])

## Extract table elements and convert to data tables. 
lh <- readHTMLTable(lh, header = TRUE, as.data.frame = FALSE)
lh <- as.data.table(lh[[1]])


## Parse future table. 

lr <- load.html(html.files[["future"]])

lr <- readHTMLTable(lr, header = TRUE, as.data.frame = FALSE, 
    elFun = function(node) {
        ## If cell contents is img element, get attribute. 
        ## Otherwise get text conent.
        img <- getNodeSet(node, ".//img")
        if(length(img) > 1) warning("More than one img element")
        if(length(img) > 0) 
            xmlGetAttr(img[[1]], "data-emoticon-name")
        else
            xmlValue(node)
    })

## Multiple tables on this page. 
names(lr) <- NULL
lr <- lapply(lr, as.data.table)    

## Remove final table - cancelled launches.
lr <- lr[-length(lr)]

## For tables headed by "Qx.201y", add this as a date. 
## Otherwise use NA date.
for(dd in lr) {
    d <- names(dd)[[1]]
    dd[, date := if(isTRUE(grepl("Q[1-4]\\.201[3-9]", d))) {
        sub(".", " ", d, fixed = TRUE)
    } else NA]
    setnames(dd, 1, "Stage")
}   

## Combine into single table.
lr <- rbindlist(lr)

## Save these tables. 
## No formatting has been applied yet.
lh.raw <- lh
lr.raw <- lr
save(lh.raw, lr.raw, file = "RData/launchstatus-raw.RData")


###############################

## Sanitize and format tables. 

## Historical. 

## Remove separator rows with text ("QX 201X")
lh <- lh[!grepl("Q[1-4] 201[3-9]", get(names(lh)[1]))]

## Rearrange and rename. 
lh <- lh[, list(operator = `Carrier/Partner`, 
            op.brand = `Local Brand`,
            country = Country,
            locale = L10N,
            oem = OEM,
            device = Model,
            osver = Version,
            date = Date,
            search.id = `Search ID`)]

## Missing values. 
lh <- lh[, lapply(lh, function(r) { 
    r[nchar(r) == 0 | r %in% c("N/A", "TBD")] <- NA; r 
})]

## Convert to country codes. 
## Remove parentheses.
lh[, country := sub("\\s+\\(.+\\).*$", "", country)]
setkey(lh, country)
lh = cc[lh][, name := NULL]
setnames(lh, "code", "country")

## Convert to locale codes.
lh[, locale := tolower(locale)]
lh[grepl("brazilian", locale, fixed = TRUE), locale := "brazilian"]
lh[, locale := locale.codes[locale]]

## Versions - no "v".
lh[, osver := sub("v", "", osver, fixed = TRUE)]

## Dates - if more than one, keep earliest. 
## Added case to handle double slashes (typo). 
lh[, date := {
    dm = gregexpr("\\d{1,2}/\\d{1,2}/{1,2}\\d{4}", date)
    d = unlist(lapply(seq_along(dm), function(i) {
        p = as.vector(dm[[i]])
        ml = attr(dm[[i]], "match.length")
        mm = unlist(lapply(seq_along(p), function(j) {
            substr(date[[i]], p[j], p[j]+ml[j]-1)
        }))
        min(mm)
    }))
    gsub("//+", "/", d) }]
lh[, date := as.Date(date, "%m/%d/%Y")]    

## Separate entries with multiple device/oems.
## Handle search IDs as well (not comma-separated at the moment). 
odn <- c("oem", "device", "search.id")
byn <- names(lh)
byn <- byn[!(byn %in% odn)]

lh <- lh[, setNames(lapply(odn, function(n) {
        v <- get(n)
        if(all(is.na(v))) return(NA_character_)
        ## Search ID is not comma-separated. 
        ## Split by length instead (ID string is fixed pattern). 
        if(identical(n, "search.id")) {
            s <- strsplit(v, ".", fixed = TRUE)[[1]]
            unlist(lapply(2:length(s), function(i) {
                s1 <- if(i == 2) s[1] else substr(s[i-1], 2, nchar(s[i-1]))
                paste(s1, substr(s[i], 1, 1), sep = ".")
            }))
        } else {
            sub("^\\s+", "", strsplit(v, ",")[[1]])
        }
    }), odn),
    by = setNames(lapply(byn, function(n) { get(n) }), byn)]


## Remove brackets from device name.
lh[, device := sub("\\s+\\(.+\\).*$", "", device)]

## Remove operator brands from operator name.
lh[, operator := sub("\\s*\\(.+\\)", "", operator)]

## Sort by date.
lh <- lh[order(date, country, operator, oem, device)]

#####

## Future launches. 

## Remove OEM partners for now. 
lr <- lr[tolower(`Partner Type`) != "oem"]

## Subset and rename.
lr <- lr[, list(stage = Stage, 
            type = `Partner Type`,
            country = Country,
            partner = `Partner Name`,
            partner.brand = `Partner Name`, 
            locale = L10N,
            oem = OEM, 
            device = Model,
            osver = Version,
            date)]
            
## Remove empty rows (using Carrier). 
# lr = lr[nchar(operator) > 0]

## Missing values. 
lr <- lr[, lapply(lr, function(r) { 
    r[nchar(r) == 0] <- NA
    r[tolower(r) %in% c("n/a", "-", "information", "?", "tbd", "tbc")] <- NA
    r 
})]

## Remove uninformative rows.
# lr = lr[!(is.na(operator) & (is.na(oem) | is.na(device)))]

## Convert to country codes. 
## Remove "(MR)". 
setkey(lr, country)
lr <- cc[lr][, name := NULL]
setnames(lr, "code", "country")

## Convert to locale codes.
lr[, locale := tolower(locale)]
## If code is in parentheses, keep.
lr[, locale := sub("^.*\\(([a-z]{2}(-[a-z]{2})?)\\).*$", "\\1", locale)]
## Otherwise remove parentheses.
lr[, locale := sub("\\s*\\(.+\\).*$", "", locale)]
## Make some specific replacements. 
lr[grepl("brazilian", locale), locale := "brazilian"]
lr[grepl("simp.+chinese", locale), locale := "chinese"]
lr[grepl("trad.+chinese", locale), locale := "taiwanese"]
## Remove multiples, for now.
lr[grepl(",", locale, fixed = TRUE), locale := NA]
lr[!is.na(locale.codes[locale]), locale := locale.codes[locale]]

## Figure out operator brands. 
## Replace with names in parentheses, if any.
opp <- lr[, grepl("\\(.+\\)", partner)]
lr[opp, partner := sub("\\s*\\(.+\\)", "", partner)]
lr[opp, partner.brand := sub("^.*\\((.+)\\).*$", "\\1", partner.brand)]

## Sanitize oem/device names. 
# lr[, oem := sub("\\s*\\(.+\\)", "", oem)]
lr[, oem := sub("\\s+TBC", "", oem)]
lr[, device := sub("\\s+TBC", "", device)]
## Get rid of weird encoding artifact. 
# lr[, device := sub('.+(\\d\\.\\d")', "\\1", device)]

## Order by date.
lr <- lr[order(sub("^(.+) (.+)$", "\\2\\1", date), country, partner, 
    oem, device)]

###############################

## Save cleaned tables.

setcolorder(lh, c("country", "operator", "op.brand",
                "oem", "device", "osver", "locale", "date", "search.id"))

setcolorder(lr, c("country", "partner", "partner.brand",
                "oem", "device", "osver", "locale", "stage", "type", "date"))
                
## Add column about FTU ping. 
## hasFTU logic: version >= 1.3 and oem != 'ZTE'

hasFTU <- function(v, m) { 
    res <- sub("^[^.]*(\\d\\.\\d).*$", "\\1", v) >= "1.3" & m != "ZTE"
    res[!is.na(res)] <- c("TRUE" = "yes", "FALSE" = "no")[as.character(res[!is.na(res)])]
    res
}

lh[, has.ftu := hasFTU(osver, oem)]
lr[, has.ftu := hasFTU(osver, oem)]

save(lh, lr, file = "RData/launchstatus-clean.RData")
# load("launchstatus-clean.RData")

## Create spreadsheets.

## Add country names. 
lh[, country := country.name(country, NA)]
lr[, country := country.name(country, NA)]

setnames(lh, c("Country", "Operator", "Operator brand", "OEM",
    "Device model", "OS version", "Locale", "Launch date", 
    "Search ID", "Has FTU"))

setnames(lr, c("Country", "Partner", "Partner brand", "OEM",
    "Device model", "OS version", "Locale", "Planning stage", 
    "Partner Type", "Timing", "Has FTU"))
    

# lh[, `Has FTU` := hasFTU(`OS version`, `OEM`)]
# lh[, `Has FTU` := c("TRUE" = "yes", "FALSE" = "no")[as.character(`Has FTU`)]]    

# lr[, `Has FTU` := hasFTU(`OS version`, `OEM`)]
# lr[, `Has FTU` := c("TRUE" = "yes", "FALSE" = "no")[as.character(`Has FTU`)]]

    
write.csv(lh, file = "csv/history.csv", row.names = FALSE, na = "")
write.csv(lr, file = "csv/future.csv", row.names = FALSE, na = "")


