

# Explore trends across cities with tree community and ownership/ landuse

# Dec 12, 2024    alexander.young@usda.gov

options(scipen=999)
library(xlsx)
library(tidyr)


# Read in .csv files. These are available on the urban datamart website

# Read in the stratification info; helps select plots by city evaluation
psc <- read.csv("FIADB_URBAN_ENTIRE_CSV/POP_STRATUM_CALC.csv")
psc$STRATUM_CN <- psc$CN
psca <- read.csv("FIADB_URBAN_ENTIRE_CSV/ID_PLOT_STRAT_CALC_ASSGN.csv")

# Tree related (ID_TREE vs ID_MOTHER_TREE). Use mother tree for now
mtre <- read.csv("FIADB_URBAN_ENTIRE_CSV/ID_MOTHER_TREE.csv")

# Condition related
plt <- read.csv("FIADB_URBAN_ENTIRE_CSV/ID_PLOT.csv")
cnd <- read.csv("FIADB_URBAN_ENTIRE_CSV/ID_COND.csv")
spcnd <- read.csv("FIADB_URBAN_ENTIRE_CSV/ID_SUBP_COND.csv")


# Ref files. Helps turns 0, 1, 2 codes into human readable values
rfl <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_FIA_LANDUSE.csv")
roc <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_OWNER_CLASS.csv")
rog <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_OWNER_GROUP.csv")
ril <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_ITREE_LANDUSE.csv")
rpc <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_PHYSIOGRAPHIC_CLASS.csv")
rcc <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_COVER_CLASS.csv")
rs <- read.csv("FIADB_URBAN_ENTIRE_CSV/REF_SPECIES.csv")
rts <-read.csv("FIADB_URBAN_ENTIRE_CSV/REF_TREE_STATUS.csv")
rny <-read.csv("FIADB_URBAN_ENTIRE_CSV/REF_NO_YES.csv")

# select evaluations
head(psc)

psc$eval_type <- substr(psc$EVALID, nchar(psc$EVALID)-3, nchar(psc$EVALID))
# take the curr evals, then report year 2022
# Should see 22 cities
eval_list <- unique(psc[psc$eval_type == "Curr" &
                        psc$REPORT_YEAR == 2022,"EVALID"])

# add in springfield which had its last reporting year in 2020
eval_list <- c(eval_list, "SpringfielMO2020Curr")                    


## Create 'out' dataframes to store each iteration of the for loop
out_city <- list()
out_Plot <- list()
out_Cond <- list()
out_Tree <- list()

# If you want to use a for loop, uncommment the next line
# for( i in 1:length(eval_list)){

# if you just want to look at one evalid, set the index value
i = 8 # for Baltimore2022Curr

# select a particular evalid.  Later a for loop can be used to add ~22 cities
sel_eval <- eval_list[i]

# From the psc table, select only the strata relevant for this evaluation
Stratum <- psc[psc$EVALID== sel_eval,]
dim(Stratum)  

# 3 evalids have more than 1 estimation unit. Just use the city estimation unit
if(sel_eval=="StLouisMO2022Curr"){
  Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of St. Louis, MO",]
}

if(sel_eval=="KansasCityMO2022Curr"){
  Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of Kansas City, MO",]
}

if(sel_eval=="SanAntonioTX2022Curr"){
  Stratum <- Stratum[Stratum$ESTN_UNIT_NAME=="City of San Antonio, TX",]
}


# Using the Stratum CN, identify plots in those strata using the psca table.
# PSCA is an assignment table for the psc table
Plot <- psca[psca$PSC_CN %in% Stratum$STRATUM_CN, ]
dim(Plot)

# select mother tree records from the set of plots in the eval
Tree <- mtre[mtre$PLT_CN %in% Plot$PLT_CN, ]
Tree$MTRE_CN <- Tree$CN  # rename CN for clarity
dim(Tree)

# Select condition records from the set of plots in the eval
Cond <- cnd[cnd$PLT_CN %in% Plot$PLT_CN,]
Cond$CND_CN <- Cond$CN # rename CN for clarity

# Remove the 'nonsampled' condition status 5
Cond <- Cond[Cond$COND_STATUS_CD!=5 , ]

# Select the subplot conditions in the eval
Subcond <- spcnd[spcnd$PLT_CN %in% Plot$PLT_CN,]

# only the subplots (i.e. 48ft radius plot.) 
#  Exclude microplots, which are SUBP 11, 12, 13,14
Subcond <- Subcond[Subcond$SUBP==1,]


# select columns to decrease # of columns to work with
Stratum <- Stratum[ , c("EVALID","REPORT_YEAR","ESTN_UNIT_ACRES","STRATUM_LABEL",
    "STRATUM_PLOT_COUNT","STRATUM_ACRES","EXPNS","STRATUM_WEIGHT",
    "STRATUM_SUBPLOT_ADJ_FACTOR","STRATUM_MICROPLOT_ADJ_FACTOR", "STRATUM_CN")]


# Match in the subp conditions by CND_CN.
Cond$CONDPROP_UNADJ  <- Subcond$CONDPROP_UNADJ [match(Cond$CND_CN , Subcond$CND_CN)]
Cond$PCT_TREE_COVER  <- Subcond$PCT_TREE_COVER [match(Cond$CND_CN , Subcond$CND_CN)]
Cond$PCT_SHRUB_SEED_COVER  <- Subcond$PCT_SHRUB_SEED_COVER [match(Cond$CND_CN , Subcond$CND_CN)]
Cond$GR_COV_PCT_BLDG  <- Subcond$GR_COV_PCT_BLDG [match(Cond$CND_CN , Subcond$CND_CN)]
Cond$GR_COV_PCT_IMPERVIOUS <- Subcond$GR_COV_PCT_IMPERVIOUS[match(Cond$CND_CN , Subcond$CND_CN)]
Cond$GR_COV_PCT_PERMEABLE <- Subcond$GR_COV_PCT_PERMEABLE[match(Cond$CND_CN , Subcond$CND_CN)]
Cond$GR_COV_PCT_HERBACEOUS <- Subcond$GR_COV_PCT_HERBACEOUS[match(Cond$CND_CN , Subcond$CND_CN)]
Cond$GR_COV_PCT_WATER <- Subcond$GR_COV_PCT_WATER[match(Cond$CND_CN , Subcond$CND_CN)]


### Add columns to the identified rows of data
Plot$PLOTID <- plt$PLOTID[match(Plot$PLT_CN, plt$CN)]
Plot$STATECD <- plt$STATECD[match(Plot$PLT_CN, plt$CN)]
Plot$COUNTY_CD <- plt$COUNTYCD[match(Plot$PLT_CN, plt$CN)]
Plot$MEAS_MONTH<- plt$MEAS_MONTH[match(Plot$PLT_CN, plt$CN)]
Plot$MEAS_YEAR <- plt$MEAS_YEAR[match(Plot$PLT_CN, plt$CN)]
Plot$PLOT_STATUS_CD<- plt$PLOT_STATUS_CD[match(Plot$PLT_CN, plt$CN)]
Plot$SAMPLE_METHOD_CD<- plt$SAMPLE_METHOD_CD[match(Plot$PLT_CN, plt$CN)]
Plot$PLOT_NONSAMPLE_REASN_CD<- plt$PLOT_NONSAMPLE_REASN_CD[match(Plot$PLT_CN, plt$CN)]
Plot$STRATUM_CN <- Plot$PSC_CN


### add stratification into to Trees
# Tree$STRATUM_LABEL <- Plot$STRATUM_LABEL[match(Tree$PLT_CN, Plot$PLT_CN)]
# Tree$STRATUM_CN <- Plot$PSC_CN[match(Tree$PLT_CN, Plot$PLT_CN)]
# Tree$EXPNS <- Stratum$EXPNS[match(Tree$STRATUM_CN, Stratum$STRATUM_CN)]
# Tree$STRATUM_SUBPLOT_ADJ_FACTOR <- Stratum$STRATUM_SUBPLOT_ADJ_FACTOR[match(Tree$STRATUM_CN, Stratum$STRATUM_CN)]
# Tree$STRATUM_MICROPLOT_ADJ_FACTOR <- Stratum$STRATUM_MICROPLOT_ADJ_FACTOR[match(Tree$STRATUM_CN, Stratum$STRATUM_CN)]



### Rename Cond data to human readable using ref table objects
Cond$OWNCD <- roc$ABBR[match( Cond$OWNCD, roc$VALUE)]
Cond$OWNGRPCD <- rog$ABBR[match( Cond$OWNGRPCD, rog$VALUE)]
Cond$FIA_LANDUSE <- rfl$ABBR[match( Cond$FIA_LANDUSE, rfl$VALUE)]
Cond$ITREE_LANDUSE <- ril$ABBR[match( Cond$ITREE_LANDUSE, ril$VALUE)]

## Looks like there are still NA values for ownership
# NA values for ownership with Water land use.
Cond[is.na(Cond$OWNGRPCD), "OWNGRPCD"] <- "Unknown water"


# Rename Tree data to human readable values
Tree$scientific <- rs$SCIENTIFIC_NAME[match(Tree$SPCD, rs$SPCD)]
Tree$STATUSCD <- rts$ABBR[match(Tree$STATUSCD, rts$VALUE)]
Tree$STANDING_DEAD_CD <- rny$ABBR[match(Tree$STANDING_DEAD_CD, rny$VALUE)]


# Match condition data into trees based on CND_CN
Tree$OWNCD <- Cond$OWNCD[match(Tree$CND_CN, Cond$CND_CN)]
Tree$OWNGRPCD <- Cond$OWNGRPCD[match(Tree$CND_CN, Cond$CND_CN)]
Tree$FIA_LANDUSE <- Cond$FIA_LANDUSE[match(Tree$CND_CN, Cond$CND_CN)]
Tree$ITREE_LANDUSE <- Cond$ITREE_LANDUSE[match(Tree$CND_CN, Cond$CND_CN)]


#############################################################

# Work towards getting city-level summary data 
### i.e. how many acres of each ownership or landuse are present

# Bring stratum identity into the Cond table
Cond$STRATUM_CN <- Plot$PSC_CN[match(Cond$PLT_CN, Plot$PLT_CN)]

# match in the condition expansion info
Cond$EXPNS <- Stratum$EXPNS[match(Cond$STRATUM_CN, Stratum$STRATUM_CN)]
Cond$STRATUM_SUBPLOT_ADJ_FACTOR <- Stratum$STRATUM_SUBPLOT_ADJ_FACTOR[match(Cond$STRATUM_CN, Stratum$STRATUM_CN)]

# POP_acres is how much each individual condition is 'worth' at the population level
Cond$POP_acres <- Cond$CONDPROP_UNADJ * Cond$EXPNS * Cond$STRATUM_SUBPLOT_ADJ_FACTOR

# Next steps sum the condition POP_acres by landuse, or ownership
lu_area <- aggregate(Cond$POP_acres,
          by=list(FIA_LANDUSE = Cond$FIA_LANDUSE),
          FUN="sum", na.rm=T)
lu_area$eval <- sel_eval

rfl$acres <- lu_area$x[match(rfl$ABBR, lu_area$FIA_LANDUSE)]
lu_df <- spread(rfl[ , c("ABBR","acres")], "ABBR", "acres")

own_area <- aggregate(Cond$POP_acres,
                     by=list(OWNGRPCD = Cond$OWNGRPCD),
                     FUN="sum", na.rm=T)
own_area$eval <- sel_eval


rog$acres <- own_area$x[match(rog$ABBR, own_area$OWNGRPCD)]
own_df <- spread(rog[ , c("ABBR","acres")], "ABBR","acres")

# create a city dataframe with some prelim info
city_df <- data.frame(
    EVALID = sel_eval,
    city =  paste0(gsub('.{8}$', '', sel_eval)) ,
    plot_count_Curr = sum(Stratum$STRATUM_PLOT_COUNT),
    city_acres = unique(Stratum$ESTN_UNIT_ACRES)  )

# bind together the single row of city-data with ownershp and landuse summary
city_summary <- cbind(city_df, 
      own_df, lu_df)


####################################

#  Last step- get ready for export to excel file
#### data inspection- there are NA values for trees with 'No Status'.
Tree[is.na(Tree$OWNCD),]
Tree[is.na(Tree$FIA_LANDUSE),]
# Remove these records
Tree <- Tree[!is.na(Tree$OWNGRPCD),]
Tree <- Tree[!is.na(Tree$FIA_LANDUSE),]


### Keep track of which EVALID is being written
Tree$EVALID <- sel_eval
Cond$EVALID <- sel_eval
Plot$EVALID <- sel_eval

# select which columns to export for the Tree object
Tree<- Tree[, c("EVALID","PLOTID","STATECD","CONDID","SUBP","STATUSCD","STANDING_DEAD_CD",
                "SPCD","scientific","NBR_STEMS","DIA","TPA_UNADJ","OWNGRPCD","FIA_LANDUSE","ITREE_LANDUSE","LEAF_AREA_ITREE","CROWN_DIEBACK_CD",
                "IS_MAINTAINED_AREA","IS_STREET_TREE","IS_PLANTED",
                #"CROWN_LIGHT_EXPOSURE","CROWN_GROUND_AREA_ITREE",
                 "PLT_CN","MTRE_CN","CND_CN"
                )]

# Select which columns of the Cond object to export
names(Cond)
Cond <- Cond[ , c("EVALID","PLOTID","STATECD","UNITCD","COUNTYCD","CONDID",
                  "COND_STATUS_CD","CONDPROP_UNADJ","POP_acres", "FIA_LANDUSE", "ITREE_LANDUSE",
                   "OWNGRPCD","OWNCD",
                   "PCT_TREE_COVER", "PCT_SHRUB_SEED_COVER",
                  "GR_COV_PCT_BLDG", "GR_COV_PCT_IMPERVIOUS", "GR_COV_PCT_PERMEABLE",
                  "GR_COV_PCT_HERBACEOUS", "CND_CN","PLT_CN")]

# select the columns for the Plot object export
names(Plot)
Plot <- Plot[ , c("EVALID","STATECD","COUNTY_CD","MEAS_MONTH","MEAS_YEAR","PLOT_STATUS_CD","SAMPLE_METHOD_CD","PLOT_NONSAMPLE_REASN_CD","PLT_CN","STRATUM_CN")]

# Make sure CN values are characters
#Stratum$STRATUM_CN <- as.character(Stratum$STRATUM_CN)
#Tree$STRATUM_CN <- as.character(Tree$STRATUM_CN)


Plot$PLT_CN <- as.character(Plot$PLT_CN)
Plot$STRATUM_CN <- as.character(Plot$STRATUM_CN)

Cond$PLT_CN <- as.character(Cond$PLT_CN)
Cond$CND_CN <- as.character(Cond$CND_CN)

# Tree$CND_CN <- as.character(Tree$CND_CN)
# Tree$MTRE_CN <- as.character(Tree$MTRE_CN)
# Tree$PLT_CN <- as.character(Tree$PLT_CN)



######  End processing of city, store results

out_city <- rbind(out_city, city_summary)
out_Plot <- rbind(out_Plot, Plot)
out_Cond <- rbind(out_Cond, Cond)
out_Tree <- rbind(out_Tree, Tree)

#  }    # uncomment this if using a for loop

out_city


### Write the .xlsx file locally

# Specify file name and path
fp <- file.path(here::here(),"cities_2022_ownership_landuse.xlsx")


# write excel sheet.  Uncomment Stratum, Plot, Cond if you'd like  

write.xlsx2(out_city, file= fp , sheetName="Cities", row.names = F)

write.xlsx2(out_Plot, file= fp , sheetName="Plot", row.names = F, append = T)
gc()
write.xlsx2(out_Cond, file= fp , sheetName="Cond", row.names = F, append = T)


write.csv(out_Tree, file=file.path(here::here(),"cities_2022_trees.xlsx"))





## prelim visuals for ownership   (inteded for use with for loop, all 20+ cities)



library(ggplot2)
library(ggforce)
library(patchwork)

out_city$prop_Ag <- out_city$Agriculture / out_city$city_acres
out_city$prop_commercial <- out_city$`Commercial/Industrial` / out_city$city_acres
out_city$prop_forest <- out_city$`Forest land` / out_city$city_acres
out_city$prop_multi_fam <- out_city$`Multi-family` / out_city$city_acres
out_city$prop_other <- out_city$OtherNonforest / out_city$city_acres
out_city$prop_chaparral <- out_city$`Rangeland/Chaparral` / out_city$city_acres
out_city$prop_recreation <- out_city$`Recreation/Cemetery` / out_city$city_acres
out_city$prop_residential <- out_city$Residential / out_city$city_acres
out_city$prop_rights_of_way <- out_city$`Rights-of-Way` / out_city$city_acres
out_city$prop_water <- out_city$Water / out_city$city_acres


# ownership acres
names(out_city)
 owner <- gather(out_city, "owner","acres", 5:8)

 
 
g1 <- ggplot(owner, aes(x=reorder(city, -city_acres), y=acres, fill=owner))+
  geom_col(position="stack", col="black")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="UFIA cities ordered by dreasing size", y="Number of acres", fill="Owner group")
g1


names(out_city)
clu <- gather(out_city, "landuse","prop", 20:29)
head(clu)

# see which landuse are most common
order_lu <- aggregate(clu$prop, by=list(landuse=clu$landuse), FUN="mean", na.rm=T)
order_lu <- order_lu[order(order_lu$x, decreasing=T),]

# Order land uses by decreasing
table(clu$landuse)

clu$landuse <- factor(clu$landuse, levels= unique(order_lu$landuse))


g2 <- ggplot(clu, aes(x=reorder(city, -city_acres), y=prop, fill=landuse))+
  geom_col(position="stack", col="black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="UFIA cities ordered by dreasing size", y="Proportion of city area")
g2

ggplot(clu, aes(x=reorder(city, -city_acres), y=prop, fill=landuse))+
  geom_col(position="stack", col="black")+
 # facet_wrap(~landuse)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="UFIA cities ordered by dreasing size", y="Proportion of the city area",fill="FIA land use")


### Above looked at proportion

## below is showing the acres of each landuse
clu <- gather(out_city, "landuse","acres", 10:19)
head(clu)

# see which landuse are most common
order_lu <- aggregate(clu$acres, by=list(landuse=clu$landuse), FUN="mean", na.rm=T)
order_lu <- order_lu[order(order_lu$x, decreasing=F),]

# Order land uses by decreasing
table(clu$landuse)

clu$landuse <- factor(clu$landuse, levels= unique(order_lu$landuse))

ggplot(clu, aes(x=reorder(city, -city_acres), y=acres, fill=landuse))+
  geom_col(position="stack", col="black")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(x="UFIA cities ordered by dreasing size")




# tree level
p1 <- ggplot(Tree, aes(x=FIA_LANDUSE, y=DIA))+
  geom_violin()+
  geom_sina()+
  facet_wrap(~FIA_LANDUSE, nrow=1, scales="free_x")+
  theme_bw()+theme(panel.grid = element_blank())+
  labs(x="Ownership group", y="Tree diameter (inches)", col="")

p2 <- ggplot(Tree, aes(x=OWNGRPCD, y=DIA))+
  geom_violin()+
  geom_sina(drop = FALSE)+
  facet_wrap(~OWNGRPCD, nrow=1, scales="free_x")+
  theme_bw()+theme(panel.grid = element_blank())+
  labs(x="FIA landuse", y="Tree diameter (inches)", col="")

p3 <- ggplot(Tree, aes(x=OWNGRPCD, y=DIA, col=FIA_LANDUSE))+
#  geom_violin()+
  geom_sina(drop = FALSE)+
  facet_wrap(~OWNGRPCD, nrow=1, scales="free_x")+
  theme_bw()+theme(panel.grid = element_blank())+
  labs(x="FIA landuse", y="Tree diameter (inches)", col="")
p3

