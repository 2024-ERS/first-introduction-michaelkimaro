# multivariate analysis of vegetation composition in relation to environmental factors

# clear everything in memory (of R)
remove(list=ls())

library(vegan) # multivariate analysis of ecological community data 
library(psych) # usefull for panel plots of multivariate datasets
library(tidyverse)

# read the vegetation data
vegdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSJFU7gDlXuBM6hgWwcYJ-c_ofNdeqBe50_lDCEWO5Du3K7kPUDRh_esyKuHpoF_GbmBAoT4ZygrGWq/pub?gid=2036214007&single=true&output=csv") |>
  filter(year==2023 & TransectPoint_ID<=1150) |>     # filter for only the only 2023 data and plots with vegetation
  dplyr::select(-c(year,bare,litter,mosses,SalicSpp)) |>  # select  only distance_m and the species names as variable to use
  tibble::column_to_rownames(var="TransectPoint_ID") |># convert distance_m to the row names of the tibble
  dplyr::select(which(colSums(vegdat) != 0)) # remove species that did not occur in this year in any plot


# read the macrotransect elevation data
elevdat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=1550309563&single=true&output=csv") %>%
  dplyr::filter(year==2023 & !is.na(TransectPoint_ID) & TransectPoint_ID<=1150) %>%
  dplyr::select(TransectPoint_ID,elevation_m)   # select  only distance_m and elevation 
elevdat

# read the macrotransect clay thickness from the soil profile dataset
claydat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=943188085&single=true&output=csv") |>
  dplyr::filter(Year==2024 & SoilType_ID %in% c("clay","clay-organic") & TransectPoint_ID<=900) |>
  dplyr::select(TransectPoint_ID,corrected_depth) |>     
  group_by(TransectPoint_ID) |> 
  dplyr::summarize(clay_cm=mean(corrected_depth,na.rm=T)) #calculate average clay layer thickness  for each pole
claydat


##### read the flooding proportion (proportion of the time of  the growing season flooded)
# from 1 april - 30 aug
flooddat<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTiJsz0xcUAhOjlTcHpT7NXhSysGMdzI6NfYm_kzpFtV99Eyea2up7dB-5a_P-jAKiwMBAXYURBTpRU/pub?gid=600890443&single=true&output=csv") |> 
  dplyr::filter(year==2023) |>
  dplyr::select(-year,-flood_ID)
flooddat

# read distance to gulley of each plot
gulleydist<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT4C7olgh28MHskOjCIQGYlY8v5z1sxza9XaCccwITnjoafF_1Ntfyl1g7ngQt4slnQlseWT6Fk3naB/pub?gid=2102201825&single=true&output=csv") |>
  dplyr::filter(Year==2023,TransectPoint_ID<=1150) |>
  dplyr::select(-Year,-DistanceToGullley_ID)
gulleydist
  
# also add redox
redox<-readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyEg6KzIt6SdtSKLKbbL3AtPbVffq-Du-3RY9Xq0T9TwPRFcgvKAYKQx89CKWhpTKczPG9hKVGUfTw/pub?gid=1911552509&single=true&output=csv") |>
  dplyr::filter(Year==2024,TransectPoint_ID<=900) %>%
  dplyr::group_by(TransectPoint_ID,ProbeDepth) %>%
  dplyr::summarize(redox_mV=mean(redox_raw2_mV,na.rm=T)) %>%
  tidyr::pivot_wider(id_cols=TransectPoint_ID,
                     names_from=ProbeDepth,
                     names_prefix = "redox",
                     values_from = redox_mV)
redox

##### merge elevation, clay thickness, flooding proportion and distance to gulley data in
# a sequential join operation in a pipe
# start from elevdat and use left_join, calling the tibble envdat
# set distance_m as the row names of the  tibble
# set the NAs for floodprop equal to zero
# . (dot) means " outcome of the pipe so far
envdat <-
  dplyr::left_join(elevdat,claydat,by="TransectPoint_ID") |>
  dplyr::left_join(flooddat,by="elevation_m") |>
  dplyr::left_join(gulleydist,by="TransectPoint_ID") |>
  dplyr::left_join(redox,by="TransectPoint_ID") |>
  dplyr::mutate(floodprob=ifelse(is.na(floodprob),0,floodprob),
                floodprob=round(floodprob,4),  # round the floodprob to 4 decimals
                clay_cm=ifelse(is.na(clay_cm),0,clay_cm)) |>
  tibble::column_to_rownames(var="TransectPoint_ID")
envdat
vegdat

# for ordination with functions from the vegan library you need an separate environmental factors dataset
# and a species composition dataset with the same rownames, indicating  the same sites / samples
# the species data (community composition) need to be in wide format
vegdat
envdat

##### explore the correlations among the environmental factors in a panel pairs plot
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="pearson")
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="spearman")
# note that the units are very different! 

##### Ordination: run a Principal Component Analysis (PCA) on the environmental variables
# .scale=T means: use correlations instead of covariances
# use .scale=T for datasets where the variables are measured in different use

# remove points with Na values
envdat1<-envdat |> 
  dplyr::mutate(TransectPoint_ID=row.names(envdat)) |>
  dplyr::filter(!TransectPoint_ID %in% c("1000", "1050", "1100","1150")) |>
  dplyr::select(-TransectPoint_ID)

pca<-prcomp(envdat1,center=T,scale=T)
pca
summary(pca) # show the results of the PCA

pca$sdev # show the weight of the axis 
pca$sdev[1]  # first axis
# show the site scores
pca$x

# the PCs are reduced dimensions of the dataset
# you reduce 6 variables to 2 dimensions
# make a biplot (variable scores plus sample score) the pca ordination
biplot(pca,xlim=c(-0.5,0.5))



##### ordination: calculate and plot a Non-metric Multidimensional Scaling (NMDS) ordination
# explore the distance (dissimilarity) in species composition between plots
d1<-vegan::vegdist(vegdat,method="bray") # use Bray-Curtis dissimilarity (non-parametric)
d2<-vegan::vegdist(vegdat,method="euclidean") # use euclidation dissimilarity (non-parametric)
d1
d2

##### improve the NMDS ordination plot by only showing the dominant species
# non-metric multidimension scaling / indirect gradient analysis (only species composition)
nmds<-vegan::metaMDS(vegdat,k=2,trace=F,trymax = 1000,distance="bray")
nmds
vegan::ordiplot(nmds,type='t')

# and show the ordination with the most abundance species with priority
SpecTotCov<-colSums(vegdat)
SpecTotCov   # calculate the total abundance of species (common vs rare)
vegan::ordiplot(nmds,display="sites",cex=1,type="t")
vegan::orditorp(nmds,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=1.1)


#### ordination: compare to a DCA -> decide what ordination we should do, linear or unimodal? 
# how long are the gradients? Should I use linear (PCA)or unimodal method (NMDS, DCA)
dca<-vegan::decorana(vegdat)
dca
# first axis is 8.1 standard deviations of species responses
# result: length of first ordination axis is >8 standard deviations
# only when <1.5 you can use a PCA or RDA
# plot the dca results as a biplot
vegan::ordiplot(dca,display="sites",cex=1,type="t",xlim=c(-5,5))
vegan::orditorp(dca,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=1.1,xlim=c(-5,5))
##### fit the environmental factors to the dca ordination surface
ef_dca<-vegan::envfit(dca~clay_cm+elevation_m+floodprob+DistGulley_m+redox5+redox10,
                      data=envdat,na.rm=T)
#add the result to the ordination plot as vectors for each variable
plot(ef_dca,add=T)
##### add contour surfaces to the dca ordination for the relevant abiotic variables
vegan::ordisurf(dca,envdat$elevation_m,add=T,col="brown")
vegan::ordisurf(dca,envdat$clay_cm,add=T,col="green")
vegan::ordisurf(dca,envdat$floodprob,add=T,col="red")
vegan::ordisurf(dca,vegdat$PlantMar,add=T,col="orange")

##### make the same plot but using a nmds
##### fit the environmental factors to the nmds ordination surface
vegan::ordiplot(nmds,display="sites",cex=1,type="t",xlim=c(-4,4))
vegan::orditorp(nmds,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=1.1,xlim=c(-4,4))
##### fit the environmental factors to the dca ordination surface
ef_nmds<-vegan::envfit(nmds~clay_cm+elevation_m+floodprob+DistGulley_m+redox5+redox10,
                      data=envdat,na.rm=T)
#add the result to the ordination plot as vectors for each variable
plot(ef_nmds,add=T)
##### add contour surfaces to the nmds ordination for the relevant abiotic variables
vegan::ordisurf(nmds,envdat$elevation_m,add=T,col="brown")
vegan::ordisurf(nmds,envdat$clay_cm,add=T,col="green")
vegan::ordisurf(nmds,envdat$floodprob,add=T,col="red")
vegan::ordisurf(nmds,vegdat$PlantMar,add=T,col="orange")



##### compare an unconstrainted (DCA) and constrained (CCA) ordination
# did you miss important environmental factors?
# show the results of the detrended correspondence analysis
dca
# the eigenvalues represent the variation explained by each axis
cca<-vegan::cca(vegdat~elevation_m+DistGulley_m+floodprob+redox5+redox10+clay_cm,
                data=envdat)
summary(cca)

cca_final<-vegan::cca(vegdat~elevation_m+DistGulley_m+floodprob,data=envdat)
cca$colsum #species scores
cca$rowsum # site scores
# kick out variables that are least significant - simplify the model
vegan::ordiplot(cca,display="sites",cex=1,type="t",xlab="CCA1 (23%)",ylab="CCA2 (18%)")
vegan::orditorp(cca,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=1.1)
# add the environmental factors to the cca ordination plot
vegan::ordisurf(cca,envdat$floodprob,add=T,col="green")
vegan::ordisurf(cca,envdat$DistGulley_m,add=T,col="blue")

# test if the variables and axes (margins) are significant
anova(cca,by="axis")
anova(cca,by="margin")

# You have measured the right things!
# for example - test this if you would have only measured clay thickness

# yes, clay thickness significantly affects vegetation composition

##### cluster analysis (classification) of  communities
# first calculate a dissimilarity matrix, using Bray-Curtis dissimilarity
d<-vegan::vegdist(vegdat,method="bray")
d # show the dissimilarity matrix (1= completely different, 0= exactly the same)


# now cluster the sites based on similarity in species composition 
# using average linkage as the sorting algorithm
cavg<-hclust(d,method="average")
plot(cavg)


# back to  clustering based on species composition - show the dendrogram and cut in in 4 communities
rect.hclust(cavg,4)
c4<-cutree(cavg,4)
c4

##### add the clustering of plots to your cca ordination
vegan::ordiplot(cca,display="sites",cex=1,type="t",xlab="CCA1 (0.87)",ylab="CCA2 (0.69)")
vegan::orditorp(cca,dis="sp", priority=SpecTotCov,
                col="red",pcol="red",pch="+",cex=1.1)
vegan::ordihull(cca,c4,lty = 2,col="green3",lwd=2)

#add the vegetation type to the environmental data
envdat2<-envdat %>%
     dplyr::mutate(vegtype=factor(c4))
envdat2
envdat2$vegtype
levels(envdat2$vegtype)<-c("Dune","High saltmarsh", "Low saltmarsh","Pioneer zone")

library(patchwork)
p1<-envdat2 %>% ggplot(aes(x=vegtype,y=floodprob)) +
   ylim(0,0.06) +
    geom_boxplot() +
    xlab(NULL)
p1
p2<-envdat2 %>% ggplot(aes(x=vegtype,y=DistGulley_m)) +
  ylim(0,400) +
  geom_boxplot() +
  xlab(NULL)
p2
p3<-envdat2 %>% ggplot(aes(x=vegtype,y=elevation_m)) +
  geom_boxplot()
p3
p1+p2+p3+patchwork::plot_layout(ncol=1)

# test if DistGulley_m is different between the vegetation types
m1<-lm(DistGulley_m~vegtype,data=envdat2)
anova(m1)
# what do you write: 
# the vegetation types were significantly different in distance to gulley (F3,18=21.36, P<0.001)
# * P<0.05, ** P<0.01, *** P<0.001
library(multcomp)
m1.tukey<-multcomp::glht(m1,mcp(vegtype="Tukey")) # only if your overall F test is significant
m1.tukey
multcomp::cld(m1.tukey)
# means with the same letter are not significantly different