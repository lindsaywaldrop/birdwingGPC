
bird.phylo$char.species<-as.character(bird.phylo$sp.summary.Species)
species_list<-as.character(species$species_list)

same_birds<-rep(0,length=nrow(bird.phylo))

for (i in 1:length(species_list)){
  temp_birds<-rep(NA,length=nrow(bird.phylo))
  temp_birds<-bird.phylo$char.species==species_list[i]
  same_birds<-same_birds+temp_birds
}

same_birds_list<-as.logical(same_birds)

both_birds<-bird.phylo[same_birds_list,]