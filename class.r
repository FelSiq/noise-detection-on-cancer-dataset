# R Code
# Class Noise
# Random: Teng C. M. Correcting Noise Data
# Pairwise: X. Zhu et al. Eliminating class noise in large datasets
# Rand and parwise class noise


index = function(data, rate) {

	value = trunc(nrow(data)*rate);
	noise = sample(rownames(data), value, replace=FALSE);
	return(noise);
}


nc.majority = function(data) {

	aux = summary(data$Class);
	aux = sort(aux, decreasing=TRUE)[1:2];
	return(names(aux));
}


rand = function(data, rate) {

	aux = list();

	if(rate == 0) {
		aux$data = data;
		return(aux);
	}

	noise = index(data, rate);
	data[noise,]$Class = unlist(lapply(noise, function(i) {
		sample(setdiff(levels(data$Class), data[i,]$Class), 1);
	}));
	
	aux$noise = noise;
	aux$data = data;
	return(aux);
}


wise = function(data, rate) {
 
	aux = list();

	if(rate == 0) {
		aux$data = data;
		return(aux);
	}

 	class = nc.majority(data);
 	noise = index(data[data$Class == class[1],], rate);
 	data[noise,]$Class = class[2];
 
	aux$noise = noise;
	aux$data = data;
 	return(aux);
}

