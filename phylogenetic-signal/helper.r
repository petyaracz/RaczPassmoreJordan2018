phylo.d2 = function(d, tr){
  library(caper)
  data = d
  
  data$name = ifelse(data$name == "Mixed or variant", "Mixed", as.character(data$name))
  
  ## Make binary variables
  binary_vars = model.matrix(~0+name, data = data)
  available_kinterms = colSums(binary_vars) %>% 
    subset(., . > 0) %>% 
    names()
  binary_vars = cbind(binary_vars, taxa = data$taxa %>% as.character()) %>%
    as.data.frame()
  
  #tree = makeditree(obj$tree)
  empty_row = rep(NA, 5)
  results = list()
  
  if('nameEskimo' %in% available_kinterms){    
    results[[1]] = phylo.d(data = binary_vars, phy = tr, 
                           names.col = taxa,
                           binvar = nameEskimo) %>%
      format_phylo.d(.)
  } else {
    results[[1]] = empty_row
  }
  
  if('nameHawaiian' %in% available_kinterms){
    results[[2]] = phylo.d(binary_vars,tr,taxa,nameHawaiian) %>%
      format_phylo.d(.)
  } else {
    results[[2]] = empty_row
  }
  
  if('nameIroquois' %in% available_kinterms){
    results[[3]] = phylo.d(binary_vars,tr,taxa,nameIroquois) %>%
      format_phylo.d(.)
  } else {
    results[[3]] = empty_row
  }
  if('nameOmaha' %in% available_kinterms){
    results[[4]] = phylo.d(binary_vars,tr,taxa,nameOmaha) %>%
      format_phylo.d(.)
  } else {
    results[[4]] = empty_row
  }
  
  if('nameSudanese' %in% available_kinterms){
    results[[5]] = phylo.d(binary_vars,tr,taxa,nameSudanese) %>%
      format_phylo.d(.)
  } else {
    results[[5]] = empty_row
  }
  
  if('nameMixed' %in% available_kinterms){
    results[[6]] = phylo.d(binary_vars,tr,taxa,nameMixed) %>%
      format_phylo.d(.)
  } else {
    results[[6]] = empty_row
  }
  
  if('nameCrow' %in% available_kinterms){
    results[[7]] = phylo.d(binary_vars,tr,taxa,nameCrow) %>%
      format_phylo.d(.)
  } else {
    results[[7]] = empty_row
  }
  
  if('nameDescriptive' %in% available_kinterms){
    results[[8]] = phylo.d(binary_vars,tr,taxa,nameDescriptive) %>%
      format_phylo.d(.)
  } else {
    results[[8]] = empty_row
  }
  
  results = results %>% do.call(rbind, .)
  dimnames(results) = list(c('Eskimo', 'Hawaiian', 'Iroquois', 'Omaha', 'Sudanese',
                             'Mixed', 'Crow', 'Descriptive'),
                           c('Absent', 'Present', 'D_Estimate', 'Brownian_Prob', 'Random_Prob'))
  results[!is.na(results[,1]),]
}

format_phylo.d = function(x){
  if(class(x) != 'try-error')
    c(
      Absent = x$StatesTable[1],
      Present = x$StatesTable[2],
      EstimateD = x$DEstimate,
      BrownianProb = x$Pval0,
      RandomProb = x$Pval1
    )
  else{
    rep(NA, 5)
  }
}

## Makes a tree fully dichotomous while catching any errors that may occur. 
## Also ensures there are no zero length branches. 
## Output is a phy object
makeditree = function(tree){
  require(ape)
  if(!is.binary.tree(tree))
    tree = multi2di(tree, random=FALSE)
  if(!is.rooted(tree))
    warning('Tree is still unrooted after making dichotomous')  
  
  ## Add a small amount to any zero-length tree branches 
  tree$edge.length[tree$edge.length == 0] = .001
  
  tree
}

# for(i in 1:length(trees[[1]]))
#   if(!is.rooted(trees[[1]][[i]])) print(i)
# 
# an_t = trees[[1]]
