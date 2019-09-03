#' A function for identifying careless classes using LPA
#' 
#' This function employs various rules of thumb that a person might use to
#' identify careless classes resulting from an LPA
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x a datafreame of simulated data
#' @export
labelCarelessClasses <- function(means, classes) {
  m = means %>% select(ls,eo,out)
  
  # Longstring Rule
  vls = c(5, -.33, -.33)
  lsClass = applyDistanceRule(vls, m)
  lsClass2 = which.max(m$ls)
  
  
  # Careful rule
  vcare = c(-.2, .4, 0)
  careClass = applyDistanceRule(vcare, m)
  careClass2 = which.max(table(classes))
  
  # even-odd rule
  veo = c(-.1, -1, .2)
  eoClass = applyDistanceRule(veo, m)
  
  if(lsClass == lsClass2) {
    ls = lsClass
    lsMatch = "strong"
  }
  else {
    ls = lsClass2
    lsMatch = "weak"
  }
  
  if(careClass == careClass2) {
    care = careClass
    careMatch = "strong"
  }
  else {
    care = careClass2
    careMatch = "weak"
  }
  
  preds = classes %>% as.data.frame %>%  mutate(x = case_when(classes == care ~ 0, 
                                                TRUE ~ 1)) %>% select(x)
  
  if(lsMatch == "strong" & careMatch == "strong"){
    match = "strong"
  }
  else {
    match = "weak"
  }
  
  
  return(list(predictions=preds$x, 
              matchStrength = list(lsMatch = lsMatch, 
                   careMatch = careMatch, 
                   OmniMatch=match)))
}


