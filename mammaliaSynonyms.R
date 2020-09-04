mammaliaSynonyms<-function(gen, specie){
  regEx<-paste0('^.*(?i)(',gen,').*$')
  synonyms<- specie %>%
    dplyr::filter(grepl(regEx, GeneID) | grepl(regEx, Symbol) | grepl(regEx, Synonyms) |
                  grepl(regEx, description) | grepl(regEx, Symbol_from_nomenclature_authority) |
                  grepl(regEx, Full_name_from_nomenclature_authority) | grepl(regEx, Other_designations)) %>%
    dplyr::select(GeneID, Symbol, Synonyms, description, Symbol_from_nomenclature_authority,
                  Full_name_from_nomenclature_authority, Other_designations)
    
  
  return(synonyms)
}