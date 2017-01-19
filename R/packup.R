
packup <- function(){
    doc <- rstudioapi::getActiveDocumentContext()
    if(doc$path == ""){
        message("Cannot packup unsaved file. Please save and checkin this file.")
        return(FALSE)
    }
    SOURCE_RMD <- grepl("([RMDrmd]){3}$", doc$path)
    if(SOURCE_RMD){
        #find the first location of the first line of the first chunk
        #insertion_target <-
    }
    else{
        insertion_target <- rstudioapi::document_position(0,0)
    }
    lib_matches <- regexec(pattern = "^\\s*library\\(\"*[a-zA-Z0-9]+\\\"*)", text = doc$contents)
    line_matches <- which(unlist(lib_matches) == 1)
    line_lengths <- unlist(lapply(lib_matches[line_matches], attr, "match.length"))
    replace_location_starts <- mapply(rstudioapi::document_position, line_matches, 1, SIMPLIFY = FALSE)
    replacte_location_ends <- mapply(rstudioapi::document_position, line_matches, line_lengths + 1, SIMPLIFY = FALSE)
    replace_range_list <- mapply(rstudioapi::document_range, replace_location_starts, replacte_location_ends, SIMPLIFY = FALSE)
    rstudioapi::modifyRange(location = replace_range_list, "")

    lib_list <- regmatches(x = doc$contents, m = lib_matches) %>%
    unlist() %>%
    sort() %>%
    unique()

    quote_count <- sum(grepl(pattern = "\"", lib_list))
    #strip all quotes
    lib_list_s <- gsub("\"", "", lib_list)

    if(quote_count >= round(length(lib_list)/2)){
        lib_list_s <- gsub("\\(","\\(\"",gsub("\\)", "\"\\)",lib_list_s))
    }
    paste0(lib_list_s, collapse = "\n") %>%
    paste0(., "\n") %>%
    rstudioapi::insertText(location = insertion_target)
}
packup()

library(ggplot2)
library(ggplot2)
library(purrr)
library("formaelle")
library("quoted")
library("dplyr")
library("AER")
library("zzzzzzzzz")
