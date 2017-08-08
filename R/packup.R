#' Packup
#' @description Moves all library(), require(), and pload() calls up to top of document and arrange in alphabetical order.
#' Depending on what convention the user has used more, all packages are homogenised to quoted or unquoted names.
#' The exact location differs from .R to .Rmd. In .R the list starts at document position 0,0. In .Rmd it is the first line
#' of the first chunk.
#' @return TRUE if sucessful, FALSE if a problem was encountered.
#' @export
packup <- function(){
    doc <- rstudioapi::getActiveDocumentContext()
    if(doc$path == ""){
        message("Cannot packup unsaved file. Please save and checkin this file.")
        return(FALSE)
    }
    SOURCE_RMD <- grepl("([RMDrmd]){3}$", doc$path)
    if(SOURCE_RMD){
        #find the first location of the first line of the first chunk
        r_chunk_matches <- which(regexpr(pattern = "^\\s*```\\{\\s*[Rr]\\s*.*\\}", doc$contents) == 1, arr.ind = TRUE)
        insertion_target <- rstudioapi::document_position(r_chunk_matches[[1]]+1,0)
    }
    else{
        insertion_target <- rstudioapi::document_position(0,0)
    }
    # Filter comment lines
    comments <- "^\\s*#"
    comment_lines <- grepl(pattern = comments, x = doc)
    doc <- doc[!comment_lines]

    # Search for library matches
    lib_patterns <- list(
        library = "(?:^\\s*library\\s*\\(\"*[a-zA-Z0-9.]+\"*\\).*$)",
        require   = "(?:^\\s*require\\s*\\(\"*[a-zA-Z0-9.]+\"*\\).*$)",
        p_load = "(?:^\\s*p_load\\s*\\(\"*[a-zA-Z0-9.]+\"*[\\s*\\,\\s*\"*[a-zA-Z0-9.]+\"*]*\\).*$)"
    )

    #lib_matches <- regexec(pattern = "^\\s*library\\(\"*[a-zA-Z0-9]+\\\"*)", text = doc$contents)
    lib_matches <- regexec(pattern = paste0(lib_patterns, collapse = "|"),
                      text = doc$contents,
                      perl = TRUE)

    line_matches <- which(unlist(lib_matches) >= 1)

    replace_location_starts <- mapply(rstudioapi::document_position, line_matches, 0, SIMPLIFY = FALSE)
    replacte_location_ends <- mapply(rstudioapi::document_position, line_matches+1 , 0, SIMPLIFY = FALSE)
    replace_range_list <- mapply(rstudioapi::document_range, replace_location_starts, replacte_location_ends, SIMPLIFY = FALSE)
    rstudioapi::modifyRange(location = replace_range_list, "")

    lib_list <- unique( sort( unlist( regmatches(x = doc$contents, m = lib_matches) ) ) )
    quote_count <- sum( grepl(pattern = "\"", lib_list) )
    #strip all quotes
    lib_list_s <- gsub("\"", "", lib_list)

    #Decide what has more usage, quotes or no quotes.
    if(quote_count >= round(length(lib_list)/2)){
        lib_list_s <- gsub("\\(","\\(\"", gsub("\\)", "\"\\)",lib_list_s) )
    }

    rstudioapi::insertText( paste0( paste0(lib_list_s, collapse = "\n"), "\n"),  location = insertion_target)
    return(TRUE)
}

