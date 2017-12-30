
#' Remove all entries from blobtab except those from taxa in taxlist
#' @param blobtab A table outputted from 'blobtools view'
#' @param A list of taxa to include
#' @return A table with all other taxa removed
extract_taxa <- function(blobtab, taxlist) {
    blobtab[(blobtab$phylum.t.8 %in% taxlist),]
}
