library(baseN)
library(bitops)

.base32 <-
    function() {
        c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "b", "c", "d", "e",
          "f", "g", "h", "j", "k", "m", "n", "p", "q", "r", "s", "t", "u", "v",
          "w", "x", "y", "z")
}

.binary32 <-
    function() {
        c("00000", "00001", "00010", "00011", "00100", "00101", "00110",
          "00111", "01000", "01001", "01010", "01011", "01100", "01101",
          "01110", "01111", "10000", "10001", "10010", "10011", "10100",
          "10101", "10110", "10111", "11000", "11001", "11010", "11011",
          "11100", "11101", "11110", "11111")
}

# Center of the geohash
fromGeohash <-function(hash) {
    digits = max(nchar(hash))
    index = matrix(intToBaseN(match(unlist(strsplit(hash, "")), .base32()) - 1), nrow=digits)
    index = matrix(unlist(strsplit(apply(index, 2, paste0, sep="", collapse=""), split="")), ncol=length(hash))

    lat_index = apply(index[1:nrow(index) %% 2 == 0,,drop=FALSE],2,as.numeric)
    lat_inc = 180 / (2 ^ (1:nrow(lat_index)))
    lat = as.numeric(lat_inc %*% (lat_index - 0.5))

    lon_index = apply(index[1:nrow(index) %% 2 == 1,,drop=FALSE],2,as.numeric)
    lon_inc = 360 / (2 ^ (1:nrow(lon_index)))
    lon = as.numeric(lon_inc %*% (lon_index - 0.5))

    return(list(lat=lat, lon=lon))
}

#' Returns SW/NE latitude/longitude bounds of specified geohash.
#' @param   geohash - Cell that bounds are required of.
#' @returns {{sw: {lat: number, lon: number}, ne: {lat: number, lon: number}}}
geohash_bounds <- function(geohash) {

        # assume the geohash is lower case and length > 0; should check this

        evenBit <- TRUE
        latMin <-  -90
        latMax <-  90;
        lonMin <- -180
        lonMax <- 180;

        ghvec <- strsplit(geohash, "")[[1]]

        for (chr in ghvec) {
            idx <- match(chr, .base32())
            if (is.na(idx)) {stop(paste0('Invalid geohash character: ', chr))}

            for (n in 4:1) {
                bitN <- bitAnd(bitShiftR(idx, n), 1)
                if (evenBit) {
                    # longitude
                    lonMid <- (lonMin+lonMax) / 2
                    if (bitN == 1) {
                        lonMin <- lonMid
                    } else {
                        lonMax <- lonMid
                    }
                } else {
                    # latitude
                    latMid <- (latMin+latMax) / 2
                    if (bitN == 1) {
                        latMin <- latMid
                    } else {
                        latMax <- latMid
                    }
                }
                evenBit <- !evenBit
            }
        }

        bounds <- list(
            'sw'=list('lat'=latMin, 'lon'=lonMin),
            'ne'=list('lat'=latMax, 'lon'=lonMax))

        return(bounds)
}
