createCard <- function() {
    card <- cbind(B = sample(x=1:15,5), I = sample(x=16:30,5), N = sample(x=31:45, 5), 
        G = sample(x=46:60,5), O = sample(x=61:75,5))
    card[3,3] <- NA
    return(card)
}

evaluate <- function(card, draw) {
    eval <- matrix(ncol = 5, nrow = 5)
    for (i in 1:5) {
        for (j in 1:5) {
            if (card[i,j] %in% draw) {
                eval[i,j] <- TRUE
            } else { eval[i,j] <- FALSE }
        }
    }
    eval[3,3] <- TRUE
    return(eval)        
}


bingo <- function(bools, game="5 in a row") {
    if (dim(bools)[1] != 5 | dim(bools)[2] != 5) stop("not a valid matrix argument.")
    if (game == "5 in a row") {
        apply(bools, 1, sum) -> rowsums
        apply(bools, 2, sum) -> colsums
        sum(bools[1,1], bools[2,2], bools[3,3], bools[4,4], bools[5,5]) -> rdiag
        sum(bools[5,1], bools[4,2], bools[3,3], bools[2,4], bools[1,5]) -> ldiag
        if (max(colsums, rowsums, ldiag, rdiag) == 5) {
            return(TRUE)
        }
        return(FALSE)
    }
    if (game == "full card") {
        if(sum(bools) == 25) {
            return(TRUE)
        }
        return(FALSE)
    }
}


bingoSim <- function(cards, draw=75, game) {
    cards <- replicate(cards, createCard(), simplify = FALSE)
    bingos <- numeric()
    for (d in 1:draw) {
        draw <- sample(1:75, d)
        evals <- lapply(cards, evaluate, draw)
        bingos[d] <- sum(sapply(evals, bingo, game = game))
    }
    return(bingos)
}


# ten replication of a bingo game with 45 cards and 1-75 draws
x <- matrix(nrow = 10, ncol = 75)
for (i in 1:10) {
    x[i,] <- bingoSim(cards = 45, draw = 75, game = "5 in a row") 
}
probs <- apply(x, 2, mean)/45


