#' Computes the Jensen-Shannon divergence between two probabiliy distributions.
#'
#' @param P A probility distribution (vector summing to one).
#' @param Q A probility distribution (vector summing to one).
#' @return The JSD of \code{P} and \code{Q}.
#'
#' @examples
#' P = prop.table(sample(1:10, 20, replace = TRUE))
#' Q = prop.table(sample(5:15, 20, replace = TRUE))
#'
#' JSD(P,Q)
#' JSD(Q,P)

JSD = function(P, Q) {
    M = (P + Q)/2
    jsd = 0.5 * KLD(P, M) + 0.5 * KLD(Q, M)
    return(jsd)
}

#' Computes the Kullback-Leibler divergence between two probabiliy distributions.
#'
#'
#'
#' KLD is defined as: \deqn{KLD(A,B) = \sum{A \times \log\left(\frac{A}{B}\right)}}
#'
#' @param A A probility distribution (vector summing to one).
#' @param B A probility distribution (vector summing to one).
#' @return The KLD of \code{A} and \code{B}.
#'
#' @examples
#' A = prop.table(sample(1:10, 20, replace = TRUE))
#' B = prop.table(sample(5:15, 20, replace = TRUE))
#'
#' KLD(A,B)
#' KLD(B,A)

KLD = function(A, B) {
    sum(A * log(A/B))
}
