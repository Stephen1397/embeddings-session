docs <- c("my favourite time of year is summer",
          "let it snow let it snow let it snow",
          "it might be too hot",
          "and now for something completely different")
api_url <- ""

out <- httr2::request(api_url) |>
  httr2::req_headers("Accept" = "application/json") |>
  httr2::req_body_json(
    list(
      model = "multi-qa-MiniLM-L6-cos-v1",
      input = docs
    )
  ) |>
  httr2::req_perform() |>
  httr2::resp_body_json() |>
  purrr::pluck("data") |>
  purrr::map("embedding") |>
  purrr::map(unlist) |>
  purrr::set_names(docs)

comparison_idx <- list(c(1,2),
                       c(1,3),
                       c(2,4))
test_pairs <- lapply(comparison_idx, \(x) docs[x])

mat <- do.call("rbind", out)
P <- prcomp(mat)[["x"]][, c(1,2)]
# Divide each val by the normalised val of the x, y pair
P_norm <- sweep(P, 1, apply(P, 1, \(x) norm(x, type = "2")), FUN = "/")

plot_doc_pair <- function(mat, normalised_pca, test_pair_idx, test_pair){
  # Remembering because the embeddings are normalised, dot product and cosine
  # similarity are equivalent
  dot_product <- (
    mat[test_pair_idx[[test_pair]], ][1, ] %*% mat[test_pair_idx[[test_pair]], ][2, ]
  )
  
  plotting_data <- data.frame(
    doc = rownames(mat[test_pair_idx[[test_pair]], ]),
    xend = normalised_pca[test_pair_idx[[test_pair]], 1],
    yend = normalised_pca[test_pair_idx[[test_pair]], 2],
    x = c(0,0),
    y = c(0,0)
  )
  
  p <- ggplot2::ggplot(plotting_data, ggplot2::aes(x = x,
                                                   y = y,
                                                   xend = xend,
                                                   yend = yend,
                                                   colour = doc)) + 
    ggplot2::geom_segment() +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(-1,1), ylim = c(-1,1)) +
    ggplot2::ggtitle(paste0("Cosine similarity: ", dot_product))
  return(p)
}

plot_doc_pair(mat, P_norm, comparison_idx, 2)
