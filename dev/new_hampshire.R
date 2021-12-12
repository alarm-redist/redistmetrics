library(geomander)

nh <- get_alarm('NH')

nh <- sf::st_transform(nh, 3437)
nh$adj <- adjacency(nh)
r <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 29,
       33, 35, 113, 152, 157, 163, 164, 166, 167, 168, 169, 170, 171, 172,
       173, 174, 175, 176, 177, 179, 191, 193, 197, 201, 205, 206, 217,
       218, 219, 220, 221, 222, 224, 225, 227, 229, 231, 232, 238, 239,
       240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252,
       253, 254, 255, 256, 257, 258, 259, 260, 262, 263, 264, 265, 266,
       267, 269, 275, 276, 277, 278, 279, 280, 281, 282, 296, 309)
d <- c(1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
       19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
       35, 115, 152, 157, 166, 167, 168, 169, 170, 171, 172, 173, 174,
       175, 176, 177, 179, 225, 238, 240, 241, 242, 243, 244, 245, 246,
       247, 248, 250, 251, 252, 253, 254, 255, 257, 258, 259, 260, 261,
       262, 263, 264, 265, 266, 268, 269, 270, 271, 272, 273, 274, 275,
       277, 278, 279, 280, 282, 283, 284, 285, 286, 287, 288, 289, 290,
       291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303,
       304, 305, 306, 307, 308, 309)

nh$r_2020 <- ifelse(seq_len(nrow(nh)) %in% r, 1L, 2L)
nh$d_2020 <- ifelse(seq_len(nrow(nh)) %in% d, 1L, 2L)

sf::st_geometry(nh) <- rmapshaper::ms_simplify(sf::st_geometry(nh), keep_shapes = TRUE, keep = .25)

# usethis::use_data(nh, overwrite = TRUE)

library(redist)

nh_map <- redist_map(nh, existing_plan = r_2020, pop_tol = 0.0005, adj = nh$adj)
# usethis::use_data(nh_map, overwrite = TRUE)

set.seed(14853)
nh_plans <- redist_smc(nh_map, 50) %>% add_reference(nh$d_2020, 'd_2020')
# usethis::use_data(nh_plans, overwrite = TRUE)

nh_m <- get_plans_matrix(nh_plans)
# usethis::use_data(nh_m, overwrite = TRUE)
