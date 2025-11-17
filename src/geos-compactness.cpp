#include <Rcpp.h>
#include "libgeos.h"
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector reock_scores_matrix(const std::vector<std::string>& wkt_vector,
                                  const IntegerMatrix& plans) {
  size_t n = wkt_vector.size();
  int n_plans = plans.ncol();
  int n_rows = plans.nrow();

  if (n != (size_t)n_rows) {
    stop("wkt_vector and plans rows must have the same length");
  }

  // Initialize GEOS context
  GEOSContextHandle_t ctx = GEOS_init_r();
  GEOSWKTReader* reader = GEOSWKTReader_create_r(ctx);

  // Read all geometries once and compute areas
  std::vector<GEOSGeometry*> geoms(n);
  NumericVector areas(n);

  for (size_t i = 0; i < n; i++) {
    geoms[i] = GEOSWKTReader_read_r(ctx, reader, wkt_vector[i].c_str());
    if (geoms[i] == NULL) {
      // Clean up on error
      for (size_t j = 0; j < i; j++) {
        GEOSGeom_destroy_r(ctx, geoms[j]);
      }
      GEOSWKTReader_destroy_r(ctx, reader);
      GEOS_finish_r(ctx);
      stop("Failed to read WKT geometry");
    }

    double area;
    if (GEOSArea_r(ctx, geoms[i], &area) == 0) {
      areas[i] = 0.0;
    } else {
      areas[i] = area;
    }
  }

  GEOSWKTReader_destroy_r(ctx, reader);

  // Find unique districts across all plans
  std::vector<int> all_dists;
  for (int i = 0; i < n_rows; i++) {
    for (int j = 0; j < n_plans; j++) {
      all_dists.push_back(plans(i, j));
    }
  }
  std::sort(all_dists.begin(), all_dists.end());
  all_dists.erase(std::unique(all_dists.begin(), all_dists.end()), all_dists.end());
  int nd = all_dists.size();

  // Result vector: nd districts * n_plans
  NumericVector results(nd * n_plans);

  // Process each plan
  for (int p = 0; p < n_plans; p++) {
    IntegerVector plan = plans(_, p);

    // Group geometries by district for this plan
    std::vector<std::vector<int>> district_indices(nd);
    NumericVector district_areas(nd, 0.0);

    for (int i = 0; i < n_rows; i++) {
      int dist = plan[i];
      // Find position in sorted districts
      auto it = std::lower_bound(all_dists.begin(), all_dists.end(), dist);
      int dist_idx = std::distance(all_dists.begin(), it);

      district_indices[dist_idx].push_back(i);
      district_areas[dist_idx] += areas[i];
    }

    // Calculate MBC for each district
    for (int d = 0; d < nd; d++) {
      if (district_indices[d].empty()) {
        results[p * nd + d] = NA_REAL;
        continue;
      }

      // Create collection of geometries (no union needed!)
      // Clone geometries for the collection since GEOS takes ownership
      std::vector<GEOSGeometry*> district_geoms_cloned;
      for (int idx : district_indices[d]) {
        GEOSGeometry* cloned = GEOSGeom_clone_r(ctx, geoms[idx]);
        if (cloned == NULL) {
          // Clean up on error
          for (GEOSGeometry* g : district_geoms_cloned) {
            GEOSGeom_destroy_r(ctx, g);
          }
          results[p * nd + d] = NA_REAL;
          continue;
        }
        district_geoms_cloned.push_back(cloned);
      }

      GEOSGeometry* collection = GEOSGeom_createCollection_r(
        ctx, GEOS_GEOMETRYCOLLECTION,
        district_geoms_cloned.data(),
        district_geoms_cloned.size()
      );

      if (collection == NULL) {
        // Clean up cloned geometries if collection creation failed
        for (GEOSGeometry* g : district_geoms_cloned) {
          GEOSGeom_destroy_r(ctx, g);
        }
        results[p * nd + d] = NA_REAL;
        continue;
      }

      // Get minimum bounding circle radius
      double radius;
      GEOSGeometry* center = NULL;
      GEOSMinimumBoundingCircle_r(ctx, collection, &radius, &center);

      // Destroy collection (this also destroys the cloned geometries inside)
      GEOSGeom_destroy_r(ctx, collection);

      if (center != NULL) {
        GEOSGeom_destroy_r(ctx, center);
      }

      if (radius < 0) {
        results[p * nd + d] = NA_REAL;
        continue;
      }

      // Calculate circle area from radius
      double circle_area = M_PI * radius * radius;

      // Calculate Reock score
      if (circle_area < 1e-10) {
        results[p * nd + d] = 0.0;
      } else {
        results[p * nd + d] = district_areas[d] / circle_area;
      }
    }
  }

  // Clean up all geometries
  for (size_t i = 0; i < n; i++) {
    GEOSGeom_destroy_r(ctx, geoms[i]);
  }

  GEOS_finish_r(ctx);

  return results;
}
