#include <Rcpp.h>
#include "libgeos.h"
#include <vector>

using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
NumericVector compute_mbc_area(const std::string& wkt_collection,
                               const IntegerMatrix& plans_chunk,
                               int nd) {
  int n_plans = plans_chunk.ncol();
  int n_rows = plans_chunk.nrow();

  // Initialize GEOS context
  GEOSContextHandle_t ctx = GEOS_init_r();
  GEOSWKTReader* reader = GEOSWKTReader_create_r(ctx);

  // Read the geometry collection
  GEOSGeometry* collection = GEOSWKTReader_read_r(ctx, reader, wkt_collection.c_str());
  GEOSWKTReader_destroy_r(ctx, reader);

  if (collection == NULL) {
    GEOS_finish_r(ctx);
    stop("Failed to read WKT collection");
  }

  int n_geoms = GEOSGetNumGeometries_r(ctx, collection);
  if (n_geoms != n_rows) {
    GEOSGeom_destroy_r(ctx, collection);
    GEOS_finish_r(ctx);
    stop("Number of geometries in collection must match number of rows in plans");
  }

  // Extract individual geometries
  std::vector<const GEOSGeometry*> geoms(n_geoms);
  for (int i = 0; i < n_geoms; i++) {
    geoms[i] = GEOSGetGeometryN_r(ctx, collection, i);
    if (geoms[i] == NULL) {
      GEOSGeom_destroy_r(ctx, collection);
      GEOS_finish_r(ctx);
      stop("Failed to extract geometry from collection");
    }
  }

  NumericVector results(nd * n_plans);

  std::vector<std::vector<int>> district_indices(nd);
  std::vector<GEOSGeometry*> district_geoms_cloned;
  district_geoms_cloned.reserve(n_rows);

  // Process each plan
  for (int p = 0; p < n_plans; p++) {
    // clear indices
    for (int d = 0; d < nd; d++) {
      district_indices[d].clear();
    }

    for (int i = 0; i < n_rows; i++) {
      int dist = plans_chunk(i, p);
      district_indices[dist - 1].push_back(i);
    }

    // calculate MBC for each district
    for (int d = 0; d < nd; d++) {
      if (district_indices[d].empty()) {
        results[p * nd + d] = NA_REAL;
        continue;
      }

      // clone geometries for this district
      district_geoms_cloned.clear();
      for (int idx : district_indices[d]) {
        GEOSGeometry* cloned = GEOSGeom_clone_r(ctx, geoms[idx]);
        if (cloned == NULL) {
          for (GEOSGeometry* g : district_geoms_cloned) {
            GEOSGeom_destroy_r(ctx, g);
          }
          results[p * nd + d] = NA_REAL;
          continue;
        }
        district_geoms_cloned.push_back(cloned);
      }

      // Create collection
      GEOSGeometry* temp_collection = NULL;
      if (district_geoms_cloned.size() == 1) {
        temp_collection = district_geoms_cloned[0];
      } else {
        temp_collection = GEOSGeom_createCollection_r(
          ctx, GEOS_GEOMETRYCOLLECTION,
          district_geoms_cloned.data(),
          district_geoms_cloned.size()
        );

        if (temp_collection == NULL) {
          for (GEOSGeometry* g : district_geoms_cloned) {
            GEOSGeom_destroy_r(ctx, g);
          }
          results[p * nd + d] = NA_REAL;
          continue;
        }
      }

      // Get minimum bounding circle radius
      double radius;
      GEOSGeometry* center = NULL;
      GEOSMinimumBoundingCircle_r(ctx, temp_collection, &radius, &center);

      // Clean up
      GEOSGeom_destroy_r(ctx, temp_collection);
      if (center != NULL) {
        GEOSGeom_destroy_r(ctx, center);
      }

      if (radius < 0) {
        results[p * nd + d] = NA_REAL;
        continue;
      }

      // Calculate and store MBC area
      results[p * nd + d] = M_PI * radius * radius;
    }
  }

  // Clean up
  GEOSGeom_destroy_r(ctx, collection);
  GEOS_finish_r(ctx);

  return results;
}
