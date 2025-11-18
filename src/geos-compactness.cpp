#include <Rcpp.h>
#include "libgeos.h"
#include <vector>

using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
NumericVector compute_mbc_area(const std::string& wkt_collection,
                               const IntegerMatrix& plans,
                               int nd) {
  const int n_plans = plans.ncol();
  const int n_rows = plans.nrow();

  // Initialize GEOS context
  GEOSContextHandle_t ctx = GEOS_init_r();
  GEOSWKTReader* reader = GEOSWKTReader_create_r(ctx);

  // Read the geometry collection
  GEOSGeometry* collection = GEOSWKTReader_read_r(ctx, reader, wkt_collection.c_str());
  GEOSWKTReader_destroy_r(ctx, reader);

  // check read
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
  std::vector<GEOSGeometry*> geoms(n_geoms);
  for (int i = 0; i < n_geoms; i++) {
    geoms[i] = const_cast<GEOSGeometry*>(GEOSGetGeometryN_r(ctx, collection, i));
    if (geoms[i] == NULL) {
      GEOSGeom_destroy_r(ctx, collection);
      GEOS_finish_r(ctx);
      stop("Failed to extract geometry from collection");
    }
  }

  NumericVector results(nd * n_plans);

  GEOSGeometry* center;
  GEOSGeometry* temp_collection;
  std::vector<std::vector<GEOSGeometry*>> district_geoms(nd);
  for (int d = 0; d < nd; d++) {
    district_geoms[d].reserve(n_rows);
  }
  // Process each plan
  for (int p = 0; p < n_plans; p++) {
    // arrange geos by district
    for (auto &vec : district_geoms) {
      vec.clear();
    }
    for (int i = 0; i < n_rows; i++) {
      int dist = plans(i, p);
      district_geoms[dist - 1].push_back(geoms[i]);
    }

    // calculate MBC for each district
    for (int d = 0; d < nd; d++) {
      int out_idx = p * nd + d;

      temp_collection = NULL;
      auto n_geom = district_geoms[d].size();
      // convert geometry list into a collection
      if (n_geom == 0) {
        results[out_idx] = NA_REAL;
        continue;
      } else if (n_geom == 1) {
        temp_collection = district_geoms[d][0];
      } else {
        temp_collection = GEOSGeom_createCollection_r(
          ctx, GEOS_GEOMETRYCOLLECTION,
          district_geoms[d].data(),
          district_geoms[d].size()
        );

        if (temp_collection == NULL) {
          results[out_idx] = NA_REAL;
          continue;
        }
      }

      // Get minimum bounding circle radius
      double radius;
      GEOSMinimumBoundingCircle_r(ctx, temp_collection, &radius, &center);
      if (center != NULL) {
        GEOSGeom_destroy_r(ctx, center);
      }

      // Calculate and store MBC area
      if (radius < 0) {
        results[out_idx] = NA_REAL;
      } else {
        results[out_idx] = M_PI * radius * radius;
      }
    }
  }

  // Clean up
  GEOSGeom_destroy_r(ctx, collection);
  GEOS_finish_r(ctx);

  return results;
}
