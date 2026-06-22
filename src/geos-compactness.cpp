#include <Rcpp.h>
#include "libgeos.h"
#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;

namespace {

using DistrictMetric = double (*)(GEOSContextHandle_t, const GEOSGeometry*);

struct ReflectionData {
  double center;
  bool reflect_x;
};

int reflect_coordinate(double* x, double* y, void* userdata) {
  ReflectionData* data = static_cast<ReflectionData*>(userdata);
  if (data->reflect_x) {
    *x = 2.0 * data->center - *x;
  } else {
    *y = 2.0 * data->center - *y;
  }
  return 1;
}

GEOSGeometry* make_owned_collection(GEOSContextHandle_t ctx,
                                    const std::vector<GEOSGeometry*>& geoms) {
  std::vector<GEOSGeometry*> clones(geoms.size(), nullptr);
  for (size_t i = 0; i < geoms.size(); i++) {
    clones[i] = GEOSGeom_clone_r(ctx, geoms[i]);
    if (clones[i] == nullptr) {
      for (size_t j = 0; j < i; j++) {
        GEOSGeom_destroy_r(ctx, clones[j]);
      }
      return nullptr;
    }
  }

  GEOSGeometry* collection = GEOSGeom_createCollection_r(
    ctx, GEOS_GEOMETRYCOLLECTION, clones.data(), clones.size());
  if (collection == nullptr) {
    for (GEOSGeometry* geom : clones) {
      GEOSGeom_destroy_r(ctx, geom);
    }
  }
  return collection;
}

double geometry_area(GEOSContextHandle_t ctx, const GEOSGeometry* geom) {
  double area;
  if (geom == nullptr || !GEOSArea_r(ctx, geom, &area)) {
    return NA_REAL;
  }
  return area;
}

double minimum_bounding_circle_area(GEOSContextHandle_t ctx,
                                    const GEOSGeometry* district) {
  double radius = -1.0;
  GEOSGeometry* center = nullptr;
  GEOSGeometry* circle = GEOSMinimumBoundingCircle_r(
    ctx, district, &radius, &center);
  if (center != nullptr) {
    GEOSGeom_destroy_r(ctx, center);
  }

  double area = geometry_area(ctx, circle);
  if (circle != nullptr) {
    GEOSGeom_destroy_r(ctx, circle);
  }
  return radius < 0 ? NA_REAL : area;
}

double convex_hull_area(GEOSContextHandle_t ctx,
                        const GEOSGeometry* district) {
  GEOSGeometry* hull = GEOSConvexHull_r(ctx, district);
  double area = geometry_area(ctx, hull);
  if (hull != nullptr) {
    GEOSGeom_destroy_r(ctx, hull);
  }
  return area;
}

double minimum_rotated_rectangle_area(GEOSContextHandle_t ctx,
                                      const GEOSGeometry* district) {
  GEOSGeometry* rectangle = GEOSMinimumRotatedRectangle_r(ctx, district);
  double area = geometry_area(ctx, rectangle);
  if (rectangle != nullptr) {
    GEOSGeom_destroy_r(ctx, rectangle);
  }
  return area;
}

double skew_score(GEOSContextHandle_t ctx, const GEOSGeometry* district) {
  GEOSGeometry* united = GEOSUnaryUnion_r(ctx, district);
  GEOSGeometry* inscribed = united == nullptr ? nullptr
    : GEOSMaximumInscribedCircle_r(ctx, united, 0.01);

  double bounding_radius = -1.0;
  GEOSGeometry* bounding_center = nullptr;
  GEOSGeometry* bounding = united == nullptr ? nullptr
    : GEOSMinimumBoundingCircle_r(
        ctx, united, &bounding_radius, &bounding_center);

  double inscribed_radius = NA_REAL;
  double bounding_area = geometry_area(ctx, bounding);
  if (inscribed != nullptr) {
    GEOSLength_r(ctx, inscribed, &inscribed_radius);
  }

  double score = NA_REAL;
  if (R_finite(inscribed_radius) && bounding_radius >= 0 &&
      R_finite(bounding_area) && bounding_area > 0) {
    // wk::crc(), used by the previous R implementation, represents circles
    // with a regular 100-sided polygon. Preserve that area convention.
    const double pi = std::acos(-1.0);
    const double circle_area_factor = 50.0 * std::sin(2.0 * pi / 100.0);
    score = std::sqrt(
      circle_area_factor * inscribed_radius * inscribed_radius / bounding_area);
  }

  if (bounding_center != nullptr) GEOSGeom_destroy_r(ctx, bounding_center);
  if (inscribed != nullptr) GEOSGeom_destroy_r(ctx, inscribed);
  if (bounding != nullptr) GEOSGeom_destroy_r(ctx, bounding);
  if (united != nullptr) GEOSGeom_destroy_r(ctx, united);
  return score;
}

double boyce_clark_score(GEOSContextHandle_t ctx,
                         const GEOSGeometry* district) {
  GEOSGeometry* united = GEOSUnaryUnion_r(ctx, district);
  // Preserve the previous implementation. It tested whether the polygon was
  // within its centroid point, so polygon inputs always used point-on-surface.
  GEOSGeometry* center = united == nullptr
    ? nullptr : GEOSPointOnSurface_r(ctx, united);

  double cx, cy, xmin, ymin, xmax, ymax;
  bool valid = center != nullptr &&
    GEOSGeomGetX_r(ctx, center, &cx) &&
    GEOSGeomGetY_r(ctx, center, &cy) &&
    GEOSGeom_getXMin_r(ctx, united, &xmin) &&
    GEOSGeom_getYMin_r(ctx, united, &ymin) &&
    GEOSGeom_getXMax_r(ctx, united, &xmax) &&
    GEOSGeom_getYMax_r(ctx, united, &ymax);

  double score = NA_REAL;
  if (valid) {
    const double pi = std::acos(-1.0);
    const double max_dist = std::hypot(ymax - ymin, xmax - xmin);
    double radials[16] = {0.0};
    double radial_sum = 0.0;

    for (int angle = 0; angle < 16; angle++) {
      const double theta = angle * pi / 8.0;
      GEOSCoordSequence* coords = GEOSCoordSeq_create_r(ctx, 2, 2);
      if (coords == nullptr) {
        valid = false;
        break;
      }
      GEOSCoordSeq_setXY_r(
        ctx, coords, 0,
        cx + max_dist * std::cos(theta),
        cy + max_dist * std::sin(theta));
      GEOSCoordSeq_setXY_r(ctx, coords, 1, cx, cy);

      GEOSGeometry* radial = GEOSGeom_createLineString_r(ctx, coords);
      GEOSGeometry* intersection = radial == nullptr
        ? nullptr : GEOSIntersection_r(ctx, radial, united);
      double length = 0.0;
      if (intersection != nullptr) {
        GEOSLength_r(ctx, intersection, &length);
      }
      radials[angle] = std::max(0.0, length);
      radial_sum += radials[angle];

      if (intersection != nullptr) GEOSGeom_destroy_r(ctx, intersection);
      if (radial != nullptr) GEOSGeom_destroy_r(ctx, radial);
    }

    if (valid && radial_sum > 0) {
      double radial_deviation = 0.0;
      for (double radial : radials) {
        radial_deviation += std::abs(
          radial / radial_sum * 100.0 - 6.25);
      }
      score = 1.0 - radial_deviation / 200.0;
    }
  }

  if (center != nullptr) GEOSGeom_destroy_r(ctx, center);
  if (united != nullptr) GEOSGeom_destroy_r(ctx, united);
  return score;
}

double symmetry_overlap_area(GEOSContextHandle_t ctx,
                             const GEOSGeometry* district,
                             bool reflect_x) {
  GEOSGeometry* united = GEOSUnaryUnion_r(ctx, district);
  GEOSGeometry* centroid = united == nullptr
    ? nullptr : GEOSGetCentroid_r(ctx, united);

  double cx, cy;
  bool valid = centroid != nullptr &&
    GEOSGeomGetX_r(ctx, centroid, &cx) &&
    GEOSGeomGetY_r(ctx, centroid, &cy);
  ReflectionData reflection{
    reflect_x ? cx : cy,
    reflect_x
  };
  GEOSGeometry* reflected = valid
    ? GEOSGeom_transformXY_r(
        ctx, united, reflect_coordinate, &reflection)
    : nullptr;
  GEOSGeometry* overlap = reflected == nullptr
    ? nullptr : GEOSIntersection_r(ctx, united, reflected);
  double area = geometry_area(ctx, overlap);

  if (overlap != nullptr) GEOSGeom_destroy_r(ctx, overlap);
  if (reflected != nullptr) GEOSGeom_destroy_r(ctx, reflected);
  if (centroid != nullptr) GEOSGeom_destroy_r(ctx, centroid);
  if (united != nullptr) GEOSGeom_destroy_r(ctx, united);
  return area;
}

double y_symmetry_overlap_area(GEOSContextHandle_t ctx,
                               const GEOSGeometry* district) {
  return symmetry_overlap_area(ctx, district, true);
}

double x_symmetry_overlap_area(GEOSContextHandle_t ctx,
                               const GEOSGeometry* district) {
  return symmetry_overlap_area(ctx, district, false);
}

NumericVector compute_district_metric(const std::string& wkt_collection,
                                      const IntegerMatrix& plans,
                                      int nd,
                                      DistrictMetric metric) {
  const int n_plans = plans.ncol();
  const int n_rows = plans.nrow();
  GEOSContextHandle_t ctx = GEOS_init_r();
  GEOSWKTReader* reader = GEOSWKTReader_create_r(ctx);
  GEOSGeometry* collection = GEOSWKTReader_read_r(
    ctx, reader, wkt_collection.c_str());
  GEOSWKTReader_destroy_r(ctx, reader);

  if (collection == nullptr) {
    GEOS_finish_r(ctx);
    stop("Failed to read WKT collection");
  }
  if (GEOSGetNumGeometries_r(ctx, collection) != n_rows) {
    GEOSGeom_destroy_r(ctx, collection);
    GEOS_finish_r(ctx);
    stop("Number of geometries in collection must match number of rows in plans");
  }

  std::vector<GEOSGeometry*> source_geometries(n_rows);
  for (int i = 0; i < n_rows; i++) {
    source_geometries[i] = const_cast<GEOSGeometry*>(
      GEOSGetGeometryN_r(ctx, collection, i));
    if (source_geometries[i] == nullptr) {
      GEOSGeom_destroy_r(ctx, collection);
      GEOS_finish_r(ctx);
      stop("Failed to extract geometry from collection");
    }
  }

  NumericVector results(nd * n_plans, NA_REAL);
  std::vector<std::vector<GEOSGeometry*>> district_geometries(nd);
  for (int p = 0; p < n_plans; p++) {
    for (auto& district : district_geometries) {
      district.clear();
    }
    for (int i = 0; i < n_rows; i++) {
      int district = plans(i, p) - 1;
      if (district < 0 || district >= nd) {
        GEOSGeom_destroy_r(ctx, collection);
        GEOS_finish_r(ctx);
        stop("District labels must be integers from 1 through nd");
      }
      district_geometries[district].push_back(source_geometries[i]);
    }

    for (int d = 0; d < nd; d++) {
      if (district_geometries[d].empty()) {
        continue;
      }
      bool owns_district = district_geometries[d].size() > 1;
      GEOSGeometry* district = owns_district
        ? make_owned_collection(ctx, district_geometries[d])
        : district_geometries[d][0];
      if (district != nullptr) {
        results[p * nd + d] = metric(ctx, district);
      }
      if (owns_district && district != nullptr) {
        GEOSGeom_destroy_r(ctx, district);
      }
    }
  }

  GEOSGeom_destroy_r(ctx, collection);
  GEOS_finish_r(ctx);
  return results;
}

} // namespace

// [[Rcpp::export(rng = false)]]
NumericVector compute_mbc_area(const std::string& wkt_collection,
                               const IntegerMatrix& plans,
                               int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, minimum_bounding_circle_area);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_convex_hull_area(const std::string& wkt_collection,
                                       const IntegerMatrix& plans,
                                       int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, convex_hull_area);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_rotated_box_area(const std::string& wkt_collection,
                                       const IntegerMatrix& plans,
                                       int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, minimum_rotated_rectangle_area);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_skew_score(const std::string& wkt_collection,
                                 const IntegerMatrix& plans,
                                 int nd) {
  return compute_district_metric(wkt_collection, plans, nd, skew_score);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_boyce_clark_score(const std::string& wkt_collection,
                                        const IntegerMatrix& plans,
                                        int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, boyce_clark_score);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_y_symmetry_overlap(const std::string& wkt_collection,
                                         const IntegerMatrix& plans,
                                         int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, y_symmetry_overlap_area);
}

// [[Rcpp::export(rng = false)]]
NumericVector compute_x_symmetry_overlap(const std::string& wkt_collection,
                                         const IntegerMatrix& plans,
                                         int nd) {
  return compute_district_metric(
    wkt_collection, plans, nd, x_symmetry_overlap_area);
}
