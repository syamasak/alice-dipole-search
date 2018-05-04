#include <cmath>
#include <cstdio>
#include <random>
#include <ctime>
typedef unsigned short ushort;
const float Infinity = INFINITY;
struct SegmentEnd { ushort index; float endPos; }; // float->ushort
//struct SegmentSearch { ushort nDivision; float width; float offset; SegmentEnd *slices; SegmentSearch *segments; };
struct SegmentSearch { ushort nDivision; float factor; float offset; SegmentEnd *slices; SegmentSearch *segments; };
struct ChebFormula {
  float (*bz)(const float *xyz);
  void (*bxyz)(const float *xyz, float *b);
};
#include "dip5k.cxx"

static inline bool QuickSearch(const SegmentSearch ss, const float z, ushort &id) {
  //int index = floor((z - ss.offset) / ss.width * ss.nDivision); // remove '/'
  const int index = floor((z - ss.offset) * ss.factor);
  if (index > ss.nDivision) return false;
  SegmentEnd se = ss.slices[index];
  id = se.index + (z < se.endPos ? 0 : 1);
  return true;
}

static inline bool GetSegmentDip(const float xyz[3], ushort &formulaId) { // remove bool
  const float x = xyz[0], y = xyz[1], z = xyz[2];
  ushort index;
  SegmentSearch zDip = dip5k_z;
  QuickSearch(zDip, z, index);
  SegmentSearch xDip = zDip.segments[index];
  if(!QuickSearch(xDip, x, index)) return false;
  SegmentSearch yDip = xDip.segments[index];
  if(!QuickSearch(yDip, y, index)) return false;
  formulaId = ((ushort*)yDip.segments)[index];
  //printf("  formulaId: %d\n", formulaId);
  return true;
}

static inline float GetBz(const float xyz[3]) {
  ushort fid;
  if (!GetSegmentDip(xyz, fid)) return NAN;
  return dip5k_params[fid].bz(xyz);
}

bool Field(const float xyz[3], float bxyz[3]) {
  ushort fid;
  if (!GetSegmentDip(xyz, fid)) return false;
  dip5k_params[fid].bxyz(xyz, bxyz);
  return true;
}

static inline void UnsafeQuickSearch(const SegmentSearch ss, const float z, ushort &id) {
  const int index = floor((z - ss.offset) * ss.factor);
  SegmentEnd se = ss.slices[index];
  id = se.index + (z < se.endPos ? 0 : 1);
}

static inline void UnsafeGetSegmentDip(const float xyz[3], ushort &formulaId) { // remove bool
  const float x = xyz[0], y = xyz[1], z = xyz[2];
  ushort index;
  QuickSearch(dip5k_z, z, index);
  SegmentSearch xDip = dip5k_z.segments[index];
  QuickSearch(xDip, x, index);
  SegmentSearch yDip = xDip.segments[index];
  QuickSearch(yDip, y, index);
  formulaId = ((ushort*)yDip.segments)[index];
}

void UnsafeField(const float xyz[3], float bxyz[3]) {
  ushort fid;
  UnsafeGetSegmentDip(xyz, fid);
  dip5k_params[fid].bxyz(xyz, bxyz);
}

static inline void QuickSearch2(const SegmentSearch ss, const float z, ushort &id) {
  const int index = floor((z - ss.offset) * ss.factor);
  if (index > ss.nDivision) return;
  SegmentEnd se = ss.slices[index];
  id = se.index + (z < se.endPos ? 0 : 1);
}

static inline void GetSegmentDip2(const float xyz[3], ushort &formulaId) {
  const float x = xyz[0], y = xyz[1], z = xyz[2];
  ushort index;
  SegmentSearch zDip = dip5k_z;
  QuickSearch(zDip, z, index);
  SegmentSearch xDip = zDip.segments[index];
  if(!QuickSearch(xDip, x, index)) return;
  SegmentSearch yDip = xDip.segments[index];
  if(!QuickSearch(yDip, y, index)) return;
  formulaId = ((ushort*)yDip.segments)[index];
}

void Field2(const float xyz[3], float bxyz[3]) {
  ushort fid;
  if (!GetSegmentDip(xyz, fid)) return;
  dip5k_params[fid].bxyz(xyz, bxyz);
}

int main(int argc, char const *argv[]) {
  std::default_random_engine generator;
  std::uniform_real_distribution<float> distribution(-1760, -536);
  std::vector<float> vec;
  size_t N = 50000000;
  for (size_t i = 0; i < N; i++) {
    vec.push_back(distribution(generator));
  }
  float xyz[] = {0, 0, 0};
  ushort fid = 11111;
  //float bz = -999;
  float bxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    xyz[2] = vec[i];
    //GetSegmentDip(xyz, fid);
    //GetBz(xyz, bz);
    Field2(xyz, bxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", xyz[0], xyz[1], xyz[2]);
  //printf("%d\n", fid);
  //printf("%f\n", bz);
  printf("%f %f %f\n", bxyz[0], bxyz[1], bxyz[2]);
  return 0;
}
