#include "AliMagF.h"
#include "AliMagWrapCheb.h"
#include <random>
#include <ctime>

int alimagf(){
  AliMagF *m = new AliMagF("m","m");
  AliMagWrapCheb *mm = m->GetMeasuredMap();
  std::default_random_engine generator;
  std::uniform_real_distribution<float> distribution(-1760, -536);
  std::vector<float> vec;
  size_t N = 50000000;
  for (size_t i = 0; i < N; i++) {
    vec.push_back(distribution(generator));
  }
  double xyz[] = {0, 0, 0};
  //ushort fid = 11111;
  int seg = 11111;
  //float bz = -999;
  double bxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    xyz[2] = vec[i];
    //GetSegmentDip(xyz, fid);
    //GetBz(xyz, bz);
    //seg = mm->FindDipSegment(xyz);
    mm->Field(xyz, bxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", xyz[0], xyz[1], xyz[2]);
  //printf("%d\n", seg);
  //printf("%f\n", bz);
  printf("%f %f %f\n", bxyz[0], bxyz[1], bxyz[2]);
  return 0;
}
