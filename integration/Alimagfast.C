#include "AliMagFast.h"
#include "AliMagF.h"
#include <random>
#include <ctime>

int Alimagfast() {
  AliMagF *m = new AliMagF("m", "m");
  //AliMagFast *m = new AliMagFast();
  std::default_random_engine generator;
  std::uniform_real_distribution<float> distribution(-1760, -536);
  //std::uniform_real_distribution<float> distribution(-250, 250);
  //std::uniform_real_distribution<float> xydist(-300, 300);
  std::uniform_real_distribution<float> xydist(-100, 100);
  std::vector<float> xs, ys, zs;
  size_t N = 50000000;
  for (size_t i = 0; i < N; i++) {
    float xyz[] = {xydist(generator),xydist(generator),distribution(generator)};
    xs.push_back(xyz[0]);
    ys.push_back(xyz[1]);
    zs.push_back(xyz[2]);
  }

  double xyz[] = {0, 0, 0};
  double bxyz[] = {0, 0, 0};
  ushort fid = 11111;
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    xyz[0] = xs[i];
    xyz[1] = ys[i];
    xyz[2] = zs[i];
    //m->GetSegmentDip(xyz, fid);
    m->Field(xyz, bxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", xyz[0], xyz[1], xyz[2]);
  //printf("%d\n", seg);
  //printf("%f\n", bz);
  printf("%f %f %f\n", bxyz[0], bxyz[1], bxyz[2]);
  return 0;
}
