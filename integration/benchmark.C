#include "AliMagFast.h"
#include "AliMagF.h"
#include "AliMagWrapCheb.h"
#include <random>
#include <ctime>

int benchmark() {
  AliMagFast *m = new AliMagFast();
  AliMagF   *m0 = new AliMagF("m0","m0");
  AliMagWrapCheb *mm = m0->GetMeasuredMap();

  std::default_random_engine g;
  std::uniform_real_distribution<float>
    dipxy(-330, 330), dipz(-1760, -535.3), solxy(-500, 500), solz(-550, 550);
  std::vector<float> px, py, pz;
  std::vector<double> dx, dy, dz;
  int N = 5000000;
  // dipole
  for (int i = 0; i < N;) {
    Double_t dxyz[] = {dipxy(g), dipxy(g), dipz(g)};
    if (mm->GetParamDip(mm->FindDipSegment(dxyz))->IsInside(dxyz)) {
      px.push_back((float)dxyz[0]);
      py.push_back((float)dxyz[1]);
      pz.push_back((float)dxyz[2]);
      dx.push_back(dxyz[0]);
      dy.push_back(dxyz[1]);
      dz.push_back(dxyz[2]);
      i++;
    }
  }

double resDip;
{
  printf("START AliMagFast Dipole\n");
  float fxyz[] = {0, 0, 0};
  float fbxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    fxyz[0] = px[i];
    fxyz[1] = py[i];
    fxyz[2] = pz[i];
    m->Field(fxyz, fbxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", fxyz[0], fxyz[1], fxyz[2]);
  printf("%f %f %f\n", fbxyz[0], fbxyz[1], fbxyz[2]);
  resDip = ms;
}

{
  printf("START AliMagF Dipole\n");
  double dxyz[] = {0, 0, 0};
  double dbxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    dxyz[0] = dx[i];
    dxyz[1] = dy[i];
    dxyz[2] = dz[i];
    m0->Field(dxyz, dbxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", dxyz[0], dxyz[1], dxyz[2]);
  printf("%f %f %f\n", dbxyz[0], dbxyz[1], dbxyz[2]);
  resDip = ms/resDip;
}
printf("Speedup: %f\n", resDip);

// solenoid
std::vector<float> fx, fy, fz;
std::vector<double> sx, sy, sz;
for (int i = 0; i < N;) {
  Double_t dxyz[] = {solxy(g), solxy(g), solz(g)};
  if (mm->GetParamSol(mm->FindSolSegment(dxyz))->IsInside(dxyz)) {
    fx.push_back((float)dxyz[0]);
    fy.push_back((float)dxyz[1]);
    fz.push_back((float)dxyz[2]);
    sx.push_back(dxyz[0]);
    sy.push_back(dxyz[1]);
    sz.push_back(dxyz[2]);
    i++;
  }
}

double resSol;
{
  printf("START AliMagFast Solenoid\n");
  float fxyz[] = {0, 0, 0};
  float fbxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    fxyz[0] = fx[i];
    fxyz[1] = fy[i];
    fxyz[2] = fz[i];
    m->Field(fxyz, fbxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", fxyz[0], fxyz[1], fxyz[2]);
  printf("%f %f %f\n", fbxyz[0], fbxyz[1], fbxyz[2]);
  resSol = ms;
}

{
  printf("START AliMagF Solenoid\n");
  double dxyz[] = {0, 0, 0};
  double dbxyz[] = {0, 0, 0};
  std::clock_t start = std::clock();
  for (size_t i = 0; i < N; i++) {
    dxyz[0] = sx[i];
    dxyz[1] = sy[i];
    dxyz[2] = sz[i];
    m0->Field(dxyz, dbxyz);
  }
  double ms = (std::clock() - start) / (double)(CLOCKS_PER_SEC / 1000);
  printf("%f ms, %f us/call\n", ms, 1000*ms/(double)N);
  printf("%f %f %f\n", dxyz[0], dxyz[1], dxyz[2]);
  printf("%f %f %f\n", dbxyz[0], dbxyz[1], dbxyz[2]);
  resSol = ms/resSol;
}
printf("Speedup: %f\n", resSol);

  return 0;
}
