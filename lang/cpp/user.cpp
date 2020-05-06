#include <Vector.h>
#include <cmath>

using namespace std;

double sqrt_sum(Vector& v)
{
  double sum = 0;
  for (int i=0; i!=v.size(); ++i)
    sum+=sqrt(v[i]);
  return sum;
}

void init_v(Vector& v) {
  for (int i=0; i!=v.size(); ++i)
    cin >> v[i];
}

int main()
{
  int hoge[] {};

  Vector v(10);
  init_v();
  double s = sqrt_sum(v);

  cout << s << "\n";
}
