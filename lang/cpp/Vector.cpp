
#include "Vector.h"

Vector::Vector(int s)
  :elem {new double[s]}, sz {s}
{
}

double& operator[](int s)
{
  return elem[i];
}

int Vector::size()
{
  return sz;
}
