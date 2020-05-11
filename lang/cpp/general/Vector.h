// Vecotr.h

class Vector {
 public:
  Vector(int s);
  double& operator[](int s);
  int size();
 private:
  double* elem;
  int sz;
};
