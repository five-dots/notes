#include <iostream>
#include <vector>

using namespace std;

int main() {
  vector<int> v(10, 1);

  for (auto i = 0; i <= v.size(); ++i) {
    int val = v[i];
    cout << val << "\n";
  }
}
