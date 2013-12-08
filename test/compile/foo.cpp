#include <iostream>

#include "bar.h"


int main ()
{
  int a, b;

  std::cout << "Enter a: ";
  std::cin >> a;
  std::cout << std::endl;

  std::cout << "Enter b: ";
  std::cin >> b;
  std::cout << std::endl;

  std::cout << "Result : " << add (a, b) << std::endl;

  return 0;

}
