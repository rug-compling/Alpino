#include <algorithm>
#include <cmath>
#include <iostream>

template<typename T>
T vmin(T v1, T v2) {
  return std::min(v1, v2);
}

template<typename T, typename... Args>
T vmin(T v1, Args... args) {
  return std::min(v1, vmin(args...));
}

template<typename T>
T logAdd_(T min, T arg) {
  return std::exp(min - arg);
}

template<typename T, typename... Args>
T logAdd_(T min, T arg, Args... args) {
  return std::exp(min - arg) + logAdd_(min, args...);
}

template<typename T, typename... Args>
T logAdd(T arg, Args... args) {
  auto min = vmin(arg, args...);
  return min - std::log(logAdd_(min, arg, args...));
}

