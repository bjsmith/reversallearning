long long systime(std::ostream* pstream__) {
  return std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::high_resolution_clock::now()).count();
}