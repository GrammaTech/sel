template <typename T, T default_value = T()> class Vec {};

Vec<int, 42> c1;
Vec<int> c11;
Vec<string, "fortytwo"> c2;
Vec<string> c22;
