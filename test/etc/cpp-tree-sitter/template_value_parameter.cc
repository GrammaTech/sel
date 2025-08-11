template <typename T, int max> class Buffer {
  T v[max];

public:
  Buffer() { / ... }
};

Buffer<char, 128> cbuf;
Buffer<int, 5000> ibuf;
Buffer<Record, 8> rbuf;
