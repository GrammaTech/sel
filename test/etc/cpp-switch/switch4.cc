void printer_leading_default(int i) {
  std::string output = "";

  switch (i) {
  default:
  case 0:
    output = "0";
    break;
  case 1:
    std::cout << "1" << std::endl;
    return;
  }

  std::cout << output << std::endl;
}
