void printer_return(int i) {
  std::string output = "";

  switch (i) {
  case 0:
    output = "0";
    break;
  case 1:
    std::cout << "1" << std::endl;
    return;
  default:
    break;
  }

  std::cout << output << std::endl;
}
