inline const FileDescriptorTables& FileDescriptorTables::GetEmptyInstance() {
  static auto file_descriptor_tables =
    internal::OnShutdownDelete(new FileDescriptorTables());
  return *file_descriptor_tables;
}
