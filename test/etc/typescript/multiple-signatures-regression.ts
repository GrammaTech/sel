class FileOperationEvent {

  isOperation(operation: FileOperation.DELETE): boolean;
  isOperation(operation: FileOperation.MOVE | FileOperation.COPY | FileOperation.CREATE): this is { readonly target: IFileStatWithMetadata };
  isOperation(operation: FileOperation): boolean {
  }
}
