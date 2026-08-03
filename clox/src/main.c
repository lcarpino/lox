#include "clox/chunk.h"
#include "clox/common.h"
#include "clox/debug.h"

int main(int argc, const char* argv[]) {
  Chunk chunk;
  initChunk(&chunk);
  writeChunk(&chunk, OP_RETURN);

  disassembleChunk(&chunk, "test chunk");
  freeChunk(&chunk);
  return 0;
}
