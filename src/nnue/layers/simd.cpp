#include <iostream>
#include <cstdlib>

#if defined(__linux__) && defined (USE_AMX)

#include <sys/syscall.h>
#include <unistd.h>

#endif

#if defined (__linux__) && defined (USE_AMX)

static const bool s_enable_amx = []() {

#define ARCH_REQ_XCOMP_PERM 0x1023
#define XFEATURE_XTILEDATA 18

  // A syscall required to enable AMX support on linux kernels.
  if (syscall(SYS_arch_prctl, ARCH_REQ_XCOMP_PERM, XFEATURE_XTILEDATA))
  {
    std::cerr << "Failed to enable XFEATURE_XTILEDATA\n\n";
    std::exit(EXIT_FAILURE);
  }

  return true;
}();

#endif
