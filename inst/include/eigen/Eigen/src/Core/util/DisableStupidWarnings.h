#ifndef EIGEN_WARNINGS_DISABLED
#define EIGEN_WARNINGS_DISABLED

// Eigen upstream suppresses compiler diagnostics in this header. Those
// suppressions use non-portable pragmas, which R CMD check rejects. They are
// disabled in this vendored copy; this does not change Eigen functionality.
#if defined(_MSC_VER)
#ifndef _SILENCE_CXX23_DENORM_DEPRECATION_WARNING
#define EIGEN_REENABLE_CXX23_DENORM_DEPRECATION_WARNING 1
#define _SILENCE_CXX23_DENORM_DEPRECATION_WARNING
#endif
#endif

#else
#ifndef EIGEN_WARNINGS_DISABLED_2
#define EIGEN_WARNINGS_DISABLED_2
#elif defined(EIGEN_INTERNAL_DEBUGGING)
#error "Do not include \"DisableStupidWarnings.h\" recursively more than twice!"
#endif

#endif  // not EIGEN_WARNINGS_DISABLED
