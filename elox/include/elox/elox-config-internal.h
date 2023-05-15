#ifndef ELOX_ELOX_CONFIG_INTERNAL_H
#define ELOX_ELOX_CONFIG_INTERNAL_H

#include <elox-config.h>

#if defined(ELOX_CONFIG_WIN32)
#define ELOX_DIRSEP      "\\"
#else
#define ELOX_DIRSEP      "/"
#endif

#endif // ELOX_ELOX_CONFIG_INTERNAL_H
