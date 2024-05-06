#ifndef ELOX_ELOX_CONFIG_INTERNAL_H
#define ELOX_ELOX_CONFIG_INTERNAL_H

#include <elox-config.h>

#if defined(ELOX_CONFIG_WIN32)
#define ELOX_DIRSEP      "\\"
#else
#define ELOX_DIRSEP      "/"
#endif

#define ELOX_CLASS_DISPLAY_SIZE (8)
#define ELOX_MAX_SUPERTYPES (256)
#define ELOX_MAX_FRAMES (64)
#define ELOX_MAX_CATCH_HANDLER_FRAMES (16)
#define ELOX_MAX_ARGS (65535)

#endif // ELOX_ELOX_CONFIG_INTERNAL_H
