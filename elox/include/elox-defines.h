// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#ifndef ELOX_ELOX_DEFINES_H
#define ELOX_ELOX_DEFINES_H

#if !defined(ELOX_PRINTF)
#if defined(__GNUC__)
#define ELOX_PRINTF(n, m) __attribute__ ((format(printf, n, m)))
#else
#define ELOX_PRINTF(n, m)
#endif // __GNUC__
#endif // ELOX_PRINTF

#endif // ELOX_ELOX_DEFINES_H
