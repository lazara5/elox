// This Source Code Form is subject to the terms of the
// Mozilla Public License, v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

#define ALPHA_MASK 0x1
#define DIGIT_MASK 0x2
#define HEX_MASK   0x4
#define WS_MASK    0x8

#define A ALPHA_MASK
#define D DIGIT_MASK
#define H HEX_MASK
#define W WS_MASK
#define AH A|H
#define DH D|H
