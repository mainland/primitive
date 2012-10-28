#ifndef haskell_primitive_memops_h
#define haskell_primitive_memops_h

#include <stdlib.h>
#include <HsFFI.h>

#if defined(__SSE2__)
#include <xmmintrin.h>
#endif /* defined(__SSE2__) */

void hsprimitive_memcpy( void *dst, int doff, void *src, int soff, size_t len );
void hsprimitive_memmove( void *dst, int doff, void *src, int soff, size_t len );

void hsprimitive_memset_Word8 (HsWord8 *, int, int, HsWord);
void hsprimitive_memset_Word16 (HsWord16 *, int, int, HsWord);
void hsprimitive_memset_Word32 (HsWord32 *, int, int, HsWord);
void hsprimitive_memset_Word64 (HsWord64 *, int, int, HsWord64);
void hsprimitive_memset_Word (HsWord *, int, int, HsWord);
void hsprimitive_memset_Ptr (HsPtr *, int, int, HsPtr);
void hsprimitive_memset_Float (HsFloat *, int, int, HsFloat);
void hsprimitive_memset_Double (HsDouble *, int, int, HsDouble);
void hsprimitive_memset_Char (HsChar *, int, int, HsChar);

#if defined(__SSE2__)
void hsprimitive_memset_FloatX4 (__m128 *, int, int, __m128);
void hsprimitive_memset_DoubleX2 (__m128d *, int, int, __m128d);
void hsprimitive_memset_Int32X4 (__m128i *, int, int, __m128i);
void hsprimitive_memset_Int64X2 (__m128i *, int, int, __m128i);
#endif /* defined(__SSE2__) */

#endif

