#include <stdlib.h>
#include <caml/mlvalues.h>

#if defined(__GNUC__)
#if ARCH_INT32_TYPE == long
#define int32_clz __builtin_clzl
#else /* ARCH_INT32_TYPE == long */
#define int32_clz __builtin_clz
#endif /* ARCH_INT32_TYPE == long */

#define int64_clz __builtin_clzll

#else /* defined(__GNUC__) */
#ifdef _MSC_VER
#include <intrin.h>
#pragma intrinsic(_BitScanReverse)

int naive_int64_clz(uint64_t v)
{
  unsigned long n;
#ifdef ARCH_SIXTYFOUR
  if (_BitScanReverse64(&n, v)) return 63-n;
  else return 64;
#else
  /* _BitScanReverse64 is not supported */
  if ((v >> 32) == 0)
    {
      if (_BitScanReverse(&n,v)) return 63-n;
      else return 64;
    }
  else
    {
      _BitScanReverse(&n,(v>>32));
      return 31-n;
    }
#endif
}

int naive_int32_clz(uint32_t v)
{
  unsigned long n;
  if (_BitScanReverse(&n, v))
#ifdef ARCH_SIXTYFOUR
    return 63 - n;
#else
    return 31 - n;
#endif
  else return 32;
}


#define int32_clz naive_int32_clz
#define int64_clz naive_int64_clz
#endif /* _MSC_VER */
#endif /* defined(__GNUC__) */

static int wrap_int32_clz(uint32_t x)
{
  int res;
  /* builtin_clz on input 0 is undefined */
  if (x == 0) res = 32;
  else
    {
      res = int32_clz(x);
#ifdef ARCH_SIXTYFOUR
      res -= 32;
#endif
    }
  return res;
}

static int wrap_int64_clz(uint64_t x)
{
  int res;
  /* builtin_clz on input 0 is undefined */
  if (x == 0) res = 64;
  else res = int64_clz(x);
  return res;
}

CAMLprim value stub_untagged_int_clz(value v1)
{
#ifdef ARCH_SIXTYFOUR
  return wrap_int64_clz((uint64_t)v1);
#else
  return wrap_int32_clz((uint32_t)v1);
#endif
}

CAMLprim value stub_int_clz(value v1)
{
  /* Do not use Long_val(v1) conversion and preserve the tag. It
     guarantees that the input to builtin_clz is non-zero, to guard
     against versions of builtin_clz that are undefined for intput 0.
     The tag does not change the number of leading zeros.
   */
#ifdef ARCH_SIXTYFOUR
  return Val_long(int64_clz((uint64_t)v1));
#else
  return Val_long(int32_clz((uint32_t)v1));
#endif
}

CAMLprim int32_t stub_int32_clz_unboxed(int32_t v)
{ return wrap_int32_clz((uint32_t) v); }

CAMLprim value stub_int32_clz_unboxed_tag(int32_t v)
{ return Val_long(wrap_int32_clz((uint32_t) v)); }

CAMLprim value stub_int32_clz(value v1)
{ return Val_long(wrap_int32_clz((uint32_t)Int32_val(v1))); }

CAMLprim int64_t stub_int64_clz_unboxed(int64_t v)
{ return wrap_int64_clz((uint64_t) v); }

CAMLprim value stub_int64_clz_unboxed_tag(int64_t v)
{ return Val_long(wrap_int64_clz((uint64_t) v)); }

CAMLprim value stub_int64_clz(value v1)
{ return Val_long(wrap_int64_clz((uint64_t) Int64_val(v1))); }

CAMLprim intnat stub_nativeint_clz_unboxed(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return wrap_int64_clz((uint64_t) v);
#else
  return wrap_int32_clz((uint32_t) v);
#endif
}

CAMLprim value stub_nativeint_clz_unboxed_tag(intnat v)
{
#ifdef ARCH_SIXTYFOUR
  return Val_long(wrap_int64_clz((uint64_t) v));
#else
  return Val_long(wrap_int32_clz((uint32_t) v));
#endif
}

CAMLprim value stub_nativeint_clz(value v1)
{
#ifdef ARCH_SIXTYFOUR
  return Val_long(wrap_int64_clz((uint64_t) Int64_val(v1)));
#else
  return Val_long(wrap_int32_clz((uint32_t) Int32_val(v1)));
#endif
}


CAMLprim int stub_never(int v)
{
  return 0;
}
