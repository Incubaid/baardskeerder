/*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2011 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdio.h>

#define cpuid(func,ax,bx,cx,dx)					\
  __asm__ __volatile__ ("cpuid":				\
			"=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func));

int has_sse_2 = 0;
int has_sse_3 = 0;
int has_sse_4_2 = 0;

void __cpu_detect(void) __attribute__((constructor));

void __cpu_detect(void)
{
  int a,b,c,d;
  cpuid(1, a, b, c, d);

  has_sse_2 = ((1 << 26) & d) != 0;
  has_sse_3 = ((1 << 0) & c) != 0;
  has_sse_4_2 = ((1 << 20) & c) != 0;

#if 0
  if (has_sse_2) {
    fprintf(stderr, "SSE2 detected.\n");
  }
  if (has_sse_3) {
    fprintf(stderr, "SSE3 detected.\n");
  }
  if (has_sse_4_2) {
    fprintf(stderr, "SSE4.2 detected. %x\n", c);
  }
#endif

}

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/memory.h>

value ml_has_sse2(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(has_sse_2?Val_true:Val_false);
}

value ml_has_sse3(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(has_sse_3?Val_true:Val_false);
}

value ml_has_sse4_2(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(has_sse_4_2?Val_true:Val_false);
}
