#include <stdio.h>
#include <stdint.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value Caml_set_size_unsafe(value v_buf, value v_size){
  CAMLparam2(v_buf, v_size);
  char *  buf = String_val(v_buf);
  int  * ibuf = (int*) buf;
  int  size = Long_val(v_size);
  ibuf[0] = size;
  CAMLreturn(Val_unit);
}

value Caml_size_from_unsafe(value v_buf, value v_pos){
  CAMLparam2(v_buf,v_pos);
  char* buf = String_val(v_buf);
  int pos = Long_val(v_pos);
  int* where = (int*) &buf[pos];
  int size = * where;
  value v_size = Val_long(size);
  CAMLreturn(v_size);
}
