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

#define _FILE_OFFSET_BITS 64

#include <unistd.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

void _bs_posix_pread_into_exactly(value fd, value buf, value count,
        value offset) {

        CAMLparam4(fd, buf, count, offset);

        int c_fd = 0;
        char *c_buf = NULL;
        size_t c_count = 0;
        off_t c_offset = 0;

        size_t read = 0;

        ssize_t r = 0;

        c_fd = Int_val(fd);
        c_buf = String_val(buf);
        c_count = Long_val(count);
        c_offset = Long_val(offset);

        while(read < c_count) {
                r = pread(c_fd, c_buf + read, c_count - read, c_offset + read);

                if(r == 0) {
                        caml_raise_end_of_file();
                }

                if(r < 0) {
                        uerror("pread", Nothing);
                }

                read += r;
        }

        CAMLreturn0;
}
